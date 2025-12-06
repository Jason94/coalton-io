(cl:in-package :cl-user)
(defpackage :io/stm/stm-impl
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/experimental/do-control-core
   #:io/utils
   #:io/monad-io
   #:io/exception
   )
  (:local-nicknames
   (:c #:coalton-library/cell)
   (:a #:coalton-threads/atomic)
   (:lk  #:coalton-threads/lock)
   (:cv  #:coalton-threads/condition-variable)
   )
  (:export
   #:TVar
   #:STM
   #:new-tvar%
   #:read-tvar%
   #:write-tvar%
   #:retry%
   #:run-tx%
   )
  )
(in-package :io/stm/stm-impl)

(cl:defmacro mem-barrier ()
  `(lisp Void ()
     (sb-thread:barrier (:read))))

(named-readtables:in-readtable coalton:coalton)

;; This is an implementation of the NOrec STM algorithm, described:
;; https://pages.cs.wisc.edu/~markhill/restricted/757/ppopp10_norec.pdf

(coalton-toplevel

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;          Main STM Types           ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (derive Eq)
  (repr :transparent)
  (define-type (TVar :a)
    (TVar% (c:Cell :a)))

  (inline)
  (declare unwrap-tvar% (TVar :a -> c:Cell :a))
  (define (unwrap-tvar% (TVar% a))
    a)

  (inline)
  (declare set-tvar% (TVar :a -> :a -> Unit))
  (define (set-tvar% tvar val)
    (c:write! (unwrap-tvar% tvar) val)
    Unit)

  (inline)
  (declare tvar-value% (TVar :a -> :a))
  (define (tvar-value% tvar)
    (c:read (unwrap-tvar% tvar)))

  (define-type (TxResult% :a)
    (TxSuccess :a)
    TxRetryAfterWrite
    TxFailed)

  (define-instance (Functor TxResult%)
    (inline)
    (define (map f result)
      (match result
        ((TxSuccess a)
         (TxSuccess (f a)))
        ((TxRetryAfterWrite)
         TxRetryAfterWrite)
        ((TxFailed)
         TxFailed))))

  (repr :native cl:cons)
  (define-type ReadEntry%)

  (repr :native cl:hash-table)
  (define-type WriteHashTable%)

  (define-struct TxData%
    (lock-snapshot (c:cell a::Word))
    (read-log (c:cell (List ReadEntry%)))
    (write-log WriteHashTable%))

  (repr :transparent)
  (define-type (STM :io :a)
    (STM% (TxData% -> :io (TxResult% :a))))

  (inline)
  (declare unwrap-stm% (STM :io :a -> (TxData% -> :io (TxResult% :a))))
  (define (unwrap-stm% (STM% f-tx))
    f-tx)

  (inline)
  (declare run-stm% (TxData% -> STM :io :a -> :io (TxResult% :a)))
  (define (run-stm% tx-data tx)
    ((unwrap-stm% tx) tx-data))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     Internal & Debug Helpers      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(coalton-toplevel
  (inline)
  (declare tx-const-io!% (MonadIo :m => :m (TxResult% :a) -> STM :m :a))
  (define (tx-const-io!% io-op)
    (STM%
     (fn (_)
       io-op)))

  (inline)
  (declare tx-io!% (MonadIo :m => :m :a -> STM :m :a))
  (define (tx-io!% io-op)
    "Not safe to use generally. Useful for writing unit-tests,
for purposes like writing to Var's and using MVar's to coordinate
threads inside of transactions to simulate different concurrent
conditions. DONT USE THIS!"
    (STM%
     (fn (_)
       (map TxSuccess io-op))))
  )

(coalton-toplevel
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;           STM Instances           ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-instance (Functor :io => Functor (STM :io))
    (inline)
    (define (map f tx)
      (STM%
       (fn (tx-data)
         (map (map f)
              (run-stm% tx-data tx))))))

  (inline)
  (declare pure-tx% (Applicative :io => :a -> STM :io :a))
  (define (pure-tx% val)
    (STM%
     (fn (_)
       (pure (TxSuccess val)))))

  (declare lifta2-tx% (Monad :io => (:a -> :b -> :c) -> STM :io :a -> STM :io :b -> STM :io :c))
  (define (lifta2-tx% fa->b->c tx-a tx-b)
    (STM%
     (fn (tx-data)
       (matchM (run-stm% tx-data tx-a)
         ((TxFailed)
          (pure TxFailed))
         ((TxRetryAfterWrite)
          (pure TxRetryAfterWrite))
         ((TxSuccess val-a)
          (do-matchM (run-stm% tx-data tx-b)
            ((TxFailed)
             (pure TxFailed))
            ((TxRetryAfterWrite)
             (pure TxRetryAfterWrite))
            ((TxSuccess val-b)
             (pure (TxSuccess (fa->b->c val-a val-b))))))))))

  (define-instance (Monad :io => Applicative (STM :io))
    (inline)
    (define pure pure-tx%)
    (define lifta2 lifta2))

  (inline)
  (declare flatmax-tx% (Monad :io => STM :io :a -> (:a -> STM :io :b) -> STM :io :b))
  (define (flatmax-tx% tx fa->stmb)
    (STM%
     (fn (tx-data)
       (matchM (run-stm% tx-data tx)
         ((TxFailed)
          (pure TxFailed))
         ((TxRetryAfterWrite)
          (pure TxRetryAfterWrite))
         ((TxSuccess val-a)
          (run-stm% tx-data
                    (fa->stmb val-a)))))))

  (define-instance (Monad :io => Monad (STM :io))
    (inline)
    (define >>= flatmax-tx%))

  (define-instance ((MonadException :io) (MonadIo :io) => MonadException (STM :io))
    (inline)
    (define (raise e)
      (tx-const-io!% (raise e)))
    (inline)
    (define (raise-dynamic dyn-e)
      (tx-const-io!% (raise-dynamic dyn-e)))
    (inline)
    (define (reraise tx catch-tx)
     (STM%
      (fn (tx-data)
        (reraise (run-stm% tx-data tx)
                 (map (run-stm% tx-data) catch-tx)))))
    (inline)
    (define (handle tx catch-tx)
      (STM%
       (fn (tx-data)
         (handle (run-stm% tx-data tx)
                 (map (run-stm% tx-data) catch-tx)))))
    (inline)
    (define (handle-all tx catch-tx)
      (STM%
       (fn (tx-data)
         (handle-all (run-stm% tx-data tx)
                     (map (run-stm% tx-data) catch-tx)))))
    (inline)
    (define (try-dynamic tx)
      (STM%
       (fn (tx-data)
         (map (fn (dyn-result--tx-result)
                (match dyn-result--tx-result
                  ((Err dyn-e)
                   (TxSuccess (Err dyn-e)))
                  ((Ok tx-result)
                   (match tx-result
                     ((TxFailed)
                      TxFailed)
                     ((TxRetryAfterWrite)
                      TxRetryAfterWrite)
                     ((TxSuccess val)
                      (TxSuccess (Ok val)))))))
              (try-dynamic (run-stm% tx-data tx)))))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;          Internal Types           ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (inline)
  (declare read-entry-addr% (ReadEntry% -> Anything))
  (define (read-entry-addr% entr)
    (lisp Anything (entr)
      (cl:car entr)))

  (inline)
  (declare read-entry-current-val% (ReadEntry% -> Anything))
  (define (read-entry-current-val% entr)
    (tvar-value%
     (lisp (TVar Anything) (entr)
       (cl:car entr))))

  (inline)
  (declare read-entry-cached-val% (ReadEntry% -> Anything))
  (define (read-entry-cached-val% entr)
    (lisp Anything (entr)
      (cl:cdr entr)))

  (inline)
  (declare new-write-hash-table% (Unit -> WriteHashTable%))
  (define (new-write-hash-table%)
    (lisp WriteHashTable% ()
      (cl:make-hash-table :test 'cl:eq)))

  (inline)
  (declare logged-write-value% (WriteHashTable% -> :a -> Optional Anything))
  (define (logged-write-value% write-log key)
     (lisp (Optional Anything) (write-log key)
       (cl:multiple-value-bind (val found?) (cl:gethash key write-log)
         (cl:if found?
                (Some val)
                None))))

  (inline)
  (declare log-write-value% (WriteHashTable% -> TVar :a -> :a -> Unit))
  (define (log-write-value% write-log addr val)
    (lisp :a (write-log addr val)
      (cl:setf (cl:gethash addr write-log) val))
    Unit)

  (inline)
  (declare commit-logged-writes (WriteHashTable% -> Unit))
  (define (commit-logged-writes write-log)
    "Actually set the TVar's value to their corresponding logged write value."
    (lisp :a (write-log)
      (cl:loop :for addr :being :the :hash-keys :of write-log
         :using (hash-value value)
         :do (call-coalton-function set-tvar% addr value))))

  (inline)
  (declare new-tx-data% (a::Word -> TxData%))
  (define (new-tx-data% initial-snapshot)
    (TxData% (c:new initial-snapshot)
             (c:new Nil)
             (new-write-hash-table%)))

  (inline)
  (declare cached-snapshot (TxData% -> a::Word))
  (define (cached-snapshot tx-data)
    (c:read (.lock-snapshot tx-data)))

  (inline)
  (declare log-read-value (TVar :a -> :a -> TxData% -> Unit))
  (define (log-read-value addr val tx-data)
    (c:push! (.read-log tx-data) (lisp ReadEntry% (addr val)
                                   (cl:cons addr val)))
    Unit)

  (inline)
  (declare read-only? (TxData% -> Boolean))
  (define (read-only? tx-data)
    (let write-log = (.write-log tx-data))
    (lisp Boolean (write-log)
      (cl:zerop (cl:hash-table-count write-log))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;        STM Implementation         ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (inline)
  (declare new-tvar% (MonadIo :m => :a -> :m (TVar :a)))
  (define (new-tvar% val)
    (wrap-io
      (TVar% (c:new val))))

  ;; NOTE: For now, using the coalton-threads Atomic Integer instead of
  ;; our atomics, because it's probably faster, since our atomic type doesn't
  ;; have a way to use sb-ext:atomic-incf. BUT, the MOST important thing
  ;; is that whatever we do use HAS to eventually call something that is
  ;; a memory barrier in SBCL.
  ;; https://www.sbcl.org/manual/sbcl.pdf
  (declare global-lock a:AtomicInteger)
  (define global-lock (a:new 0))

  (inline)
  (declare get-global-time (Unit -> a::Word))
  (define (get-global-time)
    "Read the global lock time and establish a memory barrier."
    (mem-barrier)
    (a:read global-lock))

  ;; NOTE: In general, NOrec wants to avoid mutex locks and CV's wherever possible.
  ;; It does this by with an AtomicInteger-backed sequence lock and compressing
  ;; the time when a write-transaction will need to hold the lock, so that it's
  ;; acceptable to just spin while a write-transaction is actually committing.
  ;;
  ;; HOWEVER, when a user retries because the observed TVars are in some bad state,
  ;; there's no guarantee that the period until the state is possibly valid (after
  ;; the next write commit to the read TVar's), so just spinning after a user-signalled
  ;; retry could potentially spin forever. The most fine-grained approach to this would
  ;; be to have a per-TVar CV, and a transaction awaits any CV in its read-log after
  ;; a manually retry. For now, we take the middle-ground and just have one global CV
  ;; that every write signals. Each user retry'd commit will do one attempt after
  ;; every write transaction.

  (define global-write-cv (cv:new))
  (define global-write-cv-lock (lk:new))

  (inline)
  (declare wait-for-write-tx!% (Unit -> Unit))
  (define (wait-for-write-tx!%)
    (lk:acquire global-write-cv-lock)
    (cv:await global-write-cv global-write-cv-lock)
    (lk:release global-write-cv-lock)
    Unit)

  (inline)
  (declare broadcast-write-cv!% (Unit -> Unit))
  (define (broadcast-write-cv!%)
    (cv:broadcast global-write-cv))

  (inline)
  (declare tx-begin-io% (MonadIo :m => Unit -> :m TxData%))
  (define (tx-begin-io%)
    (wrap-io
      (rec % ()
        (let snapshot = (a:read global-lock))
        (if (bit-odd? snapshot)
          (%)
          (new-tx-data% snapshot)))))

  (derive Eq)
  (define-type ValidateRes%
    TxAbort%
    (TxContinue% a::Word))

  (declare validate% (TxData% -> ValidateRes%))
  (define (validate% tx-data)
    (rec % ()
      (let start-time = (get-global-time))
      (if (bit-odd? start-time)
            (%)
        (progn
          (let check =
            (rec %% ((rem-reads (c:read (.read-log tx-data))))
              (match rem-reads
                ((Nil)
                 (if (== start-time (get-global-time))
                     (Some (TxContinue% start-time))
                     None))
                ((Cons read-entry next-reads)
                 (if (not (unsafe-pointer-eq?
                           (read-entry-current-val% read-entry)
                           (read-entry-cached-val% read-entry)))
                     (Some TxAbort%)
                     (%% next-reads))))))
          (match check
            ((None) (%))
            ((Some x) x))))))

  (declare read-tvar% (MonadIo :m => TVar :a -> STM :m :a))
  (define (read-tvar% tvar)
    (STM%
     (fn (tx-data)
       (wrap-io
         (match (logged-write-value% (.write-log tx-data) tvar)
           ((Some written-val)
            (TxSuccess (from-anything written-val)))
           ((None)
            (rec % ((val (progn (mem-barrier)
                                (tvar-value% tvar))))
              (if (== (cached-snapshot tx-data)
                      (a:read global-lock))
                  (progn
                    (log-read-value tvar val tx-data)
                    (TxSuccess val))
                  (match (validate% tx-data)
                    ((TxAbort%)
                     TxFailed)
                    ((TxContinue% time)
                     (c:write! (.lock-snapshot tx-data)
                               time)
                     (% (tvar-value% tvar))))))))))))

  (inline)
  (declare write-tvar% (MonadIo :m => TVar :a -> :a -> STM :m Unit))
  (define (write-tvar% tvar val)
    (STM%
     (fn (tx-data)
       (wrap-io
         (TxSuccess
          (log-write-value% (.write-log tx-data) tvar val))))))

  (declare tx-commit-io% (MonadIo :m => TxData% -> :m Boolean))
  (define (tx-commit-io% tx-data)
    (wrap-io
      (if (read-only? tx-data)
          True
          (progn
            (let result = (c:new True))
            (while (and
                    ;; Stop looping if we already need to abort.
                    (c:read result)
                    (not (a:cas! global-lock
                                 (c:read (.lock-snapshot tx-data))
                                 (1+ (c:read (.lock-snapshot tx-data))))))
              (let validate-res = (validate% tx-data))
              (match validate-res
                ((TxContinue% time)
                 (c:write! (.lock-snapshot tx-data) time)
                 Unit)
                ((TxAbort%)
                 (c:write! result False)
                 Unit)))
            (when (c:read result)
              (commit-logged-writes (.write-log tx-data))
              (a:incf! global-lock 1)
              (broadcast-write-cv!%)
              Unit)
            (c:read result)))))

  (inline)
  (declare retry% (MonadIo :m => STM :m :a))
  (define retry%
    (STM%
     (fn (_)
      (wrap-io
        TxRetryAfterWrite))))

  (declare run-tx% ((MonadIo :m) (MonadException :m) => STM :m :a -> :m :a))
  (define (run-tx% tx)
    (rec % ()
      (do
       (tx-data <- (tx-begin-io%))
       (do-matchM (run-stm% tx-data tx)
         ((TxFailed)
          (%))
         ((TxRetryAfterWrite)
          (progn
            (wait-for-write-tx!%)
            (%)))
         ((TxSuccess val)
          (commit-succeeded? <- (tx-commit-io% tx-data))
          (if commit-succeeded?
              (pure val)
              (%)))))))
  )
