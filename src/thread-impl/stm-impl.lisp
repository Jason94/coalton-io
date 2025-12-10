(cl:in-package :cl-user)
(defpackage :io/stm/stm-impl
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/experimental/do-control-core
   #:io/utils
   #:io/monad-io
   #:io/exception
   #:io/thread-impl/runtime
   #:io/thread-impl/stm-types
   )
  (:local-nicknames
   (:opt #:coalton-library/optional)
   (:i #:coalton-library/iterator)
   (:c #:coalton-library/cell)
   (:lk  #:coalton-threads/lock)
   (:cv  #:coalton-threads/condition-variable)
   ;; (:ax #:alexandria)
   )
  (:export
   #:TVar
   #:STM
   #:new-tvar%
   #:read-tvar%
   #:write-tvar%
   #:modify-tvar%
   #:retry%
   #:or-else%
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
  (declare flatmap-tx% (Monad :io => STM :io :a -> (:a -> STM :io :b) -> STM :io :b))
  (define (flatmap-tx% tx fa->stmb)
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
    (define >>= flatmap-tx%))

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

  (declare tx-logged-write-value% (TxData% -> :a -> Optional Anything))
  (define (tx-logged-write-value% tx-data key)
    (match (logged-write-value% (.write-log tx-data) key)
      ((Some val)
       (Some val))
      ((None)
       (match (c:read (.parent-tx tx-data))
         ((Some parent-tx)
          (tx-logged-write-value% parent-tx key))
         ((None)
          None)))))

  (inline)
  (declare log-write-value% (WriteHashTable% -> TVar :a -> :a -> Unit))
  (define (log-write-value% write-log addr val)
    (lisp :a (write-log addr val)
      (cl:setf (cl:gethash addr write-log) val))
    Unit)

  (inline)
  (declare commit-logged-writes (WriteHashTable% -> Unit))
  (define (commit-logged-writes write-log)
    "Actually set the TVar's value to its corresponding logged write value."
    (lisp :a (write-log)
      (cl:loop :for addr :being :the :hash-keys :of write-log
         :using (hash-value value)
         :do (call-coalton-function set-tvar% addr value))))

  (inline)
  (declare new-tx-data% (a::Word -> TxData%))
  (define (new-tx-data% initial-snapshot)
    (TxData% (c:new initial-snapshot)
             (c:new Nil)
             (new-write-hash-table%)
             (c:new None)))

  (inline)
  (declare child-tx-data% (TxData% -> TxData%))
  (define (child-tx-data% tx-data)
    (let new-tx-data = (new-tx-data% (c:read (.lock-snapshot tx-data))))
    (c:write! (.parent-tx new-tx-data) (Some tx-data))
    new-tx-data)

  (declare merge-read-log-into-parent-tx% (TxData% -> Unit))
  (define (merge-read-log-into-parent-tx% tx-data)
    "Merge only child tx's snapshot and read log into its parents.
For safety, disconnects the transactions when done."
    (let parent-tx? = (c:read (.parent-tx tx-data)))
    (match parent-tx?
      ((None) Unit)
      ((Some parent-tx)
       (c:write! (.lock-snapshot parent-tx)
                 (c:read (.lock-snapshot tx-data)))
       (c:write! (.read-log parent-tx)
                 (<> (c:read (.read-log tx-data))
                     (c:read (.read-log parent-tx))))
       (c:write! (.parent-tx tx-data) None)
       Unit)))

  (declare merge-into-parent-tx% (TxData% -> Unit))
  (define (merge-into-parent-tx% tx-data)
    "Merge child tx's snapshot, read log, and write log into its parents.
For safety, disconnects the transactions when done."
    (let parent-tx? = (c:read (.parent-tx tx-data)))
    (match parent-tx?
      ((None) Unit)
      ((Some parent-tx)
       (c:write! (.lock-snapshot parent-tx)
                 (c:read (.lock-snapshot tx-data)))
       (c:write! (.read-log parent-tx)
                 (<> (c:read (.read-log tx-data))
                     (c:read (.read-log parent-tx))))
       (let tx-write-log = (.write-log tx-data))
       (let pt-write-log = (.write-log parent-tx))
       (lisp Void (tx-write-log pt-write-log)
         (cl:loop :for addr :being :the :hash-keys :of tx-write-log
            :using (hash-value value)
            :do (cl:setf (cl:gethash addr pt-write-log) value)))
       (c:write! (.parent-tx tx-data) None)
       Unit)))

  (inline)
  (declare cached-snapshot (TxData% -> a::Word))
  (define (cached-snapshot tx-data)
    (c:read (.lock-snapshot tx-data)))

  (declare iterate-read-log (TxData% -> i:Iterator ReadEntry%))
  (define (iterate-read-log tx-data)
    (let base-iter = (i:into-iter (c:read (.read-log tx-data))))
    (match (c:read (.parent-tx tx-data))
      ((None)
       base-iter)
      ((Some parent-tx)
       (i:chain! base-iter (iterate-read-log parent-tx)))))

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
          (let read-log-iter = (iterate-read-log tx-data))
          (let check =
            (rec %% ((next-read? (i:next! read-log-iter)))
              (match next-read?
                ((None)
                 (if (== start-time (get-global-time))
                     (Some (TxContinue% start-time))
                     None))
                ((Some read-entry)
                 (if (not (unsafe-pointer-eq?
                           (read-entry-current-val% read-entry)
                           (read-entry-cached-val% read-entry)))
                     (Some TxAbort%)
                     (%% (i:next! read-log-iter)))))))
          (match check
            ((None) (%))
            ((Some x) x))))))

  (declare inner-read-tvar% (TVar :a -> TxData% -> TxResult% :a))
  (define (inner-read-tvar% tvar tx-data)
    (match (tx-logged-write-value% tx-data tvar)
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
                (% (tvar-value% tvar)))))))))

  (inline)
  (declare inner-write-tvar% (TVar :a -> :a -> TxData% -> TxResult% Unit))
  (define (inner-write-tvar% tvar val tx-data)
    (TxSuccess
     (log-write-value% (.write-log tx-data) tvar val)))

  (inline)
  (declare read-tvar% (MonadIo :m => TVar :a -> STM :m :a))
  (define (read-tvar% tvar)
    (STM%
     (fn (tx-data)
       (wrap-io
         (inner-read-tvar% tvar tx-data)))))

  (inline)
  (declare write-tvar% (MonadIo :m => TVar :a -> :a -> STM :m Unit))
  (define (write-tvar% tvar val)
    (STM%
     (fn (tx-data)
       (wrap-io (inner-write-tvar% tvar val tx-data)))))

  (declare modify-tvar% (MonadIo :m => TVar :a -> (:a -> :a) -> STM :m :a))
  (define (modify-tvar% tvar f)
    (STM%
     (fn (tx-data)
       (wrap-io
         (match (inner-read-tvar% tvar tx-data)
           ((TxRetryAfterWrite)
            TxRetryAfterWrite)
           ((TxFailed)
            TxFailed)
           ((TxSuccess val)
            (let result = (f val))
            ;; Respect non success status out of the write
            (map (const result)
                 (inner-write-tvar% tvar result tx-data))))))))

  (declare tx-commit-io% (MonadIo :m => TxData% -> :m Boolean))
  (define (tx-commit-io% tx-data)
    (wrap-io
      (when (opt:some? (c:read (.parent-tx tx-data)))
        (error "Cannot commit a nested transaction."))
      (if (read-only? tx-data)
          True
          (progn
            (let result = (c:new True))
            (mask-current-thread!%)
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
              (unmask-current-thread!%)
              Unit)
            (c:read result)))))

  (inline)
  (declare retry% (MonadIo :m => STM :m :a))
  (define retry%
    (STM%
     (fn (_)
      (wrap-io
        TxRetryAfterWrite))))

  (declare or-else% (MonadIo :m => STM :m :a -> STM :m :a -> STM :m :a))
  (define (or-else% tx-a tx-b)
    (STM%
     (fn (tx-data)
       (do
         (let a-tx-data = (child-tx-data% tx-data))
         (matchM (run-stm% a-tx-data tx-a)
           ((TxFailed)
            (merge-into-parent-tx% a-tx-data)
            (pure TxFailed))
           ((TxSuccess val)
            (merge-into-parent-tx% a-tx-data)
            (pure (TxSuccess val)))
           ((TxRetryAfterWrite)
            (merge-read-log-into-parent-tx% a-tx-data)
            (run-stm% tx-data tx-b)))))))

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
