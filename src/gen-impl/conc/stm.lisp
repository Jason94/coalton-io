(cl:in-package :cl-user)
(defpackage :io/gen-impl/conc/stm
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/types
   #:coalton-library/experimental/do-control-core
   #:io/utils
   #:io/classes/monad-exception
   #:io/classes/monad-io
   #:io/classes/monad-io-thread
   #:io/classes/runtime-utils
   )
  (:local-nicknames
   (:opt #:coalton-library/optional)
   (:i #:coalton-library/iterator)
   (:c #:coalton-library/cell)
   (:lk  #:coalton-threads/lock)
   (:cv  #:coalton-threads/condition-variable)
   (:at #:io/thread-impl/atomics)
   )
  (:export
   ;; Library Public
   #:TVar
   #:STM

   #:new-tvar
   #:read-tvar
   #:write-tvar
   #:swap-tvar
   #:modify-tvar
   #:modify-swap-tvar
   #:retry
   #:or-else
   #:run-tx
   #:do-run-tx

   ;; Library Private
   #:tx-io!%
   )
  )
(in-package :io/gen-impl/conc/stm)

(defmacro mem-barrier ()
  `(lisp Void ()
     (sb-thread:barrier (:read))))

(named-readtables:in-readtable coalton:coalton)

;; This is an implementation of the NOrec STM algorithm, described:
;; https://pages.cs.wisc.edu/~markhill/restricted/757/ppopp10_norec.pdf

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;          Main STM Types           ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(coalton-toplevel

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
    TxRetryAfterWrite ;; User-requested sleep until another write commit
    TxFailed) ;; Another thread wrote to an accessed TVar during our commit

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
    (lock-snapshot (c:cell Word))
    (read-log (c:cell (List ReadEntry%)))
    (write-log WriteHashTable%)
    (parent-tx (c:cell (Optional TxData%))))

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
    ;; See https://github.com/coalton-lang/coalton/issues/1730
    (inline)
    (define (>>= m f)
      (flatmap-tx% m f)))

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
  )

(coalton-toplevel

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
    (lisp Unit (write-log)
      (cl:loop :for addr :being :the :hash-keys :of write-log
         :using (hash-value value)
         :do (call-coalton-function set-tvar% addr value)
         :finally (cl:return Unit))))

  (inline)
  (declare new-tx-data% (Word -> TxData%))
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
    "Merge only the child tx's snapshot and read log into its parents'.
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
    "Merge the child tx's snapshot, read log, and write log into its parents'.
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
  (declare cached-snapshot (TxData% -> Word))
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
  (declare new-tvar% (:a -> TVar :a))
  (define (new-tvar% val)
    (TVar% (c:new val)))

  (inline)
  (declare new-tvar (MonadIo :m => :a -> :m (TVar :a)))
  (define (new-tvar val)
    "Create a mutable variable that can be used inside an atomic transaction."
    (wrap-io (new-tvar% val)))

  ;; NOTE: For now, using the coalton-threads Atomic Integer instead of
  ;; our atomics, because it's probably faster, since our atomic type doesn't
  ;; have a way to use sb-ext:atomic-incf. BUT, the MOST important thing
  ;; is that whatever we do use HAS to eventually call something that is
  ;; a memory barrier in SBCL.
  ;; https://www.sbcl.org/manual/sbcl.pdf
  ;; TODO: Upon further reading, atomic operations *do* establish a memory barrier.
  ;; It might be possible to remove the explicit memory barrier calls, which would
  ;; give the STM portability beyond SBCL.
  (declare global-lock at:AtomicInteger)
  (define global-lock (at:new-at-int 0))

  (inline)
  (declare get-global-time (Unit -> Word))
  (define (get-global-time)
    "Read the global lock time and establish a memory barrier."
    (mem-barrier)
    (at:read-at-int global-lock))

  ;; NOTE: In general, NOrec wants to avoid mutex locks and CV's wherever possible.
  ;; It does this by with an AtomicInteger-backed sequence lock and compressing
  ;; the time when a write-transaction will need to hold the lock, so that it's
  ;; acceptable to just spin while a write-transaction is actually committing.
  ;;
  ;; HOWEVER, when a user retries because the observed TVars are in some bad state,
  ;; there's no guarantee that the period until the state is possibly valid (after
  ;; the next write commit to the read TVar's) is short, so spinning after a user-signalled
  ;; retry could potentially spin forever.  For now, just have one global CV that every
  ;; write signals. Each user retry'd commit will do one attempt after every write
  ;; transaction. A better, but more complicated, solution would be to use the broadcast
  ;; pool to send a log of all written TVar's per each transaction, and woken threads
  ;; could choose to re-attempt their transaction or go back to sleep. The actual solution
  ;; is to implement runtime-level thread parking and waking.
  (define global-write-cv (cv:new))
  (define global-write-cv-lock (lk:new))

  (inline)
  (declare wait-for-write-tx!% (Runtime :rt :t => Proxy :rt -> Word -> Unit))
  (define (wait-for-write-tx!% rt-prx lock-snapshot)
    ;; CONCURRENT:
    ;; - Masks before entering the critical region
    ;; - unmask-and-await-safely% unmasks and awaits, then wakes and re-masks in a
    ;;   catch block guaranteeing lock release.
    ;; - Check for incremented lock snapshot protects against spurious wakeups without
    ;;   any intervening commit
    (mask-current! rt-prx)
    (lk:acquire global-write-cv-lock)
    (rec % ()
      (unmask-and-await-safely% rt-prx global-write-cv global-write-cv-lock)
      (if (>= lock-snapshot (get-global-time))
          (%)
          (progn
            (lk:release global-write-cv-lock)
            (unmask-current! rt-prx))))
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
        (let snapshot = (at:read-at-int global-lock))
        (if (bit-odd? snapshot)
          (%)
          (new-tx-data% snapshot)))))

  (derive Eq)
  (define-type ValidateRes%
    TxAbort%
    (TxContinue% Word))

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
                 (at:read-at-int global-lock))
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
  (declare read-tvar (MonadIo :m => TVar :a -> STM :m :a))
  (define (read-tvar tvar)
    "Read a mutable variable inside an atomic transaction."
    (STM%
     (fn (tx-data)
       (wrap-io
         (inner-read-tvar% tvar tx-data)))))

  (inline)
  (declare write-tvar (MonadIo :m => TVar :a -> :a -> STM :m Unit))
  (define (write-tvar tvar val)
    "Write to a mutable variable inside an atomic transaction."
    (STM%
     (fn (tx-data)
       (wrap-io (inner-write-tvar% tvar val tx-data)))))

  (declare swap-tvar (MonadIo :m => TVar :a -> :a -> STM :m :a))
  (define (swap-tvar tvar new-val)
    "Swap the value of a mutable variable inside an atomic transaction. Returns the old
value."
    (STM%
     (fn (tx-data)
       (wrap-io
         (match (inner-read-tvar% tvar tx-data)
           ((TxRetryAfterWrite)
            TxRetryAfterWrite)
           ((TxFailed)
            TxFailed)
           ((TxSuccess val)
            ;; Respect non success status out of the write
            (map (const val)
                 (inner-write-tvar% tvar new-val tx-data))))))))

  (declare modify-tvar (MonadIo :m => TVar :a -> (:a -> :a) -> STM :m :a))
  (define (modify-tvar tvar f)
    "Modify a mutable variable inside an atomic transaction. Returns the new value."
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

  (declare modify-swap-tvar (MonadIo :m => TVar :a -> (:a -> :a) -> STM :m :a))
  (define (modify-swap-tvar tvar f)
    "Modify a mutable variable inside an atomic transaction. Returns the old value."
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
            (map (const val)
                 (inner-write-tvar% tvar result tx-data))))))))

  (declare tx-commit-io% (MonadIoThread :rt :t :m => TxData% -> :m Boolean))
  (define (tx-commit-io% tx-data)
    ;; CONCURRENT:
    ;; - Function is a no-op for read-only transactions, so no need to mask
    ;; - Masks right before incrementing the global spinlock counter, which would render
    ;;   the STM inoperable if stopped before re-incrementing
    ;; - Unmasks only after committing the transaction, unlocking the global spinlock,
    ;;   and notifying waiting retries
    (wrap-io-with-runtime (rt-prx)
      (when (opt:some? (c:read (.parent-tx tx-data)))
        (error "Cannot commit a nested transaction."))
      (if (read-only? tx-data)
          True
          (progn
            (let result = (c:new True))
            (mask-current! rt-prx)
            (while (and
                    ;; Stop looping if we already need to abort.
                    (c:read result)
                    (not (at:int-cas global-lock
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
              (at:atomic-inc1 global-lock)
              (broadcast-write-cv!%)
              Unit)
            (unmask-current! rt-prx)
            (c:read result)))))

  (inline)
  (declare retry (MonadIo :m => STM :m :a))
  (define retry
    "Retry the current operation because the observed state is invalid. Waits for a write
transaction to commit somewhere else, and then tries this transaction again.

This is useful if the transaction needs to wait for other threads to update the data
before it can continue. For example, if the transaction reads from a (TVar Queue) and the
queue is empty, it must wait until another thread pushes onto the queue before it can
continue.

Concurrent:
  - When the transaction runs, executing retry will abort the transaction and sleep the
    thread. The thread will sleep until any write transaction commits to the STM, when
    the retrying thread will wake and retry its transaction.
"
    (STM%
     (fn (_)
      (wrap-io
        TxRetryAfterWrite))))

  (declare or-else (MonadIo :m => STM :m :a -> STM :m :a -> STM :m :a))
  (define (or-else tx-a tx-b)
    "Run TX-A. If it signals a retry, run TX-b. If both transactions signal a
retry, then the entire transaction retries."
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

  (declare run-tx ((MonadIoThread :rt :t :m) (MonadException :m) => STM :m :a -> :m :a))
  (define (run-tx tx)
     "Run an atomic transaction. If the transaction raises an exception, the transaction
is aborted and the exception is re-raised.

WARNING: The STM can abort and re-run the transaction repeatedly, until it completes with
a consistent snapshot of the data. Therefore, TX must be pure."
    ;; CONCURRENT:
    ;; - Inherits concurrent semantics from wait-for-write-tx!% and tx-commit-io%
    ;; - No other part of run-tx has any concurrent concerns
    (let m-prx = Proxy)
    (let rt-prx = (runtime-for m-prx))
    (as-proxy-of
     (rec % ()
       (do
        (tx-data <- (tx-begin-io%))
        (do-matchM (run-stm% tx-data tx)
          ((TxFailed)
           (%))
          ((TxRetryAfterWrite)
           (progn
             (wait-for-write-tx!% rt-prx (c:read (.lock-snapshot tx-data)))
             (%)))
          ((TxSuccess val)
           (commit-succeeded? <- (tx-commit-io% tx-data))
           (if commit-succeeded?
               (pure val)
               (%))))))
     m-prx))     
  )

(defmacro do-run-tx (cl:&body body)
  "Run an atomic transaction. If the transaction raises an exception, the transaction
is aborted and the exception is re-raised.

WARNING: The STM can abort and re-run the transaction repeatedly, until it completes with
a consistent snapshot of the data. Therefore, TX must be pure."
  `(run-tx
    (do
     ,@body)))
