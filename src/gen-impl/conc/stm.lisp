(cl:in-package :cl-user)
(defpackage :io/gen-impl/conc/stm
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/types
   #:coalton-library/experimental/do-control-core
   #:io/utils
   #:io/classes/exceptions
   #:io/classes/monad-io
   #:io/classes/thread
   #:io/classes/runtime-utils
   #:io/gen-impl/conc/parking
   )
  (:local-nicknames
   (:opt #:coalton-library/optional)
   (:i #:coalton-library/iterator)
   (:c #:coalton-library/cell)
   (:bt  #:io/utilities/bt-compat)
   (:at #:io/threads-impl/atomics)
   )
  (:export
   ;; Library Public
   #:TVar
   #:STM

   #:new-tvar
   #:new-tvar-tx
   #:read-tvar
   #:write-tvar
   #:swap-tvar
   #:modify-tvar
   #:modify-swap-tvar
   #:retry
   #:retry-unless
   #:retry-unless-tvar
   #:retry-with
   #:or-else
   #:run-tx
   #:do-run-tx

   ;; Library Private
   #:STM%
   #:tx-io!%
   #:new-tvar%
   #:inner-read-tvar%
   )
  )
(in-package :io/gen-impl/conc/stm)

(defmacro mem-barrier ()
  `(lisp (-> Void) ()
     (sb-thread:barrier (:read))))

(cl:declaim (cl:optimize (cl:speed 3) (cl:debug 0) (cl:safety 0)))

(named-readtables:in-readtable coalton:coalton)

;; This is an implementation of the NOrec STM algorithm, described:
;; https://pages.cs.wisc.edu/~markhill/restricted/757/ppopp10_norec.pdf

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;          Main STM Types           ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(coalton-toplevel

  (derive Eq)
  (define-type (TVar :a)
    "A Transaction Variable that can be read and modified inside an STM transaction."
    (TVar% (c:Cell :a) ParkingSet))

  (inline)
  (declare unwrap-tvar% (TVar :a -> c:Cell :a))
  (define (unwrap-tvar% (TVar% a _))
    a)

  (inline)
  (declare unwrap-tvar-pset% (TVar :a -> ParkingSet))
  (define (unwrap-tvar-pset% (TVar% _ pset))
    pset)

  (inline)
  (declare set-tvar% (TVar :a * :a -> Unit))
  (define (set-tvar% tvar val)
    (c:write! (unwrap-tvar% tvar) val)
    Unit)

  (inline)
  (declare tvar-value% (TVar :a -> :a))
  (define (tvar-value% tvar)
    (c:read (unwrap-tvar% tvar)))

  (define-exception TxAbort
    (TxRetryAfterWrite TimeoutStrategy) ;; User-requested sleep until another write commit
    TxFailed) ;; Another thread wrote to an accessed TVar during our commit

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
  (define-type (STM :a)
    "A transaction that can be run using `run-tx`."
    (STM% (TxData% -> :a)))

  (inline)
  (declare unwrap-stm% (STM :a -> (TxData% -> :a)))
  (define (unwrap-stm% (STM% f-tx))
    f-tx)

  (inline)
  (declare run-stm% (TxData% * STM :a -> :a))
  (define (run-stm% tx-data tx)
    ((unwrap-stm% tx) tx-data))

  (inline)
  (declare tx-const!% (:a -> STM :a))
  (define (tx-const!% data)
    (STM%
     (fn (_)
       data)))

  (inline)
  (declare tx-io!% (BaseIo :m => :m :a -> STM :a))
  (define (tx-io!% io-op)
    "Not safe to use generally. Useful for writing unit-tests,
for purposes like writing to Var's and using MVar's to coordinate
threads inside of transactions to simulate different concurrent
conditions. DONT USE THIS!"
    (STM%
     (fn (_)
       (run! io-op)))))

(coalton-toplevel
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;           STM Instances           ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-instance (Functor STM)
    (inline)
    (define (map f tx)
      (STM%
       (fn (tx-data)
         (f (run-stm% tx-data tx))))))

  (declare lifta2-tx% ((:a * :b -> :c) * STM :a * STM :b -> STM :c))
  (define (lifta2-tx% fa*b->c tx-a tx-b)
    (STM%
     (fn (tx-data)
       (fa*b->c (run-stm% tx-data tx-a) (run-stm% tx-data tx-b)))))

  (define-instance (Applicative STM)
    (inline)
    (define (pure x)
      (STM% ƒ_.x))
    (define lifta2 lifta2-tx%))

  (inline)
  (declare flatmap-tx% (STM :a * (:a -> STM :b) -> STM :b))
  (define (flatmap-tx% tx fa->stmb)
    (STM%
     (fn (tx-data)
       (let val-a = (run-stm% tx-data tx))
       (run-stm% tx-data
                 (fa->stmb val-a)))))

  (define-instance (Monad STM)
    ;; See https://github.com/coalton-lang/coalton/issues/1730
    (inline)
    (define (>>= m f)
      (flatmap-tx% m f)))
  )

(coalton-toplevel

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;          Internal Types           ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (inline)
  (declare read-entry-addr% (ReadEntry% -> Anything))
  (define (read-entry-addr% entr)
    (lisp (-> Anything) (entr)
      (cl:car entr)))

  (inline)
  (declare read-entry-current-val% (ReadEntry% -> Anything))
  (define (read-entry-current-val% entr)
    (tvar-value%
     (lisp (-> TVar Anything) (entr)
       (cl:car entr))))

  (inline)
  (declare read-entry-pset% (ReadEntry% -> ParkingSet))
  (define (read-entry-pset% entr)
    (unwrap-tvar-pset%
     (lisp (-> TVar Anything) (entr)
       (cl:car entr))))

  (inline)
  (declare read-entry-cached-val% (ReadEntry% -> Anything))
  (define (read-entry-cached-val% entr)
    (lisp (-> Anything) (entr)
      (cl:cdr entr)))

  (inline)
  (declare new-write-hash-table% (Void -> WriteHashTable%))
  (define (new-write-hash-table%)
    (lisp (-> WriteHashTable%) ()
      (cl:make-hash-table :test 'cl:eq)))

  (inline)
  (declare logged-write-value% (WriteHashTable% * :a -> Optional Anything))
  (define (logged-write-value% write-log key)
     (lisp (-> Optional Anything) (write-log key)
       (cl:multiple-value-bind (val found?) (cl:gethash key write-log)
         (cl:if found?
                (Some val)
                None))))

  (inline)
  (declare logged-write-psets% (WriteHashTable% -> List ParkingSet))
  (define (logged-write-psets% write-log)
    (lisp (-> List ParkingSet) (write-log)
      (cl:loop :for addr :being :the :hash-keys :of write-log
               :collect (call-coalton-function unwrap-tvar-pset% addr))))

  (declare tx-logged-write-value% (TxData% * :a -> Optional Anything))
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
  (declare log-write-value% (WriteHashTable% * TVar :a * :a -> Unit))
  (define (log-write-value% write-log addr val)
    (lisp (-> :a) (write-log addr val)
      (cl:setf (cl:gethash addr write-log) val))
    Unit)

  (inline)
  (declare commit-logged-writes (WriteHashTable% -> Unit))
  (define (commit-logged-writes write-log)
    "Actually set the TVar's value to its corresponding logged write value."
    (lisp (-> Unit) (write-log)
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
       (lisp (-> Void) (tx-write-log pt-write-log)
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
  (declare log-read-value (TVar :a * :a * TxData% -> Unit))
  (define (log-read-value addr val tx-data)
    (c:push! (.read-log tx-data) (lisp (-> ReadEntry%) (addr val)
                                   (cl:cons addr val)))
    Unit)

  (inline)
  (declare read-only? (TxData% -> Boolean))
  (define (read-only? tx-data)
    (let write-log = (.write-log tx-data))
    (lisp (-> Boolean) (write-log)
      (cl:zerop (cl:hash-table-count write-log))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;        STM Implementation         ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (inline)
  (declare new-tvar% (:a -> TVar :a))
  (define (new-tvar% val)
    (TVar% (c:new val) (new-parking-set%)))

  (inline)
  (declare new-tvar (MonadIo :m => :a -> :m (TVar :a)))
  (define (new-tvar val)
    "Create a sychronized mutable variable that can be used inside an atomic transaction."
    (wrap-io (new-tvar% val)))

  (inline)
  (declare new-tvar-tx (:a -> STM (TVar :a)))
  (define (new-tvar-tx val)
    "Create a synchronized mutable variable in the middle of an atomic transaction."
    (STM%
     (fn (_)
       (new-tvar% val))))

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
  (declare get-global-time (Void -> Word))
  (define (get-global-time)
    "Read the global lock time and establish a memory barrier."
    (mem-barrier)
    (at:read-at-int global-lock))

  (inline)
  (declare broadcast-write-tx!% (TxData% -> Void))
  (define (broadcast-write-tx!% tx-data)
    ;; CONCURRENT:
    ;;   - WARNING: Should be run in a masked region to ensure writes aren't committed
    ;;     without unparking the corresponding psets
    ;;   - Because assuming masked, no need to mask to ensure consistent unparks in
    ;;     the presence of an asynchronous stop
    (foreach (pset (logged-write-psets% (.write-log tx-data)))
      (unpark-set% pset)))

  (inline)
  (declare tx-begin% (Void -> TxData%))
  (define (tx-begin%)
    (rec % ()
      (let snapshot = (at:read-at-int global-lock))
      (if (bit-odd? snapshot)
          (%)
          (new-tx-data% snapshot))))

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

  (inline)
  (declare wait-for-write-tx!% (Runtime :rt :t => Proxy :rt * TimeoutStrategy * TxData% -> Void))
  (define (wait-for-write-tx!% rt-prx strategy tx-data)
    ;; CONCURRENT:
    ;; - Inherits concurrent semantics of park-in-sets-if%
    (let lock-snapshot = (the Word (c:read (.lock-snapshot tx-data))))
    (park-in-sets-if-with%
     rt-prx
     (fn ()
       (== (TxContinue% lock-snapshot)
           (validate% tx-data)))
     strategy
     (map read-entry-pset% (c:read (.read-log tx-data))))
    (values))

  (declare inner-read-tvar% (TVar :a * TxData% -> :a))
  (define (inner-read-tvar% tvar tx-data)
    (match (tx-logged-write-value% tx-data tvar)
      ((Some written-val)
       (from-anything written-val))
      ((None)
       (rec % ((val (progn (mem-barrier)
                           (tvar-value% tvar))))
         (if (== (cached-snapshot tx-data)
                 (at:read-at-int global-lock))
             (progn
               (log-read-value tvar val tx-data)
               val)
             (match (validate% tx-data)
               ((TxAbort%)
                (throw TxFailed))
               ((TxContinue% time)
                (c:write! (.lock-snapshot tx-data)
                          time)
                (% (tvar-value% tvar)))))))))

  (inline)
  (declare inner-write-tvar% (TVar :a * :a * TxData% -> Unit))
  (define (inner-write-tvar% tvar val tx-data)
    (log-write-value% (.write-log tx-data) tvar val))

  (inline)
  (declare read-tvar (TVar :a -> STM :a))
  (define (read-tvar tvar)
    "Read a mutable variable inside an atomic transaction."
    (STM%
     (fn (tx-data)
       (inner-read-tvar% tvar tx-data))))

  (inline)
  (declare write-tvar (TVar :a * :a -> STM Unit))
  (define (write-tvar tvar val)
    "Write to a mutable variable inside an atomic transaction."
    (STM%
     (fn (tx-data)
       (inner-write-tvar% tvar val tx-data))))

  (declare swap-tvar (TVar :a * :a -> STM :a))
  (define (swap-tvar tvar new-val)
    "Swap the value of a mutable variable inside an atomic transaction. Returns the old
value."
    (STM%
     (fn (tx-data)
       (let val = (inner-read-tvar% tvar tx-data))
       (inner-write-tvar% tvar new-val tx-data)
       val)))

  (declare modify-tvar (TVar :a * (:a -> :a) -> STM :a))
  (define (modify-tvar tvar f)
    "Modify a mutable variable inside an atomic transaction. Returns the new value."
    (STM%
     (fn (tx-data)
       (let val = (inner-read-tvar% tvar tx-data))
       (let result = (f val))
       (inner-write-tvar% tvar result tx-data)
       result)))

  (declare modify-swap-tvar (TVar :a * (:a -> :a) -> STM :a))
  (define (modify-swap-tvar tvar f)
    "Modify a mutable variable inside an atomic transaction. Returns the old value."
    (STM%
     (fn (tx-data)
       (let val = (inner-read-tvar% tvar tx-data))
       (let result = (f val))
       (inner-write-tvar% tvar result tx-data)
       val)))

  (declare tx-commit% (Runtime :rt :t => Proxy :rt * TxData% -> Boolean))
  (define (tx-commit% rt-prx tx-data)
    ;; CONCURRENT:
    ;; - Function is a no-op for read-only transactions, so no need to mask
    ;; - Masks right before incrementing the global spinlock counter, which would render
    ;;   the STM inoperable if stopped before re-incrementing
    ;; - Unmasks only after committing the transaction, unlocking the global spinlock,
    ;;   and notifying waiting retries
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
            (broadcast-write-tx!% tx-data)
            Unit)
          (unmask-current! rt-prx)
          (c:read result))))

  (inline)
  (declare retry-with (TimeoutStrategy -> STM :a))
  (define (retry-with strategy)
    "Retry the current operation because the observed state is invalid. Waits for a write
transaction to commit to a TVar that has already been read during this transaction, and
then tries this transaction again.

This is useful if the transaction needs to wait for other threads to update the data
before it can continue. For example, if the transaction reads from a (TVar Queue) and the
queue is empty, it must wait until another thread pushes onto the queue before it can
continue.

Concurrent:
  - When the transaction runs, executing retry will abort the transaction and sleep the
    thread. The thread will sleep until any relevant write transaction commits to the STM,
    when the retrying thread will wake and retry its transaction. A write transaction
    only triggers a retry if it writes to a TVar that was read before `retry` was called.
  - Will timeout depending on strategy."
    (STM%
     (fn (_)
       (throw
           (TxRetryAfterWrite strategy)))))

  (inline)
  (declare retry (STM :a))
  (define retry
    "Retry the current operation because the observed state is invalid. Waits for a write
transaction to commit to a TVar that has already been read during this transaction, and
then tries this transaction again.

This is useful if the transaction needs to wait for other threads to update the data
before it can continue. For example, if the transaction reads from a (TVar Queue) and the
queue is empty, it must wait until another thread pushes onto the queue before it can
continue.

Concurrent:
  - When the transaction runs, executing retry will abort the transaction and sleep the
    thread. The thread will sleep until any relevant write transaction commits to the STM,
    when the retrying thread will wake and retry its transaction. A write transaction
    only triggers a retry if it writes to a TVar that was read before `retry` was called."
    (retry-with NoTimeout))

  (inline)
  (declare retry-unless (Boolean -> STM Unit))
  (define (retry-unless ok?)
    "Retry unless `ok?` is true. If `ok?` is false, then waits for a write transaction
to commit to a TVar that has already been read during this transaction, and then tries
this transaction again.

Concurrent:
  - When the transaction runs, executing retry will abort the transaction and sleep the
    thread. The thread will sleep until any relevant write transaction commits to the STM,
    when the retrying thread will wake and retry its transaction. A write transaction
    only triggers a retry if it writes to a TVar that was read before `retry` was called."
    (if ok?
        (pure Unit)
        retry))

  (inline)
  (declare retry-unless-tvar (TVar Boolean -> STM Unit))
  (define (retry-unless-tvar ok?)
    "Retry unless `ok?` is true. If `ok?` is false, then waits for a write transaction
to commit to a TVar that has already been read during this transaction, and then tries
this transaction again.

Concurrent:
  - When the transaction runs, executing retry will abort the transaction and sleep the
    thread. The thread will sleep until any relevant write transaction commits to the STM,
    when the retrying thread will wake and retry its transaction. A write transaction
    only triggers a retry if it writes to a TVar that was read before `retry` was called."
    (STM%
     (fn (tx-data)
       (let val = (inner-read-tvar% ok? tx-data))
       (if val
           Unit
           (throw (TxRetryAfterWrite NoTimeout))))))

  (declare or-else (STM :a * STM :a -> STM :a))
  (define (or-else tx-a tx-b)
    "Run TX-A. If it signals a retry, run TX-b. If both transactions signal a
retry, then the entire transaction retries."
    (STM%
     (fn (tx-data)
       (let a-tx-data = (child-tx-data% tx-data))
       (catch (progn
                (let val = (run-stm% a-tx-data tx-a))
                (merge-into-parent-tx% a-tx-data)
                val)
         ((TxFailed)
          (merge-into-parent-tx% a-tx-data)
          (throw TxFailed))
         ((TxRetryAfterWrite _)
          (merge-read-log-into-parent-tx% a-tx-data)
          (run-stm% tx-data tx-b))))))

  ;; (declare run-tx ((Threads :rt :t :m) (Exceptions :m) => STM :m :a -> :m :a))
  (declare run-tx ((Threads :rt :t :m) => STM :a -> :m :a))
  (define (run-tx tx)
     "Run an atomic transaction. If the transaction raises an exception, the transaction
is aborted and the exception is re-raised.

WARNING: The STM can abort and re-run the transaction repeatedly, until it completes with
a consistent snapshot of the data. Therefore, TX must be pure."
    ;; CONCURRENT:
    ;; - Inherits concurrent semantics from wait-for-write-tx!% and tx-commit-io%
    ;; - No other part of run-tx has any concurrent concerns
    (wrap-io-with-runtime (rt-prx)
      (let commit-succeeded? = (c:new False))
      (let return = (c:new None))
      (rec % ()
        (let tx-data = (tx-begin%))
        (catch (progn
                 (let val = (run-stm% tx-data tx))
                 (let succeeded? = (tx-commit% rt-prx tx-data))
                 (c:write! commit-succeeded? succeeded?)
                 (when succeeded?
                   (c:write! return (Some val)))
                 Unit)
          ((TxRetryAfterWrite strategy)
           (wait-for-write-tx!% rt-prx strategy tx-data)
           Unit)
          (_ Unit))
        (if (c:read commit-succeeded?)
            (opt:from-some "Impossible error, please submit a bug report: Transaction failed to produce a value"
                           (c:read return))
            (%)))))
  )

(defmacro do-run-tx (cl:&body body)
  "Run an atomic transaction. If the transaction raises an exception, the transaction
is aborted and the exception is re-raised.

WARNING: The STM can abort and re-run the transaction repeatedly, until it completes with
a consistent snapshot of the data. Therefore, TX must be pure."
  `(run-tx
    (do
     ,@body)))
