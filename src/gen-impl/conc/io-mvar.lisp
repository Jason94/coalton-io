(cl:in-package :cl-user)
(defpackage :io/gen-impl/conc/mvar
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/functions
   #:coalton-library/types
   #:coalton-library/monad/classes
   #:coalton-library/experimental/do-control-core
   #:io/utils
   #:io/threads-exceptions
   #:io/classes/monad-io
   #:io/classes/exceptions
   #:io/classes/threads
   #:io/classes/runtime-utils
   #:io/resource
   #:io/threads-impl/data-broadcast-pool
   )
  (:local-nicknames
   (:opt #:coalton-library/optional)
   (:at #:io/threads-impl/atomics)
   (:lk #:coalton-threads/lock)
   (:cv #:coalton-threads/condition-variable)
   )
  (:export
   #:MVar
   #:MonadIo
   #:new-mvar
   #:new-empty-mvar
   #:take-mvar-masked
   #:take-mvar-masked-with
   #:take-mvar
   #:take-mvar-with
   #:put-mvar
   #:put-mvar-with
   #:try-take-mvar
   #:try-take-mvar-masked
   #:try-put-mvar
   #:read-mvar
   #:read-mvar-with
   #:try-read-mvar
   #:swap-mvar
   #:swap-mvar-with
   #:is-empty-mvar
   #:with-mvar

   #:MChan
   #:new-empty-chan
   #:push-chan
   #:pop-chan
   #:pop-chan-with
   #:try-pop-chan
   ))
(in-package :io/gen-impl/conc/mvar)

(cl:declaim (cl:optimize (cl:speed 3) (cl:debug 0) (cl:safety 1)))

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (define-struct (MVar :a)
    "A synchronized container that can be empty or hold an :a.

All critical MVar operations are masked. However, irresponsible stopping could still cause
deadlocks and other race conditions."
    (lock                lk:Lock)
    (read-broadcast-pool (DataBroadcastPool :a))
    (notify-full         cv:ConditionVariable)
    (notify-empty        cv:ConditionVariable)
    (data                (at:Atomic (Optional :a))))

  (inline)
  (declare new-mvar (Threads :rt :t :m => :a -> :m (MVar :a)))
  (define (new-mvar val)
    "Create a new MVar containing VAL."
    (wrap-io
      (MVar (lk:new)
            (new-broadcast-pool)
            (cv:new)
            (cv:new)
            (at:new (Some val)))))

  (inline)
  (declare new-empty-mvar (Threads :rt :t :m => :m (MVar :a)))
  (define new-empty-mvar
    "Create a new empty MVar."
    (wrap-io
      (MVar (lk:new)
            (new-broadcast-pool)
            (cv:new)
            (cv:new)
            (at:new None))))

  (declare take-mvar-masked-inner% (Runtime :rt :t => TimeoutStrategy -> MVar :a -> Proxy :rt -> :a))
  (define (take-mvar-masked-inner% strategy mvar rt-prx)
    "Concurrent: Leaves the thread masked once."
    ;; CONCURRENT: Masks before entering the critical region.
    ;; - unmask-and-await-safely% unmasks and awaits, then wakes and re-masks in a
    ;;   catch block guaranteeing lock release.
    ;; - The thread can only be stopped during unmask-and-await-safely%, thus cannot
    ;;   be stopped between emptying the MVar and notifying listeners.
    ;; - Notifying after release is valid because all waiter/notifiers evaluate guard
    ;;   condition and interpose lock acquisition before waiting/notifying.
    ;;   See https://stackoverflow.com/questions/21439359/signal-on-condition-variable-without-holding-lock
    ;; - On the post-wakeup success path, does not unmask and leaves the applied mask
    ;;   to the caller to handle.
    ;; - If the thread is stopped in unmask-and-await-safely-finally% after waking
    ;;   but before masking (very unlikely), notifies the next waiting read-consumer
    ;;   to prevent lost handoff.
    (mask-current! rt-prx)
    (lk-acquire-with (.lock mvar) strategy)
    (let ((lp (fn ()
                (match (at:read (.data mvar))
                  ((Some val)
                   (at:atomic-write (.data mvar) None)
                   (lk:release (.lock mvar))
                   (cv:notify (.notify-empty mvar))
                   val)
                  ((None)
                   (unmask-and-await-safely-finally-with%
                    rt-prx
                    strategy
                    (.notify-full mvar)
                    (.lock mvar)
                    (fn ()
                      (cv:notify (.notify-full mvar))))
                    (lp))))))
      (lp)))

  (inline)
  (declare take-mvar-masked-with (Threads :rt :t :m => TimeoutStrategy -> MVar :a -> :m :a))
  (define (take-mvar-masked-with strategy mvar)
    "Take a value from an MVar, blocking until one is available.

Concurrent:
  - WARNING: Leaves the thread masked when returns to protect caller's critical regions
    based on consuming and restoring MVar to a valid state. See MChan for an example.
  - Blocks while the MVar is empty
  - Read-consumers (including `take-mvar-masked`) are woken individual on succesfull puts,
    in order of acquisition
  - On succesful take, one blocking writer is woken in order of acquisition"
    ;; CONCURRENT: Inherits CONCURRENT semantics from take-mvar-masked-inner%
    (wrap-io-with-runtime (rt-prx)
      (take-mvar-masked-inner% strategy mvar rt-prx)))

  (inline)
  (declare take-mvar-masked (Threads :rt :t :m => MVar :a -> :m :a))
  (define (take-mvar-masked mvar)
    "Take a value from an MVar, blocking until one is available.

Concurrent:
  - WARNING: Leaves the thread masked when returns to protect caller's critical regions
    based on consuming and restoring MVar to a valid state. See MChan for an example.
  - Blocks while the MVar is empty
  - Read-consumers (including `take-mvar-masked`) are woken individual on succesfull puts,
    in order of acquisition
  - On succesful take, one blocking writer is woken in order of acquisition"
    (take-mvar-masked-with NoTimeout mvar))

  (inline)
  (declare take-mvar-with (Threads :rt :t :m => TimeoutStrategy -> MVar :a -> :m :a))
  (define (take-mvar-with strategy mvar)
    "Take a value from an MVar, blocking until one is available.

Concurrent:
  - Blocks while the MVar is empty
  - Read-consumers (including `take-mvar`) are woken individual on succesful puts,
    in order of acquisition
  - On succesful take, one blocking writer is woken in order of acquisition"
    ;; CONCURRENT:
    ;; Inherits CONCURRENT semantics from take-mvar-masked-inner%.
    ;; unmasks from take-mvar-masked-inner% before returning to caller.
    (wrap-io-with-runtime (rt-prx)
      (let result = (take-mvar-masked-inner% strategy mvar rt-prx))
      (unmask-current! rt-prx)
      result))

  (inline)
  (declare take-mvar (Threads :rt :t :m => MVar :a -> :m :a))
  (define (take-mvar mvar)
    "Take a value from an MVar, blocking until one is available.

Concurrent:
  - Blocks while the MVar is empty
  - Read-consumers (including `take-mvar`) are woken individual on succesful puts,
    in order of acquisition
  - On succesful take, one blocking writer is woken in order of acquisition"
    (take-mvar-with NoTimeout mvar))

  (declare put-mvar-with (Threads :rt :t :m => TimeoutStrategy -> MVar :a -> :a -> :m Unit))
  (define (put-mvar-with strategy mvar val)
    "Fill an empty MVar, blocking until it becomes empty.

Concurrent:
  - Blocks while the MVar is full
  - Writers (including `put-mvar`) are woken individual on succesful takes in order
    of acquisition
  - On succesful put, blocking read-consumers are woken individually in order of acquisition
  - On succesful put, all blocking read-non-consumers are woken simultaneously. New data
    is handed directly to woken read-non-consumers so they don't contend on the MVar."
    ;; CONCURRENT:
    ;; - Masks before entering the critical region.
    ;; - unmask-and-await-safely% unmasks and awaits, then wakes and re-masks in a
    ;;   catch block guaranteeing lock release.
    ;; - The thread can only be stopped during unmask-and-await-safely%, thus cannot
    ;;   be stopped between emptying the MVar and notifying listeners.
    ;; - Notifying after release is valid because all waiter/notifiers evaluate guard
    ;;   condition and interpose lock acquisition before waiting/notifying.
    ;;   See https://stackoverflow.com/questions/21439359/signal-on-condition-variable-without-holding-lock
    ;; - If the thread is stopped in unmask-and-await-safely-finally% after waking
    ;;   but before masking (very unlikely), notifies the next waiting write-consumer
    ;;   to prevent lost handoff.
    (wrap-io-with-runtime (rt-prx)
      (let lock = (.lock mvar))
      (let data = (.data mvar))
      (mask-current! rt-prx)
      (lk-acquire-with lock strategy)
      (let ((lp (fn ()
                  (match (at:read data)
                    ((None)
                     (at:atomic-write data (Some val))
                     (lk:release lock)
                     (publish (.read-broadcast-pool mvar) val)
                     (cv:notify (.notify-full mvar))
                     (unmask-current! rt-prx)
                     Unit)
                    ((Some _)
                     (unmask-and-await-safely-finally-with%
                      rt-prx
                      strategy
                      (.notify-empty mvar)
                      lock
                      (fn ()
                        (cv:notify (.notify-empty mvar))))
                     (lp))))))
        (lp))))

  (inline)
  (declare put-mvar (Threads :rt :t :m => MVar :a -> :a -> :m Unit))
  (define (put-mvar mvar val)
    "Fill an empty MVar, blocking until it becomes empty.

Concurrent:
  - Blocks while the MVar is full
  - Writers (including `put-mvar`) are woken individual on succesful takes in order
    of acquisition
  - On succesful put, blocking read-consumers are woken individually in order of acquisition
  - On succesful put, all blocking read-non-consumers are woken simultaneously. New data
    is handed directly to woken read-non-consumers so they don't contend on the MVar."
    (put-mvar-with NoTimeout mvar val))

  (declare try-take-mvar-masked-inner%-with
           (Runtime :rt :t => TimeoutStrategy -> MVar :a -> Proxy :rt -> Optional :a))
  (define (try-take-mvar-masked-inner%-with strategy mvar rt-prx)
    "Concurrent: Leaves the thread masked once."
    ;; CONCURRENT:
    ;; - Does not need to mask until acquiring lock, because atomic read and function
    ;;   return can't leave MVar in inconsistent state.
    ;; - Exit on initial read fail is valid because not obligated to retry until full.
    ;; - Subsequent read test after acquisition is required in case MVar was emptied
    ;;   between initial atomic read and lock acquisition.
    ;; - Notifying after release is valid because all waiter/notifiers evaluate guard
    ;;   condition and interpose lock acquisition before waiting/notifying.
    ;;   See https://stackoverflow.com/questions/21439359/signal-on-condition-variable-without-holding-lock
    (mask-current! rt-prx)
    (match (at:read (.data mvar))
      ((None)
       None)
      ((Some _)
       (lk-acquire-with (.lock mvar) strategy)
       (match (at:read (.data mvar))
         ((Some x)
          (at:atomic-write (.data mvar) None)
          (lk:release (.lock mvar))
          (cv:notify (.notify-empty mvar))
          (Some x))
         ((None)
         (lk:release (.lock mvar))
         None)))))

  (inline)
  (declare try-take-mvar-masked-inner% (Runtime :rt :t => MVar :a -> Proxy :rt -> Optional :a))
  (define (try-take-mvar-masked-inner% mvar rt-prx)
    "Concurrent: Leaves the thread masked once."
    (try-take-mvar-masked-inner%-with NoTimeout mvar rt-prx))

  (declare try-take-mvar (Threads :rt :t :m => MVar :a -> :m (Optional :a)))
  (define (try-take-mvar mvar)
    "Attempt to immediately take a value from an MVar. Returns None if empty.

Concurrent:
  - Can briefly block while waiting to empty the MVar, if contended
  - On succesful take, one blocking writer is woken in order of acquisition"
    ;; CONCURRENT:
    ;; Inherits CONCURRENT semantics from try-take-mvar-masked-inner%.
    ;; unmasks from try-take-mvar-masked-inner% before returning to caller.
    (wrap-io-with-runtime (rt-prx)
      (let result = (try-take-mvar-masked-inner% mvar rt-prx))
      (unmask-current! rt-prx)
      result))

  (declare try-take-mvar-masked (Threads :rt :t :m => MVar :a -> :m (Optional :a)))
  (define (try-take-mvar-masked mvar)
    "Attempt to immediately take a value from an MVar. Returns None if empty.

Concurrent:
  - WARNING: Leaves the thread masked when returns to protect caller's critical regions
    based on consuming and restoring MVar to a valid state. See MChan for an example.
  - Can briefly block while waiting to empty the MVar, if contended
  - On succesful take, one blocking writer is woken in order of acquisition"
    ;; CONCURRENT: Inherits CONCURRENT semantics from try-take-mvar-masked-inner%
    (wrap-io-with-runtime (rt-prx)
      (try-take-mvar-masked-inner% mvar rt-prx)))

  (declare try-put-mvar (Threads :rt :t :m => MVar :a -> :a -> :m Boolean))
  (define (try-put-mvar mvar val)
    "Attempt to immediately put a value into an MVar. Returns True if succeeded.

Concurrent:
  - Can briefly block while waiting to fill the MVar, if contended
  - On succesful put, blocking read-consumers are woken individually in order of acquisition
  - On succesful put, all blocking read-non-consumers are woken simultaneously. New data
    is handed directly to woken read-non-consumers so they don't contend on the MVar."
    ;; CONCURRENT:
    ;; - Does not need to mask until acquiring lock, because atomic read and function
    ;;   return can't leave MVar in inconsistent state.
    ;; - Exit on initial read fail is valid because not obligated to retry until empty.
    ;; - Subsequent read test after acquisition is required in case MVar was filled
    ;;   between initial atomic read and lock acquisition.
    ;; - Notifying after release is valid because all waiter/notifiers evaluate guard
    ;;   condition and interpose lock acquisition before waiting/notifying.
    ;;   See https://stackoverflow.com/questions/21439359/signal-on-condition-variable-without-holding-lock
    (wrap-io-with-runtime (rt-prx)
      (match (at:read (.data mvar))
        ((Some _)
         False)
        ((None)
         (mask-current! rt-prx)
         (lk:acquire (.lock mvar))
         (match (at:read (.data mvar))
           ((Some _)
            (lk:release (.lock mvar))
            (unmask-current! rt-prx)
            False)
           ((None)
            (at:atomic-write (.data mvar) (Some val))
            (lk:release (.lock mvar))
            (publish (.read-broadcast-pool mvar) val)
            (cv:notify (.notify-full mvar))
            (unmask-current! rt-prx)
            True))))))

  (declare read-mvar-with (Threads :rt :t :m => TimeoutStrategy -> MVar :a -> :m :a))
  (define (read-mvar-with strategy mvar)
    "Read a value from an MVar, blocking until one is available. Does not consume value.

Concurrent:
  - Blocks while the MVar is empty
  - Blocking read-non-consumers (including `read-mvar`) are woken simultaneously on 
    succesful put. Data is handed directly to woken readers, which don't contend on mvar."
    ;; CONCURRENT:
    ;; - Does not need to mask around read-only atomic happy path.
    ;; - subscribe blocks until a publish and masks its own critical regions.
    (wrap-io-with-runtime (rt-prx)
      (match (at:read (.data mvar))
        ((Some x)
         x)
        ((None)
         (subscribe-with rt-prx strategy (.read-broadcast-pool mvar))))))

  (declare read-mvar (Threads :rt :t :m => MVar :a -> :m :a))
  (define (read-mvar mvar)
    "Read a value from an MVar, blocking until one is available. Does not consume value.

Concurrent:
  - Blocks while the MVar is empty
  - Blocking read-non-consumers (including `read-mvar`) are woken simultaneously on 
    succesful put. Data is handed directly to woken readers, which don't contend on mvar."
    (read-mvar-with NoTimeout mvar))

  (declare try-read-mvar (Threads :rt :t :m => MVar :a -> :m (Optional :a)))
  (define (try-read-mvar mvar)
    "Attempt to immediately read a value from an MVar. Returns None if empty."
    (wrap-io
      (at:read (.data mvar))))

  (declare swap-mvar-with (Threads :rt :t :m => TimeoutStrategy -> MVar :a -> :a -> :m :a))
  (define (swap-mvar-with strategy mvar new-val)
    "Atomically replace the value in an MVar and return the old value.

Concurrent:
  - Blocks while the MVar is empty
  - Wakes the next blocking read-consumer when `swap-mvar` completes"
    ;; CONCURRENT:
    ;; - Masks before entering the critical region.
    ;; - unmask-and-await-safely% unmasks and awaits, then wakes and re-masks in a
    ;;   catch block guaranteeing lock release.
    ;; - Notifying after release is valid because all waiter/notifiers evaluate guard
    ;;   condition and interpose lock acquisition before waiting/notifying.
    ;;   See <cite SO post>
    ;; - If the thread is stopped in unmask-and-await-safely-finally% after waking
    ;;   but before masking (very unlikely), notifies the next waiting read-consumer
    ;;   to prevent lost handoff.
    (wrap-io-with-runtime (rt-prx)
      (mask-current! rt-prx)
      (lk-acquire-with (.lock mvar) strategy)
      (let ((lp (fn ()
                  (match (at:read (.data mvar))
                    ((Some old-val)
                     (at:atomic-write (.data mvar) (Some new-val))
                     (lk:release (.lock mvar))
                     (cv:notify (.notify-full mvar))
                     (unmask-current! rt-prx)
                     old-val)
                    ((None)
                     (unmask-and-await-safely-finally-with%
                      rt-prx
                      strategy
                      (.notify-full mvar)
                      (.lock mvar)
                      (fn ()
                        (cv:notify (.notify-full mvar))))
                     (lp))))))
        (lp))))

  (inline)
  (declare swap-mvar (Threads :rt :t :m => MVar :a -> :a -> :m :a))
  (define (swap-mvar mvar new-val)
    "Atomically replace the value in an MVar and return the old value.

Concurrent:
  - Blocks while the MVar is empty
  - Wakes the next blocking read-consumer when `swap-mvar` completes"
    (swap-mvar-with NoTimeout mvar new-val))

  (inline)
  (declare is-empty-mvar (Threads :rt :t :m => MVar :a -> :m Boolean))
  (define (is-empty-mvar mvar)
    "Return True if the MVar is currently empty."
    (wrap-io
      (opt:none? (at:read (.data mvar)))))
  )

(coalton-toplevel
  ;; NOTE: It would be preferable to restore the initial value of the MVar on a fail.
  ;; However, this would violate the requirement that bracket-io cleanup not block
  ;; and would leave the thread unstoppable.
  ;; TODO: Possibly check and restore if it's a non-thread stop exception? But is the
  ;; inconsistent behavior worth it? Probably not?
  (declare with-mvar ((UnliftIo :r :i) (LiftTo :r :m) (Threads :rt :t :i)
                       => MVar :a -> (:a -> :r :b) -> :m :b))
  (define (with-mvar mvar op)
    "Run an operation with the value from an MVar, blocking until one is available.
Restore the MVar value and return the result of the operation.

WARNING: If the computation raises an unhandled exception or is stopped, leaves the MVar
empty!

Concurrent:
  - WARNING: Does not mask during the computation. To ensure completion, caller must mask
  - Blocks while the MVar is empty
  - Inherits notify semantics from `put-mvar`
  - Does not leave the MVar locked during the computation. Thus, other threads can
    put the MVar during the computation and force `with-mvar` to block until empty."
    ;; CONCURRENT:
    ;; Doesn't explicitly mask (see above), but take-mvar and put-mvar mask their critical sections.
    (lift-to
     (with-run-in-io
       (fn (run)
         (lift-io
          (do
           (val <- (take-mvar mvar))
           (result <- (run (op val)))
           (put-mvar mvar val)
           (pure result)))))))
  )

(defmacro do-with-mvar ((sym mvar) cl:&body body)
  "Run an operation with the value from an MVar, blocking until one is available.
Stores the result of the operation in the MVar and returns.

WARNING: If the computation raises an unhandled exception or is stopped, leaves the MVar
empty!

Concurrent:
  - WARNING: Does not mask during the computation. To ensure completion, caller must mask
  - Blocks while the MVar is empty
  - Inherits notify semantics from `put-mvar`
  - Does not leave the MVar locked during the computation. Thus, other threads can
    put the MVar during the computation and force `with-mvar` to block until empty."
  `(with-mvar
     ,mvar
     (fn (,sym)
       (do
        ,@body))))

;;;
;;; Chan
;;;

(coalton-toplevel
  (define-type (ChanNode :a)
    (ChanNode% :a (MVar (ChanNode :a))))

  (define-struct (MChan :a)
    "A synchronized FIFO queue to pass data directionally between threads."
    (head-var (MVar (MVar (ChanNode :a))))
    (tail-var (MVar (MVar (ChanNode :a)))))

  (declare new-empty-chan (Threads :rt :t :m => :m (MChan :a)))
  (define new-empty-chan
    "Create a new empty channel."
    (do
      (cell <- new-empty-mvar)
      (head-var <- (new-mvar cell))
      (tail-var <- (new-mvar cell))
      (pure (MChan head-var tail-var))))

  (declare push-chan (Threads :rt :t :m => MChan :a -> :a -> :m Unit))
  (define (push-chan chan val)
    "Push VAL onto CHAN."
    (do
     (new-tail-var <- new-empty-mvar)
     (old-tail-var <- (take-mvar-masked (.tail-var chan))) ;; Masks the thread after this returns
     (put-mvar old-tail-var (ChanNode% val new-tail-var))
     (put-mvar (.tail-var chan) new-tail-var)
     unmask-current-thread)) ;; Cleanup after take-mvar-masked

  (inline)
  (declare pop-chan (Threads :rt :t :m => MChan :a -> :m :a))
  (define (pop-chan chan)
    "Pop the front value in CHAN. Blocks while CHAN is empty."
    (pop-chan-with NoTimeout chan))

  (declare pop-chan-with (Threads :rt :t :m => TimeoutStrategy -> MChan :a -> :m :a))
  (define (pop-chan-with strategy chan)
    "Pop the front value in CHAN. Blocks while CHAN is empty."
    (do
     (old-head-var <- (take-mvar-masked (.head-var chan))) ;; Masks the thread after this returns
     ((ChanNode% val new-head-var) <- (take-mvar-with strategy old-head-var))
     (put-mvar (.head-var chan) new-head-var)
     unmask-current-thread ;; Cleanup after take-mvar-masked
     (pure val)))

  (declare try-pop-chan (Threads :rt :t :m => MChan :a -> :m (Optional :a)))
  (define (try-pop-chan chan)
    "Attempt to pop the front value in CHAN. Does not block."
    (do-matchM (try-take-mvar-masked (.head-var chan)) ;; Masks the thread after this returns
      ((None)
       (pure None))
      ((Some old-head-var)
       ((ChanNode% val new-head-var) <- (take-mvar old-head-var))
       (put-mvar (.head-var chan) new-head-var)
       unmask-current-thread ;; Cleanup after take-mvar-masked
       (pure (Some val)))))
    
  )
