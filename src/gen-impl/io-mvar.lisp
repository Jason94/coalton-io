(cl:in-package :cl-user)
(defpackage :io/gen-impl/mvar
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/functions
   #:coalton-library/monad/classes
   #:io/utils
   #:io/thread-exceptions
   #:io/classes/monad-io
   #:io/classes/monad-exception
   #:io/classes/monad-io-thread
   #:io/resource
   ;; #:io/thread-impl/runtime
   #:io/thread-impl/data-broadcast-pool
   )
  (:local-nicknames
   (:opt #:coalton-library/optional)
   (:at #:io/thread-impl/atomics)
   (:lk #:coalton-threads/lock)
   (:cv #:coalton-threads/condition-variable)
   )
  (:export
   #:MVar
   #:MonadIo
   #:new-mvar
   #:new-empty-mvar
   #:take-mvar
   #:put-mvar
   #:try-take-mvar
   #:try-put-mvar
   #:read-mvar
   #:try-read-mvar
   #:swap-mvar
   #:is-empty-mvar
   #:with-mvar

   #:Chan
   #:new-empty-chan
   #:push-chan
   #:pop-chan
   ))
(in-package :io/gen-impl/mvar)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (define-struct (MVar :a)
    "A synchronized container that can be empty or hold one :a.
Can put data into the container, blocking until it is empty.
Can take data out of the container, blocking until it is full.

All critical MVar operations are masked, so stopping a thread that's operating on
MVar's won't leave MVar's in an inoperable state. However, irresponsible stopping
could still cause deadlocks, race conditions, etc. if other threads were
depending on the stopped thread operating on an MVar to continue."
    (lock                lk:Lock)
    (read-broadcast-pool (DataBroadcastPool :a))
    (notify-full         cv:ConditionVariable)
    (notify-empty        cv:ConditionVariable)
    (data                (at:Atomic (Optional :a))))

  (inline)
  (declare new-mvar (MonadIoThread :rt :t :m => :a -> :m (MVar :a)))
  (define (new-mvar val)
    "Create a new MVar containing VAL."
    (wrap-io
      (MVar (lk:new)
            (new-broadcast-pool)
            (cv:new)
            (cv:new)
            (at:new (Some val)))))

  (inline)
  (declare new-empty-mvar (MonadIoThread :rt :t :m => :m (MVar :a)))
  (define new-empty-mvar
    "Create a new empty MVar."
    (wrap-io
      (MVar (lk:new)
            (new-broadcast-pool)
            (cv:new)
            (cv:new)
            (at:new None))))

  (inline)
  (define (unmask-and-await-safely% rt-prx cv lock)
    "Unmask the thread. Finally, either await (still running) the CV
or just release the LOCK."
    ;; BUG: This awaits before returning the thunk. Probably just need to go with
    ;; the catch-macro option. There's just no other good way. Or, probably even
    ;; better, try to switch away from CV's to a better, less/no contention mechanism.
    (unmask-finally! rt-prx (current-thread! rt-prx)
     (fn (mode)
       (if (== Running mode)
           (cv:await cv lock)
           (progn
             (lk:release lock)
             Unit)))))

  (declare take-mvar (MonadIoThread :rt :t :m => MVar :a -> :m :a))
  (define (take-mvar mvar)
    "Take a value from an MVar, blocking until one is available."
    (wrap-io-with-runtime (rt-prx)
      ;; TODO: Remove this when the inference bug is fixed
      (let _ = (the (MVar :a) mvar))
      (mask-current! rt-prx)
      (lk:acquire (.lock mvar))
      (let ((lp (fn ()
                  (match (at:read (.data mvar))
                    ((Some val)
                     (at:atomic-write (.data mvar) None)
                     (lk:release (.lock mvar))
                     (cv:notify (.notify-empty mvar))
                     (unmask-current! rt-prx)
                     val)
                    ((None)
                     (unmask-and-await-safely% rt-prx (.notify-full mvar) (.lock mvar))
                     (mask-current! rt-prx)
                     (lp))))))
        (lp))))

  (declare put-mvar (MonadIoThread :rt :t :m => MVar :a -> :a -> :m Unit))
  (define (put-mvar mvar val)
    "Put a value into an MVar, blocking until it becomes empty."
    (wrap-io-with-runtime (rt-prx)
      (let _ = (the (MVar :a) mvar))
      (let lock = (.lock mvar))
      (let data = (.data mvar))
      (mask-current! rt-prx)
      (lk:acquire lock)
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
                     (unmask-and-await-safely% rt-prx (.notify-empty mvar) lock)
                     (mask-current! rt-prx)
                     (lp))))))
        (lp))))

  (declare try-take-mvar (MonadIoThread :rt :t :m => MVar :a -> :m (Optional :a)))
  (define (try-take-mvar mvar)
    "Attempt to take a value from an MVar; returns None if empty."
    (wrap-io-with-runtime (rt-prx)
      (let _ = (the (MVar :a) mvar))
      (mask-current! rt-prx)
      (lk:acquire (.lock mvar))
      (match (at:read (.data mvar))
        ((Some x)
         (at:atomic-write (.data mvar) None)
         (lk:release (.lock mvar))
         (cv:notify (.notify-empty mvar))
         (unmask-current! rt-prx)
         (Some x))
        ((None)
         (lk:release (.lock mvar))
         (unmask-current! rt-prx)
         None))))

  (declare try-put-mvar (MonadIoThread :rt :t :m => MVar :a -> :a -> :m Boolean))
  (define (try-put-mvar mvar val)
    "Attempt to put a value into an MVar; returns False if full and the put fails,
True if the put succeeds."
    (let _ = (the (MVar :a) mvar))
    (wrap-io-with-runtime (rt-prx)
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
         True))))

  (declare read-mvar (MonadIoThread :rt :t :m => MVar :a -> :m :a))
  (define (read-mvar mvar)
    "Read (without removing) the value from an MVar, blocking until one is available."
    (let _ = (the (MVar :a) mvar))
    (wrap-io
      (match (at:read (.data mvar))
        ((Some x)
         x)
        ((None)
         (subscribe (.read-broadcast-pool mvar))))))

  (declare try-read-mvar (MonadIoThread :rt :t :m => MVar :a -> :m (Optional :a)))
  (define (try-read-mvar mvar)
    "Attempt to read (without removing) the value from an MVar; returns None if empty."
    (let _ = (the (MVar :a) mvar))
    (wrap-io
      (at:read (.data mvar))))

  (declare swap-mvar (MonadIoThread :rt :t :m => MVar :a -> :a -> :m :a))
  (define (swap-mvar mvar new-val)
    "Atomically replace the value in an MVar and return the old value."
    (let _ = (the (MVar :a) mvar))
    (wrap-io-with-runtime (rt-prx)
      (mask-current! rt-prx)
      (lk:acquire (.lock mvar))
      (let ((lp (fn ()
                  (match (at:read (.data mvar))
                    ((Some old-val)
                     (at:atomic-write (.data mvar) (Some new-val))
                     (lk:release (.lock mvar))
                     (cv:notify (.notify-full mvar))
                     (unmask-current! rt-prx)
                     old-val)
                    ((None)
                     (unmask-and-await-safely% rt-prx (.notify-full mvar) (.lock mvar))
                     (mask-current! rt-prx)
                     (lp))))))
        (lp))))

  (declare is-empty-mvar (MonadIoThread :rt :t :m => MVar :a -> :m Boolean))
  (define (is-empty-mvar mvar)
    "Return True if the MVar is currently empty."
    (wrap-io
      (opt:none? (at:read (.data mvar)))))

  ;; TODO: https://github.com/coalton-lang/coalton/issues/1717
  ;; (declare with-mvar ((UnliftIo :r :i) (LiftTo :r :m) (MonadException :i) (MonadIoThread :rt :t :i)
  ;;                      => MVar :a -> (:a -> :r :b) -> :m :b))
  (define (with-mvar mvar op)
    "Modify with the result of an operation. Blocks until MVar is full.
If the operation raises an exception, will restore the MVar value and re-raise.
If other threads are calling PUT-MVAR while the operation is running,
they can block this thread until another thread takes the MVar."
    (let _ = (the (MVar :a) mvar))
    (lift-to
     (with-run-in-io
       (fn (run)
         (do
          (mask-current-thread)
          (result <-
           (bracket-io_ (take-mvar mvar)
                        (put-mvar mvar)
                        (fn (x)
                          (run (op x)))))
          (unmask-current-thread)
          (pure result))))))
  )

(cl:defmacro do-with-mvar ((sym mvar) cl:&body body)
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

  (define-struct (Chan :a)
    "A synchronized FIFO queue to pass data directionally between threads."
    (head-var (MVar (MVar (ChanNode :a))))
    (tail-var (MVar (MVar (ChanNode :a)))))

  ;; (declare new-empty-chan (MonadIoThread :rt :t :m => :m (Chan :a)))
  (define (new-empty-chan)
    "Create a new empty channel."
    (do
     (cell <- new-empty-mvar)
     (head-var <- (new-mvar cell))
     (tail-var <- (new-mvar cell))
     (pure (Chan head-var tail-var))))

  ;; (declare push-chan (MonadIoThread :rt :t :m => Chan :a -> :a -> :m Unit))
  (define (push-chan chan val)
    "Push VAL onto CHAN."
    (let _ = (the (Chan :a) chan))
    (do
     (new-tail-var <- new-empty-mvar)
     (old-tail-var <- (take-mvar (.tail-var chan)))
     (put-mvar old-tail-var (ChanNode% val new-tail-var))
     (put-mvar (.tail-var chan) new-tail-var)))

  ;; (declare pop-chan (MonadIoThread :rt :t :m => Chan :a -> :m :a))
  (define (pop-chan chan)
    "Pop the front value in CHAN. Blocks while CHAN is empty."
    (let _ = (the (Chan :a) chan))
    (do
     (old-tail-var <- (take-mvar (.head-var chan)))
     ((ChanNode% val new-head-var) <- (take-mvar old-tail-var))
     (put-mvar (.head-var chan) new-head-var)
     (pure val)))
  )
