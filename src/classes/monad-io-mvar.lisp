(cl:in-package :cl-user)
(defpackage :io/classes/monad-io-mvar
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/functions
   #:io/utils
   #:io/classes/monad-io
   #:io/thread-impl/data-broadcast-pool)
  (:local-nicknames
   (:st  #:coalton-library/monad/statet)
   (:env #:coalton-library/monad/environment)
   (:lk #:coalton-threads/lock)
   (:cv #:coalton-threads/condition-variable)
   (:at #:io/thread-impl/atomics))
  (:import-from #:coalton-library/experimental/do-control-loops-adv
   #:LoopT)
  (:export
   #:MVar
   #:MonadIoMVar
   #:derive-monad-io-mvar
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

   #:MChan
   #:new-empty-chan
   #:push-chan
   #:pop-chan
   ))
(in-package :io/classes/monad-io-mvar)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (define-struct (MVar :a)
    "A synchronized container that can be empty or hold one :a.
Can put data into the container, blocking until it is empty.
Can take data out of the container, blocking until it is full."
    (lock                lk:Lock)
    (read-broadcast-pool (DataBroadcastPool :a))
    (notify-full         cv:ConditionVariable)
    (notify-empty        cv:ConditionVariable)
    (data                (at:Atomic (Optional :a))))

  (define-class (Monad :m => MonadIoMVar :m)
    "Manage synchronized containers of mutable state.

All critical MVar operations are masked, so stopping a thread that's operating on
MVar's won't leave MVar's in an inoperable state. However, irresponsible stopping
could still cause deadlocks, race conditions, etc. if other threads were
depending on the stopped thread operating on an MVar to continue."
    (new-mvar
     "Create a new MVar containing an initial value."
     (:a -> :m (MVar :a)))
    (new-empty-mvar
     "Create a new empty MVar."
     (:m (MVar :a)))
    (take-mvar
     "Take a value from an MVar, blocking until one is available."
     (MVar :a -> :m :a))
    (put-mvar
     "Put a value into an MVar, blocking until it becomes empty."
     (MVar :a -> :a -> :m Unit))
    (try-take-mvar
     "Attempt to take a value from an MVar; returns None if empty."
     (MVar :a -> :m (Optional :a)))
    (try-put-mvar
     "Attempt to put a value into an MVar; returns False if full and the put fails,
True if the put succeeds."
     (MVar :a -> :a -> :m Boolean))
    (read-mvar
     "Read (without removing) the value from an MVar, blocking until one is available."
     (MVar :a -> :m :a))
    (try-read-mvar
     "Attempt to read (without removing) the value from an MVar; returns None if empty."
     (MVar :a -> :m (Optional :a)))
    (swap-mvar
     "Atomically replace the value in an MVar and return the old value."
     (MVar :a -> :a -> :m :a))
    (is-empty-mvar
     "Return True if the MVar is currently empty."
     (MVar :a -> :m Boolean))))

(cl:defmacro derive-monad-io-mvar (monad-param monadT-form)
  "Automatically derive MonadIoMVar for a monad transformer.

Example:
  (derive-monad-io-mvar :m (st:StateT :s :m))"
  `(define-instance ((MonadIoMVar ,monad-param) => MonadIoMVar ,monadT-form)
     (define new-mvar       (compose lift new-mvar))
     (define new-empty-mvar (lift new-empty-mvar))
     (define take-mvar      (compose lift take-mvar))
     (define put-mvar       (compose2 lift put-mvar))
     (define try-take-mvar  (compose lift try-take-mvar))
     (define try-put-mvar   (compose2 lift try-put-mvar))
     (define read-mvar      (compose lift read-mvar))
     (define try-read-mvar  (compose lift try-read-mvar))
     (define swap-mvar      (compose2 lift swap-mvar))
     (define is-empty-mvar  (compose lift is-empty-mvar))))

(coalton-toplevel
  ;;
  ;; Std. Library Transformer Instances
  ;;
  (derive-monad-io-mvar :m (st:StateT :s :m))
  (derive-monad-io-mvar :m (env:EnvT :e :m))
  (derive-monad-io-mvar :m (LoopT :m))
  )

(cl:defmacro do-with-mvar ((sym mvar) cl:&body body)
  `(with-mvar
     ,mvar
     (fn (,sym)
       (do
        ,@body))))
;;;
;;; MChan
;;;

(coalton-toplevel
  (define-type (ChanNode :a)
    (ChanNode% :a (MVar (ChanNode :a))))

  (define-struct (MChan :a)
    "A synchronized FIFO queue to pass data directionally between threads."
    (head-var (MVar (MVar (ChanNode :a))))
    (tail-var (MVar (MVar (ChanNode :a)))))

  (declare new-empty-chan (MonadIoMVar :m => :m (MChan :a)))
  (define new-empty-chan
    "Create a new empty channel."
    (do
     (cell <- new-empty-mvar)
     (head-var <- (new-mvar cell))
     (tail-var <- (new-mvar cell))
     (pure (MChan head-var tail-var))))

  (declare push-chan (MonadIoMVar :m => MChan :a -> :a -> :m Unit))
  (define (push-chan chan val)
    "Push VAL onto CHAN."
    (do
     (new-tail-var <- new-empty-mvar)
     (old-tail-var <- (take-mvar (.tail-var chan)))
     (put-mvar old-tail-var (ChanNode% val new-tail-var))
     (put-mvar (.tail-var chan) new-tail-var)))

  (declare pop-chan (MonadIoMVar :m => MChan :a -> :m :a))
  (define (pop-chan chan)
    "Pop the front value in CHAN. Blocks while CHAN is empty."
    (do
     (old-tail-var <- (take-mvar (.head-var chan)))
     ((ChanNode% val new-head-var) <- (take-mvar old-tail-var))
     (put-mvar (.head-var chan) new-head-var)
     (pure val)))
  )
