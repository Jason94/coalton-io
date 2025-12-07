(cl:in-package :cl-user)
(defpackage :io/mvar
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/functions
   #:coalton-library/monad/classes
   #:io/utils
   #:io/monad-io
   #:io/exception
   #:io/resource
   #:io/thread-impl/runtime)
  (:import-from #:coalton-library/experimental/do-control-loops-adv
   #:LoopT)
  (:local-nicknames
   (:c   #:coalton-library/cell)
   (:lk  #:coalton-threads/lock)
   (:cv  #:coalton-threads/condition-variable)
   (:st  #:coalton-library/monad/statet)
   (:env #:coalton-library/monad/environment)
   (:io #:io/simple-io))
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

   #:with-mvar
   #:with-mvar_
   #:do-with-mvar
   #:do-with-mvar_

   #:MChan
   #:new-empty-chan
   #:push-chan
   #:pop-chan
   
   #:implement-monad-io-mvar
   ))
(in-package :io/mvar)

(named-readtables:in-readtable coalton:coalton)

;;;
;;; MVar
;;;

(coalton-toplevel
  (define-struct (MVar :a)
    "A synchronized container that can be empty or hold one :a.
Can put data into the container, blocking until it is empty.
Can take data out of the container, blocking until it is full."
    (lock lk:Lock)
    (cvar cv:ConditionVariable)
    (data (c:Cell (Optional :a))))

  (define-class (MonadIo :m => MonadIoMVar :m)
    "Manage synchronized containers of mutable state.

All MVar operations are masked, so stopping a thread that's operating on MVar's
won't leave those MVar's in an inoperable state. However, irresponsible stopping
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
     (MVar :a -> :m Boolean))
    (with-mvar
     "Modify with the result of an operation. Blocks until MVar is full.
If the operation raises an exception, will restore the MVar value and re-raise.
If other threads are calling PUT-MVAR while the operation is running,
they can block this thread until another thread takes the MVar."
     ((UnliftIo :r :i) (LiftTo :r :m) (MonadException :i)
      => MVar :a -> (:a -> :r :b) -> :m :b)))

  (inline)
  (declare new-mvar% (MonadIo :m => :a -> :m (MVar :a)))
  (define (new-mvar% val)
    "Create a new MVar containing VAL."
    (wrap-io
      (MVar (lk:new) (cv:new) (c:new (Some val)))))

  (inline)
  (declare new-empty-mvar% (MonadIo :m => :m (MVar :a)))
  (define new-empty-mvar%
    "Create a new empty MVar."
    (wrap-io
      (MVar (lk:new) (cv:new) (c:new None))))

  (declare take-mvar% (MonadIo :m => MVar :a -> :m :a))
  (define (take-mvar% mvar)
    (wrap-io
      (mask-current-thread!%)
      (lk:acquire (.lock mvar))
      (let ((lp (fn ()
                  (match (c:read (.data mvar))
                    ((Some x)
                     (c:write! (.data mvar) None)
                     (lk:release (.lock mvar))
                     (cv:notify (.cvar mvar))
                     (unmask-current-thread!%)
                     x)
                    ((None)
                     (cv:await (.cvar mvar) (.lock mvar))
                     (lp))))))
        (lp))))

  (declare put-mvar% (MonadIo :m => MVar :a -> :a -> :m Unit))
  (define (put-mvar% mvar val)
    (wrap-io
      (let lock = (.lock mvar))
      (let data = (.data mvar))
      (let cvar = (.cvar mvar))
      (mask-current-thread!%)
      (lk:acquire lock)
      (let ((lp (fn ()
                  (match (c:read data)
                    ((None)
                     (c:write! data (Some val))
                     (lk:release lock)
                     (cv:notify cvar)
                     (unmask-current-thread!%)
                     Unit)
                    ((Some _)
                     (cv:await cvar lock)
                     (lp))))))
        (lp))))

  (declare try-take-mvar% (MonadIo :m => MVar :a -> :m (Optional :a)))
  (define (try-take-mvar% mvar)
    (wrap-io
      (mask-current-thread!%)
      (lk:acquire (.lock mvar))
      (match (c:read (.data mvar))
        ((Some x)
         (c:write! (.data mvar) None)
         (lk:release (.lock mvar))
         (cv:notify (.cvar mvar))
         (unmask-current-thread!%)
         (Some x))
        ((None)
         (lk:release (.lock mvar))
         (unmask-current-thread!%)
         None))))

  (declare try-put-mvar% (MonadIo :m => MVar :a -> :a -> :m Boolean))
  (define (try-put-mvar% mvar val)
    (wrap-io
      (mask-current-thread!%)
      (lk:acquire (.lock mvar))
      (match (c:read (.data mvar))
        ((Some _)
         (lk:release (.lock mvar))
         (unmask-current-thread!%)
         False)
        ((None)
         (c:write! (.data mvar) (Some val))
         (lk:release (.lock mvar))
         (cv:notify (.cvar mvar))
         (unmask-current-thread!%)
         True))))

  (declare read-mvar% (MonadIo :m => MVar :a -> :m :a))
  (define (read-mvar% mvar)
    (wrap-io
      (mask-current-thread!%)
      (lk:acquire (.lock mvar))
      (let ((lp (fn ()
                  (match (c:read (.data mvar))
                    ((Some x)
                     (lk:release (.lock mvar))
                     (unmask-current-thread!%)
                     x)
                    ((None)
                     (cv:await (.cvar mvar) (.lock mvar))
                     (lp))))))
        (lp))))

  ;; TODO: Could this be lock-free?
  (declare try-read-mvar% (MonadIo :m => MVar :a -> :m (Optional :a)))
  (define (try-read-mvar% mvar)
    (wrap-io
      (mask-current-thread!%)
      (lk:acquire (.lock mvar))
      (match (c:read (.data mvar))
        ((Some x)
         (lk:release (.lock mvar))
         (cv:notify (.cvar mvar))
         (unmask-current-thread!%)
         (Some x))
        ((None)
         (lk:release (.lock mvar))
         (unmask-current-thread!%)
         None))))

  (declare swap-mvar% (MonadIo :m => MVar :a -> :a -> :m :a))
  (define (swap-mvar% mvar new-val)
    (wrap-io
      (mask-current-thread!%)
      (lk:acquire (.lock mvar))
      (let ((lp (fn ()
                  (match (c:read (.data mvar))
                    ((Some old-val)
                     (c:write! (.data mvar) (Some new-val))
                     (lk:release (.lock mvar))
                     (cv:notify (.cvar mvar))
                     (unmask-current-thread!%)
                     old-val)
                    ((None)
                     (cv:await (.cvar mvar) (.lock mvar))
                     (lp))))))
        (lp))))

  (declare is-empty-mvar% (MonadIo :m => MVar :a -> :m Boolean))
  (define (is-empty-mvar% mvar)
    (wrap-io
      (mask-current-thread!%)
      (lk:acquire (.lock mvar))
      (let result =
        (match (c:read (.data mvar))
          ((Some _) False)
          ((None)   True)))
      (lk:release (.lock mvar))
      (unmask-current-thread!%)
      result))

  (declare with-mvar% ((MonadIo :m) (UnliftIo :r :i) (LiftTo :r :m) (MonadException :i)
                       => MVar :a -> (:a -> :r :b) -> :m :b))
  (define (with-mvar% mvar op)
    (lift-to
     (with-run-in-io
       (fn (run)
         (lift-io
          (do
           mask-current-thread%
           (result <-
            (bracket-io_ (take-mvar% mvar)
                         (put-mvar% mvar)
                         (fn (x)
                           (run (op x)))))
           unmask-current-thread%
           (pure result)))))))
  )

(cl:defmacro implement-monad-io-mvar (monad)
  `(define-instance (MonadIoMVar ,monad)
     (define new-mvar       new-mvar%)
     (define new-empty-mvar new-empty-mvar%)
     (define take-mvar      take-mvar%)
     (define put-mvar       put-mvar%)
     (define try-take-mvar  try-take-mvar%)
     (define try-put-mvar   try-put-mvar%)
     (define read-mvar      read-mvar%)
     (define try-read-mvar  try-read-mvar%)
     (define swap-mvar      swap-mvar%)
     (define is-empty-mvar  is-empty-mvar%)
     (define with-mvar      with-mvar%)))

(cl:defmacro derive-monad-io-mvar (monad-param monadT-form)
  "Automatically derive MonadIoMVar for a monad transformer.

Example:
  (derive-monad-io-mvar :m (st:StateT :s :m))"
  `(define-instance (MonadIoMVar ,monad-param => MonadIoMVar ,monadT-form)
     (define new-mvar       (compose lift new-mvar))
     (define new-empty-mvar (lift new-empty-mvar))
     (define take-mvar      (compose lift take-mvar))
     (define put-mvar       (compose2 lift put-mvar))
     (define try-take-mvar  (compose lift try-take-mvar))
     (define try-put-mvar   (compose2 lift try-put-mvar))
     (define read-mvar      (compose lift read-mvar))
     (define try-read-mvar  (compose lift try-read-mvar))
     (define swap-mvar      (compose2 lift swap-mvar))
     (define is-empty-mvar  (compose lift is-empty-mvar))
     (define with-mvar      with-mvar)))

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
;;; Other MVar Functions
;;;

(coalton-toplevel

  (declare with-mvar_ ((LiftTo io:IO :m) (MonadIoMVar :m) => MVar :a -> (:a -> io:IO :b) -> :m :b))
  (define with-mvar_ with-mvar)

  )

(cl:defmacro do-with-mvar_ ((sym mvar) cl:&body body)
  `(with-mvar_
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

;;
;; Simple IO Implementation
;;

(coalton-toplevel

  (implement-monad-io-mvar io:IO))
