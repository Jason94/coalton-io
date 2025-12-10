(cl:in-package :cl-user)
(defpackage :io/gen-impl/mvar
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/functions
   #:coalton-library/monad/classes
   #:io/utils
   #:io/monad-io
   #:io/classes/monad-io-mvar
   #:io/thread
   #:io/exception
   #:io/resource
   #:io/thread-impl/runtime
   #:io/thread-impl/data-broadcast-pool)
  (:import-from #:coalton-library/experimental/do-control-loops-adv
   #:LoopT)
  (:local-nicknames
   (:lk  #:coalton-threads/lock)
   (:cv  #:coalton-threads/condition-variable)
   (:opt #:coalton-library/optional)
   (:st  #:coalton-library/monad/statet)
   (:env #:coalton-library/monad/environment)
   (:io #:io/simple-io)
   (:at #:io/atomics_))
  (:export
   ;; Re-exports from io/classes/monad-io-mvar
   #:MVar
   #:MonadIoMVar
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

   ;; Remaining exports
   #:derive-monad-io-mvar

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
(in-package :io/gen-impl/mvar)

(named-readtables:in-readtable coalton:coalton)

;;;
;;; MVar
;;;

(coalton-toplevel

  (inline)
  (declare new-mvar% (MonadIo :m => :a -> :m (MVar :a)))
  (define (new-mvar% val)
    "Create a new MVar containing VAL."
    (wrap-io
      (MVar (lk:new)
            (new-broadcast-pool)
            (cv:new)
            (cv:new)
            (at:new (Some val)))))

  (inline)
  (declare new-empty-mvar% (MonadIo :m => :m (MVar :a)))
  (define new-empty-mvar%
    "Create a new empty MVar."
    (wrap-io
      (MVar (lk:new)
            (new-broadcast-pool)
            (cv:new)
            (cv:new)
            (at:new None))))

  (inline)
  (define (unmask-and-await-safely% cv lock)
    "Unmask the thread. Finally, either await (still running) the CV
or just release the LOCK."
    (unmask-current-thread-finally!%
     (fn (mode)
       (if (== Running mode)
           (cv:await cv lock)
           (progn
             (lk:release lock)
             Unit)))))

  (declare take-mvar% (MonadIo :m => MVar :a -> :m :a))
  (define (take-mvar% mvar)
    (wrap-io
      (mask-current-thread!%)
      (lk:acquire (.lock mvar))
      (let ((lp (fn ()
                  (match (at:read (.data mvar))
                    ((Some val)
                     (at:atomic-write (.data mvar) None)
                     (lk:release (.lock mvar))
                     (cv:notify (.notify-empty mvar))
                     (unmask-current-thread!%)
                     val)
                    ((None)
                     (unmask-and-await-safely% (.notify-full mvar) (.lock mvar))
                     (mask-current-thread!%)
                     (lp))))))
        (lp))))

  (declare put-mvar% (MonadIo :m => MVar :a -> :a -> :m Unit))
  (define (put-mvar% mvar val)
    (wrap-io
      (let lock = (.lock mvar))
      (let data = (.data mvar))
      (mask-current-thread!%)
      (lk:acquire lock)
      (let ((lp (fn ()
                  (match (at:read data)
                    ((None)
                     (at:atomic-write data (Some val))
                     (lk:release lock)
                     (publish (.read-broadcast-pool mvar) val)
                     (cv:notify (.notify-full mvar))
                     (unmask-current-thread!%)
                     Unit)
                    ((Some _)
                     (unmask-and-await-safely% (.notify-empty mvar) lock)
                     (mask-current-thread!%)
                     (lp))))))
        (lp))))

  (declare try-take-mvar% (MonadIo :m => MVar :a -> :m (Optional :a)))
  (define (try-take-mvar% mvar)
    (wrap-io
      (mask-current-thread!%)
      (lk:acquire (.lock mvar))
      (match (at:read (.data mvar))
        ((Some x)
         (at:atomic-write (.data mvar) None)
         (lk:release (.lock mvar))
         (cv:notify (.notify-empty mvar))
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
      (match (at:read (.data mvar))
        ((Some _)
         (lk:release (.lock mvar))
         (unmask-current-thread!%)
         False)
        ((None)
         (at:atomic-write (.data mvar) (Some val))
         (lk:release (.lock mvar))
         (publish (.read-broadcast-pool mvar) val)
         (cv:notify (.notify-full mvar))
         (unmask-current-thread!%)
         True))))

  (declare read-mvar% (MonadIo :m => MVar :a -> :m :a))
  (define (read-mvar% mvar)
    (wrap-io
      (match (at:read (.data mvar))
        ((Some x)
         x)
        ((None)
         (subscribe (.read-broadcast-pool mvar))))))

  (declare try-read-mvar% (MonadIo :m => MVar :a -> :m (Optional :a)))
  (define (try-read-mvar% mvar)
    (wrap-io
      (at:read (.data mvar))))

  (declare swap-mvar% (MonadIo :m => MVar :a -> :a -> :m :a))
  (define (swap-mvar% mvar new-val)
    (wrap-io
      (mask-current-thread!%)
      (lk:acquire (.lock mvar))
      (let ((lp (fn ()
                  (match (at:read (.data mvar))
                    ((Some old-val)
                     (at:atomic-write (.data mvar) (Some new-val))
                     (lk:release (.lock mvar))
                     (cv:notify (.notify-full mvar))
                     (unmask-current-thread!%)
                     old-val)
                    ((None)
                     (unmask-and-await-safely% (.notify-full mvar) (.lock mvar))
                     (mask-current-thread!%)
                     (lp))))))
        (lp))))

  (declare is-empty-mvar% (MonadIo :m => MVar :a -> :m Boolean))
  (define (is-empty-mvar% mvar)
    (wrap-io
      (opt:none? (at:read (.data mvar)))))

  ;; TODO: Submit Coalton issue for chained fundeps here (see also io-file)
  ;; (declare with-mvar ((UnliftIo :r :i) (LiftTo :r :m) (MonadException :i) (MonadIoThread :i :t)
  ;;                      => MVar :a -> (:a -> :r :b) -> :m :b))
  (define (with-mvar mvar op)
    "Modify with the result of an operation. Blocks until MVar is full.
If the operation raises an exception, will restore the MVar value and re-raise.
If other threads are calling PUT-MVAR while the operation is running,
they can block this thread until another thread takes the MVar."
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
     (define is-empty-mvar  is-empty-mvar%)))

(cl:defmacro derive-monad-io-mvar (monad-param monadT-form)
  "Automatically derive MonadIoMVar for a monad transformer.

Example:
  (derive-monad-io-mvar :m (st:StateT :s :m))"
  `(define-instance ((MonadIoMVar ,monad-param) (MonadIoThread ,monad-param :thrd) => MonadIoMVar ,monadT-form)
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
;;; Other MVar Functions
;;;



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

