(cl:in-package :cl-user)
(defpackage :io/gen-impl/mvar
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/functions
   #:coalton-library/monad/classes
   #:io/utils
   #:io/classes/monad-io
   #:io/classes/monad-io-mvar
   #:io/resource
   #:io/thread-impl/runtime
   #:io/thread-impl/data-broadcast-pool
   )
  (:local-nicknames
   (:opt #:coalton-library/optional)
   (:at #:io/thread-impl/atomics)
   (:lk  #:coalton-threads/lock)
   (:cv  #:coalton-threads/condition-variable)
   )
  (:export
   #:implement-monad-io-var
   ))
(in-package :io/gen-impl/mvar)

(named-readtables:in-readtable coalton:coalton)

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

  ;; TODO: Add a catch thread interrupt macro helper in thread-exceptions for internal
  ;; use, and then use it here.
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

  ;; TODO: This belongs in the classes/ file, but because we don't have generic machinery
  ;; for low-level, non-monadic masking yet, I'm leaving it here for now.
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
