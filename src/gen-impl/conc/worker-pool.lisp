(cl:in-package :cl-user)
(defpackage :io/gen-impl/conc/worker-pool
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/monad/classes
   #:coalton-library/experimental/do-control-core
   #:coalton-library/experimental/do-control-loops
   #:io/utils
   #:io/thread-exceptions
   #:io/classes/monad-exception
   #:io/classes/monad-io
   #:io/classes/monad-io-thread
   #:io/gen-impl/conc/mvar
   #:io/gen-impl/conc/group
   )
  (:local-nicknames
   (:l #:coalton-library/list))
  (:export
   #:WorkerPool
   #:new-worker-pool
   #:submit-job
   #:request-shutdown
   #:do-submit-job
   ))
(in-package :io/gen-impl/conc/worker-pool)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
    
  (define-struct (WorkerPool :i :t)
    (n-threads UFix)
    (threads (ConcurrentGroup :t Unit))
    (queue (MChan (Optional (:i Unit)))))

  (declare worker-op ((BaseIo :i) (MonadIoThread :rt :t :i) => MChan (Optional (:i Unit)) -> :i Unit))
  (define (worker-op queue)
    (do-matchM (pop-chan queue)
      ((None)
       (pure Unit))
      ((Some task)
       task
       (worker-op queue))))

  (declare fork-worker-op ((MonadIoThread :rt :t :i) (UnliftIo :i :i)
                           => MChan (Optional (:i Unit)) -> :i :t))
  (define (fork-worker-op queue)
    (fork-thread (worker-op queue)))

  (declare new-worker-pool ((MonadIoThread :rt :t :m) (UnliftIo :i :i) (MonadIoThread :rt :t :i)
                            (MonadException :m) (Concurrent :t Unit) (LiftIo :i :m)
                            => UFix -> :m (WorkerPool :i :t)))
  (define (new-worker-pool n-threads)
    (do
     (do-when (zero? n-threads)
       (raise "Worker pool must be initialized with non-zero threads."))
     (queue <- new-empty-chan)
     (threads <- (lift-io (fork-group (l:repeat n-threads (fork-worker-op queue)))))
     (pure (WorkerPool n-threads threads queue))))

  (declare submit-job ((UnliftIo :r :i) (LiftTo :r :m) (MonadIoThread :rt :t :i)
                       => WorkerPool :i :t -> :r Unit -> :m Unit))
  (define (submit-job pool job)
    (lift-to
     (with-run-in-io
       (fn (run)
         (push-chan (.queue pool) (Some (run job)))))))

  (declare request-shutdown (MonadIoThread :rt :t :m => WorkerPool :i :t -> :m Unit))
  (define (request-shutdown pool)
    (do-loop-times (_ (.n-threads pool))
      (push-chan (.queue pool) None)))

  (declare stop% ((Concurrent (ConcurrentGroup :t Unit) (List Unit)) (MonadException :m) (MonadIoThread :rt :t :m)
                  => WorkerPool :i :t -> :m Unit))
  (define (stop% pool)
    (stop (.threads pool)))

  ;; TODO: As far as I can tell, (Concurrent :t Unit) should be enough for the compiler to "find"
  ;; the (Concurrent (ConcurrentGroup :t Unit)) instance, but for some reason it needs the annotation.
  ;; Post an issue to Github and/or try to find a minimal reproduction.
  (declare await-foo ((Concurrent :t Unit) (MonadException :m) (MonadIoThread :rt :t :m)
                      (Concurrent (ConcurrentGroup :t Unit) (List Unit))
                      => WorkerPool :i :t -> :m Unit))
  (define (await-foo pool)
    (do
     (await (.threads pool))
     (pure Unit)))
  )

(coalton-toplevel

  (declare unmask-finally% ((Concurrent (ConcurrentGroup :t Unit) (List Unit)) (UnliftIo :r :i) (LiftTo :r :m)
                            (MonadIoThread :rt :t :m) (MonadException :m) (MonadIoThread :rt :t :r)
                            (Concurrent :t Unit)
                            => WorkerPool :i :t -> (UnmaskFinallyMode -> :r :b) -> :m Unit))
  (define (unmask-finally% pool f)
    (unmask-finally (.threads pool) f)
  )

  (define-instance (Concurrent (ConcurrentGroup :t Unit) (List Unit)
                    => Concurrent (WorkerPool :i :t) Unit)
    (inline)
    (define (stop pool)
      (stop (.threads pool)))
    (inline)
    (define (await pool)
      (do
       (await (.threads pool))
       (pure Unit)))
    (inline)
    (define (mask pool)
      (mask (.threads pool)))
    (inline)
    (define (unmask pool)
      (unmask (.threads pool)))
    (inline)
    (define (unmask-finally pool f)
      (unmask-finally (.threads pool) f)))
    
  )

(defmacro do-submit-job (pool cl:&body body)
  `(submit-job ,pool
    (do
     ,@body)))
