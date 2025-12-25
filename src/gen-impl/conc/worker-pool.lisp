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
  )
