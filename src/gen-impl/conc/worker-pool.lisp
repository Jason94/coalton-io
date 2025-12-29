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
   #:request-shutdown
   #:submit-job
   #:do-submit-job
   ))
(in-package :io/gen-impl/conc/worker-pool)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
    
  (define-struct (WorkerPool :i :t)
    "A pool of worker threads that execute jobs submitted to the pool."
    (n-threads UFix)
    (threads (ConcurrentGroup :t Unit))
    (queue (MChan (Optional (:i Unit)))))

  (declare worker-op ((BaseIo :i) (MonadIoThread :rt :t :i) (MonadException :i)
                      => MChan (Optional (:i Unit)) -> :i Unit))
  (define (worker-op queue)
    (do-matchM (pop-chan queue)
      ((None)
       (pure Unit))
      ((Some task)
       task
       (worker-op queue))))

  (declare fork-worker-op ((MonadIoThread :rt :t :i) (UnliftIo :i :i) (MonadException :i)
                           => MChan (Optional (:i Unit)) -> :i :t))
  (define (fork-worker-op queue)
    (fork-thread (worker-op queue)))

  (declare new-worker-pool ((MonadIoThread :rt :t :m) (UnliftIo :i :i) (MonadIoThread :rt :t :i)
                            (MonadException :i)
                            (MonadException :m) (Concurrent :t Unit) (LiftIo :i :m)
                            => UFix -> :m (WorkerPool :i :t)))
  (define (new-worker-pool n-threads)
    "Create a new worker pool. Automatically forks N-THREADS worker threads."
    ;; CONCURRENT:
    ;; - Concurrent concerns are managed by the thread group.
    ;; - If the thread is stopped after the fork completes but before the return, then the
    ;;   threads will be orphaned. There's no solution to that now (masking wouldn't help
    ;;   the fundamental problem), but it will be solved once structured concurrency is
    ;;   implemented.
    (do
     (do-when (zero? n-threads)
       (raise "Worker pool must be initialized with non-zero threads."))
     (queue <- new-empty-chan)
     (threads <- (lift-io (fork-group (l:repeat n-threads (fork-worker-op queue)))))
     (pure (WorkerPool n-threads threads queue))))

  (declare submit-job ((UnliftIo :r :i) (LiftTo :r :m) (MonadIoThread :rt :t :i)
                       (MonadException :i)
                       => WorkerPool :i :t -> :r Unit -> :m Unit))
  (define (submit-job pool job)
    "Submit a job to the worker pool. Any jobs submitted after a shutdown request will
be ignored."
    (lift-to
     (with-run-in-io
       (fn (run)
         (push-chan (.queue pool) (Some (run job)))))))

  (declare request-shutdown ((MonadIoThread :rt :t :m) (MonadException :m)
                             => WorkerPool :i :t -> :m Unit))
  (define (request-shutdown pool)
    "Request a shutdown. The threads in the pool will shutdown when all of the jobs already
in the queue are completed.

To immediately stop the threads, use `stop`."
    (do-loop-times (_ (.n-threads pool))
      (push-chan (.queue pool) None)))
  )

(coalton-toplevel
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
