(cl:in-package :cl-user)
(defpackage :io/gen-impl/conc/worker-pool
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/monad/classes
   #:coalton-library/experimental/do-control-core
   #:io/utils
   #:io/threads-exceptions
   #:io/classes/exceptions
   #:io/classes/monad-io
   #:io/classes/thread
   #:io/classes/conc/scheduler
   #:io/gen-impl/conc/group
   )
  (:import-from #:coalton-library/experimental/do-control-core
   #:do-when)
  (:import-from #:coalton-library/experimental/do-control-loops
   #:do-loop-times)
  (:local-nicknames
   (:m #:coalton/math/integral)
   (:it #:coalton/iterator)
   (:l #:coalton-library/list))
  (:export
   #:WorkerPool
   #:new-worker-pool
   #:request-shutdown
   #:submit-job
   #:submit-job-with
   #:do-submit-job
   #:do-submit-job-with

   #:IndexChunk
   #:submit-indexed-chunks
   #:do-submit-indexed-chunks
   ))
(in-package :io/gen-impl/conc/worker-pool)

(named-readtables:in-readtable coalton:coalton)

;;;
;;; WorkerPool Core
;;; 

(coalton-toplevel
    
  (define-struct (WorkerPool :s :i :t)
    "A pool of worker threads that execute jobs submitted to the pool."
    (n-threads UFix)
    (threads (ConcurrentGroup :t Unit))
    (scheduler (:s (Optional (:i Unit)))))

  (declare worker-op ((BaseIo :i) (Threads :rt :t :i) (Scheduler :s)
                      => UFix * :s (Optional (:i Unit)) -> :i Unit))
  (define (worker-op thread-index scheduler)
    (do-matchM (take-item thread-index scheduler)
      ((None)
       (pure Unit))
      ((Some task)
       task
       (worker-op thread-index scheduler))))

  (declare fork-worker-op ((Threads :rt :t :i) (UnliftIo :i :i) (Exceptions :i)
                           (Scheduler :s)
                           => UFix * :s (Optional (:i Unit)) -> :i :t))
  (define (fork-worker-op thread-index scheduler)
    (fork-thread (worker-op thread-index scheduler)))

  (declare new-worker-pool ((Threads :rt :t :m) (UnliftIo :i :i) (Threads :rt :t :i)
                            (Exceptions :i)
                            (Exceptions :m) (Concurrent :t Unit) (LiftIo :i :m)
                            (Scheduler :s)
                            => UFix * :s (Optional (:i Unit)) -> :m (WorkerPool :s :i :t)))
  (define (new-worker-pool n-threads scheduler)
    "Create a new worker pool. Automatically forks N-THREADS worker threads."
    ;; CONCURRENT:
    ;; - Concurrent concerns are managed by the thread group.
    ;; - If the thread is stopped after the fork completes but before the return, then
    ;;   structured concurrency keeps the forked threads from being orphaned.
    (do
     (do-when (zero? n-threads)
       (raise "Worker pool must be initialized with non-zero threads."))
     (threads <- (lift-io (fork-group
                           (map (fn (i) (fork-worker-op i scheduler))
                                (l:range 0 (1- n-threads))))))
     (pure (WorkerPool n-threads threads scheduler))))

  (declare submit-job-with ((UnliftIo :r :i) (LiftTo :r :m) (Threads :rt :t :i)
                            (Exceptions :i) (Scheduler :s)
                            => TimeoutStrategy * WorkerPool :s :i :t * :r :a -> :m Unit))
  (define (submit-job-with strategy pool job)
    "Submit a job to the worker pool. Any jobs submitted after a shutdown request will
be ignored.

Concurrent:
  - If the pool's scheduler is backed by a bounded data structure, then this can block
    while the scheduler is full, possibly timing out based on STRATEGY."
    (lift-to
     (with-run-in-io
       (fn (run)
         (submit-with (Some (map (const Unit) (run job)))
                      strategy
                      (.scheduler pool))))))

  (inline)
  (declare submit-job ((UnliftIo :r :i) (LiftTo :r :m) (Threads :rt :t :i)
                       (Exceptions :i) (Scheduler :s)
                       => WorkerPool :s :i :t * :r :a -> :m Unit))
  (define (submit-job pool job)
    "Submit a job to the worker pool. Any jobs submitted after a shutdown request will
be ignored.

Concurrent:
  - If the pool's scheduler is backed by a bounded data structure, then this can block
    while the scheduler is full."
    (submit-job-with NoTimeout pool job))

  (declare request-shutdown ((Threads :rt :t :m) (Exceptions :m) (Scheduler :s)
                             => WorkerPool :s :i :t -> :m Unit))
  (define (request-shutdown pool)
    "Request a shutdown. The threads in the pool will shutdown when all of the jobs already
in the queue are completed.

To immediately stop the threads, use `stop`."
    (do-loop-times (_ (.n-threads pool))
      (submit None (.scheduler pool))))
  )

(coalton-toplevel
  (define-instance ((Concurrent (ConcurrentGroup :t Unit) (List Unit)) (Scheduler :s)
                    => Concurrent (WorkerPool :s :i :t) Unit)
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

(defmacro do-submit-job-with (strategy pool cl:&body body)
  `(submit-job-with ,strategy ,pool
    (do
     ,@body)))

;;;
;;; Chunking
;;;

(coalton-toplevel
  (derive Eq)
  (define-struct IndexChunk
    "A chunk of indexed work with a start and end (exclusive)."
    (start UFix)
    (end UFix))

  (define-instance (it:IntoIterator IndexChunk UFix)
    (inline)
    (define (it:into-iter chunk)
      (it:range-increasing 1 (.start chunk) (.end chunk))))

  (declare submit-indexed-chunks ((UnliftIo :r :i) (LiftTo :r :m) (Threads :rt :t :i)
                                  (Exceptions :i) (Scheduler :s) (Exceptions :m)
                                  => WorkerPool :s :i :t * UFix * UFix * UFix * (IndexChunk -> :r :a)
                                  &key (:timeout TimeoutStrategy)
                                  -> :m Unit))
  (define (submit-indexed-chunks pool start end chunk-size chunk->job &key (timeout NoTimeout))
    "Submit chunks of indexed-work to the worker pool. The first chunk starts at `start`,
and the last chunk ends at `end`, exclusive. Each chunk has size `chunk-size`, except the
last, which has the remaining work. A job is responsible for executing all work inside the
chunk it is passed.

Any jobs submitted after a shutdown request will be ignored.

Concurrent:
  - If the pool's scheduler is backed by a bounded data structure, then this can block
    while the scheduler is full."
    (cond
      ((> start end)
       (raise "Error: `start` must come before `end`"))
      ((zero? chunk-size)
       (raise "Error: `chunk-size` cannot be 0"))
      ((== start end)
       (pure Unit))
      (True
       (let (values n-normal-chunks rem-work) = (m:divmod (- end start) chunk-size))
       (do
        (do-loop-times (i n-normal-chunks)
          (let chunk-start = (+ start (* chunk-size i)))
          (let chunk-end = (+ chunk-start chunk-size))
          (submit-job-with
           timeout
           pool
           (chunk->job (IndexChunk chunk-start chunk-end))))
        (do-when (not (zero? rem-work))
          (submit-job-with
           timeout
           pool
           (chunk->job (IndexChunk (- end rem-work) end))))))))
  )

(defmacro do-submit-indexed-chunks (pool (chunk-sym (start end chunk-size cl:&key (timeout 'NoTimeout))) cl:&body body)
  "Submit chunks of indexed-work to the worker pool. The first chunk starts at `start`,
and the last chunk ends at `end`, exclusive. Each chunk has size `chunk-size`, except the
last, which has the remaining work. A job is responsible for executing all work inside the
chunk it is passed.

Any jobs submitted after a shutdown request will be ignored.

Concurrent:
  - If the pool's scheduler is backed by a bounded data structure, then this can block
    while the scheduler is full."
  `(submit-indexed-chunks ,pool ,start ,end ,chunk-size
    (fn (,chunk-sym)
      (do
       ,@body))
    :timeout ,timeout))
      
