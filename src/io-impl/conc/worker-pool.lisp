(cl:in-package :cl-user)
(defpackage :io/io-impl/conc/worker-pool
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/monad/classes
   #:io/classes/monad-io
   #:io/classes/exceptions
   #:io/classes/thread
   #:io/classes/conc/scheduler
   #:io/threads-impl/runtime
   #:io/gen-impl/conc/worker-pool
   #:io/io-impl/runtime
   #:io/io-impl/simple-io
   )
  (:export
   #:new-worker-pool_
   #:submit-job_
   #:do-submit-job_
   #:do-submit-job-with_
   #:submit-indexed-chunks_
   #:do-submit-indexed-chunks_
   ))
(in-package :io/io-impl/conc/worker-pool)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (declare new-worker-pool_ ((Threads IoRuntime IoThread :m) (Exceptions :m)
                             (LiftIo IO :m) (Scheduler :s)
                             => UFix * :s (Optional (IO Unit)) -> :m (WorkerPool :s IO IoThread)))
  (define new-worker-pool_ new-worker-pool)

  (declare submit-job_ ((Threads IoRuntime IoThread :m)
                        (LiftTo IO :m) (Scheduler :s)
                        => WorkerPool :s IO IoThread * IO :a -> :m Unit))
  (define submit-job_ submit-job)

  (declare submit-job-with_ ((Threads IoRuntime IoThread :m)
                             (LiftTo IO :m) (Scheduler :s)
                             => TimeoutStrategy * WorkerPool :s IO IoThread * IO :a -> :m Unit))
  (define submit-job-with_ submit-job-with)

  (declare submit-indexed-chunks_ ((LiftTo IO :m) (Scheduler :s) (Exceptions :m)
                                   => WorkerPool :s IO IoThread * UFix * UFix * UFix * (IndexChunk -> IO :a)
                                   &key (:timeout TimeoutStrategy)
                                   -> :m Unit))
  (define submit-indexed-chunks_
    "Submit chunks of indexed-work to the worker pool. The first chunk starts at `start`,
and the last chunk ends at `end`, exclusive. Each chunk has size `chunk-size`, except the
last, which has the remaining work. A job is responsible for executing all work inside the
chunk it is passed.

Any jobs submitted after a shutdown request will be ignored.

Concurrent:
  - If the pool's scheduler is backed by a bounded data structure, then this can block
    while the scheduler is full."
    submit-indexed-chunks)
  )

(defmacro do-submit-job_ (pool cl:&body body)
  `(submit-job_ ,pool
    (do
     ,@body)))

(defmacro do-submit-job-with_ (strategy pool cl:&body body)
  `(submit-job_ ,strategy ,pool
    (do
     ,@body)))

(defmacro do-submit-indexed-chunks_ (pool (chunk-sym (start end chunk-size cl:&key (timeout 'NoTimeout))) cl:&body body)
  "Submit chunks of indexed-work to the worker pool. The first chunk starts at `start`,
and the last chunk ends at `end`, exclusive. Each chunk has size `chunk-size`, except the
last, which has the remaining work. A job is responsible for executing all work inside the
chunk it is passed.

Any jobs submitted after a shutdown request will be ignored.

Concurrent:
  - If the pool's scheduler is backed by a bounded data structure, then this can block
    while the scheduler is full."
  `(submit-indexed-chunks_ ,pool ,start ,end ,chunk-size
    (fn (,chunk-sym)
      (do
       ,@body))
    :timeout ,timeout))
