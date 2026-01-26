(cl:in-package :cl-user)
(defpackage :io/io-impl/conc/worker-pool
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/monad/classes
   #:io/classes/monad-io
   #:io/classes/monad-exception
   #:io/classes/monad-io-thread
   #:io/classes/conc/scheduler
   #:io/thread-impl/runtime
   #:io/gen-impl/conc/worker-pool
   #:io/io-impl/runtime
   #:io/io-impl/simple-io
   )
  (:export
   #:new-worker-pool_
   #:submit-job_
   #:do-submit-job_
   #:do-submit-job-with_
   ))
(in-package :io/io-impl/conc/worker-pool)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (declare new-worker-pool_ ((MonadIoThread IoRuntime IoThread :m) (MonadException :m)
                             (LiftIo IO :m) (Scheduler :s)
                             => UFix -> :s (Optional (IO Unit)) -> :m (WorkerPool :s IO IoThread)))
  (define new-worker-pool_ new-worker-pool)

  (declare submit-job_ ((MonadIoThread IoRuntime IoThread :m)
                        (LiftTo IO :m) (Scheduler :s)
                        => WorkerPool :s IO IoThread -> IO :a -> :m Unit))
  (define submit-job_ submit-job)

  (declare submit-job-with_ ((MonadIoThread IoRuntime IoThread :m)
                             (LiftTo IO :m) (Scheduler :s)
                             => TimeoutStrategy -> WorkerPool :s IO IoThread -> IO :a -> :m Unit))
  (define submit-job-with_ submit-job-with)
  )

(defmacro do-submit-job_ (pool cl:&body body)
  `(submit-job_ ,pool
    (do
     ,@body)))

(defmacro do-submit-job-with_ (strategy pool cl:&body body)
  `(submit-job_ ,strategy ,pool
    (do
     ,@body)))
