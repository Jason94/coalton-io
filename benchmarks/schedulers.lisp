(in-package #:io/benchmarks)

;;; Scheduler Benchmarks
;;;
;;; These benchmarks test different schedulers against each other in the same benchmarks.

(define-io-benchmark schedulers ()
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/experimental/do-control-loops
   #:io/monad-io
   #:io/simple-io
   #:io/thread
   #:io/conc/scheduler
   #:io/conc/worker-pool
   )
  (:import-from #:coalton-library/experimental/loops
   #:dotimes)
  (:local-nicknames
   (:b #:benchmark-utils)
   (:at #:io/conc/atomic)
   ))

(in-package #:benchmark-schedulers/native)

(cl:declaim (cl:optimize (cl:speed 3) (cl:safety 0)))

(coalton-toplevel

  ;; Benchmark receiving N-TASKS tasks of very minimal work (one atomic increment).
  ;; The main thread creates a single atomic integer to count completed tasks.
  ;; The main thread starts a worker pool with N-THREADs, and sleeps for 15ms to wait
  ;; for the threads to fork.
  ;; Then the main thread starts the benchmark timer.
  ;; The main thread pushes out N-TASKS tasks into the scheduler as fast as it can.
  ;; When it is finished pushing tasks, it spins until the completed task count reaches X.
  ;; When the task count hits N-TASKS, the main thread immediately stops the benchmark timer.
  ;; Then it requests a shutdown and joins the pool.
  (declare benchmark-x-receives (Scheduler :s
                                 => UFix -> UFix -> IO (:s (Optional (IO Unit))) -> Unit))
  (define (benchmark-x-receives n-tasks n-threads make-scheduler)
    (run-io!
     (do
      (scheduler <- make-scheduler)
      (completed-count <- (at:new-at-var 0))
      (pool <- (new-worker-pool n-threads scheduler))
      (sleep 15)
      (wrap-io (b:start (b:current-timer)))
      (do-times-io_ n-tasks
        (do-submit-job_ pool
          (at:modify completed-count 1+)))
      (do-loop-while
        (val <- (at:read completed-count))
        ;; This looks backwards, but it's correct as of 12/31/2025.
        ;; See https://github.com/coalton-lang/coalton/issues/1742
        (pure (>= val n-tasks)))
      (wrap-io
       (b:stop (b:current-timer))
       (b:commit (b:current-timer)))
      (request-shutdown pool)
      (await pool)
      )))
 )

(in-package #:benchmark-schedulers)

(cl:declaim (cl:optimize (cl:speed 3) (cl:safety 0)))

(defparameter *count* 10)

(coalton:coalton-toplevel
  (coalton:declare *tasks* coalton:UFix)
  (coalton:define *tasks* 100000))

(define-benchmark single-producer-1-consumer-x-tasks-mchan-scheduler ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (coalton:coalton
            (benchmark-schedulers/native::benchmark-x-receives
             *tasks*
             1
             io/conc/mchan-scheduler:new-mchan-scheduler)))
  (report trivial-benchmark::*current-timer*))

(define-benchmark single-producer-2-consumer-x-tasks-mchan-scheduler ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (coalton:coalton
            (benchmark-schedulers/native::benchmark-x-receives
             *tasks*
             2
             io/conc/mchan-scheduler:new-mchan-scheduler)))
  (report trivial-benchmark::*current-timer*))

(define-benchmark single-producer-4-consumer-x-tasks-mchan-scheduler ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (coalton:coalton
            (benchmark-schedulers/native::benchmark-x-receives
             *tasks*
             4
             io/conc/mchan-scheduler:new-mchan-scheduler)))
  (report trivial-benchmark::*current-timer*))
