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
   #:io/conc/group
   #:io/conc/worker-pool
   )
  (:import-from #:io/term
   #:write-line)
  (:import-from #:coalton-library/math/real
   #:round/)
  (:import-from #:coalton-library/experimental/loops
   #:dotimes)
  (:local-nicknames
   (:l #:coalton-library/list)
   (:b #:benchmark-utils)
   (:at #:io/conc/atomic)
   ))

(in-package #:benchmark-schedulers/native)

(cl:declaim (cl:optimize (cl:speed 3) (cl:safety 0)))

;; TODO: Make sure (request-shutdown) works here and stop isn't needed
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
      (stop pool)
      (await pool)
      )))
  )

(coalton-toplevel
  (declare force-ufix (Integer -> UFix))
  (define (force-ufix x)
    (lisp UFix (x)
      x))
  
  (declare benchmark-multi-producer-x-receives (Scheduler :s
                                                => UFix -> UFix -> UFix
                                                -> IO (:s (Optional (IO Unit)))
                                                -> Unit))
  (define (benchmark-multi-producer-x-receives n-tasks n-producers n-workers make-scheduler)
    (let tasks-per-producer = (force-ufix
                               (round/ (as Integer n-tasks) (as Integer n-producers))))
    (when (/= n-tasks (* tasks-per-producer n-producers))
      (error "n-tasks must be a multiple of n-producers"))
    (run-io!
     (do
      (scheduler <- make-scheduler)
      (ready-to-start <- (at:new-at-var False))
      (completed-count <- (at:new-at-var 0))
      ;; Start the worker pool
      (pool <- (new-worker-pool n-workers scheduler))
      ;; Start the producer threads
      (producers <-
        (fork-group (l:repeat n-producers
          (do-fork-thread_
           ;; Wait for the main thread to start the benchmark timer
           (do-loop-while
             ;; This looks backwards, but it's correct as of 12/31/2025.
             ;; See https://github.com/coalton-lang/coalton/issues/1742
             (at:read ready-to-start))
           (do-times-io_ tasks-per-producer
             (do-submit-job_ pool
               (at:modify completed-count 1+)))
           ))))
      ;; Wait for everything to start
      (sleep 15)
      (write-line "DEBUG Starting timer")
      (wrap-io (b:start (b:current-timer)))
      (at:write ready-to-start True)
      (write-line "DEBUG Looping")
      (do-loop-while
        (val <- (at:read completed-count))
        ;; This looks backwards, but it's correct as of 12/31/2025.
        ;; See https://github.com/coalton-lang/coalton/issues/1742
        (write-line (<> "DEBUG Current completed: " (as String val)))
        (pure (>= val n-tasks)))
      (wrap-io
       (b:stop (b:current-timer))
       (b:commit (b:current-timer)))
      (write-line "DEBUG Stopping pool")
      (stop pool)
      (await pool)
      (await producers)
      (pure Unit)
      )))
 )

(in-package #:benchmark-schedulers)

(cl:declaim (cl:optimize (cl:speed 3) (cl:safety 0)))

(defparameter *count* 10)

(coalton:coalton-toplevel
  (coalton:declare *tasks* coalton:UFix)
  (coalton:define *tasks* 12) ;100000)

  (coalton:declare *bounded-capacity* coalton:UFix)
  (coalton:define *bounded-capacity* 128))

;; (define-benchmark single-producer-1-consumer-x-tasks-mchan-scheduler ()
;;   (declare (optimize speed))
;;   (loop :repeat *count*
;;         :do
;;            (coalton:coalton
;;             (benchmark-schedulers/native::benchmark-x-receives
;;              *tasks*
;;              1
;;              io/conc/mchan-scheduler:new-mchan-scheduler)))
;;   (report trivial-benchmark::*current-timer*))

;; (define-benchmark single-producer-2-consumer-x-tasks-mchan-scheduler ()
;;   (declare (optimize speed))
;;   (loop :repeat *count*
;;         :do
;;            (coalton:coalton
;;             (benchmark-schedulers/native::benchmark-x-receives
;;              *tasks*
;;              2
;;              io/conc/mchan-scheduler:new-mchan-scheduler)))
;;   (report trivial-benchmark::*current-timer*))

;; (define-benchmark single-producer-4-consumer-x-tasks-mchan-scheduler ()
;;   (declare (optimize speed))
;;   (loop :repeat *count*
;;         :do
;;            (coalton:coalton
;;             (benchmark-schedulers/native::benchmark-x-receives
;;              *tasks*
;;              4
;;              io/conc/mchan-scheduler:new-mchan-scheduler)))
;;   (report trivial-benchmark::*current-timer*))

;; (define-benchmark 2-producers-2-consumers-x-tasks-mchan-scheduler ()
;;   (declare (optimize speed))
;;   (loop :repeat *count*
;;         :do
;;            (coalton:coalton
;;             (benchmark-schedulers/native::benchmark-multi-producer-x-receives
;;              *tasks*
;;              2
;;              2
;;              io/conc/mchan-scheduler:new-mchan-scheduler)))
;;   (report trivial-benchmark::*current-timer*))

;; (define-benchmark 4-producers-2-consumers-x-tasks-mchan-scheduler ()
;;   (declare (optimize speed))
;;   (loop :repeat *count*
;;         :do
;;            (coalton:coalton
;;             (benchmark-schedulers/native::benchmark-multi-producer-x-receives
;;              *tasks*
;;              4
;;              2
;;              io/conc/mchan-scheduler:new-mchan-scheduler)))
;;   (report trivial-benchmark::*current-timer*))

;; (define-benchmark 2-producers-4-consumers-x-tasks-mchan-scheduler ()
;;   (declare (optimize speed))
;;   (loop :repeat *count*
;;         :do
;;            (coalton:coalton
;;             (benchmark-schedulers/native::benchmark-multi-producer-x-receives
;;              *tasks*
;;              2
;;              4
;;              io/conc/mchan-scheduler:new-mchan-scheduler)))
;;   (report trivial-benchmark::*current-timer*))

;; (define-benchmark 4-producers-4-consumers-x-tasks-mchan-scheduler ()
;;   (declare (optimize speed))
;;   (loop :repeat *count*
;;         :do
;;            (coalton:coalton
;;             (benchmark-schedulers/native::benchmark-multi-producer-x-receives
;;              *tasks*
;;              4
;;              4
;;              io/conc/mchan-scheduler:new-mchan-scheduler)))
;;   (report trivial-benchmark::*current-timer*))

;; (define-benchmark single-producer-1-consumer-x-tasks-ring-buffer-scheduler ()
;;   (declare (optimize speed))
;;   (loop :repeat *count*
;;         :do
;;            (coalton:coalton
;;             (benchmark-schedulers/native::benchmark-x-receives
;;              *tasks*
;;              1
;;              (io/conc/ring-buffer:new-ring-buffer-scheduler *bounded-capacity*))))
;;   (report trivial-benchmark::*current-timer*))

(define-benchmark single-producer-2-consumer-x-tasks-ring-buffer-scheduler ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (coalton:coalton
            (benchmark-schedulers/native::benchmark-x-receives
             *tasks*
             2
             (io/conc/ring-buffer:new-ring-buffer-scheduler *bounded-capacity*))))
  (report trivial-benchmark::*current-timer*))

(define-benchmark single-producer-4-consumer-x-tasks-ring-buffer-scheduler ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (coalton:coalton
            (benchmark-schedulers/native::benchmark-x-receives
             *tasks*
             4
             (io/conc/ring-buffer:new-ring-buffer-scheduler *bounded-capacity*))))
  (report trivial-benchmark::*current-timer*))

;; (define-benchmark 2-producers-2-consumers-x-tasks-ring-buffer-scheduler ()
;;   (declare (optimize speed))
;;   (loop :repeat *count*
;;         :do
;;            (coalton:coalton
;;             (benchmark-schedulers/native::benchmark-multi-producer-x-receives
;;              *tasks*
;;              2
;;              2
;;              (io/conc/ring-buffer:new-ring-buffer-scheduler *bounded-capacity*))))
;;   (report trivial-benchmark::*current-timer*))

;; (define-benchmark 4-producers-2-consumers-x-tasks-ring-buffer-scheduler ()
;;   (declare (optimize speed))
;;   (loop :repeat *count*
;;         :do
;;            (coalton:coalton
;;             (benchmark-schedulers/native::benchmark-multi-producer-x-receives
;;              *tasks*
;;              4
;;              2
;;              (io/conc/ring-buffer:new-ring-buffer-scheduler *bounded-capacity*))))
;;   (report trivial-benchmark::*current-timer*))

;; (define-benchmark 2-producers-4-consumers-x-tasks-ring-buffer-scheduler ()
;;   (declare (optimize speed))
;;   (loop :repeat *count*
;;         :do
;;            (coalton:coalton
;;             (benchmark-schedulers/native::benchmark-multi-producer-x-receives
;;              *tasks*
;;              2
;;              4
;;              (io/conc/ring-buffer:new-ring-buffer-scheduler *bounded-capacity*))))
;;   (report trivial-benchmark::*current-timer*))

;; (define-benchmark 4-producers-4-consumers-x-tasks-ring-buffer-scheduler ()
;;   (declare (optimize speed))
;;   (loop :repeat *count*
;;         :do
;;            (coalton:coalton
;;             (benchmark-schedulers/native::benchmark-multi-producer-x-receives
;;              *tasks*
;;              4
;;              4
;;              (io/conc/ring-buffer:new-ring-buffer-scheduler *bounded-capacity*))))
;;   (report trivial-benchmark::*current-timer*))
