(in-package #:io/benchmarks)

(define-io-benchmark-package queues
  ((:local-nicknames
    (:c #:coalton)))
  (:use
   #:coalton
   #:coalton-prelude
   #:io/monad-io
   #:io/simple-io
   #:io/simple-io/loops
   #:io/thread
   #:io/conc/group
   #:io/conc/mvar
   #:io/conc/queues)
  (:local-nicknames
   (:b #:benchmark-utils)
   (:tm #:io/term)
   ))

(in-package #:benchmark-queues/native)

(cl:declaim (cl:optimize (cl:speed 3) (cl:safety 0)))

(coalton-toplevel

  (declare benchmark-enqueue-x-threads (Queue :q => UFix * UFix * IO (:q Boolean) -> Unit))
  (define (benchmark-enqueue-x-threads n-tasks n-threads make-queue)
    (run-io!
     (do
      ;; Setup benchmark
      (queue <- make-queue)
      (let tasks-per-thread = (coalton/math:div n-tasks n-threads))
      (start-gate <- new-empty-mvar)
      (threads <-
        (do-fork-n-threads (_ n-threads)
          (read-mvar start-gate)
          (do-repeat-io tasks-per-thread
            (enqueue True queue))))
      ;; Run the benchmark
      (sleep 2)
      (wrap-io (b:start (b:current-timer)))
      (put-mvar start-gate Unit)
      (await threads)
      ;; Cleanup
      (wrap-io
       (b:stop (b:current-timer))
       (b:commit (b:current-timer))
       Unit))))
  )

(in-package #:benchmark-queues)

(cl:declaim (cl:optimize (cl:speed 3) (cl:safety 0)))

(defparameter *count* 60)

(c:coalton-toplevel
  (c:define *tasks* (c:the c:UFix 24000)))

(define-benchmark bounded-enqueue-x-tasks-1-thread ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-queues/native::benchmark-enqueue-x-threads
             *tasks*
             1
             (io/conc/queues/bounded-mpmc:new-bounded-mpmc-queue *tasks*)))))

(define-benchmark bounded-enqueue-x-tasks-2-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-queues/native::benchmark-enqueue-x-threads
             *tasks*
             2
             (io/conc/queues/bounded-mpmc:new-bounded-mpmc-queue *tasks*)))))

(define-benchmark bounded-enqueue-x-tasks-4-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-queues/native::benchmark-enqueue-x-threads
             *tasks*
             4
             (io/conc/queues/bounded-mpmc:new-bounded-mpmc-queue *tasks*)))))

(define-benchmark bounded-enqueue-x-tasks-6-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-queues/native::benchmark-enqueue-x-threads
             *tasks*
             6
             (io/conc/queues/bounded-mpmc:new-bounded-mpmc-queue *tasks*)))))

(define-benchmark unbounded-enqueue-x-tasks-1-thread ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-queues/native::benchmark-enqueue-x-threads
             *tasks*
             1
             io/conc/queues/unbounded-mpmc:new-unbounded-mpmc-queue))))

(define-benchmark unbounded-enqueue-x-tasks-2-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-queues/native::benchmark-enqueue-x-threads
             *tasks*
             2
             io/conc/queues/unbounded-mpmc:new-unbounded-mpmc-queue))))

(define-benchmark unbounded-enqueue-x-tasks-4-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-queues/native::benchmark-enqueue-x-threads
             *tasks*
             4
             io/conc/queues/unbounded-mpmc:new-unbounded-mpmc-queue))))

(define-benchmark unbounded-enqueue-x-tasks-6-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-queues/native::benchmark-enqueue-x-threads
             *tasks*
             6
             io/conc/queues/unbounded-mpmc:new-unbounded-mpmc-queue))))
