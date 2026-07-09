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

  (declare benchmark-dequeue-x-threads (Queue :q => UFix * UFix * IO (:q Boolean) -> Unit))
  (define (benchmark-dequeue-x-threads n-tasks n-threads make-queue)
    (run-io!
     (do
      ;; Setup benchmark
      (queue <- make-queue)
      (do-repeat-io n-tasks
        (enqueue True queue))
      (let tasks-per-thread = (coalton/math:div n-tasks n-threads))
      (start-gate <- new-empty-mvar)
      (threads <-
        (do-fork-n-threads (_ n-threads)
          (read-mvar start-gate)
          (do-repeat-io tasks-per-thread
            (dequeue queue))))
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

  (declare benchmark-enqueue-dequeue-x-threads (Queue :q => UFix * UFix * UFix * IO (:q Boolean) -> Unit))
  (define (benchmark-enqueue-dequeue-x-threads n-tasks n-enqueue-threads n-dequeue-threads make-queue)
    (run-io!
     (do
      ;; Setup benchmark
      (queue <- make-queue)
      (let tasks-per-enqueue-thread = (coalton/math:div n-tasks n-enqueue-threads))
      (let tasks-per-dequeue-thread = (coalton/math:div n-tasks n-dequeue-threads))
      (start-gate <- new-empty-mvar)
      (enqueuers <-
        (do-fork-n-threads (_ n-enqueue-threads)
          (read-mvar start-gate)
          (do-repeat-io tasks-per-enqueue-thread
            (enqueue True queue))))
      (dequeuers <-
        (do-fork-n-threads (_ n-dequeue-threads)
          (read-mvar start-gate)
          (do-repeat-io tasks-per-dequeue-thread
            (dequeue queue))))
      ;; Run the benchmark
      (sleep 2)
      (wrap-io (b:start (b:current-timer)))
      (put-mvar start-gate Unit)
      (await enqueuers)
      (await dequeuers)
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

(define-benchmark bounded-dequeue-x-tasks-1-thread ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-queues/native::benchmark-dequeue-x-threads
             *tasks*
             1
             (io/conc/queues/bounded-mpmc:new-bounded-mpmc-queue *tasks*)))))

(define-benchmark bounded-dequeue-x-tasks-2-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-queues/native::benchmark-dequeue-x-threads
             *tasks*
             2
             (io/conc/queues/bounded-mpmc:new-bounded-mpmc-queue *tasks*)))))

(define-benchmark bounded-dequeue-x-tasks-4-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-queues/native::benchmark-dequeue-x-threads
             *tasks*
             4
             (io/conc/queues/bounded-mpmc:new-bounded-mpmc-queue *tasks*)))))

(define-benchmark bounded-dequeue-x-tasks-6-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-queues/native::benchmark-dequeue-x-threads
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

(define-benchmark unbounded-dequeue-x-tasks-1-thread ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-queues/native::benchmark-dequeue-x-threads
             *tasks*
             1
             io/conc/queues/unbounded-mpmc:new-unbounded-mpmc-queue))))

(define-benchmark unbounded-dequeue-x-tasks-2-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-queues/native::benchmark-dequeue-x-threads
             *tasks*
             2
             io/conc/queues/unbounded-mpmc:new-unbounded-mpmc-queue))))

(define-benchmark unbounded-dequeue-x-tasks-4-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-queues/native::benchmark-dequeue-x-threads
             *tasks*
             4
             io/conc/queues/unbounded-mpmc:new-unbounded-mpmc-queue))))

(define-benchmark unbounded-dequeue-x-tasks-6-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-queues/native::benchmark-dequeue-x-threads
             *tasks*
             6
             io/conc/queues/unbounded-mpmc:new-unbounded-mpmc-queue))))

(define-benchmark bounded-enqueue-dequeue-capacity-enqueuers-x-tasks-1-enqueue-thread-1-dequeue-thread ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-queues/native::benchmark-enqueue-dequeue-x-threads
             *tasks*
             1
             1
             (io/conc/queues/bounded-mpmc:new-bounded-mpmc-queue 1)))))

(define-benchmark bounded-enqueue-dequeue-capacity-enqueuers-x-tasks-1-enqueue-thread-3-dequeue-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-queues/native::benchmark-enqueue-dequeue-x-threads
             *tasks*
             1
             3
             (io/conc/queues/bounded-mpmc:new-bounded-mpmc-queue 1)))))

(define-benchmark bounded-enqueue-dequeue-capacity-enqueuers-x-tasks-1-enqueue-thread-6-dequeue-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-queues/native::benchmark-enqueue-dequeue-x-threads
             *tasks*
             1
             6
             (io/conc/queues/bounded-mpmc:new-bounded-mpmc-queue 1)))))

(define-benchmark bounded-enqueue-dequeue-capacity-enqueuers-x-tasks-3-enqueue-threads-1-dequeue-thread ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-queues/native::benchmark-enqueue-dequeue-x-threads
             *tasks*
             3
             1
             (io/conc/queues/bounded-mpmc:new-bounded-mpmc-queue 3)))))

(define-benchmark bounded-enqueue-dequeue-capacity-enqueuers-x-tasks-3-enqueue-threads-3-dequeue-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-queues/native::benchmark-enqueue-dequeue-x-threads
             *tasks*
             3
             3
             (io/conc/queues/bounded-mpmc:new-bounded-mpmc-queue 3)))))

(define-benchmark bounded-enqueue-dequeue-capacity-enqueuers-x-tasks-3-enqueue-threads-6-dequeue-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-queues/native::benchmark-enqueue-dequeue-x-threads
             *tasks*
             3
             6
             (io/conc/queues/bounded-mpmc:new-bounded-mpmc-queue 3)))))

(define-benchmark bounded-enqueue-dequeue-capacity-enqueuers-x-tasks-6-enqueue-threads-1-dequeue-thread ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-queues/native::benchmark-enqueue-dequeue-x-threads
             *tasks*
             6
             1
             (io/conc/queues/bounded-mpmc:new-bounded-mpmc-queue 6)))))

(define-benchmark bounded-enqueue-dequeue-capacity-enqueuers-x-tasks-6-enqueue-threads-3-dequeue-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-queues/native::benchmark-enqueue-dequeue-x-threads
             *tasks*
             6
             3
             (io/conc/queues/bounded-mpmc:new-bounded-mpmc-queue 6)))))

(define-benchmark bounded-enqueue-dequeue-capacity-enqueuers-x-tasks-6-enqueue-threads-6-dequeue-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-queues/native::benchmark-enqueue-dequeue-x-threads
             *tasks*
             6
             6
             (io/conc/queues/bounded-mpmc:new-bounded-mpmc-queue 6)))))

(define-benchmark bounded-enqueue-dequeue-capacity-tasks-x-tasks-1-enqueue-thread-1-dequeue-thread ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-queues/native::benchmark-enqueue-dequeue-x-threads
             *tasks*
             1
             1
             (io/conc/queues/bounded-mpmc:new-bounded-mpmc-queue *tasks*)))))

(define-benchmark bounded-enqueue-dequeue-capacity-tasks-x-tasks-1-enqueue-thread-3-dequeue-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-queues/native::benchmark-enqueue-dequeue-x-threads
             *tasks*
             1
             3
             (io/conc/queues/bounded-mpmc:new-bounded-mpmc-queue *tasks*)))))

(define-benchmark bounded-enqueue-dequeue-capacity-tasks-x-tasks-1-enqueue-thread-6-dequeue-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-queues/native::benchmark-enqueue-dequeue-x-threads
             *tasks*
             1
             6
             (io/conc/queues/bounded-mpmc:new-bounded-mpmc-queue *tasks*)))))

(define-benchmark bounded-enqueue-dequeue-capacity-tasks-x-tasks-3-enqueue-threads-1-dequeue-thread ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-queues/native::benchmark-enqueue-dequeue-x-threads
             *tasks*
             3
             1
             (io/conc/queues/bounded-mpmc:new-bounded-mpmc-queue *tasks*)))))

(define-benchmark bounded-enqueue-dequeue-capacity-tasks-x-tasks-3-enqueue-threads-3-dequeue-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-queues/native::benchmark-enqueue-dequeue-x-threads
             *tasks*
             3
             3
             (io/conc/queues/bounded-mpmc:new-bounded-mpmc-queue *tasks*)))))

(define-benchmark bounded-enqueue-dequeue-capacity-tasks-x-tasks-3-enqueue-threads-6-dequeue-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-queues/native::benchmark-enqueue-dequeue-x-threads
             *tasks*
             3
             6
             (io/conc/queues/bounded-mpmc:new-bounded-mpmc-queue *tasks*)))))

(define-benchmark bounded-enqueue-dequeue-capacity-tasks-x-tasks-6-enqueue-threads-1-dequeue-thread ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-queues/native::benchmark-enqueue-dequeue-x-threads
             *tasks*
             6
             1
             (io/conc/queues/bounded-mpmc:new-bounded-mpmc-queue *tasks*)))))

(define-benchmark bounded-enqueue-dequeue-capacity-tasks-x-tasks-6-enqueue-threads-3-dequeue-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-queues/native::benchmark-enqueue-dequeue-x-threads
             *tasks*
             6
             3
             (io/conc/queues/bounded-mpmc:new-bounded-mpmc-queue *tasks*)))))

(define-benchmark bounded-enqueue-dequeue-capacity-tasks-x-tasks-6-enqueue-threads-6-dequeue-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-queues/native::benchmark-enqueue-dequeue-x-threads
             *tasks*
             6
             6
             (io/conc/queues/bounded-mpmc:new-bounded-mpmc-queue *tasks*)))))

(define-benchmark unbounded-enqueue-dequeue-x-tasks-1-enqueue-thread-1-dequeue-thread ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-queues/native::benchmark-enqueue-dequeue-x-threads
             *tasks*
             1
             1
             io/conc/queues/unbounded-mpmc:new-unbounded-mpmc-queue))))

(define-benchmark unbounded-enqueue-dequeue-x-tasks-1-enqueue-thread-3-dequeue-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-queues/native::benchmark-enqueue-dequeue-x-threads
             *tasks*
             1
             3
             io/conc/queues/unbounded-mpmc:new-unbounded-mpmc-queue))))

(define-benchmark unbounded-enqueue-dequeue-x-tasks-1-enqueue-thread-6-dequeue-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-queues/native::benchmark-enqueue-dequeue-x-threads
             *tasks*
             1
             6
             io/conc/queues/unbounded-mpmc:new-unbounded-mpmc-queue))))

(define-benchmark unbounded-enqueue-dequeue-x-tasks-3-enqueue-threads-1-dequeue-thread ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-queues/native::benchmark-enqueue-dequeue-x-threads
             *tasks*
             3
             1
             io/conc/queues/unbounded-mpmc:new-unbounded-mpmc-queue))))

(define-benchmark unbounded-enqueue-dequeue-x-tasks-3-enqueue-threads-3-dequeue-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-queues/native::benchmark-enqueue-dequeue-x-threads
             *tasks*
             3
             3
             io/conc/queues/unbounded-mpmc:new-unbounded-mpmc-queue))))

(define-benchmark unbounded-enqueue-dequeue-x-tasks-3-enqueue-threads-6-dequeue-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-queues/native::benchmark-enqueue-dequeue-x-threads
             *tasks*
             3
             6
             io/conc/queues/unbounded-mpmc:new-unbounded-mpmc-queue))))

(define-benchmark unbounded-enqueue-dequeue-x-tasks-6-enqueue-threads-1-dequeue-thread ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-queues/native::benchmark-enqueue-dequeue-x-threads
             *tasks*
             6
             1
             io/conc/queues/unbounded-mpmc:new-unbounded-mpmc-queue))))

(define-benchmark unbounded-enqueue-dequeue-x-tasks-6-enqueue-threads-3-dequeue-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-queues/native::benchmark-enqueue-dequeue-x-threads
             *tasks*
             6
             3
             io/conc/queues/unbounded-mpmc:new-unbounded-mpmc-queue))))

(define-benchmark unbounded-enqueue-dequeue-x-tasks-6-enqueue-threads-6-dequeue-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-queues/native::benchmark-enqueue-dequeue-x-threads
             *tasks*
             6
             6
             io/conc/queues/unbounded-mpmc:new-unbounded-mpmc-queue))))
