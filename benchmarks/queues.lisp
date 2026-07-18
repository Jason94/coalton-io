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
   (:c #:coalton/cell)
   (:v #:coalton/vector)
   (:l #:coalton/list)
   (:tm #:io/term)
   ))

(in-package #:benchmark-queues/native)

(cl:declaim (cl:optimize (cl:speed 3) (cl:safety 0)))

(coalton-toplevel

  (define *disable-masking* True)

  (declare benchmark-vector-cache (c:Cell (Optional (Vector Boolean))))
  (define benchmark-vector-cache (c:new None))

  (declare benchmark-vector-single-thread (UFix -> Void))
  (define (benchmark-vector-single-thread n-tasks)
    "This benchmark writes to a pre-allocated vector `n-tasks` times as a control measurement."
    (run-io!
     (do
      ;; Setup benchmark
      (buffer <- (wrap-io (v:with-capacity n-tasks)))
      ;; Run the benchmark
      (wrap-io (b:start (b:current-timer)))
      (do-repeat-io n-tasks
      (wrap-io (v:push! True buffer)))
      ;; Cleanup
      (wrap-io
       (b:stop (b:current-timer))
       (b:commit (b:current-timer))
       ;; Write out the buffer to prevent SBCL from optimizing it away
       (c:write! benchmark-vector-cache (Some buffer))
       Unit))
     :disable-masking *disable-masking*)
    (values))

  (declare benchmark-list-cache (c:Cell (Optional (List Boolean))))
  (define benchmark-list-cache (c:new None))

  (declare benchmark-list-single-thread (UFix -> Void))
  (define (benchmark-list-single-thread n-tasks)
    "This benchmark builds and reverses a list with `n-tasks` items as a control measurement."
    (run-io!
     (do
      ;; Setup benchmark
      (list <- (wrap-io (c:new Nil)))
      ;; Run the benchmark
      (wrap-io
       (b:start (b:current-timer)))
      (do-repeat-io n-tasks
        (wrap-io (c:push! list True)))
      ;; Cleanup
      (wrap-io
       (b:stop (b:current-timer))
       (b:commit (b:current-timer))
       ;; Write out the buffer to prevent SBCL from optimizing it away
       (c:write! benchmark-list-cache (Some (l:reverse (c:read list))))
       Unit))
     :disable-masking *disable-masking*)
    (values))

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
       Unit))
     :disable-masking *disable-masking*))

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
       Unit))
     :disable-masking *disable-masking*))

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
       Unit))
     :disable-masking *disable-masking*))

  (declare benchmark-enqueue-try-dequeue-x-threads (Queue :q => UFix * UFix * UFix * IO (:q Boolean) -> Unit))
  (define (benchmark-enqueue-try-dequeue-x-threads n-tasks n-enqueue-threads n-dequeue-threads make-queue)
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
            (do-while-io
              (result <- (try-dequeue queue))
              (pure (none? result))))))
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
       Unit))
     :disable-masking *disable-masking*))

  (declare benchmark-enqueue-try-dequeue-pairs-x-threads (Queue :q => UFix * UFix * IO (:q Boolean) -> Unit))
  (define (benchmark-enqueue-try-dequeue-pairs-x-threads n-tasks n-threads make-queue)
    (run-io!
     (do
      ;; Setup benchmark
      (queue <- make-queue)
      (let tasks-per-thread = (coalton/math:div n-tasks n-threads))
      (start-gate <- new-empty-mvar)
      (threads <-
        (do-fork-n-threads (_ n-threads)
          (let loop-until-dequeue =
            (do-while-io
              (map none? (try-dequeue queue))))
          (read-mvar start-gate)
          (do-repeat-io tasks-per-thread
            (enqueue True queue)
            loop-until-dequeue)))
      ;; Run the benchmark
      (sleep 2)
      (wrap-io (b:start (b:current-timer)))
      (put-mvar start-gate Unit)
      (await threads)
      ;; Cleanup
      (wrap-io
       (b:stop (b:current-timer))
       (b:commit (b:current-timer))
       Unit))
     :disable-masking *disable-masking*))
  )

(in-package #:benchmark-queues)

(cl:declaim (cl:optimize (cl:speed 3) (cl:safety 0)))

(defparameter *count* 75)

(c:coalton-toplevel
  (c:define *tasks* (c:the c:UFix 48000)))

(define-benchmark control-vector-single-thread ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-queues/native::benchmark-vector-single-thread
             *tasks*))))

(define-benchmark control-list-single-thread ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-queues/native::benchmark-list-single-thread
             *tasks*))))

;; (define-benchmark bounded-enqueue-x-tasks-1-thread ()
;;   (declare (optimize speed))
;;   (loop :repeat *count*
;;         :do
;;            (c:coalton
;;             (benchmark-queues/native::benchmark-enqueue-x-threads
;;              *tasks*
;;              1
;;              (io/conc/queues/bounded-mpmc:new-bounded-mpmc-queue *tasks*)))))

;; (define-benchmark bounded-enqueue-x-tasks-2-threads ()
;;   (declare (optimize speed))
;;   (loop :repeat *count*
;;         :do
;;            (c:coalton
;;             (benchmark-queues/native::benchmark-enqueue-x-threads
;;              *tasks*
;;              2
;;              (io/conc/queues/bounded-mpmc:new-bounded-mpmc-queue *tasks*)))))

;; (define-benchmark bounded-enqueue-x-tasks-4-threads ()
;;   (declare (optimize speed))
;;   (loop :repeat *count*
;;         :do
;;            (c:coalton
;;             (benchmark-queues/native::benchmark-enqueue-x-threads
;;              *tasks*
;;              4
;;              (io/conc/queues/bounded-mpmc:new-bounded-mpmc-queue *tasks*)))))

;; (define-benchmark bounded-enqueue-x-tasks-6-threads ()
;;   (declare (optimize speed))
;;   (loop :repeat *count*
;;         :do
;;            (c:coalton
;;             (benchmark-queues/native::benchmark-enqueue-x-threads
;;              *tasks*
;;              6
;;              (io/conc/queues/bounded-mpmc:new-bounded-mpmc-queue *tasks*)))))

;; (define-benchmark bounded-dequeue-x-tasks-1-thread ()
;;   (declare (optimize speed))
;;   (loop :repeat *count*
;;         :do
;;            (c:coalton
;;             (benchmark-queues/native::benchmark-dequeue-x-threads
;;              *tasks*
;;              1
;;              (io/conc/queues/bounded-mpmc:new-bounded-mpmc-queue *tasks*)))))

;; (define-benchmark bounded-dequeue-x-tasks-2-threads ()
;;   (declare (optimize speed))
;;   (loop :repeat *count*
;;         :do
;;            (c:coalton
;;             (benchmark-queues/native::benchmark-dequeue-x-threads
;;              *tasks*
;;              2
;;              (io/conc/queues/bounded-mpmc:new-bounded-mpmc-queue *tasks*)))))

;; (define-benchmark bounded-dequeue-x-tasks-4-threads ()
;;   (declare (optimize speed))
;;   (loop :repeat *count*
;;         :do
;;            (c:coalton
;;             (benchmark-queues/native::benchmark-dequeue-x-threads
;;              *tasks*
;;              4
;;              (io/conc/queues/bounded-mpmc:new-bounded-mpmc-queue *tasks*)))))

;; (define-benchmark bounded-dequeue-x-tasks-6-threads ()
;;   (declare (optimize speed))
;;   (loop :repeat *count*
;;         :do
;;            (c:coalton
;;             (benchmark-queues/native::benchmark-dequeue-x-threads
;;              *tasks*
;;              6
;;              (io/conc/queues/bounded-mpmc:new-bounded-mpmc-queue *tasks*)))))

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

;; (define-benchmark bounded-enqueue-dequeue-capacity-enqueuers-x-tasks-1-enqueue-thread-1-dequeue-thread ()
;;   (declare (optimize speed))
;;   (loop :repeat *count*
;;         :do
;;            (c:coalton
;;             (benchmark-queues/native::benchmark-enqueue-dequeue-x-threads
;;              *tasks*
;;              1
;;              1
;;              (io/conc/queues/bounded-mpmc:new-bounded-mpmc-queue 1)))))

;; (define-benchmark bounded-enqueue-dequeue-capacity-enqueuers-x-tasks-1-enqueue-thread-3-dequeue-threads ()
;;   (declare (optimize speed))
;;   (loop :repeat *count*
;;         :do
;;            (c:coalton
;;             (benchmark-queues/native::benchmark-enqueue-dequeue-x-threads
;;              *tasks*
;;              1
;;              3
;;              (io/conc/queues/bounded-mpmc:new-bounded-mpmc-queue 1)))))

;; (define-benchmark bounded-enqueue-dequeue-capacity-enqueuers-x-tasks-1-enqueue-thread-6-dequeue-threads ()
;;   (declare (optimize speed))
;;   (loop :repeat *count*
;;         :do
;;            (c:coalton
;;             (benchmark-queues/native::benchmark-enqueue-dequeue-x-threads
;;              *tasks*
;;              1
;;              6
;;              (io/conc/queues/bounded-mpmc:new-bounded-mpmc-queue 1)))))

;; (define-benchmark bounded-enqueue-dequeue-capacity-enqueuers-x-tasks-3-enqueue-threads-1-dequeue-thread ()
;;   (declare (optimize speed))
;;   (loop :repeat *count*
;;         :do
;;            (c:coalton
;;             (benchmark-queues/native::benchmark-enqueue-dequeue-x-threads
;;              *tasks*
;;              3
;;              1
;;              (io/conc/queues/bounded-mpmc:new-bounded-mpmc-queue 3)))))

;; (define-benchmark bounded-enqueue-dequeue-capacity-enqueuers-x-tasks-3-enqueue-threads-3-dequeue-threads ()
;;   (declare (optimize speed))
;;   (loop :repeat *count*
;;         :do
;;            (c:coalton
;;             (benchmark-queues/native::benchmark-enqueue-dequeue-x-threads
;;              *tasks*
;;              3
;;              3
;;              (io/conc/queues/bounded-mpmc:new-bounded-mpmc-queue 3)))))

;; (define-benchmark bounded-enqueue-dequeue-capacity-enqueuers-x-tasks-3-enqueue-threads-6-dequeue-threads ()
;;   (declare (optimize speed))
;;   (loop :repeat *count*
;;         :do
;;            (c:coalton
;;             (benchmark-queues/native::benchmark-enqueue-dequeue-x-threads
;;              *tasks*
;;              3
;;              6
;;              (io/conc/queues/bounded-mpmc:new-bounded-mpmc-queue 3)))))

;; (define-benchmark bounded-enqueue-dequeue-capacity-enqueuers-x-tasks-6-enqueue-threads-1-dequeue-thread ()
;;   (declare (optimize speed))
;;   (loop :repeat *count*
;;         :do
;;            (c:coalton
;;             (benchmark-queues/native::benchmark-enqueue-dequeue-x-threads
;;              *tasks*
;;              6
;;              1
;;              (io/conc/queues/bounded-mpmc:new-bounded-mpmc-queue 6)))))

;; (define-benchmark bounded-enqueue-dequeue-capacity-enqueuers-x-tasks-6-enqueue-threads-3-dequeue-threads ()
;;   (declare (optimize speed))
;;   (loop :repeat *count*
;;         :do
;;            (c:coalton
;;             (benchmark-queues/native::benchmark-enqueue-dequeue-x-threads
;;              *tasks*
;;              6
;;              3
;;              (io/conc/queues/bounded-mpmc:new-bounded-mpmc-queue 6)))))

;; (define-benchmark bounded-enqueue-dequeue-capacity-enqueuers-x-tasks-6-enqueue-threads-6-dequeue-threads ()
;;   (declare (optimize speed))
;;   (loop :repeat *count*
;;         :do
;;            (c:coalton
;;             (benchmark-queues/native::benchmark-enqueue-dequeue-x-threads
;;              *tasks*
;;              6
;;              6
;;              (io/conc/queues/bounded-mpmc:new-bounded-mpmc-queue 6)))))

;; (define-benchmark bounded-enqueue-dequeue-capacity-tasks-x-tasks-1-enqueue-thread-1-dequeue-thread ()
;;   (declare (optimize speed))
;;   (loop :repeat *count*
;;         :do
;;            (c:coalton
;;             (benchmark-queues/native::benchmark-enqueue-dequeue-x-threads
;;              *tasks*
;;              1
;;              1
;;              (io/conc/queues/bounded-mpmc:new-bounded-mpmc-queue *tasks*)))))

;; (define-benchmark bounded-enqueue-dequeue-capacity-tasks-x-tasks-1-enqueue-thread-3-dequeue-threads ()
;;   (declare (optimize speed))
;;   (loop :repeat *count*
;;         :do
;;            (c:coalton
;;             (benchmark-queues/native::benchmark-enqueue-dequeue-x-threads
;;              *tasks*
;;              1
;;              3
;;              (io/conc/queues/bounded-mpmc:new-bounded-mpmc-queue *tasks*)))))

;; (define-benchmark bounded-enqueue-dequeue-capacity-tasks-x-tasks-1-enqueue-thread-6-dequeue-threads ()
;;   (declare (optimize speed))
;;   (loop :repeat *count*
;;         :do
;;            (c:coalton
;;             (benchmark-queues/native::benchmark-enqueue-dequeue-x-threads
;;              *tasks*
;;              1
;;              6
;;              (io/conc/queues/bounded-mpmc:new-bounded-mpmc-queue *tasks*)))))

;; (define-benchmark bounded-enqueue-dequeue-capacity-tasks-x-tasks-3-enqueue-threads-1-dequeue-thread ()
;;   (declare (optimize speed))
;;   (loop :repeat *count*
;;         :do
;;            (c:coalton
;;             (benchmark-queues/native::benchmark-enqueue-dequeue-x-threads
;;              *tasks*
;;              3
;;              1
;;              (io/conc/queues/bounded-mpmc:new-bounded-mpmc-queue *tasks*)))))

;; (define-benchmark bounded-enqueue-dequeue-capacity-tasks-x-tasks-3-enqueue-threads-3-dequeue-threads ()
;;   (declare (optimize speed))
;;   (loop :repeat *count*
;;         :do
;;            (c:coalton
;;             (benchmark-queues/native::benchmark-enqueue-dequeue-x-threads
;;              *tasks*
;;              3
;;              3
;;              (io/conc/queues/bounded-mpmc:new-bounded-mpmc-queue *tasks*)))))

;; (define-benchmark bounded-enqueue-dequeue-capacity-tasks-x-tasks-3-enqueue-threads-6-dequeue-threads ()
;;   (declare (optimize speed))
;;   (loop :repeat *count*
;;         :do
;;            (c:coalton
;;             (benchmark-queues/native::benchmark-enqueue-dequeue-x-threads
;;              *tasks*
;;              3
;;              6
;;              (io/conc/queues/bounded-mpmc:new-bounded-mpmc-queue *tasks*)))))

;; (define-benchmark bounded-enqueue-dequeue-capacity-tasks-x-tasks-6-enqueue-threads-1-dequeue-thread ()
;;   (declare (optimize speed))
;;   (loop :repeat *count*
;;         :do
;;            (c:coalton
;;             (benchmark-queues/native::benchmark-enqueue-dequeue-x-threads
;;              *tasks*
;;              6
;;              1
;;              (io/conc/queues/bounded-mpmc:new-bounded-mpmc-queue *tasks*)))))

;; (define-benchmark bounded-enqueue-dequeue-capacity-tasks-x-tasks-6-enqueue-threads-3-dequeue-threads ()
;;   (declare (optimize speed))
;;   (loop :repeat *count*
;;         :do
;;            (c:coalton
;;             (benchmark-queues/native::benchmark-enqueue-dequeue-x-threads
;;              *tasks*
;;              6
;;              3
;;              (io/conc/queues/bounded-mpmc:new-bounded-mpmc-queue *tasks*)))))

;; (define-benchmark bounded-enqueue-dequeue-capacity-tasks-x-tasks-6-enqueue-threads-6-dequeue-threads ()
;;   (declare (optimize speed))
;;   (loop :repeat *count*
;;         :do
;;            (c:coalton
;;             (benchmark-queues/native::benchmark-enqueue-dequeue-x-threads
;;              *tasks*
;;              6
;;              6
;;              (io/conc/queues/bounded-mpmc:new-bounded-mpmc-queue *tasks*)))))

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

;; (define-benchmark bounded-enqueue-try-dequeue-x-tasks-1-enqueue-thread-1-dequeue-thread ()
;;   (declare (optimize speed))
;;   (loop :repeat *count*
;;         :do
;;            (c:coalton
;;             (benchmark-queues/native::benchmark-enqueue-try-dequeue-x-threads
;;              *tasks*
;;              1
;;              1
;;              (io/conc/queues/bounded-mpmc:new-bounded-mpmc-queue *tasks*)))))

;; (define-benchmark bounded-enqueue-try-dequeue-x-tasks-1-enqueue-thread-3-dequeue-threads ()
;;   (declare (optimize speed))
;;   (loop :repeat *count*
;;         :do
;;            (c:coalton
;;             (benchmark-queues/native::benchmark-enqueue-try-dequeue-x-threads
;;              *tasks*
;;              1
;;              3
;;              (io/conc/queues/bounded-mpmc:new-bounded-mpmc-queue *tasks*)))))

;; (define-benchmark bounded-enqueue-try-dequeue-x-tasks-1-enqueue-thread-6-dequeue-threads ()
;;   (declare (optimize speed))
;;   (loop :repeat *count*
;;         :do
;;            (c:coalton
;;             (benchmark-queues/native::benchmark-enqueue-try-dequeue-x-threads
;;              *tasks*
;;              1
;;              6
;;              (io/conc/queues/bounded-mpmc:new-bounded-mpmc-queue *tasks*)))))

;; (define-benchmark bounded-enqueue-try-dequeue-x-tasks-3-enqueue-threads-1-dequeue-thread ()
;;   (declare (optimize speed))
;;   (loop :repeat *count*
;;         :do
;;            (c:coalton
;;             (benchmark-queues/native::benchmark-enqueue-try-dequeue-x-threads
;;              *tasks*
;;              3
;;              1
;;              (io/conc/queues/bounded-mpmc:new-bounded-mpmc-queue *tasks*)))))

;; (define-benchmark bounded-enqueue-try-dequeue-x-tasks-3-enqueue-threads-3-dequeue-threads ()
;;   (declare (optimize speed))
;;   (loop :repeat *count*
;;         :do
;;            (c:coalton
;;             (benchmark-queues/native::benchmark-enqueue-try-dequeue-x-threads
;;              *tasks*
;;              3
;;              3
;;              (io/conc/queues/bounded-mpmc:new-bounded-mpmc-queue *tasks*)))))

;; (define-benchmark bounded-enqueue-try-dequeue-x-tasks-3-enqueue-threads-6-dequeue-threads ()
;;   (declare (optimize speed))
;;   (loop :repeat *count*
;;         :do
;;            (c:coalton
;;             (benchmark-queues/native::benchmark-enqueue-try-dequeue-x-threads
;;              *tasks*
;;              3
;;              6
;;              (io/conc/queues/bounded-mpmc:new-bounded-mpmc-queue *tasks*)))))

;; (define-benchmark bounded-enqueue-try-dequeue-x-tasks-6-enqueue-threads-1-dequeue-thread ()
;;   (declare (optimize speed))
;;   (loop :repeat *count*
;;         :do
;;            (c:coalton
;;             (benchmark-queues/native::benchmark-enqueue-try-dequeue-x-threads
;;              *tasks*
;;              6
;;              1
;;              (io/conc/queues/bounded-mpmc:new-bounded-mpmc-queue *tasks*)))))

;; (define-benchmark bounded-enqueue-try-dequeue-x-tasks-6-enqueue-threads-3-dequeue-threads ()
;;   (declare (optimize speed))
;;   (loop :repeat *count*
;;         :do
;;            (c:coalton
;;             (benchmark-queues/native::benchmark-enqueue-try-dequeue-x-threads
;;              *tasks*
;;              6
;;              3
;;              (io/conc/queues/bounded-mpmc:new-bounded-mpmc-queue *tasks*)))))

;; (define-benchmark bounded-enqueue-try-dequeue-x-tasks-6-enqueue-threads-6-dequeue-threads ()
;;   (declare (optimize speed))
;;   (loop :repeat *count*
;;         :do
;;            (c:coalton
;;             (benchmark-queues/native::benchmark-enqueue-try-dequeue-x-threads
;;              *tasks*
;;              6
;;              6
;;              (io/conc/queues/bounded-mpmc:new-bounded-mpmc-queue *tasks*)))))

(define-benchmark unbounded-enqueue-try-dequeue-x-tasks-1-enqueue-thread-1-dequeue-thread ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-queues/native::benchmark-enqueue-try-dequeue-x-threads
             *tasks*
             1
             1
             io/conc/queues/unbounded-mpmc:new-unbounded-mpmc-queue))))

(define-benchmark unbounded-enqueue-try-dequeue-x-tasks-1-enqueue-thread-3-dequeue-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-queues/native::benchmark-enqueue-try-dequeue-x-threads
             *tasks*
             1
             3
             io/conc/queues/unbounded-mpmc:new-unbounded-mpmc-queue))))

(define-benchmark unbounded-enqueue-try-dequeue-x-tasks-1-enqueue-thread-6-dequeue-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-queues/native::benchmark-enqueue-try-dequeue-x-threads
             *tasks*
             1
             6
             io/conc/queues/unbounded-mpmc:new-unbounded-mpmc-queue))))

(define-benchmark unbounded-enqueue-try-dequeue-x-tasks-3-enqueue-threads-1-dequeue-thread ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-queues/native::benchmark-enqueue-try-dequeue-x-threads
             *tasks*
             3
             1
             io/conc/queues/unbounded-mpmc:new-unbounded-mpmc-queue))))

(define-benchmark unbounded-enqueue-try-dequeue-x-tasks-3-enqueue-threads-3-dequeue-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-queues/native::benchmark-enqueue-try-dequeue-x-threads
             *tasks*
             3
             3
             io/conc/queues/unbounded-mpmc:new-unbounded-mpmc-queue))))

(define-benchmark unbounded-enqueue-try-dequeue-x-tasks-3-enqueue-threads-6-dequeue-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-queues/native::benchmark-enqueue-try-dequeue-x-threads
             *tasks*
             3
             6
             io/conc/queues/unbounded-mpmc:new-unbounded-mpmc-queue))))

(define-benchmark unbounded-enqueue-try-dequeue-x-tasks-6-enqueue-threads-1-dequeue-thread ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-queues/native::benchmark-enqueue-try-dequeue-x-threads
             *tasks*
             6
             1
             io/conc/queues/unbounded-mpmc:new-unbounded-mpmc-queue))))

(define-benchmark unbounded-enqueue-try-dequeue-x-tasks-6-enqueue-threads-3-dequeue-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-queues/native::benchmark-enqueue-try-dequeue-x-threads
             *tasks*
             6
             3
             io/conc/queues/unbounded-mpmc:new-unbounded-mpmc-queue))))

(define-benchmark unbounded-enqueue-try-dequeue-x-tasks-6-enqueue-threads-6-dequeue-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-queues/native::benchmark-enqueue-try-dequeue-x-threads
             *tasks*
             6
             6
             io/conc/queues/unbounded-mpmc:new-unbounded-mpmc-queue))))

;; (define-benchmark bounded-enqueue-try-dequeue-pairs-x-tasks-1-thread ()
;;   (declare (optimize speed))
;;   (loop :repeat *count*
;;         :do
;;            (c:coalton
;;             (benchmark-queues/native::benchmark-enqueue-try-dequeue-pairs-x-threads
;;              *tasks*
;;              1
;;              (io/conc/queues/bounded-mpmc:new-bounded-mpmc-queue *tasks*)))))

;; (define-benchmark bounded-enqueue-try-dequeue-pairs-x-tasks-2-threads ()
;;   (declare (optimize speed))
;;   (loop :repeat *count*
;;         :do
;;            (c:coalton
;;             (benchmark-queues/native::benchmark-enqueue-try-dequeue-pairs-x-threads
;;              *tasks*
;;              2
;;              (io/conc/queues/bounded-mpmc:new-bounded-mpmc-queue *tasks*)))))

;; (define-benchmark bounded-enqueue-try-dequeue-pairs-x-tasks-4-threads ()
;;   (declare (optimize speed))
;;   (loop :repeat *count*
;;         :do
;;            (c:coalton
;;             (benchmark-queues/native::benchmark-enqueue-try-dequeue-pairs-x-threads
;;              *tasks*
;;              4
;;              (io/conc/queues/bounded-mpmc:new-bounded-mpmc-queue *tasks*)))))

;; (define-benchmark bounded-enqueue-try-dequeue-pairs-x-tasks-6-threads ()
;;   (declare (optimize speed))
;;   (loop :repeat *count*
;;         :do
;;            (c:coalton
;;             (benchmark-queues/native::benchmark-enqueue-try-dequeue-pairs-x-threads
;;              *tasks*
;;              6
;;              (io/conc/queues/bounded-mpmc:new-bounded-mpmc-queue *tasks*)))))

(define-benchmark unbounded-enqueue-try-dequeue-pairs-x-tasks-1-thread ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-queues/native::benchmark-enqueue-try-dequeue-pairs-x-threads
             *tasks*
             1
             io/conc/queues/unbounded-mpmc:new-unbounded-mpmc-queue))))

(define-benchmark unbounded-enqueue-try-dequeue-pairs-x-tasks-2-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-queues/native::benchmark-enqueue-try-dequeue-pairs-x-threads
             *tasks*
             2
             io/conc/queues/unbounded-mpmc:new-unbounded-mpmc-queue))))

(define-benchmark unbounded-enqueue-try-dequeue-pairs-x-tasks-4-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-queues/native::benchmark-enqueue-try-dequeue-pairs-x-threads
             *tasks*
             4
             io/conc/queues/unbounded-mpmc:new-unbounded-mpmc-queue))))

(define-benchmark unbounded-enqueue-try-dequeue-pairs-x-tasks-6-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-queues/native::benchmark-enqueue-try-dequeue-pairs-x-threads
             *tasks*
             6
             io/conc/queues/unbounded-mpmc:new-unbounded-mpmc-queue))))
