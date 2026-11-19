(in-package #:benchmark-queue-comparison)

(declaim (optimize (speed 3) (safety 0) (debug 0)))

(defparameter *sample-count* 75)
(defparameter *tasks* 48000)

(defparameter *selected-coalton-benchmarks*
  '(benchmark-queues::unbounded-enqueue-x-tasks-1-thread
    benchmark-queues::unbounded-dequeue-x-tasks-1-thread

    benchmark-queues::unbounded-enqueue-try-dequeue-pairs-x-tasks-1-thread
    benchmark-queues::unbounded-enqueue-try-dequeue-pairs-x-tasks-6-threads
    benchmark-queues::unbounded-enqueue-try-dequeue-pairs-x-tasks-12-threads

    benchmark-queues::unbounded-enqueue-try-dequeue-x-tasks-1-enqueue-thread-1-dequeue-thread
    benchmark-queues::unbounded-enqueue-try-dequeue-x-tasks-3-enqueue-threads-3-dequeue-threads
    benchmark-queues::unbounded-enqueue-try-dequeue-x-tasks-6-enqueue-threads-6-dequeue-threads

    benchmark-queues::bounded-enqueue-x-tasks-1-thread)
  "Existing coalton-io benchmarks matched by this package.")

(defun selected-coalton-benchmarks ()
  "Return the existing coalton-io benchmark names matched by this package."
  (copy-list *selected-coalton-benchmarks*))

(declaim (inline timer-start timer-stop-and-commit))

(defun timer-start ()
  (b:start b::*current-timer*))

(defun timer-stop-and-commit ()
  (b::stop b::*current-timer*)
  (b::commit b::*current-timer*))

(defun make-start-gate ()
  (bt:make-semaphore))

(defun await-start (gate)
  (bt:wait-on-semaphore gate))

(defun open-start-gate (gate thread-count)
  (bt:signal-semaphore gate :count thread-count))

(defun join-all (threads)
  (dolist (thread threads)
    (bt:join-thread thread)))

(declaim (inline channel-try-pop))

(defun channel-try-pop (queue)
  (let ((value (ch:try-recv! queue)))
    (values (opt:from-some-or nil value)
            (opt:some? value))))

(defmacro define-enqueue-benchmark
    (name make-queue-form enqueue-form)
  `(define-benchmark ,name ()
     (declare (optimize speed))
     (loop :repeat *sample-count*
           :do
              (let* ((queue ,make-queue-form)
                     (gate (make-start-gate))
                     (threads
                       (list
                        (bt:make-thread
                         (lambda ()
                           (await-start gate)
                           (dotimes (i *tasks*)
                             (declare (ignore i))
                             ,enqueue-form))))))
                (sleep 0.002)
                (timer-start)
                (open-start-gate gate 1)
                (join-all threads)
                (timer-stop-and-commit)))))

(defmacro define-pairs-benchmark
    (name thread-count make-queue-form enqueue-form try-pop-form)
  `(define-benchmark ,name ()
     (declare (optimize speed))
     (loop :repeat *sample-count*
           :do
              (let* ((queue ,make-queue-form)
                     (gate (make-start-gate))
                     (tasks-per-thread
                       (truncate *tasks* ,thread-count))
                     (threads
                       (loop :repeat ,thread-count
                             :collect
                             (bt:make-thread
                              (lambda ()
                                (await-start gate)
                                (dotimes (i tasks-per-thread)
                                  (declare (ignore i))
                                  ,enqueue-form
                                  (loop
                                    (multiple-value-bind (value presentp)
                                        ,try-pop-form
                                      (declare (ignore value))
                                      (when presentp
                                        (return))))))))))
                (sleep 0.002)
                (timer-start)
                (open-start-gate gate ,thread-count)
                (join-all threads)
                (timer-stop-and-commit)))))

(defmacro define-try-pipeline-benchmark
    (name producer-count consumer-count
     make-queue-form enqueue-form try-pop-form)
  `(define-benchmark ,name ()
     (declare (optimize speed))
     (loop :repeat *sample-count*
           :do
              (let* ((queue ,make-queue-form)
                     (gate (make-start-gate))
                     (producer-tasks
                       (truncate *tasks* ,producer-count))
                     (consumer-tasks
                       (truncate *tasks* ,consumer-count))
                     (producers
                       (loop :repeat ,producer-count
                             :collect
                             (bt:make-thread
                              (lambda ()
                                (await-start gate)
                                (dotimes (i producer-tasks)
                                  (declare (ignore i))
                                  ,enqueue-form)))))
                     (consumers
                       (loop :repeat ,consumer-count
                             :collect
                             (bt:make-thread
                              (lambda ()
                                (await-start gate)
                                (dotimes (i consumer-tasks)
                                  (declare (ignore i))
                                  (loop
                                    (multiple-value-bind (value presentp)
                                        ,try-pop-form
                                      (declare (ignore value))
                                      (when presentp
                                        (return))))))))))
                (sleep 0.002)
                (timer-start)
                (open-start-gate
                 gate
                 (+ ,producer-count ,consumer-count))
                (join-all producers)
                (join-all consumers)
                (timer-stop-and-commit)))))

;;; Matches UNBOUNDED-ENQUEUE-X-TASKS-1-THREAD.

(define-enqueue-benchmark freelock-unbounded-enqueue-1t
    (fl:make-queue)
  (fl:queue-push queue t))

(define-enqueue-benchmark lparallel-unbounded-enqueue-1t
    (lp:make-queue)
  (lp:push-queue t queue))

(define-enqueue-benchmark coalton-unbounded-enqueue-1t
    (ch:new)
  (ch:send! queue t))

;;; Matches UNBOUNDED-DEQUEUE-X-TASKS-1-THREAD.

(define-enqueue-benchmark freelock-unbounded-dequeue-1t
    (let ((queue (fl:make-queue)))
      (dotimes (i *tasks*)
        (declare (ignore i))
        (fl:queue-push queue t))
      queue)
  (fl:queue-pop queue))

(define-enqueue-benchmark lparallel-unbounded-dequeue-1t
    (let ((queue (lp:make-queue)))
      (dotimes (i *tasks*)
        (declare (ignore i))
        (lp:push-queue t queue))
      queue)
  (lp:try-pop-queue queue))

(define-enqueue-benchmark coalton-unbounded-dequeue-1t
    (let ((queue (ch:new)))
      (dotimes (i *tasks*)
        (declare (ignore i))
        (ch:send! queue t))
      queue)
  (channel-try-pop queue))

;;; Matches
;;; UNBOUNDED-ENQUEUE-TRY-DEQUEUE-PAIRS-X-TASKS-{1,6,12}-THREADS.

(define-pairs-benchmark freelock-unbounded-pairs-1t
    1
    (fl:make-queue)
  (fl:queue-push queue t)
  (fl:queue-pop queue))

(define-pairs-benchmark lparallel-unbounded-pairs-1t
    1
    (lp:make-queue)
  (lp:push-queue t queue)
  (lp:try-pop-queue queue))

(define-pairs-benchmark coalton-unbounded-pairs-1t
    1
    (ch:new)
  (ch:send! queue t)
  (channel-try-pop queue))

(define-pairs-benchmark freelock-unbounded-pairs-6t
    6
    (fl:make-queue)
  (fl:queue-push queue t)
  (fl:queue-pop queue))

(define-pairs-benchmark lparallel-unbounded-pairs-6t
    6
    (lp:make-queue)
  (lp:push-queue t queue)
  (lp:try-pop-queue queue))

(define-pairs-benchmark coalton-unbounded-pairs-6t
    6
    (ch:new)
  (ch:send! queue t)
  (channel-try-pop queue))

(define-pairs-benchmark freelock-unbounded-pairs-12t
    12
    (fl:make-queue)
  (fl:queue-push queue t)
  (fl:queue-pop queue))

(define-pairs-benchmark lparallel-unbounded-pairs-12t
    12
    (lp:make-queue)
  (lp:push-queue t queue)
  (lp:try-pop-queue queue))

(define-pairs-benchmark coalton-unbounded-pairs-12t
    12
    (ch:new)
  (ch:send! queue t)
  (channel-try-pop queue))

;;; Matches the 1P1C, 3P3C, and 6P6C
;;; UNBOUNDED-ENQUEUE-TRY-DEQUEUE-X-TASKS benchmarks.

(define-try-pipeline-benchmark
    freelock-unbounded-try-pipeline-1p1c
    1 1
    (fl:make-queue)
  (fl:queue-push queue t)
  (fl:queue-pop queue))

(define-try-pipeline-benchmark
    lparallel-unbounded-try-pipeline-1p1c
    1 1
    (lp:make-queue)
  (lp:push-queue t queue)
  (lp:try-pop-queue queue))

(define-try-pipeline-benchmark
    coalton-unbounded-try-pipeline-1p1c
    1 1
    (ch:new)
  (ch:send! queue t)
  (channel-try-pop queue))

(define-try-pipeline-benchmark
    freelock-unbounded-try-pipeline-3p3c
    3 3
    (fl:make-queue)
  (fl:queue-push queue t)
  (fl:queue-pop queue))

(define-try-pipeline-benchmark
    lparallel-unbounded-try-pipeline-3p3c
    3 3
    (lp:make-queue)
  (lp:push-queue t queue)
  (lp:try-pop-queue queue))

(define-try-pipeline-benchmark
    coalton-unbounded-try-pipeline-3p3c
    3 3
    (ch:new)
  (ch:send! queue t)
  (channel-try-pop queue))

(define-try-pipeline-benchmark
    freelock-unbounded-try-pipeline-6p6c
    6 6
    (fl:make-queue)
  (fl:queue-push queue t)
  (fl:queue-pop queue))

(define-try-pipeline-benchmark
    lparallel-unbounded-try-pipeline-6p6c
    6 6
    (lp:make-queue)
  (lp:push-queue t queue)
  (lp:try-pop-queue queue))

(define-try-pipeline-benchmark
    coalton-unbounded-try-pipeline-6p6c
    6 6
    (ch:new)
  (ch:send! queue t)
  (channel-try-pop queue))

;;; Matches BOUNDED-ENQUEUE-X-TASKS-1-THREAD. cl-freelock requires a
;;; power-of-two capacity, so both native queues use the next power of two.
;;; All 48,000 pushes succeed and neither queue reaches capacity.

(define-enqueue-benchmark freelock-bounded-enqueue-1t
    (fl:make-bounded-queue 65536)
  (fl:bounded-queue-push queue t))

(define-enqueue-benchmark lparallel-bounded-enqueue-1t
    (lp:make-queue :fixed-capacity 65536)
  (lp:push-queue t queue))

(defun run ()
  "Run the native comparison benchmarks."
  (io/benchmarks:run-benchmark
   'benchmark-queue-comparison))

(defun run-ci ()
  "Run the native comparisons and write benchmark_queue_comparison.csv."
  (io/benchmarks:run-benchmark-ci
   'benchmark-queue-comparison))
