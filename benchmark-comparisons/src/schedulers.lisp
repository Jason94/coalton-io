(in-package #:benchmark-scheduler-comparison)

(declaim (optimize (speed 3) (safety 0) (debug 0)))

(defparameter *sample-count* 25)
(defparameter *tasks* 199992)

(defparameter *selected-coalton-benchmarks*
  '(benchmark-schedulers::single-producer-1-consumer-x-tasks-unbounded-scheduler
    benchmark-schedulers::single-producer-2-consumer-x-tasks-unbounded-scheduler
    benchmark-schedulers::single-producer-4-consumer-x-tasks-unbounded-scheduler
    benchmark-schedulers::single-producer-6-consumer-x-tasks-unbounded-scheduler
    benchmark-schedulers::2-producers-2-consumers-x-tasks-unbounded-scheduler
    benchmark-schedulers::4-producers-2-consumers-x-tasks-unbounded-scheduler
    benchmark-schedulers::6-producers-2-consumers-x-tasks-unbounded-scheduler
    benchmark-schedulers::2-producers-4-consumers-x-tasks-unbounded-scheduler
    benchmark-schedulers::4-producers-4-consumers-x-tasks-unbounded-scheduler
    benchmark-schedulers::6-producers-4-consumers-x-tasks-unbounded-scheduler
    benchmark-schedulers::2-producers-6-consumers-x-tasks-unbounded-scheduler
    benchmark-schedulers::4-producers-6-consumers-x-tasks-unbounded-scheduler
    benchmark-schedulers::6-producers-6-consumers-x-tasks-unbounded-scheduler
    benchmark-schedulers::single-producer-1-consumer-x-tasks-bounded-scheduler
    benchmark-schedulers::single-producer-2-consumer-x-tasks-bounded-scheduler
    benchmark-schedulers::single-producer-4-consumer-x-tasks-bounded-scheduler
    benchmark-schedulers::single-producer-6-consumer-x-tasks-bounded-scheduler
    benchmark-schedulers::2-producers-2-consumers-x-tasks-bounded-scheduler
    benchmark-schedulers::4-producers-2-consumers-x-tasks-bounded-scheduler
    benchmark-schedulers::6-producers-2-consumers-x-tasks-bounded-scheduler
    benchmark-schedulers::2-producers-4-consumers-x-tasks-bounded-scheduler
    benchmark-schedulers::4-producers-4-consumers-x-tasks-bounded-scheduler
    benchmark-schedulers::6-producers-4-consumers-x-tasks-bounded-scheduler
    benchmark-schedulers::2-producers-6-consumers-x-tasks-bounded-scheduler
    benchmark-schedulers::4-producers-6-consumers-x-tasks-bounded-scheduler
    benchmark-schedulers::6-producers-6-consumers-x-tasks-bounded-scheduler)
  "Existing coalton-io scheduler benchmarks matched by this package.")

(defun selected-coalton-benchmarks ()
  "Return the existing coalton-io scheduler benchmark names matched by this package."
  (copy-list *selected-coalton-benchmarks*))

(defstruct (atomic-value (:constructor make-atomic-value (value)))
  value)

(declaim (inline increment submit-lparallel-task-optim
                 submit-coalton-task-optim))

(defun increment (value)
  (at:atomic-update (atomic-value-value value) #'1+))

(defun await-completion (completed)
  (loop :while (< (atomic-value-value completed) *tasks*)))

(defun end-kernel (kernel)
  (let ((lp:*kernel* kernel))
    (lp:end-kernel :wait t)))

(defun await-lparallel-tasks (channel)
  (dotimes (i *tasks*)
    (declare (ignore i))
    (lp:receive-result channel)))

(defun benchmark-lparallel-single-producer (worker-count)
  (let ((completed (make-atomic-value 0))
        (lp:*kernel* (lp:make-kernel worker-count)))
    (unwind-protect
         (let ((channel (lp:make-channel)))
           (sleep 0.015)
           (b:start b::*current-timer*)
           (dotimes (i *tasks*)
             (declare (ignore i))
             (lp:submit-task channel #'increment completed))
           (await-lparallel-tasks channel)
           (await-completion completed)
           (b::stop b::*current-timer*)
           (b::commit b::*current-timer*))
      (lp:end-kernel :wait t))))

(defun benchmark-lparallel-multi-producer (producer-count worker-count)
  (let ((tasks-per-producer (truncate *tasks* producer-count)))
    (unless (= *tasks* (* tasks-per-producer producer-count))
      (error "*TASKS* must be a multiple of PRODUCER-COUNT"))
    (let ((ready (make-atomic-value nil))
          (completed (make-atomic-value 0))
          (lp:*kernel* (lp:make-kernel worker-count)))
      (unwind-protect
           (let* ((channel (lp:make-channel))
                  (producers
                    (loop :repeat producer-count
                          :collect
                          (bt:make-thread
                           (lambda ()
                             (loop :until (atomic-value-value ready))
                             (dotimes (i tasks-per-producer)
                               (declare (ignore i))
                               (lp:submit-task channel #'increment completed)))))))
             (sleep 0.015)
             (b:start b::*current-timer*)
             (unless (at:cas (atomic-value-value ready) nil t)
               (error "Producer start gate was already open"))
             (dolist (producer producers)
               (bt:join-thread producer))
             (await-lparallel-tasks channel)
             (await-completion completed)
             (b::stop b::*current-timer*)
             (b::commit b::*current-timer*))
        (lp:end-kernel :wait t)))))

(defun submit-lparallel-task-optim (kernel completed)
  (lpk::submit-raw-task
   (lpk::make-task
    (lambda ()
      (increment completed)))
   kernel))

(defun benchmark-lparallel-single-producer-optim (worker-count)
  (let ((completed (make-atomic-value 0))
        (kernel (lp:make-kernel worker-count)))
    (unwind-protect
         (progn
           (sleep 0.015)
           (b:start b::*current-timer*)
           (dotimes (i *tasks*)
             (declare (ignore i))
             (submit-lparallel-task-optim kernel completed))
           (await-completion completed)
           (b::stop b::*current-timer*)
           (b::commit b::*current-timer*))
      (end-kernel kernel))))

(defun benchmark-lparallel-multi-producer-optim (producer-count worker-count)
  (let ((tasks-per-producer (truncate *tasks* producer-count)))
    (unless (= *tasks* (* tasks-per-producer producer-count))
      (error "*TASKS* must be a multiple of PRODUCER-COUNT"))
    (let* ((ready (make-atomic-value nil))
           (completed (make-atomic-value 0))
           (kernel (lp:make-kernel worker-count))
           (producers
             (loop :repeat producer-count
                   :collect
                   (bt:make-thread
                    (lambda ()
                      (loop :until (atomic-value-value ready))
                      (dotimes (i tasks-per-producer)
                        (declare (ignore i))
                        (submit-lparallel-task-optim kernel completed)))))))
      (unwind-protect
           (progn
             (sleep 0.015)
             (b:start b::*current-timer*)
             (unless (at:cas (atomic-value-value ready) nil t)
               (error "Producer start gate was already open"))
             (await-completion completed)
             (b::stop b::*current-timer*)
             (b::commit b::*current-timer*)
             (dolist (producer producers)
               (bt:join-thread producer)))
        (end-kernel kernel)))))

(in-package #:benchmark-scheduler-comparison/native)

(coalton-toplevel

  (repr :native comp::atomic-value)
  (define-type CompletionCounter)

  (declare new-completion-counter (Void -> CompletionCounter))
  (define (new-completion-counter)
    (lisp (-> CompletionCounter) ()
      (comp::make-atomic-value 0)))

  (inline)
  (declare complete-task! (CompletionCounter -> UFix))
  (define (complete-task! counter)
    (lisp (-> UFix) (counter)
      (comp::increment counter)))

  (declare check-completed! (CompletionCounter * UFix -> Void))
  (define (check-completed! counter tasks)
    (lisp (-> Void) (counter tasks)
      (cl:assert (cl:= (comp::atomic-value-value counter) tasks))
      (cl:values)))

  (declare with-pool (UFix * (Void -> Void) -> Void))
  (define (with-pool worker-count workload)
    (let ((previous-worker-count (par:worker-count)))
      (let ((run (fn ()
                   (par:set-worker-count! worker-count)
                   (match (par:await (par:async (fn () Unit)))
                     ((Unit) (workload)))))
            (cleanup (fn ()
                       (par:shutdown!)
                       (par:set-worker-count! previous-worker-count))))
        (lisp (-> Void) (run cleanup)
          (cl:unwind-protect (cl:funcall run)
            (cl:funcall cleanup))))))

  (declare start-sample! (Void -> Void))
  (define (start-sample!)
    (lisp (-> Void) ()
      (cl:sleep 0.015)
      (b:start b::*current-timer*)
      (cl:values)))

  (declare commit-sample! (Void -> Void))
  (define (commit-sample!)
    (lisp (-> Void) ()
      (b::stop b::*current-timer*)
      (b::commit b::*current-timer*)
      (cl:values)))

  (declare submit-tasks! (CompletionCounter * arr:LispArray (par:Future UFix) -> Void))
  (define (submit-tasks! counter futures)
    (for ((declare i UFix)
          (i 0 (1+ i)))
      :repeat (arr:length futures)
      (arr:set! futures i (par:async (fn () (complete-task! counter))))))

  (declare await-tasks! (arr:LispArray (par:Future UFix) -> Void))
  (define (await-tasks! futures)
    (for ((declare i UFix)
          (i 0 (1+ i)))
      :repeat (arr:length futures)
      (match (par:await (arr:aref futures i))
        (_ (values)))))

  (declare benchmark-single-producer (UFix * UFix -> Void))
  (define (benchmark-single-producer tasks worker-count)
    (let ((counter (new-completion-counter))
          (futures (arr:make-uninitialized tasks)))
      (with-pool worker-count
        (fn ()
          (start-sample!)
          (submit-tasks! counter futures)
          (await-tasks! futures)
          (check-completed! counter tasks)
          (commit-sample!)))))

  (declare benchmark-multi-producer (UFix * UFix * UFix -> Void))
  (define (benchmark-multi-producer tasks producer-count worker-count)
    (let ((tasks-per-producer (math:div tasks producer-count))
          (counter (new-completion-counter))
          (ready (atomic:new False)))
      (unless (== tasks (* tasks-per-producer producer-count))
        (error "*TASKS* must be a multiple of PRODUCER-COUNT"))
      (let ((batches (the (List (arr:LispArray (par:Future UFix)))
                          (iter:collect!
                           (map (fn (_) (arr:make-uninitialized tasks-per-producer))
                                (iter:up-to producer-count))))))
        (with-pool worker-count
          (fn ()
            (let ((producers
                    (map (fn (batch)
                           (thread:spawn
                            (fn ()
                              (for ()
                                :while (not (atomic:read ready))
                                (values))
                              (submit-tasks! counter batch)
                              Unit)))
                         batches)))
              (start-sample!)
              (unless (atomic:compare-and-swap! ready False True)
                (error "Producer start gate was already open"))
              (iter:for-each! (fn (producer)
                                (match (thread:join producer)
                                  ((Unit) (values))))
                              (iter:into-iter producers))
              (iter:for-each! await-tasks! (iter:into-iter batches))
              (check-completed! counter tasks)
              (commit-sample!))))))))

(in-package #:benchmark-scheduler-comparison)

(defun benchmark-coalton-single-producer (worker-count)
  (coalton:coalton
   (benchmark-scheduler-comparison/native::benchmark-single-producer
    (coalton:lisp (coalton:-> coalton:UFix) () *tasks*)
    (coalton:lisp (coalton:-> coalton:UFix) () worker-count))))

(defun benchmark-coalton-multi-producer (producer-count worker-count)
  (coalton:coalton
   (benchmark-scheduler-comparison/native::benchmark-multi-producer
    (coalton:lisp (coalton:-> coalton:UFix) () *tasks*)
    (coalton:lisp (coalton:-> coalton:UFix) () producer-count)
    (coalton:lisp (coalton:-> coalton:UFix) () worker-count))))

(defun submit-coalton-task-optim (pool completed)
  (rt::inject
   pool
   (rt::%make-job
    (lambda ()
      (increment completed))
    nil
    :task)))

(defun benchmark-coalton-single-producer-optim (worker-count)
  (let ((completed (make-atomic-value 0))
        (pool (rt::start-pool worker-count)))
    (unwind-protect
         (progn
           (sleep 0.015)
           (b:start b::*current-timer*)
           (dotimes (i *tasks*)
             (declare (ignore i))
             (submit-coalton-task-optim pool completed))
           (await-completion completed)
           (b::stop b::*current-timer*)
           (b::commit b::*current-timer*))
      (rt::stop-pool pool))))

(defun benchmark-coalton-multi-producer-optim (producer-count worker-count)
  (let ((tasks-per-producer (truncate *tasks* producer-count)))
    (unless (= *tasks* (* tasks-per-producer producer-count))
      (error "*TASKS* must be a multiple of PRODUCER-COUNT"))
    (let* ((ready (make-atomic-value nil))
           (completed (make-atomic-value 0))
           (pool (rt::start-pool worker-count))
           (producers
             (loop :repeat producer-count
                   :collect
                   (bt:make-thread
                    (lambda ()
                      (loop :until (atomic-value-value ready))
                      (dotimes (i tasks-per-producer)
                        (declare (ignore i))
                        (submit-coalton-task-optim pool completed)))))))
      (unwind-protect
           (progn
             (sleep 0.015)
             (b:start b::*current-timer*)
             (unless (at:cas (atomic-value-value ready) nil t)
               (error "Producer start gate was already open"))
             (await-completion completed)
             (b::stop b::*current-timer*)
             (b::commit b::*current-timer*)
             (dolist (producer producers)
               (bt:join-thread producer)))
        (rt::stop-pool pool)))))

(define-benchmark lparallel-scheduler-1-producer-1-worker ()
  (declare (optimize speed))
  (loop :repeat *sample-count*
        :do (benchmark-lparallel-single-producer 1))
  (report b::*current-timer*))

(define-benchmark lparallel-scheduler-1-producer-2-workers ()
  (declare (optimize speed))
  (loop :repeat *sample-count*
        :do (benchmark-lparallel-single-producer 2))
  (report b::*current-timer*))

(define-benchmark lparallel-scheduler-1-producer-4-workers ()
  (declare (optimize speed))
  (loop :repeat *sample-count*
        :do (benchmark-lparallel-single-producer 4))
  (report b::*current-timer*))

(define-benchmark lparallel-scheduler-1-producer-6-workers ()
  (declare (optimize speed))
  (loop :repeat *sample-count*
        :do (benchmark-lparallel-single-producer 6))
  (report b::*current-timer*))

(define-benchmark lparallel-scheduler-2-producers-2-workers ()
  (declare (optimize speed))
  (loop :repeat *sample-count*
        :do (benchmark-lparallel-multi-producer 2 2))
  (report b::*current-timer*))

(define-benchmark lparallel-scheduler-4-producers-2-workers ()
  (declare (optimize speed))
  (loop :repeat *sample-count*
        :do (benchmark-lparallel-multi-producer 4 2))
  (report b::*current-timer*))

(define-benchmark lparallel-scheduler-6-producers-2-workers ()
  (declare (optimize speed))
  (loop :repeat *sample-count*
        :do (benchmark-lparallel-multi-producer 6 2))
  (report b::*current-timer*))

(define-benchmark lparallel-scheduler-2-producers-4-workers ()
  (declare (optimize speed))
  (loop :repeat *sample-count*
        :do (benchmark-lparallel-multi-producer 2 4))
  (report b::*current-timer*))

(define-benchmark lparallel-scheduler-4-producers-4-workers ()
  (declare (optimize speed))
  (loop :repeat *sample-count*
        :do (benchmark-lparallel-multi-producer 4 4))
  (report b::*current-timer*))

(define-benchmark lparallel-scheduler-6-producers-4-workers ()
  (declare (optimize speed))
  (loop :repeat *sample-count*
        :do (benchmark-lparallel-multi-producer 6 4))
  (report b::*current-timer*))

(define-benchmark lparallel-scheduler-2-producers-6-workers ()
  (declare (optimize speed))
  (loop :repeat *sample-count*
        :do (benchmark-lparallel-multi-producer 2 6))
  (report b::*current-timer*))

(define-benchmark lparallel-scheduler-4-producers-6-workers ()
  (declare (optimize speed))
  (loop :repeat *sample-count*
        :do (benchmark-lparallel-multi-producer 4 6))
  (report b::*current-timer*))

(define-benchmark lparallel-scheduler-6-producers-6-workers ()
  (declare (optimize speed))
  (loop :repeat *sample-count*
        :do (benchmark-lparallel-multi-producer 6 6))
  (report b::*current-timer*))

(define-benchmark lparallel-scheduler-1-producer-1-worker-optim ()
  (declare (optimize speed))
  (loop :repeat *sample-count*
        :do (benchmark-lparallel-single-producer-optim 1))
  (report b::*current-timer*))

(define-benchmark lparallel-scheduler-1-producer-2-workers-optim ()
  (declare (optimize speed))
  (loop :repeat *sample-count*
        :do (benchmark-lparallel-single-producer-optim 2))
  (report b::*current-timer*))

(define-benchmark lparallel-scheduler-1-producer-4-workers-optim ()
  (declare (optimize speed))
  (loop :repeat *sample-count*
        :do (benchmark-lparallel-single-producer-optim 4))
  (report b::*current-timer*))

(define-benchmark lparallel-scheduler-1-producer-6-workers-optim ()
  (declare (optimize speed))
  (loop :repeat *sample-count*
        :do (benchmark-lparallel-single-producer-optim 6))
  (report b::*current-timer*))

(define-benchmark lparallel-scheduler-2-producers-2-workers-optim ()
  (declare (optimize speed))
  (loop :repeat *sample-count*
        :do (benchmark-lparallel-multi-producer-optim 2 2))
  (report b::*current-timer*))

(define-benchmark lparallel-scheduler-4-producers-2-workers-optim ()
  (declare (optimize speed))
  (loop :repeat *sample-count*
        :do (benchmark-lparallel-multi-producer-optim 4 2))
  (report b::*current-timer*))

(define-benchmark lparallel-scheduler-6-producers-2-workers-optim ()
  (declare (optimize speed))
  (loop :repeat *sample-count*
        :do (benchmark-lparallel-multi-producer-optim 6 2))
  (report b::*current-timer*))

(define-benchmark lparallel-scheduler-2-producers-4-workers-optim ()
  (declare (optimize speed))
  (loop :repeat *sample-count*
        :do (benchmark-lparallel-multi-producer-optim 2 4))
  (report b::*current-timer*))

(define-benchmark lparallel-scheduler-4-producers-4-workers-optim ()
  (declare (optimize speed))
  (loop :repeat *sample-count*
        :do (benchmark-lparallel-multi-producer-optim 4 4))
  (report b::*current-timer*))

(define-benchmark lparallel-scheduler-6-producers-4-workers-optim ()
  (declare (optimize speed))
  (loop :repeat *sample-count*
        :do (benchmark-lparallel-multi-producer-optim 6 4))
  (report b::*current-timer*))

(define-benchmark lparallel-scheduler-2-producers-6-workers-optim ()
  (declare (optimize speed))
  (loop :repeat *sample-count*
        :do (benchmark-lparallel-multi-producer-optim 2 6))
  (report b::*current-timer*))

(define-benchmark lparallel-scheduler-4-producers-6-workers-optim ()
  (declare (optimize speed))
  (loop :repeat *sample-count*
        :do (benchmark-lparallel-multi-producer-optim 4 6))
  (report b::*current-timer*))

(define-benchmark lparallel-scheduler-6-producers-6-workers-optim ()
  (declare (optimize speed))
  (loop :repeat *sample-count*
        :do (benchmark-lparallel-multi-producer-optim 6 6))
  (report b::*current-timer*))

(define-benchmark coalton-scheduler-1-producer-1-worker ()
  (declare (optimize speed))
  (loop :repeat *sample-count*
        :do (benchmark-coalton-single-producer 1))
  (report b::*current-timer*))

(define-benchmark coalton-scheduler-1-producer-2-workers ()
  (declare (optimize speed))
  (loop :repeat *sample-count*
        :do (benchmark-coalton-single-producer 2))
  (report b::*current-timer*))

(define-benchmark coalton-scheduler-1-producer-4-workers ()
  (declare (optimize speed))
  (loop :repeat *sample-count*
        :do (benchmark-coalton-single-producer 4))
  (report b::*current-timer*))

(define-benchmark coalton-scheduler-1-producer-6-workers ()
  (declare (optimize speed))
  (loop :repeat *sample-count*
        :do (benchmark-coalton-single-producer 6))
  (report b::*current-timer*))

(define-benchmark coalton-scheduler-2-producers-2-workers ()
  (declare (optimize speed))
  (loop :repeat *sample-count*
        :do (benchmark-coalton-multi-producer 2 2))
  (report b::*current-timer*))

(define-benchmark coalton-scheduler-4-producers-2-workers ()
  (declare (optimize speed))
  (loop :repeat *sample-count*
        :do (benchmark-coalton-multi-producer 4 2))
  (report b::*current-timer*))

(define-benchmark coalton-scheduler-6-producers-2-workers ()
  (declare (optimize speed))
  (loop :repeat *sample-count*
        :do (benchmark-coalton-multi-producer 6 2))
  (report b::*current-timer*))

(define-benchmark coalton-scheduler-2-producers-4-workers ()
  (declare (optimize speed))
  (loop :repeat *sample-count*
        :do (benchmark-coalton-multi-producer 2 4))
  (report b::*current-timer*))

(define-benchmark coalton-scheduler-4-producers-4-workers ()
  (declare (optimize speed))
  (loop :repeat *sample-count*
        :do (benchmark-coalton-multi-producer 4 4))
  (report b::*current-timer*))

(define-benchmark coalton-scheduler-6-producers-4-workers ()
  (declare (optimize speed))
  (loop :repeat *sample-count*
        :do (benchmark-coalton-multi-producer 6 4))
  (report b::*current-timer*))

(define-benchmark coalton-scheduler-2-producers-6-workers ()
  (declare (optimize speed))
  (loop :repeat *sample-count*
        :do (benchmark-coalton-multi-producer 2 6))
  (report b::*current-timer*))

(define-benchmark coalton-scheduler-4-producers-6-workers ()
  (declare (optimize speed))
  (loop :repeat *sample-count*
        :do (benchmark-coalton-multi-producer 4 6))
  (report b::*current-timer*))

(define-benchmark coalton-scheduler-6-producers-6-workers ()
  (declare (optimize speed))
  (loop :repeat *sample-count*
        :do (benchmark-coalton-multi-producer 6 6))
  (report b::*current-timer*))

(define-benchmark coalton-scheduler-1-producer-1-worker-optim ()
  (declare (optimize speed))
  (loop :repeat *sample-count*
        :do (benchmark-coalton-single-producer-optim 1))
  (report b::*current-timer*))

(define-benchmark coalton-scheduler-1-producer-2-workers-optim ()
  (declare (optimize speed))
  (loop :repeat *sample-count*
        :do (benchmark-coalton-single-producer-optim 2))
  (report b::*current-timer*))

(define-benchmark coalton-scheduler-1-producer-4-workers-optim ()
  (declare (optimize speed))
  (loop :repeat *sample-count*
        :do (benchmark-coalton-single-producer-optim 4))
  (report b::*current-timer*))

(define-benchmark coalton-scheduler-1-producer-6-workers-optim ()
  (declare (optimize speed))
  (loop :repeat *sample-count*
        :do (benchmark-coalton-single-producer-optim 6))
  (report b::*current-timer*))

(define-benchmark coalton-scheduler-2-producers-2-workers-optim ()
  (declare (optimize speed))
  (loop :repeat *sample-count*
        :do (benchmark-coalton-multi-producer-optim 2 2))
  (report b::*current-timer*))

(define-benchmark coalton-scheduler-4-producers-2-workers-optim ()
  (declare (optimize speed))
  (loop :repeat *sample-count*
        :do (benchmark-coalton-multi-producer-optim 4 2))
  (report b::*current-timer*))

(define-benchmark coalton-scheduler-6-producers-2-workers-optim ()
  (declare (optimize speed))
  (loop :repeat *sample-count*
        :do (benchmark-coalton-multi-producer-optim 6 2))
  (report b::*current-timer*))

(define-benchmark coalton-scheduler-2-producers-4-workers-optim ()
  (declare (optimize speed))
  (loop :repeat *sample-count*
        :do (benchmark-coalton-multi-producer-optim 2 4))
  (report b::*current-timer*))

(define-benchmark coalton-scheduler-4-producers-4-workers-optim ()
  (declare (optimize speed))
  (loop :repeat *sample-count*
        :do (benchmark-coalton-multi-producer-optim 4 4))
  (report b::*current-timer*))

(define-benchmark coalton-scheduler-6-producers-4-workers-optim ()
  (declare (optimize speed))
  (loop :repeat *sample-count*
        :do (benchmark-coalton-multi-producer-optim 6 4))
  (report b::*current-timer*))

(define-benchmark coalton-scheduler-2-producers-6-workers-optim ()
  (declare (optimize speed))
  (loop :repeat *sample-count*
        :do (benchmark-coalton-multi-producer-optim 2 6))
  (report b::*current-timer*))

(define-benchmark coalton-scheduler-4-producers-6-workers-optim ()
  (declare (optimize speed))
  (loop :repeat *sample-count*
        :do (benchmark-coalton-multi-producer-optim 4 6))
  (report b::*current-timer*))

(define-benchmark coalton-scheduler-6-producers-6-workers-optim ()
  (declare (optimize speed))
  (loop :repeat *sample-count*
        :do (benchmark-coalton-multi-producer-optim 6 6))
  (report b::*current-timer*))

(defun run ()
  "Run the scheduler comparison benchmarks."
  (io/benchmarks:run-benchmark
   'benchmark-scheduler-comparison))

(defun run-ci ()
  "Run the scheduler comparisons and write benchmark_scheduler_comparison.csv."
  (io/benchmarks:run-benchmark-ci
   'benchmark-scheduler-comparison))
