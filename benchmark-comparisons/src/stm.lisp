(in-package #:benchmark-stm-comparison)

(declaim (optimize (speed 3) (safety 0) (debug 0)))

(defparameter *sample-count* 30)
(defparameter *workload* 57600)

(declaim (type fixnum *sample-count* *workload*))

(defparameter *selected-coalton-benchmarks*
  '(benchmark-stm::small-tx-1-worker
    benchmark-stm::small-tx-2-worker
    benchmark-stm::small-tx-4-worker
    benchmark-stm::small-tx-6-worker
    benchmark-stm::large-tx-1-worker
    benchmark-stm::large-tx-2-worker
    benchmark-stm::large-tx-4-worker
    benchmark-stm::large-tx-6-worker
    benchmark-stm::loop-tarr-1-worker
    benchmark-stm::loop-tarr-2-worker
    benchmark-stm::loop-tarr-4-worker
    benchmark-stm::loop-tarr-6-worker
    benchmark-stm::loop-large-tarr-1-worker
    benchmark-stm::loop-large-tarr-2-worker
    benchmark-stm::loop-large-tarr-4-worker
    benchmark-stm::loop-large-tarr-6-worker
    benchmark-stm::write-separate-1-workers
    benchmark-stm::write-separate-2-workers
    benchmark-stm::write-separate-4-workers
    benchmark-stm::write-separate-6-workers
    benchmark-stm::increment-round-robin-2-workers
    benchmark-stm::increment-round-robin-4-workers
    benchmark-stm::increment-round-robin-6-workers)
  "Existing coalton-io STM benchmarks matched by this package.")

(defun selected-coalton-benchmarks ()
  "Return the existing coalton-io STM benchmark names matched by this package."
  (copy-list *selected-coalton-benchmarks*))

(declaim (inline timer-start timer-stop-and-commit))

(defun timer-start ()
  (b:start b::*current-timer*))

(defun timer-stop-and-commit ()
  (b::stop b::*current-timer*)
  (b::commit b::*current-timer*))

(declaim (ftype (function (fixnum) fixnum) divide-workload))

(defun divide-workload (divisor)
  (declare (type fixnum divisor))
  (multiple-value-bind (quotient remainder)
      (truncate *workload* divisor)
    (unless (zerop remainder)
      (error "Workload ~D is not divisible by ~D" *workload* divisor))
    (the fixnum quotient)))

(defun make-tvar-vector (size)
  (declare (type fixnum size))
  (let ((tvars (make-array size)))
    (dotimes (i size tvars)
      (setf (svref tvars i) (stm:tvar 0)))))

(declaim (ftype (function (simple-vector) fixnum) sum-tvars))

(defun sum-tvars (tvars)
  (declare (type simple-vector tvars))
  (stm:atomic
    (loop :for tvar :across tvars
          :sum (the fixnum (stm:$ tvar)) :of-type fixnum)))

(defun run-workers (worker-count worker)
  "Run WORKER once in each indexed thread, timing only released work and joins."
  (declare (type fixnum worker-count) (type function worker))
  (let* ((ready-gate (bt:make-semaphore))
         (start-gate (bt:make-semaphore))
         (threads
           (loop :for worker-index :below worker-count
                 :collect
                 (let ((worker-index worker-index))
                   (declare (type fixnum worker-index))
                   (bt:make-thread
                    (lambda ()
                      (bt:signal-semaphore ready-gate)
                      (bt:wait-on-semaphore start-gate)
                      (funcall worker worker-index)))))))
    (dotimes (i worker-count)
      (declare (ignore i))
      (bt:wait-on-semaphore ready-gate))
    (timer-start)
    (bt:signal-semaphore start-gate :count worker-count)
    (dolist (thread threads)
      (bt:join-thread thread))
    (timer-stop-and-commit)))

(defun benchmark-small-tx (worker-count)
  (declare (type fixnum worker-count))
  (let ((tvar (stm:tvar 0))
        (work-per-thread (divide-workload worker-count)))
    (declare (type fixnum work-per-thread))
    (run-workers
     worker-count
     (lambda (worker-index)
       (declare (ignore worker-index) (type fixnum worker-index))
       (dotimes (i work-per-thread)
         (declare (ignore i))
         (stm:atomic
           (incf (the fixnum (stm:$ tvar)))))))
    (let ((actual (stm:atomic (the fixnum (stm:$ tvar)))))
      (unless (= actual *workload*)
        (error "STMX small transaction with ~D workers expected ~D, received ~D"
               worker-count *workload* actual)))))

(defun benchmark-large-tx (worker-count)
  (declare (type fixnum worker-count))
  (let ((tvar1 (stm:tvar 0))
        (tvar2 (stm:tvar 0))
        (tvar3 (stm:tvar 0))
        (tvar4 (stm:tvar 0))
        (tvar5 (stm:tvar 0))
        (tvar6 (stm:tvar 0))
        (tvar7 (stm:tvar 0))
        (tvar8 (stm:tvar 0))
        (iterations-per-thread (divide-workload (* worker-count 8))))
    (declare (type fixnum iterations-per-thread))
    (run-workers
     worker-count
     (lambda (worker-index)
       (declare (ignore worker-index) (type fixnum worker-index))
       (dotimes (i iterations-per-thread)
         (declare (ignore i))
         (stm:atomic
           (incf (the fixnum (stm:$ tvar1)))
           (incf (the fixnum (stm:$ tvar2)))
           (incf (the fixnum (stm:$ tvar3)))
           (incf (the fixnum (stm:$ tvar4)))
           (incf (the fixnum (stm:$ tvar5)))
           (incf (the fixnum (stm:$ tvar6)))
           (incf (the fixnum (stm:$ tvar7)))
           (incf (the fixnum (stm:$ tvar8)))))))
    (let ((actual
            (stm:atomic
              (+ (the fixnum (stm:$ tvar1))
                 (the fixnum (stm:$ tvar2))
                 (the fixnum (stm:$ tvar3))
                 (the fixnum (stm:$ tvar4))
                 (the fixnum (stm:$ tvar5))
                 (the fixnum (stm:$ tvar6))
                 (the fixnum (stm:$ tvar7))
                 (the fixnum (stm:$ tvar8))))))
      (unless (= actual *workload*)
        (error "STMX large transaction with ~D workers expected ~D, received ~D"
               worker-count *workload* actual)))))

(defun benchmark-loop-tarr (worker-count tarr-size)
  (declare (type fixnum worker-count tarr-size))
  (let* ((tvars (make-tvar-vector tarr-size))
         (iterations-per-thread
           (divide-workload (* worker-count tarr-size))))
    (declare (type simple-vector tvars)
             (type fixnum iterations-per-thread))
    (run-workers
     worker-count
     (lambda (worker-index)
       (declare (ignore worker-index) (type fixnum worker-index))
       (dotimes (iteration iterations-per-thread)
         (declare (ignore iteration))
         (dotimes (i tarr-size)
           (let ((tvar (svref tvars i)))
             (stm:atomic
               (incf (the fixnum (stm:$ tvar)))))))))
    (let ((actual (sum-tvars tvars)))
      (unless (= actual *workload*)
        (error "STMX tvar array of size ~D with ~D workers expected ~D, received ~D"
               tarr-size worker-count *workload* actual)))))

(defun benchmark-write-separate (worker-count)
  (declare (type fixnum worker-count))
  (let ((tvars (make-tvar-vector worker-count))
        (work-per-thread (divide-workload worker-count)))
    (declare (type simple-vector tvars)
             (type fixnum work-per-thread))
    (run-workers
     worker-count
     (lambda (worker-index)
       (declare (type fixnum worker-index))
       (let ((tvar (svref tvars worker-index)))
         (dotimes (i work-per-thread)
           (declare (ignore i))
           (stm:atomic
             (incf (the fixnum (stm:$ tvar))))))))
    (let ((actual (sum-tvars tvars)))
      (unless (= actual *workload*)
        (error "STMX separate writes with ~D workers expected ~D, received ~D"
               worker-count *workload* actual)))))

(defun benchmark-increment-round-robin (worker-count)
  (declare (type fixnum worker-count))
  (let ((count (stm:tvar 0)))
    (run-workers
     worker-count
     (lambda (worker-index)
       (declare (type fixnum worker-index))
       (loop
         (let ((value
                 (stm:atomic
                   (let ((value (the fixnum (stm:$ count))))
                     (declare (type fixnum value))
                     (cond
                       ((>= value *workload*)
                        value)
                       ((= worker-index (mod value worker-count))
                        (setf (stm:$ count) (the fixnum (1+ value))))
                       (t
                        (stm:retry)))))))
           (when (>= value *workload*)
             (return))))))
    (let ((actual (stm:atomic (the fixnum (stm:$ count)))))
      (unless (= actual *workload*)
        (error "STMX round robin with ~D workers expected ~D, received ~D"
               worker-count *workload* actual)))))

(defmacro define-stmx-benchmark (name function &rest arguments)
  `(define-benchmark ,name ()
     (declare (optimize speed))
     (loop :repeat *sample-count*
           :do (,function ,@arguments))
     (report b::*current-timer*)))

;;; Matches SMALL-TX-{1,2,4,6}-WORKER.

(define-stmx-benchmark stmx-small-tx-1-worker benchmark-small-tx 1)
(define-stmx-benchmark stmx-small-tx-2-worker benchmark-small-tx 2)
(define-stmx-benchmark stmx-small-tx-4-worker benchmark-small-tx 4)
(define-stmx-benchmark stmx-small-tx-6-worker benchmark-small-tx 6)

;;; Matches LARGE-TX-{1,2,4,6}-WORKER.

(define-stmx-benchmark stmx-large-tx-1-worker benchmark-large-tx 1)
(define-stmx-benchmark stmx-large-tx-2-worker benchmark-large-tx 2)
(define-stmx-benchmark stmx-large-tx-4-worker benchmark-large-tx 4)
(define-stmx-benchmark stmx-large-tx-6-worker benchmark-large-tx 6)

;;; Matches LOOP-TARR-{1,2,4,6}-WORKER with six transactional cells.

(define-stmx-benchmark stmx-loop-tarr-1-worker benchmark-loop-tarr 1 6)
(define-stmx-benchmark stmx-loop-tarr-2-worker benchmark-loop-tarr 2 6)
(define-stmx-benchmark stmx-loop-tarr-4-worker benchmark-loop-tarr 4 6)
(define-stmx-benchmark stmx-loop-tarr-6-worker benchmark-loop-tarr 6 6)

;;; Matches LOOP-LARGE-TARR-{1,2,4,6}-WORKER with 60 transactional cells.

(define-stmx-benchmark stmx-loop-large-tarr-1-worker benchmark-loop-tarr 1 60)
(define-stmx-benchmark stmx-loop-large-tarr-2-worker benchmark-loop-tarr 2 60)
(define-stmx-benchmark stmx-loop-large-tarr-4-worker benchmark-loop-tarr 4 60)
(define-stmx-benchmark stmx-loop-large-tarr-6-worker benchmark-loop-tarr 6 60)

;;; Matches WRITE-SEPARATE-{1,2,4,6}-WORKERS.

(define-stmx-benchmark stmx-write-separate-1-workers benchmark-write-separate 1)
(define-stmx-benchmark stmx-write-separate-2-workers benchmark-write-separate 2)
(define-stmx-benchmark stmx-write-separate-4-workers benchmark-write-separate 4)
(define-stmx-benchmark stmx-write-separate-6-workers benchmark-write-separate 6)

;;; Matches INCREMENT-ROUND-ROBIN-{2,4,6}-WORKERS.

(define-stmx-benchmark stmx-increment-round-robin-2-workers
    benchmark-increment-round-robin 2)
(define-stmx-benchmark stmx-increment-round-robin-4-workers
    benchmark-increment-round-robin 4)
(define-stmx-benchmark stmx-increment-round-robin-6-workers
    benchmark-increment-round-robin 6)

(defun run ()
  "Run the STMX comparison benchmarks."
  (io/benchmarks:run-benchmark
   'benchmark-stm-comparison))

(defun run-ci ()
  "Run the STMX comparisons and write benchmark_stm_comparison.csv."
  (io/benchmarks:run-benchmark-ci
   'benchmark-stm-comparison))
