(in-package #:io/benchmarks)

;;; STM Benchmarks
;;;
;;; These benchmarks test the STM at different thread counts
;;; with different sizes of transactions.

(define-io-benchmark stm ()
  (:use
   #:coalton
   #:coalton-prelude
   #:io/utils
   #:io/monad-io
   #:io/simple-io
   #:io/thread
   #:io/conc/ring-buffer
   #:io/conc/worker-pool
   #:io/conc/stm)
  (:local-nicknames
   (:b #:benchmark-utils)
   ))

(in-package #:benchmark-stm/native)

(cl:declaim (cl:optimize (cl:speed 3) (cl:safety 0)))

(coalton-toplevel
  ;; Divisible by 8 to be able to evenly split workloads at all thread counts
  (define workload (the UFix 80000))
  
  (declare small-tx-n-times (UFix -> Void))
  (define (small-tx-n-times n-threads)
    (run-io!
     (do
      (tvar <- (new-tvar 0))
      (n-finished <- (new-tvar 0))
      (let work-per-thread = (coalton/math:div workload n-threads))
      (scheduler <- (new-ring-buffer-scheduler n-threads))
      (pool <- (new-worker-pool n-threads scheduler))
      (wrap-io (b:start (b:current-timer)))
      (do-times-io_ n-threads
        (do-submit-job_ pool
          (do-times-io_ work-per-thread
            (do-run-tx
              (modify-tvar tvar 1+)))
          (run-tx (modify-tvar n-finished 1+))))
      (do-run-tx
        (n-finished <- (read-tvar n-finished))
        (retry-unless (== n-threads n-finished)))
      (wrap-io
       (b:stop (b:current-timer))
       (b:commit (b:current-timer)))
      (request-shutdown pool)
      (await pool)))
    (values))

  (declare large-tx-n-times (UFix -> Void))
  (define (large-tx-n-times n-threads)
    (run-io!
     (do
      (tvar1 <- (new-tvar 0))
      (tvar2 <- (new-tvar 0))
      (tvar3 <- (new-tvar 0))
      (tvar4 <- (new-tvar 0))
      (tvar5 <- (new-tvar 0))
      (tvar6 <- (new-tvar 0))
      (tvar7 <- (new-tvar 0))
      (tvar8 <- (new-tvar 0))
      (n-finished <- (new-tvar 0))
      (let work-per-thread = (coalton/math:div workload n-threads))
      (scheduler <- (new-ring-buffer-scheduler n-threads))
      (pool <- (new-worker-pool n-threads scheduler))
      (wrap-io (b:start (b:current-timer)))
      (do-times-io_ n-threads
        (do-submit-job_ pool
          (do-times-io_ work-per-thread
            (do-run-tx
              (modify-tvar tvar1 1+)
              (modify-tvar tvar2 1+)
              (modify-tvar tvar3 1+)
              (modify-tvar tvar4 1+)))
          (do-times-io_ work-per-thread
            (do-run-tx
              (modify-tvar tvar5 1+)
              (modify-tvar tvar6 1+)
              (modify-tvar tvar7 1+)
              (modify-tvar tvar8 1+)))
          (run-tx (modify-tvar n-finished 1+))))
      (do-run-tx
        (n-finished <- (read-tvar n-finished))
        (retry-unless (== n-threads n-finished)))
      (wrap-io
       (b:stop (b:current-timer))
       (b:commit (b:current-timer)))
      (request-shutdown pool)
      (await pool)))
    (values))
 )


(in-package #:benchmark-stm)

(cl:declaim (cl:optimize (cl:speed 3) (cl:safety 0)))

(defparameter *count* 50)

(define-benchmark small-tx-1-worker ()
  (loop :repeat *count*
        :do
           (coalton:coalton
            (benchmark-stm/native::small-tx-n-times 1)))
  (report trivial-benchmark::*current-timer*))

(define-benchmark small-tx-2-worker ()
  (loop :repeat *count*
        :do
           (coalton:coalton
            (benchmark-stm/native::small-tx-n-times 2)))
  (report trivial-benchmark::*current-timer*))

(define-benchmark small-tx-4-worker ()
  (loop :repeat *count*
        :do
           (coalton:coalton
            (benchmark-stm/native::small-tx-n-times 4)))
  (report trivial-benchmark::*current-timer*))

(define-benchmark small-tx-8-worker ()
  (loop :repeat *count*
        :do
           (coalton:coalton
            (benchmark-stm/native::small-tx-n-times 8)))
  (report trivial-benchmark::*current-timer*))

(define-benchmark large-tx-1-worker ()
  (loop :repeat *count*
        :do
           (coalton:coalton
            (benchmark-stm/native::large-tx-n-times 1)))
  (report trivial-benchmark::*current-timer*))

(define-benchmark large-tx-2-worker ()
  (loop :repeat *count*
        :do
           (coalton:coalton
            (benchmark-stm/native::large-tx-n-times 2)))
  (report trivial-benchmark::*current-timer*))

(define-benchmark large-tx-4-worker ()
  (loop :repeat *count*
        :do
           (coalton:coalton
            (benchmark-stm/native::large-tx-n-times 4)))
  (report trivial-benchmark::*current-timer*))

(define-benchmark large-tx-8-worker ()
  (loop :repeat *count*
        :do
           (coalton:coalton
            (benchmark-stm/native::large-tx-n-times 8)))
  (report trivial-benchmark::*current-timer*))
