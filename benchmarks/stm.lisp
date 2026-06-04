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
   #:io/exceptions
   #:io/simple-io
   #:io/thread
   #:io/conc/ring-buffer
   #:io/conc/worker-pool
   #:io/conc/stm)
  (:import-from #:coalton/experimental/do-control-core
   #:do-when)
  (:local-nicknames
   (:f #:coalton/format)
   (:l #:coalton/list)
   (:b #:benchmark-utils)
   (:tm #:io/term)
   (:tr #:io/conc/stm/tarray)
   ))

(in-package #:benchmark-stm/native)

(cl:declaim (cl:optimize (cl:speed 3) (cl:safety 0)))

(coalton-toplevel
  ;; Divisible by 8 to be able to evenly split workloads at all thread counts
  (define workload (the UFix 80000))
  
  (declare small-tx-n-times (UFix -> Void))
  (define (small-tx-n-times n-threads)
    "This benchmark tests multiple threads writing to the same tvar. Note that
this is the pathological worst case for an STM (and really, for any synchronized
data structure, such as lock or atomic based ones)."
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
    "This benchmark tests multiple threads all writing to the same multiple tvars.
Note that this is the pathological worst case for an STM (and really, for any
synchronized data structure, such as lock or atomic based ones)."
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

  (declare loop-tarr-n-times (UFix * UFix -> Void))
  (define (loop-tarr-n-times n-threads tarr-size)
    "This benchmark tests committing to each cell in a tarray in a row, in separate
transactions. This is a much better use case for the STM, because threads can commit to
separate locations and avoid conflicting.

Two benchmarks use this function. One uses a tarr-size equal to 8 and one uses tarr-size
equal to 80. The latter should be even better for the STM because there should be less
contention."
    (run-io!
     (do
      ;; Setup benchmark parameters and shared data
      (tarr <- (tr:new-tarray tarr-size 0))
      (n-finished <- (new-tvar 0))
      (let work-per-thread = (coalton/math:div workload n-threads))
      (let iterations-per-thread = (coalton/math:div work-per-thread tarr-size))
      (let ixs = (l:range 0 (1- tarr-size)))
      (scheduler <- (new-ring-buffer-scheduler n-threads))
      (pool <- (new-worker-pool n-threads scheduler))
      ;; Run the benchmark
      (wrap-io (b:start (b:current-timer)))
      (do-times-io_ n-threads
        (do-submit-job_ pool
          (do-times-io_ iterations-per-thread
            (do-foreach-io_ (i ixs)
              (do-run-tx
                (x <- (tr:aref# tarr i))
                (tr:set tarr i (1+ x)))))
          (run-tx (modify-tvar n-finished 1+))))
      (do-run-tx
        (n-finished <- (read-tvar n-finished))
        (retry-unless (== n-threads n-finished)))
      ;; Cleanup and verify correctness
      (wrap-io
       (b:stop (b:current-timer))
       (b:commit (b:current-timer)))
      (request-shutdown pool)
      (await pool)
      (sum <- (new-tvar 0))
      (do-foreach-io_ (i ixs)
        (do-run-tx
          (x <- (tr:aref# tarr i))
          (modify-tvar sum (fn (y) (+ x y)))))
      (sum <- (run-tx (read-tvar sum)))
      (do-when (/= sum workload)
        (raise (f:format f:Str "loop-tarr-~a-times expected total ~a but received ~a" n-threads workload sum)))))
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

(define-benchmark loop-tarr-1-worker ()
  (loop :repeat *count*
        :do
           (coalton:coalton
            (benchmark-stm/native::loop-tarr-n-times 1 8)))
  (report trivial-benchmark::*current-timer*))

(define-benchmark loop-tarr-2-worker ()
  (loop :repeat *count*
        :do
           (coalton:coalton
            (benchmark-stm/native::loop-tarr-n-times 2 8)))
  (report trivial-benchmark::*current-timer*))

(define-benchmark loop-tarr-4-worker ()
  (loop :repeat *count*
        :do
           (coalton:coalton
            (benchmark-stm/native::loop-tarr-n-times 4 8)))
  (report trivial-benchmark::*current-timer*))

(define-benchmark loop-tarr-8-worker ()
  (loop :repeat *count*
        :do
           (coalton:coalton
            (benchmark-stm/native::loop-tarr-n-times 8 8)))
  (report trivial-benchmark::*current-timer*))

(define-benchmark loop-large-tarr-1-worker ()
  (loop :repeat *count*
        :do
           (coalton:coalton
            (benchmark-stm/native::loop-tarr-n-times 1 80)))
  (report trivial-benchmark::*current-timer*))

(define-benchmark loop-large-tarr-2-worker ()
  (loop :repeat *count*
        :do
           (coalton:coalton
            (benchmark-stm/native::loop-tarr-n-times 2 80)))
  (report trivial-benchmark::*current-timer*))

(define-benchmark loop-large-tarr-4-worker ()
  (loop :repeat *count*
        :do
           (coalton:coalton
            (benchmark-stm/native::loop-tarr-n-times 4 80)))
  (report trivial-benchmark::*current-timer*))

(define-benchmark loop-large-tarr-8-worker ()
  (loop :repeat *count*
        :do
           (coalton:coalton
            (benchmark-stm/native::loop-tarr-n-times 8 80)))
  (report trivial-benchmark::*current-timer*))
