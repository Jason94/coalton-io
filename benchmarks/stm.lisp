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
  (:import-from #:coalton/experimental/do-control-loops
   #:do-loop-while
   #:do-loop-times)
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
  ;; Divisible by 2, 4, & 6 (twice) to be able to evenly split workloads at all thread counts
  (define workload (the UFix 576000))
  
  (declare small-tx-n-times (UFix -> Void))
  (define (small-tx-n-times n-threads)
    "This benchmark tests multiple threads writing to the same tvar. Note that
this is the pathological worst case for an STM (and really, for any synchronized
data structure, such as lock or atomic based ones)."
    (run-io!
     (do
      ;; Setup benchmark parameters and shared data
      (tvar <- (new-tvar 0))
      (let work-per-thread = (coalton/math:div workload n-threads))
      (scheduler <- (new-ring-buffer-scheduler n-threads))
      (pool <- (new-worker-pool n-threads scheduler))
      ;; Run the benchmark
      (wrap-io (b:start (b:current-timer)))
      (do-times-io_ n-threads
        (do-submit-job_ pool
          (do-times-io_ work-per-thread
            (do-run-tx
              (modify-tvar tvar 1+)))))
      ;; Cleanup and verify correctness
      (request-shutdown pool)
      (await pool)
      (wrap-io
       (b:stop (b:current-timer))
       (b:commit (b:current-timer)))
      (val <- (run-tx (read-tvar tvar)))
      (do-when (/= val workload)
        (raise (f:format f:Str "small-tx-~a-times expected total ~a but received ~a" n-threads workload val)))))
    (values))

  (declare large-tx-n-times (UFix -> Void))
  (define (large-tx-n-times n-threads)
    "This benchmark tests multiple threads all writing to the same multiple tvars.
Note that this is the pathological worst case for an STM (and really, for any
synchronized data structure, such as lock or atomic based ones)."
    (run-io!
     (do
      ;; Setup benchmark parameters and shared data
      (tvar1 <- (new-tvar 0))
      (tvar2 <- (new-tvar 0))
      (tvar3 <- (new-tvar 0))
      (tvar4 <- (new-tvar 0))
      (tvar5 <- (new-tvar 0))
      (tvar6 <- (new-tvar 0))
      (tvar7 <- (new-tvar 0))
      (tvar8 <- (new-tvar 0))
      (let work-per-thread = (coalton/math:div workload n-threads))
      (let iterations-per-thread = (coalton/math:div work-per-thread 8))
      (scheduler <- (new-ring-buffer-scheduler n-threads))
      (pool <- (new-worker-pool n-threads scheduler))
      ;; Run the benchmark
      (wrap-io (b:start (b:current-timer)))
      (do-times-io_ n-threads
        (do-submit-job_ pool
          (do-times-io_ iterations-per-thread
            (do-run-tx
              (modify-tvar tvar1 1+)
              (modify-tvar tvar2 1+)
              (modify-tvar tvar3 1+)
              (modify-tvar tvar4 1+)
              (modify-tvar tvar5 1+)
              (modify-tvar tvar6 1+)
              (modify-tvar tvar7 1+)
              (modify-tvar tvar8 1+)))))
      ;; Cleanup and verify correctness
      (request-shutdown pool)
      (await pool)
      (wrap-io
       (b:stop (b:current-timer))
       (b:commit (b:current-timer)))
      (raise-result
       (do-run-tx
         (val1 <- (read-tvar tvar1))
         (val2 <- (read-tvar tvar2))
         (val3 <- (read-tvar tvar3))
         (val4 <- (read-tvar tvar4))
         (val5 <- (read-tvar tvar5))
         (val6 <- (read-tvar tvar6))
         (val7 <- (read-tvar tvar7))
         (val8 <- (read-tvar tvar8))
         (let total = (sum (make-list val1 val2 val3 val4 val5 val6 val7 val8)))
         (pure
          (if (/= total workload)
              (Err (f:format f:Str "large-tx-~a-times expected total ~a but received ~a" n-threads workload total))
              (Ok Unit)))))))
    (values))

  (declare loop-tarr-n-times (UFix * UFix -> Void))
  (define (loop-tarr-n-times n-threads tarr-size)
    "This benchmark tests committing to each cell in a tarray in a row, in separate
transactions. This is a much better use case for the STM, because threads can commit to
separate locations and avoid conflicting.

Two benchmarks use this function. One uses a tarr-size equal to 6 and one uses tarr-size
equal to 60. The latter should be even better for the STM because there should be less
contention."
    (run-io!
     (do
      ;; Setup benchmark parameters and shared data
      (tarr <- (tr:new-tarray tarr-size 0))
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
                (tr:set tarr i (1+ x)))))))
      ;; Cleanup and verify correctness
      (request-shutdown pool)
      (await pool)
      (wrap-io
       (b:stop (b:current-timer))
       (b:commit (b:current-timer)))
      (sum <- (new-tvar 0))
      (do-foreach-io_ (i ixs)
        (do-run-tx
          (x <- (tr:aref# tarr i))
          (modify-tvar sum (fn (y) (+ x y)))))
      (sum <- (run-tx (read-tvar sum)))
      (do-when (/= sum workload)
        (raise (f:format f:Str "loop-tarr-~a-times expected total ~a but received ~a" n-threads workload sum)))))
    (values))

  (declare write-separate (UFix -> Void))
  (define (write-separate n-threads)
    "This benchmark tests committing to a unique tvar for each thread. This is the best
case scenario for the STM."
    (run-io!
     (do
      ;; Setup benchmark parameters and shared data
      (tarr <- (tr:new-tarray n-threads 0))
      (let work-per-thread = (coalton/math:div workload n-threads))
      (scheduler <- (new-ring-buffer-scheduler n-threads))
      (pool <- (new-worker-pool n-threads scheduler))
      ;; Run the benchmark
      (wrap-io (b:start (b:current-timer)))
      (do-loop-times (i n-threads)
        (do-submit-job_ pool
          (do-times-io_ work-per-thread
            (do-run-tx
              (x <- (tr:aref# tarr i))
              (tr:set tarr i (1+ x))))))
      ;; Cleanup and verify correctness
      (request-shutdown pool)
      (await pool)
      (wrap-io
       (b:stop (b:current-timer))
       (b:commit (b:current-timer)))
      (sum <- (new-tvar 0))
      (do-loop-times (i n-threads)
        (do-run-tx
          (x <- (tr:aref# tarr i))
          (modify-tvar sum (fn (y) (+ x y)))))
      (sum <- (run-tx (read-tvar sum)))
      (do-when (/= sum workload)
        (raise (f:format f:Str "loop-tarr-~a-times expected total ~a but received ~a" n-threads workload sum)))))
    (values))

  (declare increment-round-robin (UFix -> Void))
  (define (increment-round-robin n-threads)
    "This benchmark tests `retry` performance. Higher thread counts lead to more spurious
wakes."
    (run-io!
     (do
      ;; Setup benchmark parameters and shared data
      (count <- (new-tvar 0))
      (scheduler <- (new-ring-buffer-scheduler n-threads))
      (pool <- (new-worker-pool n-threads scheduler))
      ;; Run the benchmark
      (wrap-io (b:start (b:current-timer)))
      (do-loop-times (i n-threads)
        (do-submit-job_ pool
          (do-loop-while
            (val <-
              (do-run-tx
                (val <- (read-tvar count))
                (cond
                  ((>= val workload)
                   (pure val))
                  ((== i (mod val n-threads))
                   (modify-tvar count 1+))
                  (True
                   retry))))
            (pure (< val workload)))))
      ;; Cleanup and verify correctness
      (request-shutdown pool)
      (await pool)
      (wrap-io
       (b:stop (b:current-timer))
       (b:commit (b:current-timer)))
      (count <- (run-tx (read-tvar count)))
      (do-when (/= count workload)
        (raise (f:format f:Str "increment-round-robin with ~a threads expected total ~a but received ~a"
                         n-threads
                         workload
                         count)))))
    (values))
 )

(in-package #:benchmark-stm)

(cl:declaim (cl:optimize (cl:speed 3) (cl:safety 0)))

(defparameter *count* 55)

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

(define-benchmark small-tx-6-worker ()
  (loop :repeat *count*
        :do
           (coalton:coalton
            (benchmark-stm/native::small-tx-n-times 6)))
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

(define-benchmark large-tx-6-worker ()
  (loop :repeat *count*
        :do
           (coalton:coalton
            (benchmark-stm/native::large-tx-n-times 6)))
  (report trivial-benchmark::*current-timer*))

(define-benchmark loop-tarr-1-worker ()
  (loop :repeat *count*
        :do
           (coalton:coalton
            (benchmark-stm/native::loop-tarr-n-times 1 6)))
  (report trivial-benchmark::*current-timer*))

(define-benchmark loop-tarr-2-worker ()
  (loop :repeat *count*
        :do
           (coalton:coalton
            (benchmark-stm/native::loop-tarr-n-times 2 6)))
  (report trivial-benchmark::*current-timer*))

(define-benchmark loop-tarr-4-worker ()
  (loop :repeat *count*
        :do
           (coalton:coalton
            (benchmark-stm/native::loop-tarr-n-times 4 6)))
  (report trivial-benchmark::*current-timer*))

(define-benchmark loop-tarr-6-worker ()
  (loop :repeat *count*
        :do
           (coalton:coalton
            (benchmark-stm/native::loop-tarr-n-times 6 6)))
  (report trivial-benchmark::*current-timer*))

(define-benchmark loop-large-tarr-1-worker ()
  (loop :repeat *count*
        :do
           (coalton:coalton
            (benchmark-stm/native::loop-tarr-n-times 1 60)))
  (report trivial-benchmark::*current-timer*))

(define-benchmark loop-large-tarr-2-worker ()
  (loop :repeat *count*
        :do
           (coalton:coalton
            (benchmark-stm/native::loop-tarr-n-times 2 60)))
  (report trivial-benchmark::*current-timer*))

(define-benchmark loop-large-tarr-4-worker ()
  (loop :repeat *count*
        :do
           (coalton:coalton
            (benchmark-stm/native::loop-tarr-n-times 4 60)))
  (report trivial-benchmark::*current-timer*))

(define-benchmark loop-large-tarr-6-worker ()
  (loop :repeat *count*
        :do
           (coalton:coalton
            (benchmark-stm/native::loop-tarr-n-times 6 60)))
  (report trivial-benchmark::*current-timer*))

(define-benchmark write-separate-1-workers ()
  (loop :repeat *count*
        :do
           (coalton:coalton
            (benchmark-stm/native::write-separate 1)))
  (report trivial-benchmark::*current-timer*))

(define-benchmark write-separate-2-workers ()
  (loop :repeat *count*
        :do
           (coalton:coalton
            (benchmark-stm/native::write-separate 2)))
  (report trivial-benchmark::*current-timer*))

(define-benchmark write-separate-4-workers ()
  (loop :repeat *count*
        :do
           (coalton:coalton
            (benchmark-stm/native::write-separate 4)))
  (report trivial-benchmark::*current-timer*))

(define-benchmark write-separate-6-workers ()
  (loop :repeat *count*
        :do
           (coalton:coalton
            (benchmark-stm/native::write-separate 6)))
  (report trivial-benchmark::*current-timer*))

;; NOTE: Not benchmarking increment-round-robin with 1 worker
;; because it will never retry, so it would just benchmark
;; that many modify-tvars in a loop.

(define-benchmark increment-round-robin-2-workers ()
  (loop :repeat *count*
        :do
           (coalton:coalton
            (benchmark-stm/native::increment-round-robin 2)))
  (report trivial-benchmark::*current-timer*))

(define-benchmark increment-round-robin-4-workers ()
  (loop :repeat *count*
        :do
           (coalton:coalton
            (benchmark-stm/native::increment-round-robin 4)))
  (report trivial-benchmark::*current-timer*))

(define-benchmark increment-round-robin-6-workers ()
  (loop :repeat *count*
        :do
           (coalton:coalton
            (benchmark-stm/native::increment-round-robin 6)))
  (report trivial-benchmark::*current-timer*))

