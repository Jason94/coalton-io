(in-package #:io/benchmarks)

;;; Simple IO Benchmarks
;;;
;;; These benchmarks test the efficiency of the core IO monad against non-monadic
;;; code doing the same thing. Benchmarks test both using the efficient-fused
;;; looping operations and naive looping/recursion.

(define-io-benchmark-package simple-io ()
  (:use
   #:coalton
   #:coalton-prelude
   #:io/utils
   #:io/monad-io
   #:io/simple-io)
  (:import-from #:coalton-library/experimental/loops
   #:dotimes)
  (:local-nicknames
   (:c #:coalton-library/cell)
   (:l #:coalton-library/list)
   (:itr #:coalton-library/iterator)))

(in-package #:benchmark-simple-io/native)

(cl:declaim (cl:optimize (cl:speed 3) (cl:safety 0)))

(coalton-toplevel
  (declare *n* UFix)
  (define *n* 10000000))

;;; The increment-hash-list benchmark takes the range of stringified ints from [0, n) and
;;; and loops through the list, hashing the increment of each list. The goal it to test
;;; a tight loop of very light, CPU-bound work. It does not allocate the results in a
;;; result list.
;;;
;;; Benchmark runners should be careful to not include the allocation of the initial
;;; range in the benchmark.

(coalton-toplevel

  (define x-cell-hash (c:new (hash 0)))

  (declare lst (List Integer))
  (define lst (l:range 0 (into *n*)))

  (declare increment-list-loop-non-monadic (Void -> Void))
  (define (increment-list-loop-non-monadic)
    (foreach (x lst)
      (c:write! x-cell-hash (hash (1+ x)))))

  (declare increment-list-loop-io-fused (Void -> Void))
  (define (increment-list-loop-io-fused)
    (run-io!
     (do-foreach-io_ (x lst)
       (wrap-io
        (c:write! x-cell-hash (hash (1+ x)))
        Unit)))
    (values))

  (declare increment-list-recursive-non-monadic (Void -> Void))
  (define (increment-list-recursive-non-monadic)
    (rec % ((rem lst))
      (match rem
        ((Cons x rest)
         (c:write! x-cell-hash (hash (1+ x)))
         (% rest))
        ((Nil)
         (values)))))

  (declare increment-list-recursive-monadic-non-fused (Void -> Void))
  (define (increment-list-recursive-monadic-non-fused)
    (run-io!
     (rec % ((rem lst))
       (match rem
         ((Cons x rest)
          (do
           (wrap-io
            (c:write! x-cell-hash (hash (1+ x))))
           (% rest)))
         ((Nil)
          (pure Unit)))))
    (values))
  )

(coalton-toplevel
  (define x-cell (c:new 0))

  (define (reset)
    (c:write! x-cell 0))

  (declare calculate-hash (Void -> Hash))
  (define (calculate-hash)
    (hash (c:increment! x-cell)))
  
  (declare hash-n-times-non-monadic (Void -> Void))
  (define (hash-n-times-non-monadic)
    (reset)
    (let cell = (c:new (hash 0)))
    (dotimes (_ *n*)
      (c:write! cell
                (calculate-hash))))

  (declare hash-n-times-non-monadic-lambda (Void -> Void))
  (define (hash-n-times-non-monadic-lambda)
    (reset)
    (let cell = (c:new (hash 0)))
    (let f = (fn () (calculate-hash)))
    (dotimes (_ *n*)
      (c:write! cell
                (noinline (f)))))                

  (declare hash-n-times-monadic-fused (Void -> Void))
  (define (hash-n-times-monadic-fused)
    (reset)
    (let cell = (c:new (hash 0)))
    (run-io!
     (do-times-io_ *n*
       (wrap-io
         (c:write! cell (calculate-hash))
         Unit)))
    (values))
                          
  )

(in-package #:benchmark-simple-io)

(cl:declaim (cl:optimize (cl:speed 3) (cl:safety 0)))

(defparameter *warmup* 10)
(defparameter *count* 50)

(io/benchmarks:define-io-benchmark increment-list-loop-non-monadic (:warmup-count *warmup* :sample-count *count*)
  (coalton:call-coalton-function
   benchmark-simple-io/native::increment-list-loop-non-monadic))

(io/benchmarks:define-io-benchmark increment-list-loop-io-fused (:warmup-count *warmup* :sample-count *count*)
  (coalton:call-coalton-function
   benchmark-simple-io/native::increment-list-loop-io-fused))

(io/benchmarks:define-io-benchmark increment-list-recursive-non-monadic (:warmup-count *warmup* :sample-count *count*)
  (coalton:call-coalton-function
   benchmark-simple-io/native::increment-list-recursive-non-monadic))

;; This benchmark is particularly slow, so run with 1/4 of the normal count
(io/benchmarks:define-io-benchmark increment-list-recursive-monadic-non-fused (:warmup-count *warmup* :sample-count *count*)
  (coalton:call-coalton-function
   benchmark-simple-io/native::increment-list-recursive-monadic-non-fused))

(io/benchmarks:define-io-benchmark hash-n-times-non-monadic (:warmup-count *warmup* :sample-count *count*)
  (coalton:call-coalton-function
   benchmark-simple-io/native::hash-n-times-non-monadic))

(io/benchmarks:define-io-benchmark hash-n-times-non-monadic-lambda (:warmup-count *warmup* :sample-count *count*)
  (coalton:call-coalton-function
   benchmark-simple-io/native::hash-n-times-non-monadic-lambda))

(io/benchmarks:define-io-benchmark hash-n-times-monadic-fused (:warmup-count *warmup* :sample-count *count*)
  (coalton:call-coalton-function
   benchmark-simple-io/native::hash-n-times-monadic-fused))
