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
   #:io/simple-io
   #:io/simple-io/loops
   )
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

  (declare iterate-list-iterative-loop (Void -> Void))
  (define (iterate-list-iterative-loop)
    (foreach (x lst)
      (c:write! x-cell-hash (hash (1+ x)))))

  (declare iterate-list-monadic-unfused-recursion (Void -> Void))
  (define (iterate-list-monadic-unfused-recursion)
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

  (declare iterate-list-monadic-fused-loop (Void -> Void))
  (define (iterate-list-monadic-fused-loop)
    (run-io!
     (do-foreach-io (x lst)
       (wrap-io
        (c:write! x-cell-hash (hash (1+ x)))
        Unit)))
    (values))
  )

(coalton-toplevel
  (define x-cell (c:new 0))

  (define (reset)
    (c:write! x-cell 0))

  (declare calculate-hash (Void -> Hash))
  (define (calculate-hash)
    (hash (c:increment! x-cell)))
  
  (declare repeat-n-times-iterative-loop (Void -> Void))
  (define (repeat-n-times-iterative-loop)
    (reset)
    (let cell = (c:new (hash 0)))
    (dotimes (_ *n*)
      (c:write! cell
                (calculate-hash))))

  (declare repeat-n-times-iterative-loop-lambda (Void -> Void))
  (define (repeat-n-times-iterative-loop-lambda)
    (reset)
    (let cell = (c:new (hash 0)))
    (let f = (fn () (calculate-hash)))
    (dotimes (_ *n*)
      (c:write! cell
                (noinline (f)))))                

  (declare repeat-n-times-monadic-fused-loop (Void -> Void))
  (define (repeat-n-times-monadic-fused-loop)
    (reset)
    (let cell = (c:new (hash 0)))
    (run-io!
     (do-repeat-io *n*
       (wrap-io
         (c:write! cell (calculate-hash))
         Unit)))
    (values))
                          
  )

(in-package #:benchmark-simple-io)

(cl:declaim (cl:optimize (cl:speed 3) (cl:safety 0)))

(defparameter *warmup* 10)
(defparameter *count* 50)

(io/benchmarks:define-io-benchmark iterate-list-iterative-loop (:warmup-count *warmup* :sample-count *count*)
  (coalton:call-coalton-function
   benchmark-simple-io/native::iterate-list-iterative-loop))

(io/benchmarks:define-io-benchmark iterate-list-monadic-unfused-recursion (:warmup-count *warmup* :sample-count *count*)
  (coalton:call-coalton-function
   benchmark-simple-io/native::iterate-list-monadic-unfused-recursion))

(io/benchmarks:define-io-benchmark iterate-list-monadic-fused-loop (:warmup-count *warmup* :sample-count *count*)
  (coalton:call-coalton-function
   benchmark-simple-io/native::iterate-list-monadic-fused-loop))

(io/benchmarks:define-io-benchmark repeat-n-times-iterative-loop (:warmup-count *warmup* :sample-count *count*)
  (coalton:call-coalton-function
   benchmark-simple-io/native::repeat-n-times-iterative-loop))

(io/benchmarks:define-io-benchmark repeat-n-times-iterative-loop-lambda (:warmup-count *warmup* :sample-count *count*)
  (coalton:call-coalton-function
   benchmark-simple-io/native::repeat-n-times-iterative-loop-lambda))

(io/benchmarks:define-io-benchmark repeat-n-times-monadic-fused-loop (:warmup-count *warmup* :sample-count *count*)
  (coalton:call-coalton-function
   benchmark-simple-io/native::repeat-n-times-monadic-fused-loop))
