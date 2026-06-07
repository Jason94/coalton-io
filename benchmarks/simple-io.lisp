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
  (:import-from #:io/io-impl/simple-io
   #:run-io-unhandled!)
  (:import-from #:coalton-library/experimental/loops
   #:dotimes)
  (:local-nicknames
   (:c #:coalton-library/cell)
   (:l #:coalton-library/list)
   (:itr #:coalton-library/iterator)
   (:mt #:io/mut)))

(in-package #:benchmark-simple-io/native)

(cl:declaim (cl:optimize (cl:speed 3) (cl:safety 0)))

(named-readtables:in-readtable coalton:coalton)

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

  (declare repeat-n-times-monadic-fused-loop (Void -> Void))
  (define (repeat-n-times-monadic-fused-loop)
    (reset)
    (let cell = (c:new (hash 0)))
    (run-io-unhandled!
     (do-repeat-io *n*
       (wrap-io
         (c:write! cell (calculate-hash))
         Unit)))
    (values))
  )


(coalton-toplevel

  ;;;
  ;;; Internal Cell Benchmarks
  ;;; 
  
  (declare index-n-times-iterative-loop (Void -> Hash))
  (define (index-n-times-iterative-loop)
    (let var = (c:new (hash 0)))
    (dotimes (i *n*)
      (c:write! var (hash i)))
    (c:read var))

  (declare index-n-times-monadic-fused-loop (Void -> Hash))
  (define (index-n-times-monadic-fused-loop)
    (let var = (c:new (hash 0)))
    (run-io-unhandled!
     (do
      (do-times-io (i *n*)
        (wrap-io
         (c:write! var (hash i))))
      (pure (c:read var)))))

  (declare index-n-times-full-monadic-fused-loop (Void -> Hash))
  (define (index-n-times-full-monadic-fused-loop)
    (run-io-unhandled!
     (do
      (var <- (mt:new-var (hash 0)))
      (do-times-io (i *n*)
        (mt:write var (hash i)))
      (mt:read var))))

  ;;;
  ;;; External Cell Benchmarks
  ;;; 
  
  (define index-cell (c:new (hash 0)))

  (define (reset-index)
    (c:write! index-cell (hash 0)))

  (declare index-n-times-iterative-loop-external (Void -> Hash))
  (define (index-n-times-iterative-loop-external)
    (reset-index)
    (dotimes (i *n*)
      (c:write! index-cell
                (hash i)))
    (c:read index-cell))

  (declare index-n-times-monadic-fused-loop-external (Void -> Hash))
  (define (index-n-times-monadic-fused-loop-external)
    (reset-index)
    (run-io-unhandled!
     (do
      (do-times-io (i *n*)
        (wrap-io
         (c:write! index-cell
                   (hash i))))
      (pure (c:read index-cell)))))      

  (define index-mut
    (run-io! (mt:new-var (hash 0))))

  (inline)
  (declare reset-mut (IO Hash))
  (define reset-mut
    (mt:write index-mut (hash 0)))

  (declare index-n-times-full-monadic-fused-loop-external (Void -> Hash))
  (define (index-n-times-full-monadic-fused-loop-external)
    (run-io-unhandled!
     (do
      reset-mut
      (do-times-io (i *n*)
        (mt:write index-mut (hash i)))
      (mt:read index-mut))))
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

(io/benchmarks:define-io-benchmark repeat-n-times-monadic-fused-loop (:warmup-count *warmup* :sample-count *count*)
  (coalton:call-coalton-function
   benchmark-simple-io/native::repeat-n-times-monadic-fused-loop))

(io/benchmarks:define-io-benchmark index-n-times-iterative-loop (:warmup-count *warmup* :sample-count *count*)
  (coalton:call-coalton-function
   benchmark-simple-io/native::index-n-times-iterative-loop))

(io/benchmarks:define-io-benchmark index-n-times-monadic-fused-loop (:warmup-count *warmup* :sample-count *count*)
  (coalton:call-coalton-function
   benchmark-simple-io/native::index-n-times-monadic-fused-loop))

(io/benchmarks:define-io-benchmark index-n-times-full-monadic-fused-loop (:warmup-count *warmup* :sample-count *count*)
  (coalton:call-coalton-function
   benchmark-simple-io/native::index-n-times-full-monadic-fused-loop))

(io/benchmarks:define-io-benchmark index-n-times-iterative-loop-external (:warmup-count *warmup* :sample-count *count*)
  (coalton:call-coalton-function
   benchmark-simple-io/native::index-n-times-iterative-loop-external))

(io/benchmarks:define-io-benchmark index-n-times-monadic-fused-loop-external (:warmup-count *warmup* :sample-count *count*)
  (coalton:call-coalton-function
   benchmark-simple-io/native::index-n-times-monadic-fused-loop-external))

(io/benchmarks:define-io-benchmark index-n-times-full-monadic-fused-loop-external (:warmup-count *warmup* :sample-count *count*)
  (coalton:call-coalton-function
   benchmark-simple-io/native::index-n-times-full-monadic-fused-loop-external))
