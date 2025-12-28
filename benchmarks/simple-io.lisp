(in-package #:io/benchmarks)

;;; Simple IO Benchmarks
;;;
;;; These benchmarks test the efficiency of the core IO monad against non-monadic
;;; code doing the same thing. Benchmarks test both using the efficient-fused
;;; looping operations and naive looping/recursion.

(define-io-benchmark simple-io ()
  (:use
   #:coalton
   #:coalton-prelude
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

;;; The increment-hash-list benchmark takes the range of stringified ints from [0, n) and
;;; and loops through the list, hashing the increment of each list. The goal it to test
;;; a tight loop of very light, CPU-bound work. It does not allocate the results in a
;;; result list.
;;;
;;; Benchmark runners should be careful to not include the allocation of the initial
;;; range in the benchmark.

(coalton-toplevel

  (declare lst (List Integer))
  (define lst (l:range 0 3000000))

  (declare increment-list-loop-non-monadic (Unit -> Unit))
  (define (increment-list-loop-non-monadic)
    (for x in lst
      (hash (1+ x))))

  ;; TODO: For some reason SBCL is able to specialize some arithmetic in the non-monadic
  ;; version, but not this one. That's going to give it some degree of artificial edge
  ;; in the benchmark.
  (declare increment-list-loop-io-fused (Unit -> Unit))
  (define (increment-list-loop-io-fused)
    (run-io!
     (do-foreach-io_ (x lst)
       (wrap-io (hash (1+ x))))))
  )

(coalton-toplevel
  (declare increment-list-recursive-non-monadic (Unit -> Unit))
  (define (increment-list-recursive-non-monadic)
    (rec % ((rem lst))
      (match rem
        ((Cons x rest)
         (hash (1+ x))
         (% rest))
        ((Nil)
         Unit))))

  (declare increment-list-recursive-monadic-non-fused (Unit -> Unit))
  (define (increment-list-recursive-monadic-non-fused)
    (run-io!
     (rec % ((rem lst))
       (match rem
         ((Cons x rest)
          (do
           (pure (hash (1+ x)))
           (% rest)))
         ((Nil)
          (pure Unit))))))
  )

(coalton-toplevel
  (declare *n* UFix)
  (define *n* 500000)

  (define x-cell (c:new 0))

  (declare calculate-hash (Unit -> Hash))
  (define (calculate-hash)
    (hash (c:increment! x-cell)))
  
  (declare hash-n-times-non-monadic (Unit -> Unit))
  (define (hash-n-times-non-monadic)
    (let cell = (c:new (hash 0)))
    (dotimes (_ *n*)
      (c:write! cell
                (calculate-hash))))

  (declare hash-n-times-non-monadic-lambda (Unit -> Unit))
  (define (hash-n-times-non-monadic-lambda)
    (let cell = (c:new (hash 0)))
    (let f = (fn () (calculate-hash)))
    (dotimes (_ *n*)
      (c:write! cell
                (noinline (f)))))                

  (declare hash-n-times-monadic (Unit -> Unit))
  (define (hash-n-times-monadic)
    (let cell = (c:new (hash 0)))
    (run-io!
     (do-times-io_ *n*
       (wrap-io
         (c:write! cell (calculate-hash))
         Unit))))
                          
  )

(in-package #:benchmark-simple-io)

(cl:declaim (cl:optimize (cl:speed 3) (cl:safety 0)))

(defparameter *count* 49)

(define-benchmark increment-list-loop-non-monadic ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do (with-benchmark-sampling
              (coalton:call-coalton-function
               benchmark-simple-io/native::increment-list-loop-non-monadic)))
  (report trivial-benchmark::*current-timer*))

(define-benchmark increment-list-loop-io-fused ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do (with-benchmark-sampling
              (coalton:call-coalton-function
               benchmark-simple-io/native::increment-list-loop-io-fused)))
  (report trivial-benchmark::*current-timer*))

(define-benchmark increment-list-recursive-non-monadic ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do (with-benchmark-sampling
              (coalton:call-coalton-function
               benchmark-simple-io/native::increment-list-recursive-non-monadic)))
  (report trivial-benchmark::*current-timer*))

(define-benchmark increment-list-recursive-monadic-non-fused ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do (with-benchmark-sampling
              (coalton:call-coalton-function
               benchmark-simple-io/native::increment-list-recursive-monadic-non-fused)))
  (report trivial-benchmark::*current-timer*))

(define-benchmark hash-n-times-non-monadic ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do (with-benchmark-sampling
              (coalton:call-coalton-function
               benchmark-simple-io/native::hash-n-times-non-monadic)))
  (report trivial-benchmark::*current-timer*))

(define-benchmark hash-n-times-non-monadic-lambda ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do (with-benchmark-sampling
              (coalton:call-coalton-function
               benchmark-simple-io/native::hash-n-times-non-monadic-lambda)))
  (report trivial-benchmark::*current-timer*))

(define-benchmark hash-n-times-monadic ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do (with-benchmark-sampling
              (coalton:call-coalton-function
               benchmark-simple-io/native::hash-n-times-monadic)))
  (report trivial-benchmark::*current-timer*))
