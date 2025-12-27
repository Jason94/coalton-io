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
  (:local-nicknames
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
  (define lst (l:range 0 999999))

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

(in-package #:benchmark-simple-io)

(cl:declaim (cl:optimize (cl:speed 3) (cl:safety 0)))

(defvar *count* 10)

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
