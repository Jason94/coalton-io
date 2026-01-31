(defpackage coalton-io/tests/io
  (:use #:coalton #:coalton-prelude #:coalton-testing
        #:coalton-library/types
        #:io/utils
        #:io/simple-io
        #:io/monad-io)
  (:local-nicknames
   (:tm #:io/term)
   (:exc #:io/exceptions)
   (:mt #:io/mut)
   (:r #:coalton-library/result)
   (:opt #:coalton-library/optional)
   (:l #:coalton-library/list)
   (:c #:coalton-library/cell))
  )
(in-package :coalton-io/tests/io)

(named-readtables:in-readtable coalton:coalton)

(fiasco:define-test-package #:coalton-io/tests/io-fiasco)

(coalton-fiasco-init #:coalton-io/tests/io-fiasco)

(coalton-toplevel
  (declare io-const (:a -> IO :a))
  (define (io-const a)
    (wrap-io a)))

(define-test test-run-wrapped-io ()
  (is (== 5 (run! (io-const 5)))))

(define-test test-repeated-runs ()
  (is (== 1 (run! (io-const 1))))
  (is (== 2 (run! (io-const 2))))
  (is (== 3 (run! (io-const 3))))
  (is (== 4 (run! (io-const 4)))))

(define-test test-do-notation ()
  (let result =
    (run!
     (do
      (x <- (io-const 5))
      (y <- (io-const 10))
      (pure (+ x y)))))
  (is (== 15 result)))

(define-test test-map-into-io-empty ()
  (let result =
    (run-io!
     (do-map-into-io_ (x (make-list))
       (pure (+ x 10)))))
  (is (== Nil result)))

(define-test test-map-into-io ()
  (let result =
    (run-io!
     (do-map-into-io_ (x (make-list 0 10 20 30))
       (pure (+ x 10)))))
  (is (== (make-list 10 20 30 40) result)))

(define-test test-foreach-io-empty ()
  (let run-ints = (c:new Nil))
  (run-io!
   (do-foreach-io_ (x (the (List Integer) (make-list)))
     (wrap-io
       (c:push! run-ints x))))
  (is (== Nil (c:read run-ints))))

(define-test test-foreach-io ()
  (let run-ints = (c:new Nil))
  (let last-var =
    (run-io!
     (do
      (last-var <- (mt:new-var -1))
      (do-foreach-io_ (x (make-list 0 10 20 30))
        (mt:write last-var x)
        (wrap-io
         (c:push! run-ints x)))
      (mt:read last-var))))
  (is (== (make-list 0 10 20 30)
          (l:reverse (c:read run-ints))))
  (is (== 30 last-var)))

;;;
;;; Test Exceptions
;;;

(coalton-toplevel

  (derive Eq)
  (repr :lisp)
  (define-type TestException
    (TE String))

  (define-instance (Signalable TestException)
    (define (error (TE s))
      (error s)))

  (derive Eq)
  (repr :lisp)
  (define-type TestExceptionTwo
    (TE2 String))

  (define-instance (Signalable TestExceptionTwo)
    (define (error (TE2 s))
      (error s)))
  )

(define-test test-useless-catch ()
  (let result =
    (run-io!
     (handle-all-io
      (wrap-io 10)
      (const (wrap-io -10)))))
  (is (== 10 result)))

(define-test test-raise-handle-any ()
  (let result =
    (run-io!
     (handle-all-io
      (raise-io (TE "Error"))
      (const (pure "Caught an error!")))))
  (is (== "Caught an error!" result)))

(coalton-toplevel
  (declare long-op (IO String))
  (define long-op
    (do
     (x <- (wrap-io 10))
     (y <- (wrap-io 100))
     (raise-io (TE "Error"))
     (z <- (pure (+ x y)))
     (pure (into z)))))

(define-test test-raise-in-long-do-handle-any ()
  (let result =
    (run-io!
     (handle-all-io
      long-op
      (const (pure "Caught an error!")))))
  (is (== "Caught an error!" result)))

(coalton-toplevel
  (declare handle-te (TestException -> IO String))
  (define (handle-te (TE msg))
    (pure (<> "Caught: " msg)))

  (declare handle-te2 (TestExceptionTwo -> IO String))
  (define (handle-te2 (TE2 msg))
    (pure (<> "Caught TE2: " msg)))
  )

(define-test test-handle-actual-type ()
  (let result =
    (run-io!
     (handle-io
      (raise-io (TE "Error"))
      handle-te)))
  (is (== "Caught: Error" result)))

(define-test test-handle-different-type ()
  (let result =
    (run-io!
     (handle-io
      (handle-io
       (raise-io (TE "Error"))
       handle-te2)
      handle-te)))
  (is (== "Caught: Error" result)))

(define-test test-unhandled-errors ()
  (let res =
    (run-io!
     (exc:try-all
      (wrap-io
        (error "Test Error")
        1))))
  (is (opt:none? res)))
