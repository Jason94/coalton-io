(defpackage :coalton-io/tests/mut
  (:use #:coalton #:coalton-prelude #:coalton-testing
        #:io/simple-io
        #:io/monad-io
        #:io/mut))
(in-package :coalton-io/tests/mut)

(named-readtables:in-readtable coalton:coalton)

(fiasco:define-test-package #:coalton-io/tests/mut-fiasco)
(coalton-fiasco-init #:coalton-io/tests/mut-fiasco)

(define-test test-mut-new-read ()
  (is (== 10
          (run-io!
            (do
              (v <- (new-var 10))
              (read v))))))

(define-test test-mut-write-returns-old ()
  (is (== (Tuple 1 2)
          (run-io!
            (do
              (v   <- (new-var 1))
              (old <- (write v 2))
              (new <- (read v))
              (pure (Tuple old new)))))))

(define-test test-mut-modify-returns-new ()
  (is (== 8
          (run-io!
            (do
              (v   <- (new-var 5))
              (new <- (modify v (fn (x) (+ x 3))))
              (pure new))))))

(define-test test-mut-sequenced-write-modify ()
  (is (== (make-list 0 8 9)
          (run-io!
            (do
              (v     <- (new-var 0))
              (old1  <- (write v 4))
              (r2 <- (modify v (fn (x) (* x 2))))
              (r3 <- (modify v (fn (x) (+ x 1))))
              (pure (make-list old1 r2 r3)))))))
