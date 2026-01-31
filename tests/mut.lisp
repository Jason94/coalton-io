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

(define-test test-mut-modify-returns-old ()
  (is (== (Tuple 5 8)
          (run-io!
            (do
              (v   <- (new-var 5))
              (old <- (modify v (fn (x) (+ x 3))))
              (new <- (read v))
              (pure (Tuple old new)))))))

(define-test test-mut-sequenced-write-modify ()
  (is (== (make-list 0 4 4 8 8 9)
          (run-io!
            (do
              (v     <- (new-var 0))
              (oldW  <- (write v 4))
              (r1    <- (read v))
              (oldM1 <- (modify v (fn (x) (* x 2))))
              (r2    <- (read v))
              (oldM2 <- (modify v (fn (x) (+ x 1))))
              (r3    <- (read v))
              (pure (make-list oldW r1 oldM1 r2 oldM2 r3)))))))
