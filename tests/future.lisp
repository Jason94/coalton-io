(defpackage :coalton-io/tests/future
  (:use #:coalton #:coalton-prelude #:coalton-testing
        #:io/simple-io
        #:io/exceptions
        #:io/threads
        #:io/conc/future
        #:io/mutable-var))
(in-package :coalton-io/tests/future)

(named-readtables:in-readtable coalton:coalton)

(fiasco:define-test-package #:coalton-io/tests/future-fiasco)
(coalton-fiasco-init #:coalton-io/tests/future-fiasco)

(define-test test-fork-no-error ()
  (let result =
    (run-io!
     (do
      (fut <-
        (do-fork-future_
          (pure 1)))
      (x <- (await fut))
      (pure x))))
  (is (== 1 result)))

(define-test test-fork-error ()
  (let result =
    (run-io!
     (do
      (fut <-
        (do-fork-future_
          (raise "Error")
          (pure 1)))
      (x? <- (try (await fut)))
      (pure x?))))
  (is (== (Err "Error") result)))
