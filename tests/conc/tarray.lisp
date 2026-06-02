(defpackage :coalton-io/tests/conc/stm/tarray
  (:use #:coalton #:coalton-prelude #:coalton-testing
   #:coalton-library/experimental/do-control-core
   #:io/utils
   #:io/simple-io
   #:io/conc/stm
   #:io/conc/stm/tarray
   ))
(in-package :coalton-io/tests/conc/stm/tarray)

(named-readtables:in-readtable coalton:coalton)

(fiasco:define-test-package #:coalton-io/tests/conc/stm/tarray-fiasco)
(coalton-fiasco-init #:coalton-io/tests/conc/stm/tarray-fiasco)

(define-test test-tarray-read-safe-inbounds ()
  (let result =
    (run-io!
     (do
      (tarr <- (new-tarray 3 0))
      (run-tx (aref tarr 0)))))
  (is (== (Some 0) result)))

(define-test test-tarray-read-safe-out-of-bounds ()
  (let result =
    (run-io!
     (do
      (tarr <- (new-tarray 3 0))
      (run-tx (aref tarr 100)))))
  (is (== None result)))

(define-test test-tarray-read-unsafe ()
  (let result =
    (run-io!
     (do
      (tarr <- (new-tarray 3 0))
      (run-tx (aref# tarr 0)))))
  (is (== 0 result)))

(define-test test-tarray-set ()
  (let result =
    (run-io!
     (do
      (tarr <- (new-tarray 3 0))
      (do-run-tx
        (set tarr 0 100)
        (aref# tarr 0)))))
  (is (== 100 result)))
