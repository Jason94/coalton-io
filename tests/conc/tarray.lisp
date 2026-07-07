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
      (run-tx (at tarr 0)))))
  (is (== (Some 0) result)))

(define-test test-tarray-read-safe-out-of-bounds ()
  (let result =
    (run-io!
     (do
      (tarr <- (new-tarray 3 0))
      (run-tx (at tarr 100)))))
  (is (== None result)))

(define-test test-tarray-read-unsafe ()
  (let result =
    (run-io!
     (do
      (tarr <- (new-tarray 3 0))
      (run-tx (at# tarr 0)))))
  (is (== 0 result)))

(define-test test-tarray-set ()
  (let result =
    (run-io!
     (do
      (tarr <- (new-tarray 3 0))
      (do-run-tx
        (set tarr 0 100)
        (at# tarr 0)))))
  (is (== 100 result)))

(define-test test-tarray-tvar-at ()
  (let result =
    (run-io!
     (do
      (tarr <- (new-tarray 1 0))
      (let tvar? = (tvar-at tarr 0))
      (do-run-tx
        (match tvar?
          ((Some tvar)
           (write-tvar tvar 100))
          ((None)
           (pure Unit)))
        (at# tarr 0)))))
  (is (== 100 result)))

(define-test test-tarray-tvar-at# ()
  (let result =
    (run-io!
     (do
      (tarr <- (new-tarray 1 0))
      (let tvar = (tvar-at# tarr 0))
      (do-run-tx
        (write-tvar tvar 100)
        (at# tarr 0)))))
  (is (== 100 result)))

(define-test test-tarray-new-tarray-apply ()
  (let (Tuple3 a b c) =
    (run-io!
     (do
      (tarr <- (new-tarray-apply 3 (fn (x)
                                      (pure x))))
      (do-run-tx
        (a <- (at# tarr 0))
        (b <- (at# tarr 1))
        (c <- (at# tarr 2))
        (pure (Tuple3 a b c))))))
  (is (== a 0))
  (is (== b 1))
  (is (== c 2)))

(define-test test-tarray-to-arr ()
  (let result =
    (run-io!
     (do
      (tarr <- (new-tarray-apply 3 (fn (x)
                                     (pure x))))
      (run-tx (to-arr tarr)))))
  (is (== [0 1 2] result)))
