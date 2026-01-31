(defpackage :coalton-io/tests/atomic
  (:use #:coalton #:coalton-prelude #:coalton-testing
        #:io/simple-io
        #:io/conc/atomic
        #:io/exceptions
        #:io/thread)
  (:local-nicknames
   (:l #:coalton-library/list))
  )
(in-package :coalton-io/tests/atomic)

(named-readtables:in-readtable coalton:coalton)

(fiasco:define-test-package #:coalton-io/tests/io-atomic-fiasco)
(coalton-fiasco-init #:coalton-io/tests/io-atomic-fiasco)

(define-test test-modify-exception ()
  (let (Tuple modify-result x) =
    (run-io!
     (do
      (avar <- (new-at-var 0))
      (modify-result <-
       (try-all
        (modify avar
                (fn (_)
                  (error "Test Error")
                  -100))))
      (x <- (read avar))
      (pure (Tuple modify-result x)))))
  (is (== None modify-result))
  (is (== 0 x)))

(define-test test-modify-swap-exception ()
  (let (Tuple modify-result x) =
    (run-io!
     (do
      (avar <- (new-at-var 0))
      (modify-result <-
       (try-all
        (modify-swap avar
                     (fn (_)
                       (error "Test Error")
                       -100))))
      (x <- (read avar))
      (pure (Tuple modify-result x)))))
  (is (== None modify-result))
  (is (== 0 x)))
