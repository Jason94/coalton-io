(defpackage :coalton-io/tests/future
  (:use #:coalton #:coalton-prelude #:coalton-testing
   #:io/simple-io
   #:io/exceptions
   #:io/thread
   #:io/conc/future
   #:io/mut
   #:io/conc/mvar
   #:io/tests/utils)
  (:local-nicknames
   (:tm #:io/term)))
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

(define-test test-await-stopped-future-raises ()
  (let result =
    (run-io!
     (do
      (started-gate <- s-new)
      (fut <-
       (do-fork-future_
         (s-signal started-gate)
         (sleep 10000)))
      (s-await started-gate)
      (stop fut)
      (try-all (await fut)))))
  (is (none? result)))
