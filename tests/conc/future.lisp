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

(define-test test-satisfy-future ()
  (let result =
    (do-run-io!
      ((Tuple f p) <- new-future)
      (satisfy-future p 10)
      (await-future f)))
  (is (== 10 result)))

(define-test test-await-future-multiple ()
  (let result =
    (do-run-io!
      ((Tuple f p) <- new-future)
      (satisfy-future p 10)
      (await-future f)
      (await-future f)))
  (is (== 10 result)))

(define-test test-fail-future ()
  (let result =
    (do-run-io!
      ((Tuple f p) <- new-future)
      (fail-future p (TE "Failed Future"))
      (try
       (await-future f))))
  (let _ = (the (Result :e Unit) result))
  (is (== (Err (TE "Failed Future")) result)))

(define-test test-satisfy-true-on-empty-future ()
  (let result =
    (do-run-io!
      ((Tuple _ p) <- new-future)
      (satisfy-future p 10)))
  (is (== True result)))

(define-test test-satisfy-false-on-satisfied-future ()
  (let result =
    (do-run-io!
      ((Tuple _ p) <- new-future)
      (satisfy-future p 10)
      (satisfy-future p 10)))
  (is (== False result)))

(define-test test-satisfy-on-satisfied-future-doesnt-overwrite ()
  (let result =
    (do-run-io!
      ((Tuple f p) <- new-future)
      (satisfy-future p 10)
      (satisfy-future p 20)
      (await-future f)))
  (is (== 10 result)))

(define-test test-satisfy-false-on-failed-future ()
  (let result =
    (do-run-io!
      ((Tuple _ p) <- new-future)
      (fail-future p (TE "Failed Future"))
      (satisfy-future p 10)))
  (is (== False result)))

(define-test test-fail-true-on-empty-future ()
  (let result =
    (do-run-io!
      ((Tuple _ p) <- new-future)
      (fail-future p (TE "Failed Future"))))
  (is (== True result)))

(define-test test-fail-false-on-satisfied-future ()
  (let result =
    (do-run-io!
      ((Tuple _ p) <- new-future)
      (satisfy-future p 10)
      (fail-future p (TE "Failed Future"))))
  (is (== False result)))

(define-test test-fail-false-on-failed-future ()
  (let result =
    (do-run-io!
      ((Tuple _ p) <- new-future)
      (fail-future p (TE "Failed Future"))
      (fail-future p (TE "Failed Future"))))
  (is (== False result)))

(define-test test-fail-failed-future-doesnt-overwrite ()
  (let result =
    (do-run-io!
      ((Tuple f p) <- new-future)
      (fail-future p (TE "Failed Future"))
      (fail-future p (TE "Failed Future2"))
      (try
       (await-future f))))
  (is (== (the (Result TestException (Optional Unit)) result)
          (Err (TE "Failed Future")))))

(define-test test-await-future-timeout ()
  (let result =
    (do-run-io!
      ((Tuple f _) <- new-future)
      (try-all
       (await-future f :timeout (Timeout 1)))))
  (is (== (the (Optional String) result)
          None)))  

(define-test test-try-read-empty-future ()
  (let result =
    (do-run-io!
      ((Tuple f _) <- new-future)
      (try-read-future f)))
  (let _ = (the (Optional String) result))
  (is (== None result)))

(define-test test-try-read-satisfied-future ()
  (let result =
    (do-run-io!
      ((Tuple f p) <- new-future)
      (satisfy-future p 10)
      (try-read-future f)))
  (is (== (Some 10) result)))

(define-test test-try-read-failed-future ()
  (let result =
    (do-run-io!
      ((Tuple f p) <- new-future)
      (fail-future p (TE "Failed Future"))
      (try
       (try-read-future f))))
  (is (== (the (Result TestException (Optional Unit))
               (Err (TE "Failed Future")))
          result)))

(define-test test-fork-future ()
  (let result =
    (do-run-io!
      ((Tuple f _) <-
        (do-fork-future_ (p)
          (satisfy-future p 10)))
      (await-future f)))
  (is (== 10 result)))
