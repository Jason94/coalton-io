(defpackage :coalton-io/tests/conc/queues/unbounded-mpmc
  (:use #:coalton #:coalton-prelude #:coalton-testing
   #:coalton/optional
   #:coalton-library/types
   #:io/monad-io
   #:io/simple-io
   #:io/simple-io/loops
   #:io/thread
   #:io/mut
   #:io/exceptions
   #:io/conc/queues
   #:io/conc/queues/unbounded-mpmc
   #:io/tests/utils
   ))
(in-package :coalton-io/tests/conc/queues/unbounded-mpmc)

(named-readtables:in-readtable coalton:coalton)

(fiasco:define-test-package #:coalton-io/tests/conc/unbounded-mpmc-queue-fiasco)
(coalton-fiasco-init #:coalton-io/tests/conc/unbounded-mpmc-queue-fiasco)

;;;
;;; Test main queue functions
;;; 

(define-test test-try-dequeue-empty ()
  (let result =
    (the (Optional Unit)
         (run-io!
          (do
           (buffer <- new-unbounded-mpmc-queue)
           (try-dequeue buffer)))))
  (is (== None result)))

(define-test test-enqueue-try-dequeue-once ()
  (let result =
    (run-io!
     (do
      (buffer <- new-unbounded-mpmc-queue)
      (enqueue 10 buffer)
      (try-dequeue buffer))))
  (is (== (Some 10) result)))

(define-test test-enqueue-try-dequeue-many ()
  (let result =
    (run-io!
     (do
      (buffer <- new-unbounded-mpmc-queue)
      (do-repeat-io 2048
        (enqueue 0 buffer))
      (enqueue 10 buffer)
      (do-repeat-io 2048
        (try-dequeue buffer))
      (try-dequeue buffer))))
  (is (== (Some 10) result)))

(define-test test-enqueue-dequeue-once ()
  (let result =
    (run-io!
     (do
      (buffer <- new-unbounded-mpmc-queue)
      (enqueue 10 buffer)
      (dequeue buffer))))
  (is (== 10 result)))

(define-test test-enqueue-dequeue-many ()
  (let result =
    (run-io!
     (do
      (buffer <- new-unbounded-mpmc-queue)
      (do-repeat-io 2048
        (enqueue 0 buffer))
      (enqueue 10 buffer)
      (do-repeat-io 2048
        (dequeue buffer))
      (dequeue buffer))))
  (is (== 10 result)))

(define-test test-dequeue-block-then-enqueue ()
  (let result =
    (run-io!
     (do
      (buffer <- new-unbounded-mpmc-queue)
      (result <- (new-var None))
      (finished-gate <- s-new)
      (do-fork-thread_
        (val <- (dequeue buffer))
        (write result (Some val))
        (s-signal finished-gate))
      (sleep 2)
      (enqueue 10 buffer)
      (s-await finished-gate) 
      (read result))))
  (is (== (Some 10) result)))

(define-test test-dequeue-timeout-on-empty ()
  (let result =
    (run-io!
     (do
      (buffer <- new-unbounded-mpmc-queue)
      (try-all (dequeue buffer :timeout (Timeout 1))))))
  (is (== (the (Optional Boolean) None)
          result)))

;;;
;;; Edge case tests
;;; 

(define-test test-stores-nil-values ()
  (let result =
    (run-io!
     (do
      (buffer <- new-unbounded-mpmc-queue)
      (enqueue True buffer)
      (enqueue False buffer)
      (enqueue True buffer)
      (a <- (dequeue buffer))
      (b <- (dequeue buffer))
      (c <- (dequeue buffer))
      (pure [a b c]))))
  (is (== (make-list True False True)
          result)))
