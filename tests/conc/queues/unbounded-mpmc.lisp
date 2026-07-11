(defpackage :coalton-io/tests/conc/queues/unbounded-mpmc
  (:use #:coalton #:coalton-prelude #:coalton-testing
   #:coalton/optional
   #:coalton-library/types
   #:io/simple-io
   #:io/simple-io/loops
   #:io/thread
   #:io/exceptions
   #:io/conc/queues
   #:io/conc/queues/unbounded-mpmc
   ))
(in-package :coalton-io/tests/conc/queues/unbounded-mpmc)

(named-readtables:in-readtable coalton:coalton)

(fiasco:define-test-package #:coalton-io/tests/conc/unbounded-mpmc-queue-fiasco)
(coalton-fiasco-init #:coalton-io/tests/conc/unbounded-mpmc-queue-fiasco)

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
      (do-repeat-io 2;048
        (enqueue 0 buffer))
      (enqueue 10 buffer)
      (do-repeat-io 2;048
        (try-dequeue buffer))
      (try-dequeue buffer))))
  (is (== (Some 10) result)))

;; (test-enqueue-try-dequeue-many)
