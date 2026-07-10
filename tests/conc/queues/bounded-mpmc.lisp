(defpackage :coalton-io/tests/conc/queues/bounded-mpmc
  (:use #:coalton #:coalton-prelude #:coalton-testing
   #:coalton-library/types
   #:io/simple-io
   #:io/thread
   #:io/exceptions
   #:io/conc/queues
   #:io/conc/queues/bounded-mpmc
   )
  )
(in-package :coalton-io/tests/conc/queues/bounded-mpmc)

(named-readtables:in-readtable coalton:coalton)

(fiasco:define-test-package #:coalton-io/tests/conc/bounded-mpmc-queue-fiasco)
(coalton-fiasco-init #:coalton-io/tests/conc/bounded-mpmc-queue-fiasco)

(define-test test-enqueue-dequeue-once ()
  (let result =
    (run-io!
     (do
      (buffer <- (new-bounded-mpmc-queue 4))
      (enqueue 1 buffer)
      (dequeue buffer))))
  (is (== 1 result)))

(define-test test-enqueue-dequeue-to-capacity ()
  (let (Tuple a b) =
    (run-io!
     (do
      (buffer <- (new-bounded-mpmc-queue 4))
      (enqueue 1 buffer)
      (enqueue 2 buffer)
      (enqueue 3 buffer)
      (enqueue 4 buffer)
      (a <- (dequeue buffer))
      (dequeue buffer)
      (dequeue buffer)
      (b <- (dequeue buffer))
      (pure (Tuple a b)))))
  (is (== 1 a))
  (is (== 4 b)))

(define-test test-enqueue-timeout ()
  (let result =
    (run-io!
     (do
      (buffer <- (new-bounded-mpmc-queue 2))
      (enqueue 1 buffer)
      (enqueue 2 buffer)
      (try-all
       (enqueue 100 buffer :timeout (Timeout 1))))))
  (is (== None result)))

(define-test test-enqueue-timeout-leaves-in-valid-state ()
  (let result =
    (run-io!
     (do
      (buffer <- (new-bounded-mpmc-queue 1))
      (enqueue 1 buffer)
      (try-all
       (enqueue 2 buffer :timeout (Timeout 1)))
      (try-all (dequeue buffer :timeout (Timeout 10))))))
  (is (== (Some 1) result)))

(define-test test-dequeue-timeout ()
  (let result =
    (run-io!
     (do
      (buffer <- (the (IO (BoundedMpmcQueue Integer))
                      (new-bounded-mpmc-queue 2)))
      (try-all
       (dequeue buffer :timeout (Timeout 1))))))
  (is (== None result)))

(define-test test-dequeue-timeout-leaves-in-valid-state ()
  (let result =
    (run-io!
     (do
      (buffer <- (new-bounded-mpmc-queue 1))
      (try-all
       (dequeue buffer :timeout (Timeout 1)))
      (enqueue-result <-
       (try-all (enqueue 1 buffer :timeout (Timeout 10))))
      (dequeue-result <-
       (try-all (dequeue buffer :timeout (Timeout 10))))
      (pure (Tuple enqueue-result dequeue-result)))))
  (is (== (Tuple (Some Unit) (Some 1)) result)))

