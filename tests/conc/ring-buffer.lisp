(defpackage :coalton-io/tests/conc/ring-buffer
  (:use #:coalton #:coalton-prelude #:coalton-testing
   #:coalton-library/types
   #:io/simple-io
   #:io/thread
   #:io/exceptions
   #:io/conc/ring-buffer
   )
  )
(in-package :coalton-io/tests/conc/ring-buffer)

(named-readtables:in-readtable coalton:coalton)

(fiasco:define-test-package #:coalton-io/tests/conc/ring-buffer-fiasco)
(coalton-fiasco-init #:coalton-io/tests/conc/ring-buffer-fiasco)

(define-test test-enqueue-dequeue-once ()
  (let result =
    (run-io!
     (do
      (buffer <- (new-ring-buffer 4))
      (enqueue 1 buffer)
      (dequeue buffer))))
  (is (== 1 result)))

(define-test test-enqueue-dequeue-to-capacity ()
  (let (Tuple a b) =
    (run-io!
     (do
      (buffer <- (new-ring-buffer 4))
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
      (buffer <- (new-ring-buffer 2))
      (enqueue 1 buffer)
      (enqueue 2 buffer)
      (try-all
       (enqueue 100 buffer :timeout (Timeout 1))))))
  (is (== None result)))


(define-test test-dequeue-timeout ()
  (let result =
    (run-io!
     (do
      (buffer <- (the (IO (RingBuffer Integer))
                      (new-ring-buffer 2)))
      (try-all
       (dequeue buffer :timeout (Timeout 1))))))
  (is (== None result)))
