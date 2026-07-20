(defpackage :io/stress/queues
  (:use #:coalton #:coalton-prelude #:coalton-testing
   #:io/simple-io
   #:io/stress/stress-tests
   #:io/conc/queues
   #:io/conc/queues/unbounded-mpmc
   )
  (:local-nicknames
   (:tm #:io/term)))
(in-package :io/stress/queues)

(named-readtables:in-readtable coalton:coalton)

(fiasco:define-test-package #:io/stress/queues-fiasco)
(coalton-fiasco-init #:io/stress/queues-fiasco)

(defmacro unbounded-mpmc-linearized-producer-consumers-stress-test (n-prod n-cons cl:&key (count 600000))
  `(progn
     (let result =
       (run-io!
        (do
         (buffer <- new-unbounded-mpmc-queue)
         (linearized-producer-consumers-stress-test
          ,count
          ,n-prod
          ,n-cons
          (fn (x)
            (do
             (enqueue x buffer)
             (pure True)))
          (try-dequeue buffer)
          (map none? (try-dequeue buffer))))))
     (is (== (Ok Unit)
             result))))

(define-test unbounded-mpmc-linearized-producer-consumers-stress-test-1-prod-1-cons ()
  (unbounded-mpmc-linearized-producer-consumers-stress-test 1 1))

(define-test unbounded-mpmc-linearized-producer-consumers-stress-test-6-prod-1-cons ()
  (unbounded-mpmc-linearized-producer-consumers-stress-test 6 1))

(define-test unbounded-mpmc-linearized-producer-consumers-stress-test-1-prod-6-cons ()
  (unbounded-mpmc-linearized-producer-consumers-stress-test 1 6))

(define-test unbounded-mpmc-linearized-producer-consumers-stress-test-6-prod-6-cons ()
  (unbounded-mpmc-linearized-producer-consumers-stress-test 6 6))
