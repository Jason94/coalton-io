(defpackage :io/stress/queues
  (:use #:coalton #:coalton-prelude #:coalton-testing
   #:io/simple-io
   #:io/stress/stress-tests
   #:io/conc/queues
   #:io/conc/queues/bounded-mpmc
   #:io/conc/queues/unbounded-mpmc
   )
  (:local-nicknames
   (:tm #:io/term)))
(in-package :io/stress/queues)

(named-readtables:in-readtable coalton:coalton)

(fiasco:define-test-package #:io/stress/queues-fiasco)
(coalton-fiasco-init #:io/stress/queues-fiasco)

(defmacro queue-linearized-producer-consumers-stress-test (constructor-form n-prod n-cons cl:&key (count 4000000))
  `(progn
     (let result =
       (run-io!
        (do
         (buffer <- ,constructor-form)
         (linearized-producer-consumers-stress-test
          ,count
          ,n-prod
          ,n-cons
          (fn (x)
            (try-enqueue x buffer))
          (try-dequeue buffer)
          (map none? (try-dequeue buffer))))))
     (is (== (Ok Unit)
             result))))

(define-test unbounded-mpmc-linearized-producer-consumers-stress-test-1-prod-1-cons ()
  (queue-linearized-producer-consumers-stress-test new-unbounded-mpmc-queue 1 1))

(define-test unbounded-mpmc-linearized-producer-consumers-stress-test-6-prod-1-cons ()
  (queue-linearized-producer-consumers-stress-test new-unbounded-mpmc-queue 6 1))

(define-test unbounded-mpmc-linearized-producer-consumers-stress-test-1-prod-6-cons ()
  (queue-linearized-producer-consumers-stress-test new-unbounded-mpmc-queue 1 6))

(define-test unbounded-mpmc-linearized-producer-consumers-stress-test-6-prod-6-cons ()
  (queue-linearized-producer-consumers-stress-test new-unbounded-mpmc-queue 6 6))

(coalton-toplevel
  (define +test-bounded-count+ (the UFix 1000000))
  (define +test-bounded-capacity+ (the UFix 2048)))

(define-test bounded-mpmc-linearized-producer-consumers-stress-test-1-prod-1-cons ()
  (queue-linearized-producer-consumers-stress-test
   (new-bounded-mpmc-queue +test-bounded-capacity+)
   1
   1
   :count +test-bounded-count+))

(define-test bounded-mpmc-linearized-producer-consumers-stress-test-6-prod-1-cons ()
  (queue-linearized-producer-consumers-stress-test
   (new-bounded-mpmc-queue +test-bounded-capacity+)
   6
   1
   :count +test-bounded-count+))

(define-test bounded-mpmc-linearized-producer-consumers-stress-test-1-prod-6-cons ()
  (queue-linearized-producer-consumers-stress-test
   (new-bounded-mpmc-queue +test-bounded-capacity+)
   1
   6
   :count +test-bounded-count+))

(define-test bounded-mpmc-linearized-producer-consumers-stress-test-6-prod-6-cons ()
  (queue-linearized-producer-consumers-stress-test
   (new-bounded-mpmc-queue +test-bounded-capacity+)
   6
   6
   :count +test-bounded-count+))
