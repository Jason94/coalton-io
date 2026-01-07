(defpackage :coalton-io/tests/conc/parking
  (:use #:coalton #:coalton-prelude #:coalton-testing
   #:coalton-library/types
   #:io/utils
   #:io/monad-io
   #:io/simple-io
   #:io/mut
   #:io/thread
   #:io/conc/parking
   #:io/tests/utils
   )
  (:local-nicknames
   (:tm #:io/term)
   )
  )
(in-package :coalton-io/tests/conc/parking)

(named-readtables:in-readtable coalton:coalton)

(fiasco:define-test-package #:coalton-io/tests/conc/parking-fiasco)
(coalton-fiasco-init #:coalton-io/tests/conc/parking-fiasco)

(define-test test-park-single-queue-then-wake ()
  (let result =
    (run-io!
     (do
      (p-queue <- new-parking-queue)
      (started-gate <- s-new)
      (finished-gate <- s-new)
      (finished? <- (new-var False))
      (thread <-
       (do-fork-thread_
         (s-signal started-gate)
         (park-in-queue-if_ (pure True) p-queue)
         (write finished? True)
         (s-signal finished-gate)))
      ;; Wait for the thread to start and sleep 2ms to allow it to park
      (s-await started-gate)
      (sleep 2)
      ;; Unpark the queue and wait for the finish
      (unpark-queue p-queue)
      (s-await finished-gate)
      (read finished?))))
  (is (== True result)))

(define-test test-park-two-queues-then-wake-on-first ()
  (let result =
    (run-io!
     (do
      (p-queue-1 <- new-parking-queue)
      (p-queue-2 <- new-parking-queue)
      (started-gate <- s-new)
      (finished-gate <- s-new)
      (finished? <- (new-var False))
      (thread <-
       (do-fork-thread_
         (s-signal started-gate)
         (park-in-queues-if_ (pure True) (make-list p-queue-1 p-queue-2))
         (write finished? True)
         (s-signal finished-gate)))
      ;; Wait for the thread to start and sleep 2ms to allow it to park
      (s-await started-gate)
      (sleep 2)
      ;; Unpark the first queue and wait for the thread finish
      (unpark-queue p-queue-1)
      (s-await finished-gate)
      (read finished?))))
  (is (== True result)))

(define-test test-park-two-queues-then-wake-on-second ()
  (let result =
    (run-io!
     (do
      (p-queue-1 <- new-parking-queue)
      (p-queue-2 <- new-parking-queue)
      (started-gate <- s-new)
      (finished-gate <- s-new)
      (finished? <- (new-var False))
      (thread <-
       (do-fork-thread_
         (s-signal started-gate)
         (park-in-queues-if_ (pure True) (make-list p-queue-1 p-queue-2))
         (write finished? True)
         (s-signal finished-gate)))
      ;; Wait for the thread to start and sleep 2ms to allow it to park
      (s-await started-gate)
      (sleep 2)
      ;; Unpark the second queue and wait for the thread finish
      (unpark-queue p-queue-2)
      (s-await finished-gate)
      (read finished?))))
  (is (== True result)))

(define-test test-doesnt-wake-on-stale-queue ()
  (let result =
    (run-io!
     (do
      (p-queue-1 <- new-parking-queue)
      (p-queue-2 <- new-parking-queue)
      (started-gate <- s-new)
      (finished? <- (new-var False))
      (thread <-
       (do-fork-thread_
         (s-signal started-gate)
         ;; First park on both queues
         (park-in-queues-if_ (pure True) (make-list p-queue-1 p-queue-2))
         ;; Then only park on the first, so that the entry in the second is stale
         (park-in-queue-if_ (pure True) p-queue-1)
         (write finished? True)))
      ;; Wait for the thread to start and sleep 2ms to allow it to park
      (s-await started-gate)
      (sleep 2)
      ;; Unpark the first queue and wait for the thread to re-park
      (unpark-queue p-queue-1)
      (sleep 2)
      ;; Now the thread is re-parked on just the first queue. Unpark the second queue,
      ;; which has a stale generation entry for the thread, and shouldn't unpark it.
      (unpark-queue p-queue-2)
      ;; Wait 2ms to verify the therad didn't unpark and finish, then return 
      (sleep 2)
      (read finished?))))
  (is (== False result)))

