(defpackage :coalton-io/tests/conc/parking
  (:use #:coalton #:coalton-prelude #:coalton-testing
   #:coalton-library/types
   #:coalton-library/experimental/do-control-loops
   #:io/utils
   #:io/monad-io
   #:io/simple-io
   #:io/mutable-var
   #:io/threads
   #:io/conc/parking
   #:io/tests/utils
   )
  (:import-from #:io/gen-impl/conc/parking
   #:num-waiters)
  (:local-nicknames
   (:tm #:io/terminal)
   )
  )
(in-package :coalton-io/tests/conc/parking)

(named-readtables:in-readtable coalton:coalton)

(fiasco:define-test-package #:coalton-io/tests/conc/parking-fiasco)
(coalton-fiasco-init #:coalton-io/tests/conc/parking-fiasco)

(define-test test-park-single-set-then-wake ()
  (let result =
    (run-io!
     (do
      (p-set <- new-parking-set)
      (started-gate <- s-new)
      (finished-gate <- s-new)
      (finished? <- (new-var False))
      (thread <-
       (do-fork-thread_
         (s-signal started-gate)
         (park-in-set-if_ (pure True) p-set)
         (write finished? True)
         (s-signal finished-gate)))
      ;; Wait for the thread to start and sleep 2ms to allow it to park
      (s-await started-gate)
      (do-loop-while
        (sleep 2)
        (num-parked <- (num-waiters p-set))
        ;; NOTE: do-loop-while on booleans is backwards
        ;; https://github.com/coalton-lang/coalton/issues/1742
        (pure (not (zero? num-parked))))
      ;; Unpark the set and wait for the finish
      (unpark-set p-set)
      (s-await finished-gate)
      (read finished?))))
  (is (== True result)))

(define-test test-park-two-sets-then-wake-on-first ()
  (let result =
    (run-io!
     (do
      (p-set-1 <- new-parking-set)
      (p-set-2 <- new-parking-set)
      (started-gate <- s-new)
      (finished-gate <- s-new)
      (finished? <- (new-var False))
      (thread <-
       (do-fork-thread_
         (s-signal started-gate)
         (park-in-sets-if_ (pure True) (make-list p-set-1 p-set-2))
         (write finished? True)
         (s-signal finished-gate)))
      ;; Wait for the thread to start and sleep 2ms to allow it to park
      (s-await started-gate)
      (sleep 2)
      ;; Unpark the first set and wait for the thread finish
      (unpark-set p-set-1)
      (s-await finished-gate)
      (read finished?))))
  (is (== True result)))

(define-test test-park-two-sets-then-wake-on-second ()
  (let result =
    (run-io!
     (do
      (p-set-1 <- new-parking-set)
      (p-set-2 <- new-parking-set)
      (started-gate <- s-new)
      (finished-gate <- s-new)
      (finished? <- (new-var False))
      (thread <-
       (do-fork-thread_
         (s-signal started-gate)
         (park-in-sets-if_ (pure True) (make-list p-set-1 p-set-2))
         (write finished? True)
         (s-signal finished-gate)))
      ;; Wait for the thread to start and sleep 2ms to allow it to park
      (s-await started-gate)
      (sleep 2)
      ;; Unpark the second set and wait for the thread finish
      (unpark-set p-set-2)
      (s-await finished-gate)
      (read finished?))))
  (is (== True result)))

(define-test test-doesnt-wake-on-stale-set ()
  (let result =
    (run-io!
     (do
      (p-set-1 <- new-parking-set)
      (p-set-2 <- new-parking-set)
      (started-gate <- s-new)
      (finished? <- (new-var False))
      (thread <-
       (do-fork-thread_
         (s-signal started-gate)
         ;; First park on both sets
         (park-in-sets-if_ (pure True) (make-list p-set-1 p-set-2))
         ;; Then only park on the first, so that the entry in the second is stale
         (park-in-set-if_ (pure True) p-set-1)
         (write finished? True)))
      ;; Wait for the thread to start and sleep 2ms to allow it to park
      (s-await started-gate)
      (sleep 2)
      ;; Unpark the first set and wait for the thread to re-park
      (unpark-set p-set-1)
      (sleep 2)
      ;; Now the thread is re-parked on just the first set. Unpark the second set,
      ;; which has a stale generation entry for the thread, and shouldn't unpark it.
      (unpark-set p-set-2)
      ;; Wait 2ms to verify the therad didn't unpark and finish, then return 
      (sleep 2)
      (read finished?))))
  (is (== False result)))
