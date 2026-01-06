(defpackage :coalton-io/tests/thread
  (:use #:coalton #:coalton-prelude #:coalton-testing
        #:io/utils
        #:io/monad-io
        #:io/simple-io
        #:io/exception
        #:io/thread
        #:io/mut
        #:io/conc/mvar
        #:io/tests/utils
        )
  (:local-nicknames
   (:tm #:io/term)
   (:opt #:coalton-library/optional)
   (:lk #:coalton-threads/lock))
  )
(in-package :coalton-io/tests/thread)

(named-readtables:in-readtable coalton:coalton)

(fiasco:define-test-package #:coalton-io/tests/thread-fiasco)
(coalton-fiasco-init #:coalton-io/tests/thread-fiasco)

;; NOTE: These are really tests for IoRuntime specifically. They probably
;; shouldn't live here. Some of this might be testing the (small amount of)
;; monad-io-thread generic machinery. But all of the IoRuntime specific tests
;; should be moved into a runtime specific test suite.

(coalton-toplevel
  (derive Eq)
  (repr :enum)
  (define-type Flag
    Unset
    Set))

(define-test test-fork-executes ()
  (let result =
    (run-io!
      (do
        (lock <- (wrap-io (lk:new)))
        (flag <- (new-var Unset))
        (fork-thread_
          (do
            (wrap-io (lk:acquire lock))
            (write flag Set)
            (wrap-io (lk:release lock))))
        (rec % ()
          (do
            (got <- (wrap-io (lk:acquire-no-wait lock)))
            (if got
                (do
                  (v <- (read flag))
                  (wrap-io (lk:release lock))
                  ;; Even if we got the lock, we probably beat the forked
                  ;; thread to it, so repeat until the var has been set.
                  (if (== Set v)
                    (pure v)
                    (%)))
                (%)))))))
  (is (== Set result)))

(define-test test-do-fork-executes ()
  (let result =
    (run-io!
      (do
        (lock <- (wrap-io (lk:new)))
        (flag <- (new-var Unset))
        (do-fork-thread_
          (wrap-io (lk:acquire lock))
          (write flag Set)
          (wrap-io (lk:release lock)))
        (rec % ()
          (do
            (got <- (wrap-io (lk:acquire-no-wait lock)))
            (if got
                (do
                  (v <- (read flag))
                  (wrap-io (lk:release lock))
                  ;; Even if we got the lock, we probably beat the forked
                  ;; thread to it, so repeat until the var has been set.
                  (if (== Set v)
                    (pure v)
                    (%)))
                (%)))))))
  (is (== Set result)))

(define-test test-join ()
  (let result =
    (run-io!
     (do
      (gate <- s-new)
      (value <- (new-var None))
      (thread <-
       (do-fork-thread_
         (s-signal gate)
         (write value (Some 10))))
      (s-await gate)
      (join-thread thread)
      (read value))))
  (is (== (Some 10) result)))

(define-test test-join-target-thread-raises ()
  (let result =
    (run-io!
     (do
      (gate <- s-new)
      (thread <-
       (do-fork-thread_
         (s-signal gate)
         (raise "Error in target thread!")))
      (s-await gate)
      (try-all (join-thread thread)))))
  (is (== None result)))

(define-test test-join-stopped-target-doesnt-raise ()
  (let result =
    (run-io!
     (do
      (start-gate <- s-new)
      (wait-gate <- s-new)
      (thread <-
       (do-fork-thread_
         (s-signal start-gate)
         (s-await wait-gate)
         (pure Unit)))
      (s-await start-gate)
      (stop-thread thread)
      (try-all (join-thread thread)))))
  (is (== (Some Unit) result)))

(define-test test-stop ()
  (let result =
    (run-io!
     (do
      (gate <- s-new)
      (flag <- (new-var Unset))
      (thread <-
        (do-fork-thread_
          (s-signal gate)
          (sleep 2)
          (write flag Set)))
      (s-await gate)
      (stop-thread thread)
      ;; Unfortunately we have to sleep here, because the stopped thread will
      ;; have no way to signal to us.
      (sleep 4)
      (read flag))))
  (is (== Unset result)))

(define-test test-current-thread ()
  (let (Tuple outer-handle inner-handle) =
    (run-io!
     (do
      (gate <- s-new)
      (pass-inner-handle <- (new-var None))
      (outer-handle <-
        (do-fork-thread_
          (inner-handle <- current-thread)
          (write pass-inner-handle (Some inner-handle))
          (s-signal gate)))
      (s-await gate)
      (inner-handle <- (read pass-inner-handle))
      (pure (Tuple outer-handle inner-handle)))))
  (is (== (Some outer-handle) inner-handle)))

(define-test test-mask-current-thread-thread ()
  (let result =
    (run-io!
     (do
      (masked-gate <- s-new)
      (stopped-gate <- s-new)
      (value <- (new-var None))
      (thread <-
        (do-fork-thread_
          mask-current-thread
          (s-signal masked-gate)
          (s-await stopped-gate)
          (write value (Some 10))
          ))
      ;; Wait for the thread to mask itself before stopping it
      (s-await masked-gate)
      (stop-thread thread)
      (s-signal stopped-gate)
      (sleep 5)
      (read value))))
  (is (== result (Some 10))))

(define-test test-unmask-current-thread-thread ()
  (let result =
    (run-io!
     (do
      (masked-gate <- s-new)
      (stopped-gate <- s-new)
      (value <- (new-var None))
      (thread <-
        (do-fork-thread_
          mask-current-thread
          unmask-current-thread
          (s-signal masked-gate)
          (s-await stopped-gate)
          (write value (Some 10))))
      ;; Wait for the thread to mask and unmask itself before stopping it
      (s-await masked-gate)
      (stop-thread thread)
      (s-signal stopped-gate)
      ;; Unfortunately the thread can't signal back, so sleep a few MS to make sure
      (sleep 5)
      (read value))))
  (is (== result None)))

(define-test test-stop-on-unmask ()
  (let result =
    (run-io!
     (do
      (masked-gate <- s-new)
      (stopped-gate <- s-new)
      (value <- (new-var None))
      (thread <-
        (do-fork-thread_
          mask-current-thread
          (s-signal masked-gate)
          (s-await stopped-gate)
          (write value (Some 5))
          unmask-current-thread
          (write value (Some 10))))
      ;; Wait for the thread to mask itself before stopping it
      (s-await masked-gate)
      (stop-thread thread)
      (s-signal stopped-gate)
      ;; Unfortunately the thread can't signal back, so sleep a few MS to make sure
      (sleep 5)
      (read value))))
  (is (== result (Some 5))))

(define-test test-nested-mask ()
  (let result =
    (run-io!
     (do
      (masked-gate <- s-new)
      (stopped-gate <- s-new)
      (finished-gate <- s-new)
      (value <- (new-var None))
      (thread <-
        (do-fork-thread_
          mask-current-thread
          mask-current-thread
          unmask-current-thread
          (s-signal masked-gate)
          (s-await stopped-gate)
          (write value (Some 10))
          (s-signal finished-gate)))
      ;; Wait for the thread to mask itself before stopping it
      (s-await masked-gate)
      (stop-thread thread)
      (s-signal stopped-gate)
      (s-await finished-gate)
      (read value))))
  (is (== result (Some 10))))

(define-test test-mask-other-thread ()
  (let result =
    (run-io!
     (do
      (started-gate <- s-new)
      (stopped-gate <- s-new)
      (finished-gate <- s-new)
      (value <- (new-var None))
      (thread <-
        (do-fork-thread_
          (s-signal started-gate)
          (s-await stopped-gate)
          (write value (Some 10))
          (s-signal finished-gate)))
      ;; Wait for the thread to start before masking and stopping it
      (s-await started-gate)
      (mask-thread thread)
      (stop-thread thread)
      (s-signal stopped-gate)
      (s-await finished-gate)
      (read value))))
  (is (== result (Some 10))))

(define-test test-unmask-other-thread ()
  (let result =
    (run-io!
     (do
      (started-gate <- s-new)
      (stopped-gate <- s-new)
      (value <- (new-var None))
      (thread <-
        (do-fork-thread_
          (s-signal started-gate)
          (s-await stopped-gate)
          (write value (Some 10))))
      ;; Wait for the thread to start before stopping it
      (s-await started-gate)
      (mask-thread thread)
      (unmask-thread thread)
      (stop-thread thread)
      (s-signal stopped-gate)
      (sleep 2)
      (read value))))
  (is (== result None)))

(define-test test-unmask-other-thread-stop-on-unmask ()
  (let result =
    (run-io!
     (do
      (started-gate <- s-new)
      (stopped-gate <- s-new)
      (value <- (new-var None))
      (thread <-
        (do-fork-thread_
          (s-signal started-gate)
          (s-await stopped-gate)
          (write value (Some 10))))
      ;; Wait for the thread to start before stopping it
      (s-await started-gate)
      (mask-thread thread)
      (stop-thread thread)
      (unmask-thread thread)
      (sleep 2)
      (read value))))
  (is (== result None)))

(define-test test-unmask-finally-other-thread-runs-thunk-if-stopped ()
  (let result =
    (run-io!
     (do
      (started-gate <- s-new)
      (stopped-gate <- s-new)
      (value <- (new-var None))
      (hit-finally <- (new-var None))
      (thread <-
        (do-fork-thread_
          (s-signal started-gate)
          (s-await stopped-gate)
          (write value (Some 10))))
      ;; Wait for the thread to start before stopping it
      (s-await started-gate)
      (mask-thread thread)
      (stop-thread thread)
      (unmask-thread-finally_ thread (fn (mode)
                                       (write hit-finally (Some mode))))
      (s-signal stopped-gate)
      (sleep 2)
      (val <- (read value))
      (hit-finally? <- (read hit-finally))
      (pure (Tuple val hit-finally?)))))
  (is (== (Tuple None (Some Stopped)) result)))

(define-test test-unmask-finally-other-thread-runs-thunk-if-not-stopped ()
  (let result =
    (run-io!
     (do
      (started-gate <- s-new)
      (continue-gate <- s-new)
      (value <- (new-var None))
      (hit-finally <- (new-var None))
      (thread <-
        (do-fork-thread_
          (s-signal started-gate)
          (s-await continue-gate)
          (write value (Some 10))))
      ;; Wait for the thread to start before stopping it
      (s-await started-gate)
      (mask-thread thread)
      (unmask-thread-finally_ thread (fn (mode)
                                       (write hit-finally (Some mode))))
      (s-signal continue-gate)
      (sleep 2)
      (val <- (read value))
      (hit-finally? <- (read hit-finally))
      (pure (Tuple val hit-finally?)))))
  (is (== (Tuple (Some 10) (Some Running)) result)))

;;;
;;; Parking tests
;;;

(define-test test-park-and-immediately-unpark ()
  (let result =
    (run-io!
     (do
      (generation-set-gate <- s-new)
      (generation <- (new-var None))
      (finished-gate <- s-new)
      (finished? <- (new-var False))
      (thread <-
       (do-fork-thread_
         (park-current-thread-if_
          (fn (gen)
            (do
             (write generation (Some gen))
             (s-signal generation-set-gate)))
          (pure True))
         (write finished? True)
         (s-signal finished-gate)))
      ;; Wait for the thread to set the generation and sleep 2ms to allow it to park
      (s-await generation-set-gate)
      (sleep 2)
      (generation <- (read generation))
      (unpark-thread (opt:from-some "Test execution failure" generation)
                     thread)
      (s-await finished-gate)
      (read finished?)
       )))
  (is (== True result)))

(define-test test-stale-unparks-are-ignored ()
  (let result =
    (run-io!
     (do
      (generation-set-gate <- s-new)
      (generation <- (new-var None))
      (2nd-generation <- (new-var None))
      (finished-gate <- s-new)
      (finished? <- (new-var False))
      (thread <-
       (do-fork-thread_
         (park-current-thread-if_
          (fn (gen)
            (do
             (write generation (Some gen))
             (s-signal generation-set-gate)))
          (pure True))
         (park-current-thread-if_
          (fn (gen)
            (do
             (write 2nd-generation (Some gen))
             (s-signal generation-set-gate)))
          (pure True))
         (write finished? True)
         (s-signal finished-gate)))
      ;; Wait for the thread to set the generation and sleep 2ms to allow it to park
      (s-await generation-set-gate)
      (sleep 2)
      (generation <- (read generation))
      (unpark-thread (opt:from-some "Test execution failure" generation)
                     thread)
      ;; Wait for the thread to set the 2nd generation and sleep 2ms for it to park again
      (s-await generation-set-gate)
      (sleep 2)
      ;; Attempt to wake it with the stale generation, wait for 2ms for it to wake, and
      ;; check finished?
      (unpark-thread (opt:from-some "Test execution failure" generation)
                     thread)
      (sleep 2)
      (finished-after-stale <- (read finished?))
      ;; Unpark with the fresh generation and wait for it to finish
      (2nd-generation <- (read 2nd-generation))
      (unpark-thread (opt:from-some "Test execution failure" 2nd-generation)
                     thread)
      (s-await finished-gate)
      (finished-after-fresh <- (read finished?))
      (pure (Tuple finished-after-stale finished-after-fresh)))))
  (is (== (Tuple False True)
          result)))

