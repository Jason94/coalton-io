(defpackage :coalton-io/tests/thread
  (:use #:coalton #:coalton-prelude #:coalton-testing
        #:io/utils
        #:io/simple-io
        #:io/thread
        #:io/mut
        #:io/mvar
        #:io/tests/utils
        )
  (:local-nicknames
   (:lk #:coalton-threads/lock))
  )
(in-package :coalton-io/tests/thread)

(named-readtables:in-readtable coalton:coalton)

(fiasco:define-test-package #:coalton-io/tests/thread-fiasco)
(coalton-fiasco-init #:coalton-io/tests/thread-fiasco)

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
