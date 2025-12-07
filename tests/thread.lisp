(defpackage :coalton-io/tests/thread
  (:use #:coalton #:coalton-prelude #:coalton-testing
        #:io/simple-io
        #:io/thread
        #:io/mut
        #:io/mvar)
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

;;; NOTE:
;;; (1) MVar's are preferred over Locks to signal between threads.
;;; Here, using locks avoids a circular "dependency" between the tests.
;;; (2) This is *not* an efficient way to use locks. MVar's or Condition
;;; Variables would be much better. However, for testing purposes iterating
;;; until the forked thread has completed keeps the tests simple.
;;; (3) In production code, it would be better to write an IO layer over
;;; a non-pure datastructure like Lock, instead of using `wrap-io` throughout.
;;; Using `wrap-io` like this makes it easier to accidentally leak side
;;; effects in an unpredictable way.

(define-test test-fork-executes ()
  (let result =
    (run-io!
      (do
        (lock <- (wrap-io (lk:new)))
        (flag <- (new-var Unset))
        (fork
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
        (do-fork
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

;;; NOTE: Switching back to MVar's from locks for tests that aren't
;;; on the basics of forking threads.

(define-test test-stop ()
  (let result =
    (run-io!
     (do
      (gate <- new-empty-mvar)
      (flag <- (new-var Unset))
      (thread <-
        (do-fork
          (put-mvar gate Unit)
          (sleep 2)
          (write flag Set)))
      (take-mvar gate)
      (stop thread)
      (sleep 4)
      (read flag))))
  (is (== Unset result)))
