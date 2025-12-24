(defpackage :coalton-io/tests/mvar
  (:use #:coalton #:coalton-prelude #:coalton-testing
   #:io/utils
   #:io/monad-io
   #:io/simple-io
   #:io/exception
   #:io/mut
   #:io/conc/mvar
   #:io/thread
   #:io/tests/utils
   )
  (:local-nicknames
   (:l #:coalton-library/list))
  )
(in-package :coalton-io/tests/mvar)

(named-readtables:in-readtable coalton:coalton)

(fiasco:define-test-package #:coalton-io/tests/mvar-fiasco)
(coalton-fiasco-init #:coalton-io/tests/mvar-fiasco)

;;;
;;; Single threaded tests
;;;

(define-test test-mvar-read-initial-value ()
  (let result =
    (run-io!
      (do
        (mv <- (new-mvar 10))
        (read-mvar mv))))
  (is (== 10 result)))

(define-test test-mvar-subsequent-read ()
  (let result =
    (run-io!
      (do
        (mv <- (new-mvar 10))
        (a <- (read-mvar mv))
        (b <- (read-mvar mv))
        (pure (Tuple a b)))))
  (is (== (Tuple 10 10) result)))

(define-test test-mvar-take-initial-value ()
  (let result =
    (run-io!
      (do
        (mv <- (new-mvar 10))
        (take-mvar mv))))
  (is (== 10 result)))

(define-test test-mvar-try-take-initial-value ()
  (let result =
    (run-io!
     (do
      (mv <- (new-mvar 10))
      (try-take-mvar mv))))
  (is (== (Some 10) result)))

(define-test test-mvar-try-take-empty ()
  (let result =
    (the (Optional Integer)
         (run-io!
          (do
           (mv <- new-empty-mvar)
           (try-take-mvar mv)))))
  (is (== None result)))

(define-test test-mvar-try-put-empty ()
  (let result =
    (run-io!
     (do
      (mv <- new-empty-mvar)
      (put-result <- (try-put-mvar mv 10))
      (val <- (read-mvar mv))
      (pure (Tuple put-result val)))))
  (is (== (Tuple True 10)
          result)))

(define-test test-mvar-try-put-full ()
  (let result =
    (run-io!
     (do
      (mv <- (new-mvar 0))
      (put-result <- (try-put-mvar mv 10))
      (val <- (read-mvar mv))
      (pure (Tuple put-result val)))))
  (is (== (Tuple False 0)
          result)))

(define-test test-mvar-is-empty ()
  (let result =
    (run-io!
     (do
      (mv <- (the (:m (MVar Integer))
                  new-empty-mvar) )
      (is-empty-mvar mv))))
  (is (== True result)))

(define-test test-mvar-is-not-empty ()
  (let result =
    (run-io!
     (do
      (mv <- (new-mvar 10))
      (is-empty-mvar mv))))
  (is (== False result)))

(define-test test-mvar-put-empty ()
  (let result =
    (run-io!
     (do
      (mv <- new-empty-mvar)
      (put-mvar mv 10)
      (read-mvar mv))))
  (is (== 10 result)))

(define-test test-mvar-swap ()
  (let result =
    (run-io!
     (do
      (mv <- (new-mvar 10))
      (old <- (swap-mvar mv -10))
      (new <- (read-mvar mv))
      (pure (Tuple old new)))))
  (is (== (Tuple 10 -10) result)))

(define-test test-with-mvar ()
  (let result =
    (run-io!
     (do
      (mv <- (new-mvar 10))
      (result1 <-
       (do-with-mvar_ (x mv)
         (pure (<> "Got mvar value " (into x)))))
      (result-val <- (read-mvar mv))
      (pure (Tuple result1 result-val)))))
  (is (== (Tuple "Got mvar value 10" 10)
          result)))

(define-test test-with-mvar-exception ()
  (let result =
    (run-io!
     (do
      (mv <- (new-mvar 10))
      (result1 <-
       (try
        (do-with-mvar_ (x mv)
          (raise "Error inside do-with-mvar")
          (pure (+ x 100)))))
      (result-val <- (try-read-mvar mv))
      (pure (Tuple result1 result-val)))))
  (is (== (Tuple (Err "Error inside do-with-mvar") None)
          result)))

;; These tests thread MVar operations in different sequences
;; to make sure any internal locks are released properly. If
;; there's a bug in releasing the locks, it'll throw a recursive
;; lock attempt error. Because these are all single-threaded,
;; these tests can only test the non-blocking paths.

(define-test test-try-take-empty-then-put ()
  (run-io!
   (do
    (mv <- new-empty-mvar)
    (try-take-mvar mv)
    (put-mvar mv 100))))

(define-test test-try-take-full-then-put ()
  (run-io!
   (do
    (mv <- (new-mvar 10))
    (try-take-mvar mv)
    (put-mvar mv 100))))

(define-test test-try-put-empty-then-take ()
  (run-io!
   (do
    (mv <- new-empty-mvar)
    (try-put-mvar mv 100)
    (take-mvar mv))))

(define-test test-try-put-full-then-take ()
  (run-io!
   (do
    (mv <- (new-mvar 10))
    (try-put-mvar mv 100)
    (take-mvar mv))))

(define-test test-try-read-empty-then-put ()
  (run-io!
   (do
    (mv <- new-empty-mvar)
    (try-read-mvar mv)
    (put-mvar mv 100))))

(define-test test-try-read-full-then-take ()
  (run-io!
   (do
    (mv <- (new-mvar 10))
    (try-read-mvar mv)
    (take-mvar mv))))

(define-test test-put-read-try-read-take-try-take-empty ()
  (run-io!
   (do
    (mv <- new-empty-mvar)
    (put-mvar mv 10)
    (read-mvar mv)
    (try-read-mvar mv)
    (take-mvar mv)
    (try-take-mvar mv))))

;;;
;;; Multi-Threaded Tests
;;;

(define-test test-put-wakes-take ()
  (let result =
    (run-io!
     (do
      (s <- s-new)
      (mv <- new-empty-mvar)
      (result <- (new-var None))
      (do-fork-thread_
        ;; Let the main thread now we started
        (s-signal s)
        ;; Take the MVar
        (contents <- (take-mvar mv))
        (write result (Some contents))
        ;; Signal done
        (s-signal s))
      ;; Wait for the reader to start, then wait briefly for it to hit take
      (s-await s)
      (sleep 5)
      (put-mvar mv 100)
      ;; Wait for reader to finish, then return its result
      (s-await s)
      (read result))))
  (is (== (Some 100) result)))

(define-test test-put-wakes-read ()
  (let result =
    (run-io!
     (do
      (s <- s-new)
      (mv <- new-empty-mvar)
      (result <- (new-var None))
      (do-fork-thread_
        ;; Let the main thread now we started
        (s-signal s)
        ;; Read the MVar
        (contents <- (read-mvar mv))
        (write result (Some contents))
        ;; Signal done
        (s-signal s))
      ;; Wait for the reader to start, then wait briefly for it to hit read
      (s-await s)
      (sleep 5)
      (put-mvar mv 100)
      ;; Wait for reader to finish, then return its result
      (s-await s)
      (read result))))
  (is (== (Some 100) result)))

(define-test test-take-wakes-put ()
  (let result =
    (run-io!
     (do
      (s <- s-new)
      (mv <- (new-mvar 0))
      (do-fork-thread_
        ;; Let the main thread now we started
        (s-signal s)
        ;; Put the MVar
        (put-mvar mv 100)
        ;; Signal done
        (s-signal s))
      ;; Wait for the reader to start, then wait briefly for it to hit put
      (s-await s)
      (sleep 5)
      (mvar-initial <- (take-mvar mv))
      ;; Wait for reader to finish, then return its result
      (s-await s)
      (mvar-final <- (try-read-mvar mv))
      (pure (Tuple mvar-initial mvar-final)))))
  (is (== (Tuple 0 (Some 100))
          result)))

(define-test test-read-chains-wakes ()
  (let result =
    (run-io!
     (do
      (s <- s-new)
      (mv <- new-empty-mvar)
      (result-a <- (new-var None))
      (result-b <- (new-var None))
      (do-fork-thread_
        ;; Let the main thread know we started
        (s-signal s)
        ;; Read the MVar
        (contents <- (read-mvar mv))
        (write result-a (Some contents))
        ;; Signal done
        (s-signal s))
      (do-fork-thread_
        ;; Let the main thread know we started
        (s-signal s)
        ;; Read the MVar
        (contents <- (read-mvar mv))
        (write result-b (Some contents))
        ;; Signal done
        (s-signal s))
      ;; Wait for both readers to start, then briefly wait to put
      (s-await s)
      (s-await s)
      (sleep 5)
      (put-mvar mv 100)
      ;; Wait for readers to finish, then return their results
      (s-await s)
      (s-await s)
      (result-a <- (read result-a))
      (result-b <- (read result-b))
      (pure (Tuple result-a result-b)))))
  (is (== (Tuple (Some 100) (Some 100))
          result)))

;; These tests thread MVar operations in different sequences
;; to make sure any internal locks are released properly. If
;; there's a bug in releasing the locks, it'll throw a recursive
;; lock attempt error. Because these are all multi-threaded,
;; these tests target the blocked paths.

(define-test test-full-put-then-read ()
  (run-io!
   (do
    (s-start <- s-new)
    (s-finish <- s-new)
    (mv <- (new-mvar 0))
    (do-fork-thread_
      (s-signal s-start)
      (put-mvar mv 10)
      (s-signal s-finish))
    (do-fork-thread_
      (s-await s-start)
      (sleep 5)
      (read-mvar mv)
      (s-signal s-finish))
    ;; Wait for read-mvar to complete, then take to unblock put-mvar
    (s-await s-finish)
    (take-mvar mv)
    (s-await s-finish))))

(define-test test-take-empty-then-read ()
  (run-io!
   (do
    (s-threads <- s-new)
    (s-start <- s-new)
    (s-finish <- s-new)
    (mv <- new-empty-mvar)
    (do-fork-thread_
      (s-signal s-threads)
      (s-signal s-start)
      (take-mvar mv)
      (s-signal s-finish))
    (do-fork-thread_
      (s-await s-threads)
      (s-signal s-start)
      (read-mvar mv)
      (s-signal s-finish))
    ;; Wait for both to block, then put to unblock them
    (s-await s-start)
    (s-await s-start)
    (sleep 5)
    (put-mvar mv 10)
    (s-await s-finish)
    (put-mvar mv 10)
    (s-await s-finish))))

(define-test test-stop-take-while-blocking ()
  (let result =
    (run-io!
     (do
      (s-start <- s-new)
      (mv <- new-empty-mvar)
      (thread <-
        (do-fork-thread_
          (s-signal s-start)
          (take-mvar mv)))
      (s-await s-start)
      (sleep 2)
      (stop-thread thread)
      (try-all (join-thread thread)))))
  (is (== (Some Unit) result)))

(define-test test-stop-put-while-blocking ()
  (let result =
    (run-io!
     (do
      (s-start <- s-new)
      (mv <- (new-mvar Unit))
      (thread <-
        (do-fork-thread_
          (s-signal s-start)
          (put-mvar mv Unit)))
      (s-await s-start)
      (sleep 2)
      (stop-thread thread)
      (try-all (join-thread thread)))))
  (is (== (Some Unit) result)))

(define-test test-stop-read-while-blocking ()
  (let result =
    (run-io!
     (do
      (s-start <- s-new)
      (mv <- new-empty-mvar)
      (thread <-
        (do-fork-thread_
          (s-signal s-start)
          (read-mvar mv)))
      (s-await s-start)
      (sleep 2)
      (stop-thread thread)
      (try-all (join-thread thread)))))
  (is (== (Some Unit) result)))

(define-test test-stop-after-take-happy-path ()
  (let result =
    (run-io!
     (do
      (s-start <- s-new)
      (s-forever <- s-new)
      (mv <- (new-mvar Unit))
      (thread <-
        (do-fork-thread_
          (take-mvar mv)
          (s-signal s-start)
          (s-await s-forever)
          (pure Unit)))
      (s-await s-start)
      (stop-thread thread)
      (try-all (join-thread thread)))))
  (is (== (Some Unit) result)))

(define-test test-stop-after-take-block ()
  (let result =
    (run-io!
     (do
      (s-start <- s-new)
      (s-after <- s-new)
      (s-forever <- s-new)
      (mv <- new-empty-mvar)
      (thread <-
        (do-fork-thread_
          (s-signal s-start)
          (take-mvar mv)
          (s-signal s-after)
          (s-await s-forever)
          (pure Unit)))
      (s-await s-start)
      (sleep 2)
      (put-mvar mv Unit)
      (s-await s-after)
      (stop-thread thread)
      (try-all (join-thread thread)))))
  (is (== (Some Unit) result)))

(define-test test-stop-after-put-happy-path ()
  (let result =
    (run-io!
     (do
      (s-start <- s-new)
      (s-forever <- s-new)
      (mv <- new-empty-mvar)
      (thread <-
        (do-fork-thread_
          (put-mvar mv Unit)
          (s-signal s-start)
          (s-await s-forever)
          (pure Unit)))
      (s-await s-start)
      (stop-thread thread)
      (try-all (join-thread thread)))))
  (is (== (Some Unit) result)))

(define-test test-stop-after-put-block ()
  (let result =
    (run-io!
     (do
      (s-start <- s-new)
      (s-after <- s-new)
      (s-forever <- s-new)
      (mv <- (new-mvar Unit))
      (thread <-
        (do-fork-thread_
          (s-signal s-start)
          (put-mvar mv Unit)
          (s-signal s-after)
          (s-await s-forever)
          (pure Unit)))
      (s-await s-start)
      (sleep 2)
      (take-mvar mv)
      (s-await s-after)
      (stop-thread thread)
      (try-all (join-thread thread)))))
  (is (== (Some Unit) result)))

(define-test test-stop-after-read-happy-path ()
  (let result =
    (run-io!
     (do
      (s-start <- s-new)
      (s-forever <- s-new)
      (mv <- (new-mvar Unit))
      (thread <-
        (do-fork-thread_
          (read-mvar mv)
          (s-signal s-start)
          (s-await s-forever)
          (pure Unit)))
      (s-await s-start)
      (stop-thread thread)
      (try-all (join-thread thread)))))
  (is (== (Some Unit) result)))

(define-test test-stop-after-read-block ()
  (let result =
    (run-io!
     (do
      (s-start <- s-new)
      (s-after <- s-new)
      (s-forever <- s-new)
      (mv <- new-empty-mvar)
      (thread <-
        (do-fork-thread_
          (s-signal s-start)
          (read-mvar mv)
          (s-signal s-after)
          (s-await s-forever)
          (pure Unit)))
      (s-await s-start)
      (sleep 2)
      (put-mvar mv Unit)
      (s-await s-after)
      (stop-thread thread)
      (try-all (join-thread thread)))))
  (is (== (Some Unit) result)))

;;;
;;; Test the "leave masked" functions
;;;

(define-test test-take-masked-leaves-masked ()
  (let result =
    (run-io!
     (do
      (s-start <- s-new)
      (s-after <- s-new)
      (s-stopped <- s-new)
      (mv <- new-empty-mvar)
      (result <- new-empty-mvar)
      (thread <-
        (do-fork-thread_
          (s-signal s-start)
          (take-mvar-masked mv)
          (s-signal s-after)
          (s-await s-stopped)
          (put-mvar result (Some True))))
      (s-await s-start)
      (sleep 2)
      (put-mvar mv Unit)
      (s-await s-after)
      (stop-thread thread)
      (s-signal s-stopped)
      (take-mvar result))))
  (is (== (Some True) result)))
