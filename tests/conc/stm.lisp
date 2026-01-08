(defpackage :coalton-io/tests/conc/stm
  (:use #:coalton #:coalton-prelude #:coalton-testing
        #:coalton-library/experimental/do-control-core
        #:io/utils
        #:io/simple-io
        #:io/exception
        #:io/mut
        #:io/conc/mvar
        #:io/thread
        #:io/conc/future
        #:io/conc/stm)
  (:import-from #:io/term
   #:write-line)
  (:import-from #:io/gen-impl/conc/stm
   #:tx-io!%)
  (:local-nicknames
   (:l #:coalton-library/list)
   (:tm #:io/term))
  )
(in-package :coalton-io/tests/conc/stm)

(named-readtables:in-readtable coalton:coalton)

(fiasco:define-test-package #:coalton-io/tests/conc/stm-fiasco)
(coalton-fiasco-init #:coalton-io/tests/conc/stm-fiasco)

;;;
;;; Single threaded tests
;;;

(define-test test-read-tvar ()
  (let result =
    (run-io!
     (do
      (a <- (new-tvar 0))
      (run-tx (read-tvar a)))))
  (is (== 0 result)))

(define-test test-read-multiple-tvars ()
  (let result =
    (run-io!
     (do
      (a <- (new-tvar 0))
      (b <- (new-tvar "b"))
      (do-run-tx
        (a-val <- (read-tvar a))
        (b-val <- (read-tvar b))
        (pure (Tuple a-val b-val))))))
  (is (== (Tuple 0 "b") result)))

(define-test test-write-read-tx ()
  (let result =
    (run-io!
     (do
      (a <- (new-tvar 0))
      (do-run-tx
        (write-tvar a 10)
        (read-tvar a)))))
  (is (== 10 result)))

(define-test test-modify-tvar ()
  (let result =
    (run-io!
     (do
      (a <- (new-tvar 0))
      (do-run-tx
        (write-tvar a 10)
        (modify-result <- (modify-tvar a (* 2)))
        (read-result <- (read-tvar a))
        (pure (Tuple modify-result read-result))))))
  (is (== (Tuple 20 20) result)))

(define-test test-write-read-separate-txs ()
  (let result =
    (run-io!
     (do
      (a <- (new-tvar 0))
      (run-tx (write-tvar a 10))
      (run-tx (read-tvar a)))))
  (is (== 10 result)))

(define-test test-exception-aborts-transaction ()
  (let result =
    (run-io!
     (do
      (a <- (new-tvar 0))
      (result1 <-
       (try-all
        (do-run-tx
          (write-tvar a 100)
          (raise "Raising exception after write")
          (read-tvar a))))
      (result2 <- (run-tx (read-tvar a)))
      (pure (Tuple result1 result2)))))
  (is (== (Tuple None 0)
          result)))

(define-test test-tx-wrap-error ()
  (let result =
    (run-io!
     (do
      (a <- (new-tvar 0))
      (result1 <-
       (try-all
        (do-run-tx
          (write-tvar a 10)
          (next-val <- (wrap-error
                         (error "Coalton error!")
                         100))
          (write-tvar a next-val)
          (read-tvar a))))
      (result2 <- (run-tx (read-tvar a)))
      (pure (Tuple result1 result2)))))
  (is (== (Tuple None 0)
          result)))

(define-test test-or-else-tx1-write ()
  (let result =
    (run-io!
     (do
      (a <- (new-tvar 0))
      (do-run-tx
        (or-else (write-tvar a 10)
                 (write-tvar a 20))
        (read-tvar a)))))
  (is (== 10 result)))

(define-test test-or-else-tx1-retry-tx2-write ()
  (let result =
    (run-io!
     (do
      (a <- (new-tvar 0))
      (do-run-tx
        (or-else retry
                 (write-tvar a 20))
        (read-tvar a)))))
  (is (== 20 result)))

(define-test test-or-else-tx1-write-retry-tx2-write ()
  (let result =
    (run-io!
     (do
      (a <- (new-tvar 0))
      (b <- (new-tvar 0))
      (do-run-tx
        (or-else (do
                  (write-tvar a 10)
                  retry)
                 (write-tvar b 20))
        (a-val <- (read-tvar a))
        (b-val <- (read-tvar b))
        (pure (Tuple a-val b-val))))))
  (let _ = (the (Tuple Integer Integer) result))
  (is (== (Tuple 0 20) result)))

;;;
;;; Multi-threaded tests
;;;

(define-test test-retry-only-after-write-tx ()
  (let result =
    (run-io!
     (do
      (retry-count <- (new-var 0))
      (x <- (new-tvar 0))
      (retry-gate <- new-empty-mvar)
      (result-fut <-
        (do-fork-future_
         (do-run-tx
           (n-retries <- (tx-io!% (read retry-count)))
           (x-val <- (read-tvar x))
           (do-when (zero? x-val)
             (tx-io!% (modify retry-count (+ 1)))
             (tx-io!% (try-put-mvar retry-gate Unit))
             retry)
           (pure x-val))))
      ;; Wait until the retry is triggered
      (take-mvar retry-gate)
      (run-tx (read-tvar x))
      (sleep 1)
      (run-tx (write-tvar x 10))
      ;; Wait until the second retry is finished
      (result <- (await result-fut))
      (num-retries <- (read retry-count))
      (pure (Tuple result num-retries)))))
  (let _ = (the (Tuple Integer Integer) result))
  (is (== (Tuple 10 1)
          result)))

(define-test test-retry-only-after-write-to-read-tvars ()
  (let result =
    (run-io!
     (do
      (retry-count <- (new-var 0))
      (used-tvar <- (new-tvar 0))
      (ignored-tvar <- (new-tvar 0))
      (retry-gate <- new-empty-mvar)
      (finished-gate <- new-empty-mvar)
      (do-fork-thread_
        (do-run-tx
          (x-val <- (read-tvar used-tvar))
          (do-when (zero? x-val)
            (tx-io!% (modify retry-count (+ 1)))
            (tx-io!% (try-put-mvar retry-gate Unit))
            retry)
          (tx-io!% (try-put-mvar finished-gate Unit))))
      ;; Wait until the retry is triggered
      (take-mvar retry-gate)
      (sleep 2)
      ;; Write to the ignored TVar, not triggering a retry
      (run-tx (write-tvar ignored-tvar 10))
      (sleep 2)
      (retry-count-after-ignored <- (read retry-count))
      ;; Write to the used TVar, triggering a retry
      (run-tx (write-tvar used-tvar 10))
      ;; Wait until the retry is finished
      (take-mvar finished-gate)
      (num-retries <- (read retry-count))
      (pure (Tuple retry-count-after-ignored num-retries)))))
  (is (== (Tuple 1 1)
          result)))

(define-test test-write-interrupts-read ()
  (let (Tuple3 observed-as observed-bs result) =
    (run-io!
     (do
      (a <- (new-tvar 0))
      (b <- (new-tvar 100))
      (observed-as <- (new-var Nil))
      (observed-bs <- (new-var Nil))
      (read-gate <- new-empty-mvar)
      (write-gate <- new-empty-mvar)
      (result-fut <-
        (fork-future_
         (do-run-tx
           (a-val <- (read-tvar a))
           (tx-io!% (modify observed-as (Cons a-val)))
           ;; Let the write-tx know that we've read the first tvar
           (tx-io!% (put-mvar read-gate Unit))
           ;; Wait for the write-tx to write to both tvars
           (tx-io!% (take-mvar write-gate))
           (b-val <- (read-tvar b))
           (tx-io!% (modify observed-bs (Cons b-val)))
           (pure (Tuple a-val b-val)))))
      (do-fork-thread_
        (take-mvar read-gate)
        (do-run-tx
          (write-tvar a 1)
          (write-tvar b 101))
        (put-mvar write-gate Unit)
        ;; Let the read-tx get through its second try
        (take-mvar read-gate)
        (put-mvar write-gate Unit))
      (result <- (await result-fut))
      (observed-as <- (read observed-as))
      (observed-bs <- (read observed-bs))
      (pure (Tuple3 observed-as observed-bs result)))))
  (is (== (make-list 1 0) observed-as))
  (is (== (make-list 101) observed-bs))
  (is (== (Tuple 1 101) result)))

(define-test test-reset-read-log-on-retry ()
  ;; If a tvar is read on a failed commit, but isn't read on a subsequent
  ;; retry, it shouldn't cause an inconsistency failure if its written to
  ;; in the subsequent retry.
  (let (Tuple4 observed-as observed-bs observed-cs result) =
    (run-io!
     (do
      (a <- (new-tvar 0))
      (b <- (new-tvar 10))
      (c <- (new-tvar 100))
      (observed-as <- (new-var Nil))
      (observed-bs <- (new-var Nil))
      (observed-cs <- (new-var Nil))
      (read-gate <- new-empty-mvar)
      (write-gate <- new-empty-mvar)
      (result-fut <-
        (fork-future_
         (do-run-tx
           (a-val <- (read-tvar a))
           (tx-io!% (modify observed-as (Cons a-val)))
           (b-val <-
             (if (even? a-val)
               (read-tvar b)
               (pure -10)))
           (tx-io!% (modify observed-bs (Cons b-val)))
           ;; Let the write-tx know that we've read
           (tx-io!% (put-mvar read-gate Unit))
           ;; Wait for the write-tx to write to its tvars
           (tx-io!% (take-mvar write-gate))
           (c-val <- (read-tvar c))
           (tx-io!% (modify observed-cs (Cons c-val)))
           (pure (Tuple3 a-val b-val c-val)))))
      (take-mvar read-gate)
      (do-run-tx
        (write-tvar a 1)
        (write-tvar b 11))
      (put-mvar write-gate Unit)
      ;; Let the read-tx get through its second try
      (take-mvar read-gate)
      (do-run-tx
        (write-tvar b 12))
      (put-mvar write-gate Unit)
      ;; Get the results
      (result <- (await result-fut))
      (observed-as <- (read observed-as))
      (observed-bs <- (read observed-bs))
      (observed-cs <- (read observed-cs))
      (pure (Tuple4 (reverse observed-as)
                    (reverse observed-bs)
                    (reverse observed-cs)
                    result)))))
  (let _ = (the (List Integer) observed-as))
  (let _ = (the (List Integer) observed-cs))
  (is (== (make-list 0 1) observed-as))
  (is (== (make-list 10 -10) observed-bs))
  (is (== (make-list 100) observed-cs))
  (is (== (Tuple3 1 -10 100) result)))

(define-test test-or-else-both-retry ()
  (let result =
    (run-io!
     (do
      (retried? <- (new-tvar False))
      (a <- (new-tvar 0))
      (b <- (new-tvar 0))
      (retry-gate <- new-empty-mvar)
      (thd <-
        (do-fork-future_
          (do-run-tx
            (or-else (do
                      (retried? <- (read-tvar retried?))
                      (if retried?
                          (write-tvar a 10)
                          (do
                           (write-tvar a 5)
                           (tx-io!% (put-mvar retry-gate Unit))
                           retry)))
                     (do
                      (write-tvar b 20)
                      retry)))))
      ;; Wait for both transactions to fail the first time
      (take-mvar retry-gate)
      (run-tx (write-tvar retried? True))
      ;; Wait for final transaction to finish
      (await thd)
      ;; Check the results
      (a-val <- (run-tx (read-tvar a)))
      (b-val <- (run-tx (read-tvar b)))
      (pure (Tuple a-val b-val)))))
  (let _ = (the (Tuple Integer Integer) result))
  (is (== (Tuple 10 0) result)))

;; Any variables that the first TX reads should cause the entire or-else transaction
;; to retry if they become inconsistent, even if tx-2 winds up being committed.

(define-test test-or-else-tx1-commit-log ()
  (let (Tuple4 count-1-run count-2-run tx1-successfully-wrote tx2-successfully-wrote) =
    (run-io!
     (do
      (count-1-run <- (new-var 0))
      (count-2-run <- (new-var 0))
      (read-in-tx1 <- (new-tvar 0))
      (write-in-tx1 <- (new-tvar False))
      (write-in-tx2 <- (new-tvar False))
      (retry-gate <- new-empty-mvar)
      (allow-tx2-finish-gate <- new-empty-mvar)
      (thd <-
        (do-fork-future_
          (do-run-tx
            (or-else (do
                      (tx-io!% (modify count-1-run 1+))
                      (read-tvar read-in-tx1)
                      (write-tvar write-in-tx1 True)
                      retry)
                     (do
                      (tx-io!% (modify count-2-run 1+))
                      (write-tvar write-in-tx2 True)
                      (tx-io!% (try-put-mvar retry-gate Unit))
                      (tx-io!% (take-mvar allow-tx2-finish-gate)))))))
      ;; Wait for tx-2 to finish the first time
      (take-mvar retry-gate)
      (run-tx (write-tvar read-in-tx1 -100)) ; Dirtying tx1's read log should trigger or-else to re-run
      (put-mvar allow-tx2-finish-gate Unit)
      ;; Allow tx-2 to complete the second time
      (put-mvar allow-tx2-finish-gate Unit)
      ;; Wait for final transaction to finish
      (await thd)
      ;; Check the results
      (count-1-run <- (read count-1-run))
      (count-2-run <- (read count-2-run))
      (write-in-tx1 <- (run-tx (read-tvar write-in-tx1)))
      (write-in-tx2 <- (run-tx (read-tvar write-in-tx2)))
      (pure (Tuple4 count-1-run count-2-run write-in-tx1 write-in-tx2)))))
  (is (== 2 count-1-run))
  (is (== 2 count-2-run))
  (is (== False tx1-successfully-wrote))
  (is (== True tx2-successfully-wrote)))
