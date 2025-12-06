(defpackage :coalton-io/tests/stm
  (:use #:coalton #:coalton-prelude #:coalton-testing
        #:coalton-library/experimental/do-control-core
        #:io/utils
        #:io/simple-io
        #:io/exception
        #:io/mut
        #:io/mvar
        #:io/thread
        #:io/future
        #:io/stm)
  (:import-from #:io/term
   #:write-line)
  (:import-from #:io/stm/stm-impl
   #:tx-io!%)
  (:local-nicknames
   (:l #:coalton-library/list))
  )
(in-package :coalton-io/tests/stm)

(named-readtables:in-readtable coalton:coalton)

(fiasco:define-test-package #:coalton-io/tests/stm-fiasco)
(coalton-fiasco-init #:coalton-io/tests/stm-fiasco)

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
        (do-fork-future
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
        (fork-future
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
      (do-fork
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
        (fork-future
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
