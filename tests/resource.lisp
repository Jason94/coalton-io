(defpackage :coalton-io/tests/resource
  (:use #:coalton #:coalton-prelude #:coalton-testing
   #:coalton-library/experimental/do-control-core
   #:io/monad-io
   #:io/simple-io
   #:io/mut
   #:io/resource
   #:io/thread
   #:io/threads-exceptions
   #:io/conc/mvar
   #:io/exceptions)
  (:import-from #:io/term
   #:write-line)
  (:local-nicknames
   (:s #:coalton-threads/semaphore)
   )
  )
(in-package :coalton-io/tests/resource)

(named-readtables:in-readtable coalton:coalton)

(fiasco:define-test-package #:coalton-io/tests/resource-fiasco)

(coalton-fiasco-init #:coalton-io/tests/resource-fiasco)

(coalton-toplevel
  (derive Eq)
  (repr :lisp)
  (define-type BracketError
    (BE String))

  (define-instance (Signalable BracketError)
    (define (error (BE s))
      (error s))))

;; NOTE: Not using MVar's for this because they *also* mask.

(coalton-toplevel
  (declare s-new (MonadIo :m => :m s:Semaphore))
  (define s-new
    (wrap-io (s:new)))

  (define (s-signal s)
    (wrap-io (s:signal s 1)))

  (define (s-await s)
    (wrap-io (s:await s)))
  )

(define-test test-bracket-io_-cleanup-on-error ()
  (let result =
    (run-io!
     (do
      (cleanup <- (new-var False))
      (err <- (try (bracket-io_ (pure Unit)
                                (const (write cleanup True))
                                (fn (_) (raise-io_ (BE "Raised Error"))))))
      (cleaned? <- (read cleanup))
      (pure (Tuple err cleaned?)))))
  (is (== (Tuple (Err (BE "Raised Error")) True)
          result)))

(define-test test-bracket-io_-cleans-up-when-stopped ()
  (let cleanup-completed? =
    (run-io!
     (do
      (cleanup <- (new-var False))
      (start-gate <- s-new)
      (cleanup-done-gate <- s-new)
      (wait-forever <- s-new)
      (thread <-
        (do-fork-thread_
          (bracket-io_
            (pure Unit)
            (fn (_) (do
                     (write cleanup True)
                     (s-signal cleanup-done-gate)))
            (fn (_) (do (s-signal start-gate)
                        (s-await wait-forever))))))
      ;; Ensure the computation has started before stopping it
      (s-await start-gate)
      (stop-thread thread)
      (s-await cleanup-done-gate)
      (read cleanup))))
  (is cleanup-completed?))

(define-test test-bracket-io-cleanup-receives-completed-status ()
  (let result =
    (run-io!
     (do
      (exit-case-result <- (new-var None))
      (bracket-io (pure Unit)
       (fn (_resource exit-case)
         (write exit-case-result (Some (the (ExitCase BracketError) exit-case))))
       (fn (_) (pure Unit)))
      (read exit-case-result))))
  (is (== (Some Completed) result)))

(define-test test-bracket-io-cleanup-on-error-with-exitcase ()
  (let result =
    (run-io!
     (do
      (cleanup <- (new-var False))
      (err <- (try (bracket-io (pure Unit)
                               (fn (_resource exit-case)
                                 (do-match exit-case
                                   ((Errored (BE msg))
                                    (write cleanup True)
                                    (pure exit-case))
                                   (_ (pure exit-case))))
                               (fn (_) (raise-io_ (BE "Raised Error"))))))
      (cleaned? <- (read cleanup))
      (pure (Tuple err cleaned?)))))
  (is (== (Tuple (Err (BE "Raised Error")) True)
          result)))

(define-test test-bracket-io-cleans-up-when-stopped ()
  (let cleanup-completed? =
    (run-io!
     (do
      (cleanup <- (new-var False))
      (start-gate <- s-new)
      (cleanup-done-gate <- s-new)
      (wait-forever <- s-new)
      (thread <-
        (do-fork-thread_
          (bracket-io
            (pure Unit)
            (fn (_resource exit-case)
              (do-match exit-case
                ((Errored (InterruptCurrentThread _))
                 (write cleanup True)
                 (s-signal cleanup-done-gate))
                (_
                 (s-signal cleanup-done-gate))))
            (fn (_) (do (s-signal start-gate)
                        (s-await wait-forever))))))
      ;; Ensure the computation has started before stopping it
      (s-await start-gate)
      (stop-thread thread)
      (sleep 5)
      (read cleanup))))
  (is (not cleanup-completed?)))
