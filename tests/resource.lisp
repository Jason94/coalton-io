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
   #:io/exceptions
   #:io/tests/utils)
  (:import-from #:io/term
   #:write-line)
  (:local-nicknames
   (:bt #:io/utilities/bt-compat)
   ))
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
      (error s)))
  )

(define-test test-bracket-lifecycle-masked-cleanup-on-error ()
  (let result =
    (run-io!
     (do
      (cleanup <- (new-var False))
      (err <- (try (bracket-lifecycle-masked (pure Unit)
                                (const (write cleanup True))
                                (fn (_) (raise-io_ (BE "Raised Error"))))))
      (cleaned? <- (read cleanup))
      (pure (Tuple err cleaned?)))))
  (is (== (Tuple (Err (BE "Raised Error")) True)
          result)))

(define-test test-bracket-lifecycle-masked-cleans-up-when-stopped ()
  (let cleanup-completed? =
    (run-io!
     (do
      (cleanup <- (new-var False))
      (start-gate <- s-new)
      (cleanup-done-gate <- s-new)
      (wait-forever <- s-new)
      (thread <-
        (do-fork-thread_
          (bracket-lifecycle-masked
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
  (is (== True
          cleanup-completed?)))

(define-test test-bracket-lifecycle-masked-case-cleanup-receives-completed-status ()
  (let result =
    (run-io!
     (do
      (exit-case-result <- (new-var None))
      (bracket-lifecycle-masked-case (pure Unit)
       (fn (_resource exit-case)
         (write exit-case-result (Some exit-case)))
       (fn (_) (pure Unit)))
      (read exit-case-result))))
  (is (== (Some Completed) result)))

(define-test test-bracket-lifecycle-masked-case-cleanup-on-error-with-exitcase ()
  (let result =
    (run-io!
     (do
      (cleanup <- (new-var False))
      (err <- (try (bracket-lifecycle-masked-case (pure Unit)
                               (fn (_resource exit-case)
                                 (do-match exit-case
                                   ((Errored)
                                    (write cleanup True)
                                    (pure exit-case))
                                   (_ (pure exit-case))))
                               (fn (_) (raise-io_ (BE "Raised Error"))))))
      (cleaned? <- (read cleanup))
      (pure (Tuple err cleaned?)))))
  (is (== (Tuple (Err (BE "Raised Error")) True)
          result)))

(define-test test-bracket-lifecycle-masked-case-cleans-up-when-stopped ()
  (let cleanup-completed? =
    (run-io!
     (do
      (cleanup <- (new-var False))
      (start-gate <- s-new)
      (cleanup-done-gate <- s-new)
      (wait-forever <- s-new)
      (thread <-
        (do-fork-thread_
          (bracket-lifecycle-masked-case
            (pure Unit)
            (fn (_resource exit-case)
              (do-match exit-case
                ((Errored)
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
  (is (== True
          cleanup-completed?)))
