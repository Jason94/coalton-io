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
  (:import-from #:io/threads-impl/runtime
   #:is-masked?%)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                 bracket-lifecycle-masked                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-test test-bracket-lifecycle-masked-no-release-on-acquire-error ()
  (let result =
    (run-io!
     (do
      (release <- (new-var False))
      (err <- (try (bracket-lifecycle-masked (raise-io_ (BE "Raised Error in Acquire"))
                                             (const (write release True))
                                             ƒ_.(pure Unit))))
      (released? <- (read release))
      (pure (Tuple err released?)))))
  (is (== (Tuple (Err (BE "Raised Error in Acquire")) False)
          result)))

(define-test test-bracket-lifecycle-masked-release-on-operation-error ()
  (let result =
    (run-io!
     (do
      (release <- (new-var False))
      (err <- (try (bracket-lifecycle-masked (pure Unit)
                                             (const (write release True))
                                             (fn (_) (raise-io_ (BE "Raised Error"))))))
      (released? <- (read release))
      (pure (Tuple err released?)))))
  (is (== (Tuple (Err (BE "Raised Error")) True)
          result)))

(define-test test-bracket-lifecycle-masked-masks-during-acquire ()
  (let result =
    (run-io!
     (do
      (masked? <- (new-var False))
      (thread <- (do-fork-thread_
        (bracket-lifecycle-masked (do
                                   (t <- current-thread)
                                   (m? <- (wrap-io (is-masked?% t)))
                                   (write masked? m?)
                                   (pure Unit))
                                  ƒ_.(pure Unit)
                                  ƒ_.(pure Unit))))
      (join-thread thread)
      (read masked?))))
  (is (== True result)))

(define-test test-bracket-lifecycle-masked-unmasks-during-operation ()
  (let result =
    (run-io!
     (do
      (masked? <- (new-var False))
      (thread <- (do-fork-thread_
        (bracket-lifecycle-masked (pure Unit)
                                  ƒ_.(pure Unit)
                                  ƒ_.(do
                                      (t <- current-thread)
                                      (m? <- (wrap-io (is-masked?% t)))
                                      (write masked? m?)))))
      (join-thread thread)
      (read masked?))))
  (is (== False result)))

(define-test test-bracket-lifecycle-masked-masks-during-release ()
  (let result =
    (run-io!
     (do
      (masked? <- (new-var False))
      (thread <- (do-fork-thread_
        (bracket-lifecycle-masked (pure Unit)
                                  ƒ_.(do
                                      (t <- current-thread)
                                      (m? <- (wrap-io (is-masked?% t)))
                                      (write masked? m?))
                                  ƒ_.(pure Unit))))
      (join-thread thread)
      (read masked?))))
  (is (== True result)))

(define-test test-bracket-lifecycle-masked-unmasks-on-acquire-error ()
  (let result =
    (run-io!
     (do
      (thread <-
        (do-fork-thread_ :unhandled Swallow
          (bracket-lifecycle-masked (raise-io_ (BE "Raised Error in Acquire"))
                                    ƒ_.(pure Unit)
                                    ƒ_.(pure Unit))))
      (try-all (join-thread thread))
      (wrap-io (is-masked?% thread)))))
  (is (== False result)))

(define-test test-bracket-lifecycle-masked-unmasks-on-operation-error ()
  (let result =
    (run-io!
     (do
      (thread <-
        (do-fork-thread_ :unhandled Swallow
          (bracket-lifecycle-masked (pure Unit)
                                    ƒ_.(pure Unit)
                                    ƒ_.(raise-io_ (BE "Raised Error in Operation")))))
      (try-all (join-thread thread))
      (wrap-io (is-masked?% thread)))))
  (is (== False result)))

(define-test test-bracket-lifecycle-masked-unmasks-on-release-error ()
  (let result =
    (run-io!
     (do
      (thread <-
        (do-fork-thread_ :unhandled Swallow
          (bracket-lifecycle-masked (pure Unit)
                                    ƒ_.(raise-io_ (BE "Raised Error in Release"))
                                    ƒ_.(pure Unit))))
      (try-all (join-thread thread))
      (wrap-io (is-masked?% thread)))))
  (is (== False result)))

(define-test test-bracket-lifecycle-masked-releases-when-stopped-in-operation ()
  (let release-completed? =
    (run-io!
     (do
      (release <- (new-var False))
      (start-gate <- s-new)
      (release-done-gate <- s-new)
      (wait-forever <- s-new)
      (thread <-
        (do-fork-thread_
          (bracket-lifecycle-masked
            (pure Unit)
            (fn (_) (do
                     (write release True)
                     (s-signal release-done-gate)))
            (fn (_) (do (s-signal start-gate)
                        (s-await wait-forever))))))
      ;; Ensure the operation has started before stopping it
      (s-await start-gate)
      (stop-thread thread)
      (s-await release-done-gate)
      (read release))))
  (is (== True
          release-completed?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;              bracket-lifecycle-masked-case                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-test test-bracket-lifecycle-masked-case-no-release-on-acquire-error ()
  (let result =
    (run-io!
     (do
      (release <- (new-var False))
      (err <- (try (bracket-lifecycle-masked-case (raise-io_ (BE "Raised Error in Acquire"))
                                                  ƒ__.(write release True)
                                                  ƒ_.(pure Unit))))
      (released? <- (read release))
      (pure (Tuple err released?)))))
  (is (== (Tuple (Err (BE "Raised Error in Acquire")) False)
          result)))

(define-test test-bracket-lifecycle-masked-case-release-on-operation-error ()
  (let result =
    (run-io!
     (do
      (release <- (new-var False))
      (err <- (try (bracket-lifecycle-masked-case (pure Unit)
                                                  ƒ__.(write release True)
                                                  (fn (_) (raise-io_ (BE "Raised Error"))))))
      (released? <- (read release))
      (pure (Tuple err released?)))))
  (is (== (Tuple (Err (BE "Raised Error")) True)
          result)))

(define-test test-bracket-lifecycle-masked-case-masks-during-acquire ()
  (let result =
    (run-io!
     (do
      (masked? <- (new-var False))
      (thread <- (do-fork-thread_
        (bracket-lifecycle-masked-case (do
                                        (t <- current-thread)
                                        (m? <- (wrap-io (is-masked?% t)))
                                        (write masked? m?)
                                        (pure Unit))
                                       ƒ__.(pure Unit)
                                       ƒ_.(pure Unit))))
      (join-thread thread)
      (read masked?))))
  (is (== True result)))

(define-test test-bracket-lifecycle-masked-case-unmasks-during-operation ()
  (let result =
    (run-io!
     (do
      (masked? <- (new-var False))
      (thread <- (do-fork-thread_
        (bracket-lifecycle-masked-case (pure Unit)
                                       ƒ__.(pure Unit)
                                       ƒ_.(do
                                           (t <- current-thread)
                                           (m? <- (wrap-io (is-masked?% t)))
                                           (write masked? m?)))))
      (join-thread thread)
      (read masked?))))
  (is (== False result)))

(define-test test-bracket-lifecycle-masked-case-masks-during-release ()
  (let result =
    (run-io!
     (do
      (masked? <- (new-var False))
      (thread <- (do-fork-thread_
        (bracket-lifecycle-masked-case (pure Unit)
                                       ƒ__.(do
                                            (t <- current-thread)
                                            (m? <- (wrap-io (is-masked?% t)))
                                            (write masked? m?))
                                       ƒ_.(pure Unit))))
      (join-thread thread)
      (read masked?))))
  (is (== True result)))

(define-test test-bracket-lifecycle-masked-case-unmasks-on-acquire-error ()
  (let result =
    (run-io!
     (do
      (thread <-
        (do-fork-thread_ :unhandled Swallow
          (bracket-lifecycle-masked-case (raise-io_ (BE "Raised Error in Acquire"))
                                         ƒ__.(pure Unit)
                                         ƒ_.(pure Unit))))
      (try-all (join-thread thread))
      (wrap-io (is-masked?% thread)))))
  (is (== False result)))

(define-test test-bracket-lifecycle-masked-case-unmasks-on-operation-error ()
  (let result =
    (run-io!
     (do
      (thread <-
        (do-fork-thread_ :unhandled Swallow
          (bracket-lifecycle-masked-case (pure Unit)
                                         ƒ__.(pure Unit)
                                         ƒ_.(raise-io_ (BE "Raised Error in Operation")))))
      (try-all (join-thread thread))
      (wrap-io (is-masked?% thread)))))
  (is (== False result)))

(define-test test-bracket-lifecycle-masked-case-unmasks-on-release-error ()
  (let result =
    (run-io!
     (do
      (thread <-
        (do-fork-thread_ :unhandled Swallow
          (bracket-lifecycle-masked-case (pure Unit)
                                         ƒ__.(raise-io_ (BE "Raised Error in Release"))
                                         ƒ_.(pure Unit))))
      (try-all (join-thread thread))
      (wrap-io (is-masked?% thread)))))
  (is (== False result)))

(define-test test-bracket-lifecycle-masked-case-release-receives-completed-status ()
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

(define-test test-bracket-lifecycle-masked-case-release-on-error-with-exitcase ()
  (let result =
    (run-io!
     (do
      (release <- (new-var False))
      (err <- (try (bracket-lifecycle-masked-case (pure Unit)
                               (fn (_resource exit-case)
                                 (do-match exit-case
                                   ((Errored)
                                    (write release True)
                                    (pure exit-case))
                                   (_ (pure exit-case))))
                               (fn (_) (raise-io_ (BE "Raised Error"))))))
      (released? <- (read release))
      (pure (Tuple err released?)))))
  (is (== (Tuple (Err (BE "Raised Error")) True)
          result)))

(define-test test-bracket-lifecycle-masked-case-releases-when-stopped-in-operation ()
  (let release-completed? =
    (run-io!
     (do
      (release <- (new-var False))
      (start-gate <- s-new)
      (release-done-gate <- s-new)
      (wait-forever <- s-new)
      (thread <-
        (do-fork-thread_
          (bracket-lifecycle-masked-case
            (pure Unit)
            (fn (_resource exit-case)
              (do-match exit-case
                ((Errored)
                 (write release True)
                 (s-signal release-done-gate))
                (_
                 (s-signal release-done-gate))))
            (fn (_) (do (s-signal start-gate)
                        (s-await wait-forever))))))
      ;; Ensure the operation has started before stopping it
      (s-await start-gate)
      (stop-thread thread)
      (s-await release-done-gate)
      (read release))))
  (is (== True
          release-completed?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      bracket-masked                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-test test-bracket-masked-no-release-on-acquire-error ()
  (let result =
    (run-io!
     (do
      (release <- (new-var False))
      (err <- (try (bracket-masked (raise-io_ (BE "Raised Error in Acquire"))
                                   (const (write release True))
                                   ƒ_.(pure Unit))))
      (released? <- (read release))
      (pure (Tuple err released?)))))
  (is (== (Tuple (Err (BE "Raised Error in Acquire")) False)
          result)))

(define-test test-bracket-masked-release-on-operation-error ()
  (let result =
    (run-io!
     (do
      (release <- (new-var False))
      (err <- (try (bracket-masked (pure Unit)
                                   (const (write release True))
                                   (fn (_) (raise-io_ (BE "Raised Error"))))))
      (released? <- (read release))
      (pure (Tuple err released?)))))
  (is (== (Tuple (Err (BE "Raised Error")) True)
          result)))

(define-test test-bracket-masked-masks-during-acquire ()
  (let result =
    (run-io!
     (do
      (masked? <- (new-var False))
      (thread <- (do-fork-thread_
        (bracket-masked (do
                         (t <- current-thread)
                         (m? <- (wrap-io (is-masked?% t)))
                         (write masked? m?)
                         (pure Unit))
                        ƒ_.(pure Unit)
                        ƒ_.(pure Unit))))
      (join-thread thread)
      (read masked?))))
  (is (== True result)))

(define-test test-bracket-masked-masks-during-operation ()
  (let result =
    (run-io!
     (do
      (masked? <- (new-var False))
      (thread <- (do-fork-thread_
        (bracket-masked (pure Unit)
                        ƒ_.(pure Unit)
                        ƒ_.(do
                            (t <- current-thread)
                            (m? <- (wrap-io (is-masked?% t)))
                            (write masked? m?)))))
      (join-thread thread)
      (read masked?))))
  (is (== True result)))

(define-test test-bracket-masked-masks-during-release ()
  (let result =
    (run-io!
     (do
      (masked? <- (new-var False))
      (thread <- (do-fork-thread_
        (bracket-masked (pure Unit)
                        ƒ_.(do
                            (t <- current-thread)
                            (m? <- (wrap-io (is-masked?% t)))
                            (write masked? m?))
                        ƒ_.(pure Unit))))
      (join-thread thread)
      (read masked?))))
  (is (== True result)))

(define-test test-bracket-masked-unmasks-on-acquire-error ()
  (let result =
    (run-io!
     (do
      (thread <-
        (do-fork-thread_ :unhandled Swallow
          (bracket-masked (raise-io_ (BE "Raised Error in Acquire"))
                          ƒ_.(pure Unit)
                          ƒ_.(pure Unit))))
      (try-all (join-thread thread))
      (wrap-io (is-masked?% thread)))))
  (is (== False result)))

(define-test test-bracket-masked-unmasks-on-operation-error ()
  (let result =
    (run-io!
     (do
      (thread <-
        (do-fork-thread_ :unhandled Swallow
          (bracket-masked (pure Unit)
                          ƒ_.(pure Unit)
                          ƒ_.(raise-io_ (BE "Raised Error in Operation")))))
      (try-all (join-thread thread))
      (wrap-io (is-masked?% thread)))))
  (is (== False result)))

(define-test test-bracket-masked-unmasks-on-release-error ()
  (let result =
    (run-io!
     (do
      (thread <-
        (do-fork-thread_ :unhandled Swallow
          (bracket-masked (pure Unit)
                          ƒ_.(raise-io_ (BE "Raised Error in Release"))
                          ƒ_.(pure Unit))))
      (try-all (join-thread thread))
      (wrap-io (is-masked?% thread)))))
  (is (== False result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     bracket-unmasked                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-test test-bracket-unmasked-no-release-on-acquire-error ()
  (let result =
    (run-io!
     (do
      (release <- (new-var False))
      (err <- (try (bracket-unmasked (raise-io_ (BE "Raised Error in Acquire"))
                                     (const (write release True))
                                     ƒ_.(pure Unit))))
      (released? <- (read release))
      (pure (Tuple err released?)))))
  (is (== (Tuple (Err (BE "Raised Error in Acquire")) False)
          result)))

(define-test test-bracket-unmasked-release-on-operation-error ()
  (let result =
    (run-io!
     (do
      (release <- (new-var False))
      (err <- (try (bracket-unmasked (pure Unit)
                                     (const (write release True))
                                     (fn (_) (raise-io_ (BE "Raised Error"))))))
      (released? <- (read release))
      (pure (Tuple err released?)))))
  (is (== (Tuple (Err (BE "Raised Error")) True)
          result)))

(define-test test-bracket-unmasked-releases-when-stopped-in-operation ()
  (let release-completed? =
    (run-io!
     (do
      (release <- (new-var False))
      (start-gate <- s-new)
      (release-done-gate <- s-new)
      (wait-forever <- s-new)
      (thread <-
        (do-fork-thread_
          (bracket-unmasked
            (pure Unit)
            (fn (_) (do
                     (write release True)
                     (s-signal release-done-gate)))
            (fn (_) (do (s-signal start-gate)
                        (s-await wait-forever))))))
      ;; Ensure the operation has started before stopping it
      (s-await start-gate)
      (stop-thread thread)
      (s-await release-done-gate)
      (read release))))
  (is (== True
          release-completed?)))
