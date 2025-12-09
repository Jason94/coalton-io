(cl:in-package :cl-user)
(defpackage :io/resource
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/types
   #:coalton-library/experimental/do-control-core
   #:io/utils
   #:io/monad-io
   #:io/exception
   #:io/thread)
  (:import-from #:coalton-library/types
   #:RuntimeRepr)
  (:import-from #:coalton-library/experimental/do-control-loops-adv
   #:LoopT)
  (:local-nicknames
   (:io #:io/simple-io)
   )
  (:export
   #:ExitCase
   #:Completed
   #:Errored

   #:bracket-io
   #:bracket-io_
   #:bracket-masked
   #:bracket-masked_
   #:bracket-no-mask
   #:bracket-no-mask_
   ))
(in-package :io/resource)

(named-readtables:in-readtable coalton:coalton)

;; NOTE: This package is largely based on the Cats bracket-io & resource types.
;; See https://typelevel.org/cats-effect/docs/std/resource.

(coalton-toplevel

  (derive Eq)
  (repr :lisp)
  (define-type (ExitCase :e)
    "Signals the exit condition for an effectful computation using some resource."
    Completed
   (Errored :e))

(declare bracket-io ((MonadIoThread :m IoThread) (MonadException :m) (Exception :e)
                       => :m :r
                       -> (:r -> ExitCase :e -> :m :a)
                       -> (:r -> :m :b)
                       -> :m :b))
(define (bracket-io acquire-op release-op computation-op)
  "First, acquire a resource with ACQUIRE-OP. Then run COMPUTATION-OP with the
resource. Finally, run RELEASE-OP on the resource and ExitCase of the computation.
Guarantees that RELEASE-OP will be run regardless of if COMPUTATION-OP raises
an exception. If COMPUTATION-OP raises an exception, it will be re-raised after the
resource cleans up. If ACQUIRE-OP or RELEASE-OP raise an exception,
then release is not guaranteed."
    (do
     (mask-current)
     (resource <- acquire-op)
     (soft-mask (do
                 (unmask-current-finally
                  (fn (mode)
                    (match mode
                      ((Running)
                       (pure Unit))
                      ((Stopped)
                       (do
                        (release-op resource (Errored (InterruptCurrentThread "")))
                        (pure Unit))))))
                 (computation-op resource))
                (fn (result?)
                  (match result?
                    ((Ok _)
                     (release-op resource Completed))
                    ((Err e)
                     (release-op resource (Errored e)))))))))

  (declare bracket-masked ((MonadIoThread :m IoThread) (MonadException :m) (Exception :e)
                           => :m :r
                           -> (:r -> ExitCase :e -> :m :a)
                           -> (:r -> :m :b)
                           -> :m :b))
  (define (bracket-masked acquire-op release-op computation-op)
    "Like BRACKET-IO but masks the current thread from before acquiring the resource
until after cleanup finishes. Uses a normal UNMASK to restore masking after
cleanup completes."
    (do
     (mask-current)
     (result? <-
      (try
       (do
        (resource <- acquire-op)
        (result? <- (try (computation-op resource)))
        (do-match result?
          ((Ok result)
           (release-op resource Completed)
           (pure result))
          ((Err e)
           (release-op resource (Errored e))
           (raise e))))))
     (unmask-current)
     (match result?
       ((Ok result)
        (pure result))
       ((Err e)
        (raise e)))))

  (declare bracket-no-mask ((MonadException :m) (Exception :e)
                            => :m :r
                            -> (:r -> ExitCase :e -> :m :a)
                            -> (:r -> :m :b)
                            -> :m :b))
  (define (bracket-no-mask acquire-op release-op computation-op)
    "Like BRACKET-IO but without masking cleanup operations."
    (do
     (resource <- acquire-op)
     (result? <- (try (computation-op resource)))
     (do-match result?
       ((Ok result)
        (release-op resource Completed)
        (pure result))
       ((Err e)
        (release-op resource (Errored e))
        (raise e)))))

(declare bracket-io_ ((MonadIoThread :m IoThread) (MonadException :m)
                       => :m :r
                       -> (:r -> :m :a)
                       -> (:r -> :m :b)
                       -> :m :b))
  (define (bracket-io_ acquire-op release-op computation-op)
    "First, acquire a resource with ACQUIRE-OP. Then run COMPUTATION-OP with the
resource. Finally, run RELEASE-OP on the resource and ExitCase of the computation.
This version runs RELEASE-OP for any kind of error, and doesn't take an ExitCase.
Cleanup runs under a soft mask to ensure it can't be interrupted."
    (do
     (mask-current)
     (resource <- acquire-op)
     (soft-mask (do
                 (unmask-current-finally
                  (fn (mode)
                    (match mode
                      ((Running)
                       (pure Unit))
                      ((Stopped)
                       (do
                        (release-op resource)
                        (pure Unit))))))
                 (computation-op resource))
                (const (release-op resource)))))

  (declare bracket-masked_ ((MonadIoThread :m IoThread) (MonadException :m)
                            => :m :r
                            -> (:r -> :m :a)
                            -> (:r -> :m :b)
                            -> :m :b))
  (define (bracket-masked_ acquire-op release-op computation-op)
    "Like BRACKET-IO_ but masks the current thread from before acquiring the
resource until after cleanup completes, restoring masking with UNMASK-CURRENT."
    (do
     (mask-current)
     (result? <-
      (try
       (do
        (resource <- acquire-op)
        (result? <- (try (computation-op resource)))
        (do-match result?
          ((Ok result)
           (release-op resource)
           (pure result))
          ((Err e)
           (release-op resource)
           (raise e))))))
     (unmask-current)
     (match result?
       ((Ok result)
        (pure result))
       ((Err e)
        (raise e)))))

  (declare bracket-no-mask_ (MonadException :m
                            => :m :r
                            -> (:r -> :m :a)
                            -> (:r -> :m :b)
                            -> :m :b))
  (define (bracket-no-mask_ acquire-op release-op computation-op)
    "Like BRACKET-IO_ but without masking the cleanup operation."
    (do
     (resource <- acquire-op)
     (reraise (do
               (result <- (computation-op resource))
               (release-op resource)
               (pure result))
              (const (release-op resource)))))
  )
