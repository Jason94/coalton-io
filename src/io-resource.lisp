(cl:in-package :cl-user)
(defpackage :io/resource
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/experimental/do-control-core
   #:io/classes/thread
   #:io/classes/exceptions
   #:io/gen-impl/thread
   #:coalton-library/types)
  (:export
   #:ExitCase
   #:Completed
   #:Errored

   #:bracket-lifecycle-masked
   #:bracket-masked
   #:bracket-unmasked
   ))
(in-package :io/resource)

(named-readtables:in-readtable coalton:coalton)

;; NOTE: This package is largely based on the Cats bracket-io & resource types.
;; See https://typelevel.org/cats-effect/docs/std/resource.

(coalton-toplevel

  (derive Eq)
  (repr :enum)
  (define-type ExitCase
    "Signals the exit condition for an effectful computation using some resource."
    Completed
    Errored)

  (declare bracket-lifecycle-masked ((Exceptions :m) (Threads :rt :t :m)
                                    => :m :r
                                    * (:r * ExitCase -> :m :a)
                                    * (:r -> :m :b)
                                    -> :m :b))
  (define (bracket-lifecycle-masked acquire-op release-op computation-op)
    "Acquire a resource, run a computation with it, and release it. Guarantees that
RELEASE-OP will run if ACQUIRE-OP completes. If COMPUTATION-OP raises an exception,
it will be re-raised after the resource cleans up. If ACQUIRE-OP or RELEASE-OP raise
an exception, then release is not guaranteed.

Concurrent:
- Masks the thread during resource acquisition and release.
- The computation is not masked, and if another thread stops this one during the
  computation then the resource the resource will not be released."
    (do
     (resource <- (with-mask acquire-op))
     (result? <- (try-dynamic (computation-op resource)))
     (with-mask
         (do-match result?
           ((Ok result)
            (release-op resource Completed)
            (pure result))
           ((Err e)
            (release-op resource Errored)
            (raise-dynamic e))))))

  (declare bracket-masked ((Exceptions :m) (Threads :rt :t :m)
                           => :m :r
                           * (:r * ExitCase -> :m :a)
                           * (:r -> :m :b)
                           -> :m :b))
  (define (bracket-masked acquire-op release-op computation-op)
    "Acquire a resource, run a computation with it, and release it. Guarantees that RELEASE-OP will run
if ACQUIRE-OP completes. If COMPUTATION-OP raises an exception, it will be re-raised after the
resource cleans up. If ACQUIRE-OP or RELEASE-OP raise an exception, then release is not guaranteed.
Masks the thread during the entire operation, including the computation."
    (do-with-mask
     (resource <- acquire-op)
     (result? <- (try-dynamic (computation-op resource)))
     (do-match result?
       ((Ok result)
        (release-op resource Completed)
        (pure result))
       ((Err e)
        (release-op resource Errored)
        (raise-dynamic e)))))

  (declare bracket-unmasked ((Exceptions :m) (Threads :rt :t :m)
                             => :m :r
                             * (:r * ExitCase -> :m :a)
                             * (:r -> :m :b)
                             -> :m :b))
  (define (bracket-unmasked acquire-op release-op computation-op)
    (do
     (resource <- acquire-op)
     (result? <- (try-dynamic (computation-op resource)))
     (do-match result?
       ((Ok result)
        (release-op resource Completed)
        (pure result))
       ((Err e)
        (release-op resource Errored)
        (raise-dynamic e)))))
  )
