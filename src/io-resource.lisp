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
   #:bracket-lifecycle-masked-case
   #:bracket-masked
   #:bracket-masked-case
   #:bracket-unmasked
   #:bracket-unmasked-case
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

  (declare bracket-unmasked ((Exceptions :m) (Threads :rt :t :m)
                             => :m :r
                             * (:r -> :m :a)
                             * (:r -> :m :b)
                             -> :m :b))
  (define (bracket-unmasked acquire-op release-op computation-op)
    "Acquire a resource, run a computation with it, and release it. Guarantees that
`release-op` will run if `computation-op` raises. If `computation-op` raises an exception,
it will be re-raised after the resource cleans up. If `acquire-op` or `release-op` raise
an exception, then release is not guaranteed.

Concurrent:
  - Does not mask.
  - Cannot guarantee resource will be released on an asynchronous stop. If you need to
    guarantee release during an asynchronous stop, use `bracket-lifecycle-masked` or
    `bracket-masked`."
    ;; CONCURRENT:
    ;; To elaborate on why it isn't possible to design this function to be safe in the
    ;; presence of asynchronous stops:
    ;;   - `acquire-op` acquires the resource and returns it to this function. That means
    ;;     that this function cannot distinguish between the following cases:
    ;;      (1) the thread was stopped during `acquire-op` but before it acquired the resource
    ;;      (2) the thread was stopped during `acquire-op` after it acquired the resource
    ;;   - Even if this function could distinguish between the two, it couldn't even call
    ;;     `release-op` until `acquire-op` had returned the resource. An asynchronous stop
    ;;     could happen between those two events.
    ;; Put another way:
    ;;   - The time from the initial call to `acquire-op` and the "attachment" of the acquired
    ;;     resource to the `resource` variable is a critical region, during which the program
    ;;     could be stopped and potentially left invalid.
    ;;   - The critical region can't be eliminated by atomics, masking, or similar means
    ;;     given the constraints of not masking and the opaque `acquire-op`.
    ;;   - In this case, that is exactly what `bracket-lifecycle-masked` does by masking.
    (do
     (resource <- acquire-op)
     (finally
      (computation-op resource)
      (release-op resource))))

  (declare bracket-unmasked-case ((Exceptions :m) (Threads :rt :t :m)
                                  => :m :r
                                  * (:r * ExitCase -> :m :a)
                                  * (:r -> :m :b)
                                  -> :m :b))
  (define (bracket-unmasked-case acquire-op release-op computation-op)
    "Acquire a resource, run a computation with it, and release it. Guarantees that
`release-op` will run if `computation-op` raises. If COMPUTATION-OP raises an exception,
it will be re-raised after the resource cleans up. If `acquire-op` or `release-op` raise
an exception, then release is not guaranteed.

`release-op` receives both the acquired resource and an `ExitCase` indicating whether
the computation completed successfully (Completed) or errored (Errored).

Concurrent:
  - Does not mask.
  - Cannot guarantee resource will be released on an asynchronous stop. If you need to
    guarantee release during an asynchronous stop, use `bracket-lifecycle-masked-case`
    or `bracket-masked-case`."
    ;; CONCURRENT: See `bracket-unmasked` for explanation of concurrent limitations.
    (do
     (resource <- acquire-op)
     (on-success-or-exception
      (computation-op resource)
      (release-op resource Completed)
      (release-op resource Errored))))
  
  (inline)
  (declare bracket-masked ((Exceptions :m) (Threads :rt :t :m)
                           => :m :r
                           * (:r -> :m :a)
                           * (:r -> :m :b)
                           -> :m :b))
  (define (bracket-masked acquire-op release-op computation-op)
    "Acquire a resource, run a computation with it, and release it. Guarantees that RELEASE-OP will run
if ACQUIRE-OP completes. If COMPUTATION-OP raises an exception, it will be re-raised after the
resource cleans up. If ACQUIRE-OP or RELEASE-OP raise an exception, then release is not guaranteed.

The entire sequence is masked, including `computation-op`.

Concurrent:
  - Masks the thread before ACQUIRE-OP starts.
  - Unmasks the thread after RELEASE-OP finishes."
    (do-with-mask
      (bracket-unmasked acquire-op release-op computation-op)))
  
  (inline)
  (declare bracket-masked-case ((Exceptions :m) (Threads :rt :t :m)
                                => :m :r
                                * (:r * ExitCase -> :m :a)
                                * (:r -> :m :b)
                                -> :m :b))
  (define (bracket-masked-case acquire-op release-op computation-op)
    "Acquire a resource, run a computation with it, and release it. Guarantees that RELEASE-OP will run
if ACQUIRE-OP completes. If COMPUTATION-OP raises an exception, it will be re-raised after the
resource cleans up. If ACQUIRE-OP or RELEASE-OP raise an exception, then release is not guaranteed.
RELEASE-OP receives both the acquired resource and an ExitCase indicating whether
the computation completed successfully (Completed) or errored (Errored).

The entire sequence is masked, including `computation-op`.

Concurrent:
  - Masks the thread before ACQUIRE-OP starts.
  - Unmasks the thread after RELEASE-OP finishes."
    (do-with-mask
      (bracket-unmasked-case acquire-op release-op computation-op)))

  (inline)
  (declare bracket-lifecycle-masked ((Exceptions :m) (Threads :rt :t :m)
                                    => :m :r
                                    * (:r -> :m :a)
                                    * (:r -> :m :b)
                                    -> :m :b))
  (define (bracket-lifecycle-masked acquire-op release-op computation-op)
    "Acquire a resource, run a computation with it, and release it. Guarantees that
RELEASE-OP will run if ACQUIRE-OP completes. If COMPUTATION-OP raises an exception,
it will be re-raised after the resource cleans up. If ACQUIRE-OP or RELEASE-OP raise
an exception, then release is not guaranteed.

RELEASE-OP receives only the acquired resource.

Concurrent:
  - Masks the thread during resource acquisition and release.
  - The computation is not masked, but if another thread stops this one during the
    computation then the resource the resource will still be released."
    (bracket-masked acquire-op release-op (map with-unmask computation-op)))

  (inline)
  (declare bracket-lifecycle-masked-case ((Exceptions :m) (Threads :rt :t :m)
                                         => :m :r
                                         * (:r * ExitCase -> :m :a)
                                         * (:r -> :m :b)
                                         -> :m :b))
  (define (bracket-lifecycle-masked-case acquire-op release-op computation-op)
    "Acquire a resource, run a computation with it, and release it. Guarantees that
RELEASE-OP will run if ACQUIRE-OP completes. If COMPUTATION-OP raises an exception,
it will be re-raised after the resource cleans up. If ACQUIRE-OP or RELEASE-OP raise
an exception, then release is not guaranteed.

RELEASE-OP receives both the acquired resource and an ExitCase indicating whether
the computation completed successfully (Completed) or errored (Errored).

Concurrent:
  - Masks the thread during resource acquisition and release.
  - The computation is not masked, but if another thread stops this one during the
    computation then the resource the resource will still be released."
    (bracket-masked-case acquire-op release-op (map with-unmask computation-op)))
  )
