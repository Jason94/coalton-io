(cl:in-package :cl-user)
(defpackage :io/resource
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/experimental/do-control-core
   #:io/classes/thread
   #:io/classes/exceptions
   #:coalton-library/types)
  (:export
   #:ExitCase
   #:Completed
   #:Errored

   #:bracket-io
   #:bracket-io_
   #:bracket-io-masked
   #:bracket-io-masked_
   #:with-mask
   #:do-with-mask
   ))
(in-package :io/resource)

(named-readtables:in-readtable coalton:coalton)

;; NOTE: This package is largely based on the Cats bracket-io & resource types.
;; See https://typelevel.org/cats-effect/docs/std/resource.

(coalton-toplevel

  (declare with-mask ((Threads :rt :t :m) (Exceptions :m)
                      => :m :a -> :m :a))
  (define (with-mask op)
    "Mask the current thread while running OP, automatically unmasking
afterward."
    (do
     mask-current-thread
     (reraise
      (do
       (result <- op)
       unmask-current-thread
       (pure result))
      (fn (_)
        unmask-current-thread))))
  )

(defmacro do-with-mask (cl:&body body)
  "Evaluate BODY with the current thread masked, automatically unmasking
afterward."
  `(with-mask
     (do
      ,@body)))

(coalton-toplevel

  (derive Eq)
  (repr :lisp)
  (define-type (ExitCase :e)
    "Signals the exit condition for an effectful computation using some resource."
    Completed
    (Errored :e))

  (inline)
  (declare try-result ((Exceptions :m) (RuntimeRepr :e)
                       => :m :a -> :m (Result :e :a)))
  (define (try-result op)
    (handle
     (map Ok op)
     (compose pure Err)))

  (declare bracket-io ((Exceptions :m) (Threads :rt :t :m) (RuntimeRepr :e) (Signalable :e)
                       => :m :r
                       -> (:r -> ExitCase :e -> :m :a)
                       -> (:r -> :m :b)
                       -> :m :b))
  (define (bracket-io acquire-op release-op computation-op)
    "WARNING: BRACKET-IO will *only* cleanup if the raised exception matches :e,
or if the computation succeedes. To guarantee cleanup after any exception,
use BRACKET-IO_

Acquire a resource, run a computation with it, and release it. Guarantees that
RELEASE-OP will run if ACQUIRE-OP completes. If COMPUTATION-OP raises an exception,
it will be re-raised after the resource cleans up. If ACQUIRE-OP or RELEASE-OP raise
an exception, then release is not guaranteed.

Concurrent:
- Masks the thread during resource acquisition and release.
- The computation is not masked, and if another thread stops this one during the
  computation then the resource the resource will not be released."
    (do
     (resource <- (with-mask acquire-op))
     (result? <- (try-result (computation-op resource)))
     (with-mask
         (do-match result?
           ((Ok result)
            (release-op resource Completed)
            (pure result))
           ((Err e)
            (release-op resource (Errored e))
            (raise e))))))

  (declare bracket-io-masked ((Exceptions :m) (Threads :rt :t :m) (RuntimeRepr :e) (Signalable :e)
                              => :m :r
                              -> (:r -> ExitCase :e -> :m :a)
                              -> (:r -> :m :b)
                              -> :m :b))
  (define (bracket-io-masked acquire-op release-op computation-op)
    "WARNING: BRACKET-IO-MASKED will *only* cleanup if the raised exception matches :e, or if the
computation succeedes. To guarantee cleanup after any exception, use BRACKET-IO-MASKED_

Acquire a resource, run a computation with it, and release it. Guarantees that RELEASE-OP will run
if ACQUIRE-OP completes. If COMPUTATION-OP raises an exception, it will be re-raised after the
resource cleans up. If ACQUIRE-OP or RELEASE-OP raise an exception, then release is not guaranteed.
Masks the thread during the entire operation, including the computation."
    (do-with-mask
     (resource <- acquire-op)
     (result? <- (try-result (computation-op resource)))
     (do-match result?
       ((Ok result)
        (release-op resource Completed)
        (pure result))
       ((Err e)
        (release-op resource (Errored e))
        (raise e)))))

  (declare bracket-io_ ((Exceptions :m) (Threads :rt :t :m)
                        => :m :r
                        -> (:r -> :m :a)
                        -> (:r -> :m :b)
                        -> :m :b))
  (define (bracket-io_ acquire-op release-op computation-op)
    "Acquire a resource, run a computation with it, and release it. Guarantees that
RELEASE-OP will run if ACQUIRE-OP completes. If COMPUTATION-OP raises an exception,
it will be re-raised after the resource cleans up. If ACQUIRE-OP or RELEASE-OP raise
an exception, then release is not guaranteed.

Masks the thread during resource acquisition and release. The computation is not
masked, but if another thread stops this one during the computation then the resource
will release before the thread is stopped."
    (do
     (resource <- (with-mask acquire-op))
     (reraise (do
               (result <- (computation-op resource))
               (with-mask (release-op resource))
               (pure result))
              (fn (_)
                (with-mask (release-op resource))))))

  (declare bracket-io-masked_ ((Exceptions :m) (Threads :rt :t :m)
                               => :m :r
                               -> (:r -> :m :a)
                               -> (:r -> :m :b)
                               -> :m :b))
  (define (bracket-io-masked_ acquire-op release-op computation-op)
    "Acquire a resource, run a computation with it, and release it. Guarantees that RELEASE-OP will
run if ACQUIRE-OP completes. If COMPUTATION-OP raises an exception, it will be re-raised after the
resource cleans up. If ACQUIRE-OP or RELEASE-OP raise an exception, then release is not guaranteed.
Masks the thread during the entire operation, including the computation."
    (do-with-mask
     (resource <- acquire-op)
     (reraise (do
               (result <- (computation-op resource))
               (release-op resource)
               (pure result))
              (fn (_)
                (release-op resource)))))

  )
