(cl:in-package :cl-user)
(defpackage :io/resource
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/types
   #:coalton-library/experimental/do-control-core
   #:io/utils
   #:io/thread
   #:io/monad-io
   #:io/exception)
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
   #:with-mask
   #:do-with-mask
   ))
(in-package :io/resource)

(named-readtables:in-readtable coalton:coalton)

;; NOTE: This package is largely based on the Cats bracket-io & resource types.
;; See https://typelevel.org/cats-effect/docs/std/resource.

(coalton-toplevel

  ;; TODO: Move this and the macro back to io-thread once the classes are
  ;; split and moved up front in the load order.
  (declare with-mask ((MonadIoThread :m :t) (MonadException :m)
                      => :m :a -> :m :a))
  (define (with-mask op)
    "Mask the current thread while running OP, automatically unmasking
afterward."
    (do
     mask-current
     (reraise
      (do
       (result <- op)
       unmask-current
       (pure result))
      (fn (_)
        unmask-current))))

  (derive Eq)
  (repr :lisp)
  (define-type (ExitCase :e)
    "Signals the exit condition for an effectful computation using some resource."
    Completed
    (Errored :e))

  ;; TODO: This will handle 99.99% of cases. But for the *very* rare case where
  ;; the thread receives a stop during an intermediate period of the IO monad
  ;; machinery in between finishing, for example, acquiring and starting the
  ;; computation, then this will throw back up to run-io-handled!%. The only
  ;; way to fix that *very* rare corner case is to add the ability to store retry
  ;; operations in the IO type itself, and have run-io-handled!% call those
  ;; as a last resort.
  ;; See tests/thread-async-boundary.lisp for more.
  (declare bracket-io ((MonadException :m) (MonadIoThread :m :t) (RuntimeRepr :e) (Signalable :e)
                       => :m :r
                       -> (:r -> ExitCase :e -> :m :a)
                       -> (:r -> :m :b)
                       -> :m :b))
  (define (bracket-io acquire-op release-op computation-op)
    "Acquire a resource, run a computation with it, and release it. Guarantees that
RELEASE-OP will run if ACQUIRE-OP completes. If COMPUTATION-OP raises an exception,
it will be re-raised after the resource cleans up. If ACQUIRE-OP or RELEASE-OP raise
an exception, then release is not guaranteed.

Masks the thread during resource acquisition and release. The computation is not
masked, but if another thread stops this one during the computation then the resource
will release before the thread is stopped."
    (do
     (resource <- (with-mask acquire-op))
     (result? <- (try (computation-op resource)))
     (with-mask
         (do-match result?
           ((Ok result)
            (release-op resource Completed)
            (pure result))
           ((Err e)
            (release-op resource (Errored e))
            (raise e))))))

  (declare bracket-io_ ((MonadException :m) (MonadIoThread :m :t)
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

  )

(cl:defmacro do-with-mask (cl:&body body)
  "Evaluate BODY with the current thread masked, automatically unmasking
afterward."
  `(with-mask
     (do
      ,@body)))
