(cl:in-package :cl-user)
(defpackage :io/classes/monad-exception
  (:use
   #:coalton
   #:coalton-prelude
   #:io/utils)
  (:import-from #:coalton-library/types
   #:RuntimeRepr)
  (:export
   #:MonadException
   #:raise
   #:raise-dynamic
   #:reraise
   #:handle
   #:handle-all
   #:try-dynamic))
(in-package :io/classes/monad-exception)

(named-readtables:in-readtable coalton:coalton)

;; NOTE: Unlike most IO classes, this one can't be implemented on top of
;; standard MonadIo functionality. As such, it has no implement macro.
;;
;; Also, `handle` can't be simply lifted with `lift`, so there's no
;; derive monad either. See the implementations for the Std. Library
;; transformers below for examples.

(coalton-toplevel
  ;; NOTE: The second argument for several of these could be :m :a. Wrapping
  ;; in a function call allows transformer instances to avoid running-down
  ;; to the base MonadException layer in the stack, even if no
  ;; exceptions are raised. The alternative is to limit those functions to
  ;; just instances of UnliftIo.
  (define-class (Monad :m => MonadException :m)
    "A Monad that can raise and handle exceptions. IMPORTANT: Any MonadException
must catch and wrap all unhandled errors inside a wrap-io call as an UnhandledError.
See utils/catch-thunk."
    (raise
     "Raise an exception."
     ((RuntimeRepr :e) (Signalable :e) => :e -> :m :a))
    (raise-dynamic
     "Raise an exception wrapped in a Dynamic. Mainly useful to hand-off eexceptions
between IO instances."
     (Dynamic -> :m :a))
    (reraise
     "Run an operation, run a catch operation if the first operation raised,
then re-raise the exception. If the catch operation raises, that exception will
be emitted instead of the original exception."
     (:m :a -> (Unit -> :m :b) -> :m :a))
    (handle
     "Run an operation, immediately handling if it raised an exception
that matches :e."
     (RuntimeRepr :e => :m :a -> (:e -> :m :a) -> :m :a))
    (handle-all
     "Run an operation, immediately handling any exceptions raised."
     (:m :a -> (Unit -> :m :a) -> :m :a))
    (try-dynamic
     "Bring any unhandled exceptions into a Result wrapped in Dynamic."
     (:m :a -> :m (Result Dynamic :a)))))
