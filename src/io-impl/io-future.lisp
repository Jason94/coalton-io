(cl:in-package :cl-user)
(defpackage :io/io-impl/future
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/monad/classes
   #:io/classes/monad-io-thread
   #:io/thread-impl/runtime
   #:io/gen-impl/conc/future
   #:io/io-impl/runtime
   #:io/io-impl/simple-io
   )
  (:export
   #:fork-future_
   #:do-fork-future_
   ))
(in-package :io/io-impl/future)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (declare fork-future_ ((MonadIoThread IoRuntime IoThread :m) (LiftTo IO :m)
                         => IO :a -> :m (Future :a)))
  (define fork-future_
    "Spawn a new future, which will run and eventually return the result
from TASK. The future is guaranteed to only ever run at most once, when
the produced :m is run."
    fork-future)
  )

(cl:defmacro do-fork-future_ (cl:&body body)
  `(fork-future_
    (do
     ,@body)))
