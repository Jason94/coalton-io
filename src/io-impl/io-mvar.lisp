(cl:in-package :cl-user)
(defpackage :io/io-impl/mvar
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/monad/classes
   #:io/threads-impl/runtime
   #:io/classes/thread
   #:io/gen-impl/conc/mvar
   #:io/io-impl/runtime
   #:io/io-impl/simple-io
   )
  (:export
   #:with-mvar_
   #:do-with-mvar_
   ))
(in-package :io/io-impl/mvar)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (declare with-mvar_ ((Threads IoRuntime IoThread :m) (LiftTo IO :m)
                       => MVar :a -> (:a -> IO :b) -> :m :b))
  (define with-mvar_
    "Run an operation with the value from an MVar, blocking until one is available.
Restore the MVar value and return the result of the operation.

WARNING: If the computation raises an unhandled exception or is stopped, leaves the MVar
empty!

Concurrent:
  - WARNING: Does not mask during the computation. To ensure completion, caller must mask
  - Blocks while the MVar is empty
  - Inherits notify semantics from `put-mvar`
  - Does not leave the MVar locked during the computation. Thus, other threads can
    put the MVar during the computation and force `with-mvar` to block until empty."
    with-mvar)
  )

(defmacro do-with-mvar_ ((sym mvar) cl:&body body)
  "Run an operation with the value from an MVar, blocking until one is available.
Stores the result of the operation in the MVar and returns.

WARNING: If the computation raises an unhandled exception or is stopped, leaves the MVar
empty!

Concurrent:
  - WARNING: Does not mask during the computation. To ensure completion, caller must mask
  - Blocks while the MVar is empty
  - Inherits notify semantics from `put-mvar`
  - Does not leave the MVar locked during the computation. Thus, other threads can
    put the MVar during the computation and force `with-mvar` to block until empty."
  `(with-mvar_
     ,mvar
     (fn (,sym)
       (do
        ,@body))))
