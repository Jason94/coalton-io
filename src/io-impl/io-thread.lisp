(cl:in-package :cl-user)
(defpackage :io/io-impl/thread
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/monad/classes
   #:io/thread-impl/runtime
   #:io/classes/monad-io-thread
   #:io/io-impl/runtime
   #:io/io-impl/simple-io
   )
  (:export
   #:fork-thread_
   #:do-fork-thread_
   ))
(in-package :io/io-impl/thread)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (declare fork-thread_ ((MonadIoThread IoRuntime IoThread :m) (LiftTo IO :m)
                  => IO :a -> :m IoThread))
  (define fork-thread_ fork-thread)
  )

(cl:defmacro do-fork-thread_ (cl:&body body)
  `(fork-thread_
    (do
     ,@body)))
