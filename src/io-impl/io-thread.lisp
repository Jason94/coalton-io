(cl:in-package :cl-user)
(defpackage :io/io-impl/thread
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/monad/classes
   #:io/thread-impl/runtime
   #:io/classes/monad-io-thread
   #:io/io-impl/simple-io
   )
  (:export
   #:fork_
   #:do-fork_
   ))
(in-package :io/io-impl/thread)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (declare fork_ ((MonadIoThread :m IoThread) (LiftTo IO :m)
                  => IO :a -> :m IoThread))
  (define fork_ fork)
  )

(cl:defmacro do-fork_ (cl:&body body)
  `(fork_
    (do
     ,@body)))
