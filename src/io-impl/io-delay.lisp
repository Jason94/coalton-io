(cl:in-package :cl-user)
(defpackage :io/io-impl/delay
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/monad/classes
   #:io/threads-exceptions
   #:io/classes/monad-io
   #:io/classes/exceptions
   #:io/classes/thread
   #:io/classes/delay
   #:io/io-impl/io-runtime/runtime
   #:io/io-impl/io-runtime/delay
   #:io/io-impl/runtime
   #:io/io-impl/simple-io
   )
  (:export
   ;; Library Public
   #:fork-delay_
   #:do-fork-delay_
   ))
(in-package :io/io-impl/delay)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (inline)
  (declare fork-delay_ ((LiftTo IO :m)
                         => IO :a
                         &key
                         (:unhandled UnhandledExceptionStrategy)
                         (:scope (ForkScope IoThread))
                         -> :m (IoDelay :a)))
  (define (fork-delay_ op &key (unhandled LogAndSwallow) (scope Structured))
    "Delay a calculation in an IoThread. Can specify an unhandled exception strategy and
fork scope."
    (fork-delay op :unhandled unhandled :scope scope))
  )

(defmacro do-fork-delay_ (cl:&body forms)
  (cl:multiple-value-bind (unhandled scope body)
      (parse-fork-keywords% forms)
    `(fork-delay_
      (do
       ,@body)
      :unhandled ,unhandled
      :scope ,scope)))
