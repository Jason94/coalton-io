(cl:in-package :cl-user)
(defpackage :io/io-impl/thread
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/monad/classes
   #:io/thread-exceptions
   #:io/thread-impl/runtime
   #:io/classes/monad-exception
   #:io/classes/monad-io-thread
   #:io/io-impl/runtime
   #:io/io-impl/simple-io
   )
  (:export
   #:fork-thread_
   #:fork-thread-throw_
   #:do-fork-thread_
   #:do-fork-thread-throw_
   #:unmask-thread-finally_
   #:unmask-finally_
   ))
(in-package :io/io-impl/thread)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (declare fork-thread_ ((MonadIoThread IoRuntime IoThread :m) (LiftTo IO :m)
                  => IO :a -> :m IoThread))
  (define fork-thread_ fork-thread)

  (declare fork-thread-throw_ ((MonadIoThread IoRuntime IoThread :m) (LiftTo IO :m)
                  => IO :a -> :m IoThread))
  (define fork-thread-throw_ fork-thread-throw)

  (declare unmask-thread-finally_ ((LiftTo IO :m) (MonadException :m)
                            (MonadIoThread IoRuntime IoThread :m)
                            => IoThread -> (UnmaskFinallyMode -> IO :b) -> :m Unit))
  (define unmask-thread-finally_ unmask-thread-finally)

  (declare unmask-finally_ ((LiftTo IO :m) (MonadException :m) (Concurrent :c :a)
                            (MonadIoThread IoRuntime IoThread :m)
                            => :c -> (UnmaskFinallyMode -> IO :b) -> :m Unit))
  (define unmask-finally_ unmask-finally)
  )

(defmacro do-fork-thread_ (cl:&body body)
  `(fork-thread_
    (do
     ,@body)))

(defmacro do-fork-thread-throw_ (cl:&body body)
  `(fork-thread-throw_
    (do
     ,@body)))
