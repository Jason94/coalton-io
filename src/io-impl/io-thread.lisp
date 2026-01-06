(cl:in-package :cl-user)
(defpackage :io/io-impl/thread
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/monad/classes
   #:io/thread-exceptions
   #:io/thread-impl/runtime
   #:io/classes/monad-io
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
   #:park-current-thread-if_
   ))
(in-package :io/io-impl/thread)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (inline)
  (declare fork-thread_ ((MonadIoThread IoRuntime IoThread :m) (LiftTo IO :m)
                  => IO :a -> :m IoThread))
  (define fork-thread_ fork-thread)

  (inline)
  (declare fork-thread-throw_ ((MonadIoThread IoRuntime IoThread :m) (LiftTo IO :m)
                  => IO :a -> :m IoThread))
  (define fork-thread-throw_ fork-thread-throw)

  (inline)
  (declare unmask-thread-finally_ ((LiftTo IO :m) (MonadException :m)
                            (MonadIoThread IoRuntime IoThread :m)
                            => IoThread -> (UnmaskFinallyMode -> IO :b) -> :m Unit))
  (define unmask-thread-finally_ unmask-thread-finally)

  (inline)
  (declare unmask-finally_ ((LiftTo IO :m) (MonadException :m) (Concurrent :c :a)
                            (MonadIoThread IoRuntime IoThread :m)
                            => :c -> (UnmaskFinallyMode -> IO :b) -> :m Unit))
  (define unmask-finally_ unmask-finally)

  (inline)
  (declare park-current-thread-if_ (MonadIo :m
                                    => (Generation -> IO Unit) -> IO Boolean -> :m Unit))
  (define park-current-thread-if_
    "Parks the current thread if SHOULD-PARK? returns True. Will park the thread until
woken by an unpark from another thread. Upon an unpark, the thread will resume even if
SHOULD-PARK? is False! SHOULD-PARK? is only checked to determine if the thread should
park, *not* if it should resume.

Concurrent:
  - WARNING: SHOULD-PARK? must not block, or the thread could be left blocked in a masked
    state.
  - Can briefly block while trying to park the thread, if contended."
    park-current-thread-if)
  )

(defmacro do-fork-thread_ (cl:&body body)
  `(fork-thread_
    (do
     ,@body)))

(defmacro do-fork-thread-throw_ (cl:&body body)
  `(fork-thread-throw_
    (do
     ,@body)))
