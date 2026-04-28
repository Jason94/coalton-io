(cl:in-package :cl-user)
(defpackage :io/io-impl/thread
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/monad/classes
   #:io/threads-exceptions
   #:io/threads-impl/runtime
   #:io/classes/monad-io
   #:io/classes/exceptions
   #:io/classes/thread
   #:io/io-impl/runtime
   #:io/io-impl/simple-io
   )
  (:export
   #:fork-thread_
   #:do-fork-thread_
   #:unmask-thread-finally_
   #:unmask-finally_
   #:park-current-thread-if_
   ))
(in-package :io/io-impl/thread)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (inline)
  (declare fork-thread_ ((Threads IoRuntime IoThread :m) (LiftTo IO :m)
                         => IO :a
                         &key
                         (:unhandled UnhandledExceptionStrategy)
                         (:scope (ForkScope IoThread))
                         -> :m IoThread))
  (define (fork-thread_ op &key (unhandled LogAndSwallow) (scope Structured))
    "Spawn a new IoThread. Can specify an unhandled exception strategy and fork scope."
    (fork-thread op :unhandled unhandled :scope scope))

  (inline)
  (declare unmask-thread-finally_ ((LiftTo IO :m) (Exceptions :m)
                            (Threads IoRuntime IoThread :m)
                            => IoThread * (UnmaskFinallyMode -> IO Unit) -> :m Unit))
  (define unmask-thread-finally_ unmask-thread-finally)

  (inline)
  (declare unmask-finally_ ((LiftTo IO :m) (Exceptions :m) (Concurrent :c :a)
                            (Threads IoRuntime IoThread :m)
                            => :c * (UnmaskFinallyMode -> IO Unit) -> :m Unit))
  (define unmask-finally_ unmask-finally)

  (inline)
  (declare park-current-thread-if_ (MonadIo :m
                                    => (Generation -> IO Unit) * IO Boolean
                                    &key (:timeout TimeoutStrategy)
                                    -> :m Unit))
  (define park-current-thread-if_
    "Parks the current thread if SHOULD-PARK? returns True. Will park the thread until
woken by an unpark from another thread. Upon an unpark, the thread will resume even if
SHOULD-PARK? is False! SHOULD-PARK? is only checked to determine if the thread should
park, *not* if it should resume. Can specify a timeout.

Concurrent:
  - WARNING: SHOULD-PARK? must not block, or the thread could be left blocked in a masked
    state.
  - Can briefly block while trying to park the thread, if contended."
    park-current-thread-if)
  )

(cl:defun parse-fork-keywords_% (forms)
  (cl:let ((unhandled 'LogAndSwallow)
           (scope 'Structured))
    (cl:loop :while (cl:and forms (cl:keywordp (cl:first forms)))
             :do (cl:let ((key (cl:pop forms)))
                   (cl:unless forms
                     (cl:error "Missing value for ~S in DO-FORK-THREAD_" key))
                   (cl:ecase key
                     (:unhandled (cl:setf unhandled (cl:pop forms)))
                     (:scope (cl:setf scope (cl:pop forms))))))
    (cl:values unhandled scope forms)))

(defmacro do-fork-thread_ (cl:&body forms)
  (cl:multiple-value-bind (unhandled scope body)
      (parse-fork-keywords_% forms)
    `(fork-thread_
      (do
       ,@body)
      :unhandled ,unhandled
      :scope ,scope)))
