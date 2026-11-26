(cl:in-package :cl-user)
(defpackage :io/io-impl/future
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/monad/classes
   #:io/classes/thread
   #:io/gen-impl/conc/future
   #:io/io-impl/io-runtime/runtime
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
  (inline)
  (declare fork-future_ ((LiftTo IO :m) (Threads IoRuntime IoThread :m)
                         => (Promise :a -> IO :b)
                         &key
                         (:unhandled UnhandledExceptionStrategy)
                         (:scope (ForkScope IoThread))
                         -> :m (Tuple (Future :a) IoThread)))
  (define (fork-future_ thunk &key (unhandled LogAndSwallow) (scope Structured))
    "Create a future-promise pair. Start a new thread with the `Promise`. Return the
`Future` and a handle to the running thread.

If the thread raises an unhandled exception, the default behavior is to log it to
*ERROR-OUTPUT* and swallow it until/if the thread is joined. The default fork scope is
structured."
    (fork-future thunk :unhandled unhandled :scope scope))
  )

(defmacro do-fork-future_ ((promise-sym) cl:&body forms)
  (cl:multiple-value-bind (unhandled scope body)
      (parse-fork-keywords% forms)
    `(fork-future_
      (fn (,promise-sym)
        (do
         ,@body))
      :scope ,scope :unhandled ,unhandled)))
