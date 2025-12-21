(cl:in-package :cl-user)
(defpackage :io/thread-exceptions
  (:use
   #:coalton
   #:coalton-prelude
   #:io/utils
   )
  (:export
   #:ThreadingException
   #:InterruptCurrentThread
   #:ThreadingException/InterruptCurrentThread

   #:SynchronousThreadException
   #:JoinedFailedThread
   #:SynchronousThreadException/JoinedFailedThread

   #:UnmaskFinallyMode
   #:Stopped
   #:Running
   ))
(in-package :io/thread-exceptions)

(coalton-toplevel
  (define-exception ThreadingException
    "Thread exception sent asynchronously between threads.
This type isn't really an exception, it's more of a message."
    (InterruptCurrentThread String))

  (define-instance (Signalable ThreadingException)
    (define (error x)
      (error x)))

  (define-exception SynchronousThreadException
    "Exceptions that a thread raises whenever it encounters a threading
related problem. Unlike ThreadingException, these are actual exceptions."
    (JoinedFailedThread Dynamic))

  (define-instance (Signalable SynchronousThreadException)
    (define (error x)
      (match x
        ((JoinedFailedThread inner-err)
         (error (build-str "Attempted to join a failed thread. Thread failed with error: "
                           (force-string inner-err)))))))

  (derive Eq)
  (repr :enum)
  (define-type UnmaskFinallyMode
    "When a thread unmasks and calls a cleanup operation, the
thread is either:

* Still running, and will cleanup and continue
* Received a stop while it was masked, and will cleanup and then
  terminate itself."
    Running
    Stopped)

  (define-instance (Into UnmaskFinallyMode String)
    (define (into a)
      (match a
        ((Running) "Running")
        ((Stopped) "Stopped"))))
  )
