(cl:in-package :cl-user)
(defpackage :io/threads-exceptions
  (:use
   #:coalton
   #:coalton-prelude
   #:io/utils
   )
  (:export
   ;; Library Public
   #:ThreadingException
   #:InterruptCurrentThread
   #:ThreadingException/InterruptCurrentThread

   #:SynchronousThreadException
   #:JoinedFailedThread
   #:SynchronousThreadException/JoinedFailedThread
   #:TimeoutException
   #:SynchronousThreadException/TimeoutException
   #:UnexpectedStop
   #:SynchronousThreadException/UnexpectedStop

   #:UnmaskFinallyMode
   #:Stopped
   #:Running

   ;; Library Private
   #:dynamic-is-threading-exception?
   #:is-threading-exception

   #:dynamic-is-unexpected-stop-exc?
   ))
(in-package :io/threads-exceptions)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (define-exception ThreadingException
    "Thread exception sent asynchronously between threads.
This type isn't really an exception, it's more of a message."
    (InterruptCurrentThread String))

    (declare dynamic-is-threading-exception? (Dynamic -> Boolean))
  (define (dynamic-is-threading-exception? dyn)
    "Returns true if the dynamic val is a threading exception or an
IoError containing a ThreadingException."
    (let (Dynamic% val _) = dyn)
    (or
     (lisp (-> Boolean) (val) (cl:typep val 'ThreadingException))
     (match (cast dyn)
       ((Some io-err)
        (is-threading-exception io-err))
       ((None)
        False))))

  (declare is-threading-exception (IoError -> Boolean))
  (define (is-threading-exception io-err)
    "Return True if IO-ERR contains a threading exception."
    (let val = (match io-err
                 ((UnhandledError e _)
                  e)
                 ((HandledError (Dynamic% e _) _)
                  e)))
   (lisp (-> Boolean) (val) (cl:typep val 'ThreadingException)))

  (define-exception SynchronousThreadException
    "Exceptions that a thread raises whenever it encounters a threading
related problem. Unlike ThreadingException, these are actual exceptions."
    (JoinedFailedThread Dynamic)
    (TimeoutException String)
    UnexpectedStop)

  (inline)
  (declare dynamic-is-unexpected-stop-exc? (Dynamic -> Boolean))
  (define (dynamic-is-unexpected-stop-exc? dyn)
    (let (Dynamic% val _) = dyn)
    (lisp (-> Boolean) (val)
      (cl:typep val 'SynchronousThreadException/UnexpectedStop)))

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
