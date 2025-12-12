(cl:in-package :cl-user)
(defpackage :io/thread-exceptions
  (:use
   #:coalton
   #:coalton-prelude
   )
  (:export
   #:ThreadingException
   #:InterruptCurrentThread

   #:UnmaskFinallyMode
   #:Stopped
   #:Running
   ))
(in-package :io/thread-exceptions)

(coalton-toplevel
  (define-exception ThreadingException
    (InterruptCurrentThread String))

  (define-instance (Signalable ThreadingException)
    (define (error x)
      (error x)))

  (derive Eq)
  (repr :enum)
  (define-type UnmaskFinallyMode
    Running
    Stopped)

  (define-instance (Into UnmaskFinallyMode String)
    (define (into a)
      (match a
        ((Running) "Running")
        ((Stopped) "Stopped"))))
  )
