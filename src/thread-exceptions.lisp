(cl:in-package :cl-user)
(defpackage :io/thread-exceptions
  (:use
   #:coalton
   #:coalton-prelude
   )
  (:export
   #:ThreadingException
   #:InterruptCurrentThread))
(in-package :io/thread-exceptions)

(coalton-toplevel
  (define-exception ThreadingException
    (InterruptCurrentThread String))

  (define-instance (Signalable ThreadingException)
    (define (error x)
      (error x))))
