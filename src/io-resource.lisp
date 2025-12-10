(cl:in-package :cl-user)
(defpackage :io/resource
  (:use
   #:coalton
   #:coalton-prelude
   #:io/gen-impl/resource)
  (:export
   #:ExitCase
   #:Completed
   #:Errored

   #:bracket-io
   #:bracket-io_
   #:with-mask
   #:do-with-mask
   ))
(in-package :io/resource)
