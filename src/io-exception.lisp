(cl:in-package :cl-user)
(defpackage :io/exceptions
  (:use
   #:coalton
   #:coalton-prelude
   #:io/classes/exceptions)
  (:export
   ;; Re-exports from io/classes/exceptions
   #:Exceptions
   #:raise
   #:raise-dynamic
   #:reraise
   #:handle
   #:handle-all
   #:try-dynamic

   #:try
   #:try-all
   #:try-result
   #:raise-result
   #:raise-result-dynamic
   #:wrap-error_
   #:wrap-error

   #:do-reraise
   #:do-handle
   #:do-handle-all
   ))
(in-package :io/exceptions)
