(cl:in-package :cl-user)
(defpackage :io/exception
  (:use
   #:coalton
   #:coalton-prelude
   #:io/gen-impl/exception)
  (:export
   ;; Re-exports from io/classes/monad-exception
   #:MonadException
   #:raise
   #:raise-dynamic
   #:reraise
   #:handle
   #:handle-all
   #:try-dynamic

   ;; Remaining exports
   #:try
   #:try-all
   #:do-reraise
   #:do-handle
   #:do-handle-all

   #:raise-result
   #:raise-result-dynamic
   #:wrap-error_
   #:wrap-error
   ;; Re-export for convenience
   #:UnhandledError
   ))
(in-package :io/exception)
