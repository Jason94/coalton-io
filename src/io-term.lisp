(cl:in-package :cl-user)
(defpackage :io/term
  (:use
   #:coalton
   #:coalton-prelude
   #:io/gen-impl/term)
  (:export
   ;; Re-exports from io/classes/monad-io-term
   #:MonadIoTerm
   #:write
   #:write-line
   #:read-line

   ;; Remaining exports
   #:derive-monad-io-term
   #:implement-monad-io-term
   ))
(in-package :io/term)
