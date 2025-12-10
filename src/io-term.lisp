(cl:in-package :cl-user)
(defpackage :io/term
  (:use
   #:io/classes/monad-io-term
   #:io/gen-impl/term)
  (:export
   ;; Re-exports from io/classes/monad-io-term
   #:MonadIoTerm
   #:derive-monad-io-term
   #:write
   #:write-line
   #:read-line

   ;; Re-exports from io/gen-impl/term
   #:implement-monad-io-term
   ))
(in-package :io/term)
