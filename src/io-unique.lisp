(cl:in-package :cl-user)
(defpackage :io/unique
  (:use
   #:coalton
   #:coalton-prelude
   #:io/gen-impl/unique)
  (:export
   ;; Re-exports from io/classes/monad-io-unique
   #:MonadIoUnique
   #:Unique
   #:new-unique
   #:to-int

   ;; Remaining exports
   #:derive-monad-io-unique
   #:implement-monad-io-unique
   ))
(in-package :io/unique)
