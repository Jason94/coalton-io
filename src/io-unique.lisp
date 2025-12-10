(cl:in-package :cl-user)
(defpackage :io/unique
  (:use
   #:io/classes/monad-io-unique
   #:io/gen-impl/unique)
  (:export
   ;; Re-exports from io/classes/monad-io-unique
   #:MonadIoUnique
   #:derive-monad-io-unique
   #:Unique
   #:new-unique
   #:to-int

   ;; Re-exports from io/gen-impl/unique
   #:implement-monad-io-unique
   ))
(in-package :io/unique)
