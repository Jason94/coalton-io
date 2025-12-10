(cl:in-package :cl-user)
(defpackage :io/mut
  (:use
   #:io/classes/monad-io-var
   #:io/gen-impl/mut
   )
  (:export
   ;; Re-exports from io/classes/monad-io-var
   #:Var
   #:MonadIoVar
   #:derive-monad-var
   #:new-var
   #:read
   #:write
   #:modify

   ;; Re-exports from io/gen-impl/mut
   #:implement-monad-io-var))
(in-package :io/mut)
