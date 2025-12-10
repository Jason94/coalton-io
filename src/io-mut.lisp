(cl:in-package :cl-user)
(defpackage :io/mut
  (:use
   #:coalton
   #:coalton-prelude
   #:io/gen-impl/mut)
  (:export
   #:Var
   #:MonadIoVar
   #:new-var
   #:read
   #:write
   #:modify

   #:derive-monad-var
   #:implement-monad-io-var))
(in-package :io/mut)
