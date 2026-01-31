(cl:in-package :cl-user)
(defpackage :io/mut
  (:use
   #:io/classes/mut
   #:io/gen-impl/mut
   )
  (:export
   ;; Re-exports from io/classes/mut
   #:Var
   #:MutableVar
   #:derive-mutable-var
   #:new-var
   #:read
   #:write
   #:modify

   ;; Re-exports from io/gen-impl/mut
   #:implement-mutable-var))
(in-package :io/mut)
