(cl:in-package :cl-user)
(defpackage :io/mutable-var
  (:use
   #:io/classes/mutable-var
   #:io/gen-impl/mut
   )
  (:export
   ;; Re-exports from io/classes/mutable-var
   #:Var
   #:MutableVar
   #:derive-mutable-var
   #:new-var
   #:read
   #:write
   #:modify

   ;; Re-exports from io/gen-impl/mut
   #:implement-mutable-var))
(in-package :io/mutable-var)
