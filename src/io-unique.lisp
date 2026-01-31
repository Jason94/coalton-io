(cl:in-package :cl-user)
(defpackage :io/unique
  (:use
   #:io/classes/unique
   #:io/gen-impl/unique)
  (:export
   ;; Re-exports from io/classes/unique
   #:UniqueGen
   #:derive-unique-gen
   #:Unique
   #:new-unique
   #:to-int

   ;; Re-exports from io/gen-impl/unique
   #:implement-unique-gen
   ))
(in-package :io/unique)
