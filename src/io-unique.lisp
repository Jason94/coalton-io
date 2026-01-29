(cl:in-package :cl-user)
(defpackage :io/unique-gen
  (:use
   #:io/classes/unique-gen
   #:io/gen-impl/unique)
  (:export
   ;; Re-exports from io/classes/unique-gen
   #:UniqueGen
   #:derive-unique-gen
   #:Unique
   #:new-unique
   #:to-int

   ;; Re-exports from io/gen-impl/unique
   #:implement-unique-gen
   ))
(in-package :io/unique-gen)
