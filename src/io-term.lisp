(cl:in-package :cl-user)
(defpackage :io/term
  (:use
   #:io/classes/term
   #:io/gen-impl/term)
  (:export
   ;; Re-exports from io/classes/term
   #:Terminal
   #:derive-terminal
   #:write
   #:write-line
   #:read-line

   ;; Re-exports from io/gen-impl/term
   #:implement-terminal
   ))
(in-package :io/term)
