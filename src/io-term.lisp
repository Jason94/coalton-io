(cl:in-package :cl-user)
(defpackage :io/terminal
  (:use
   #:io/classes/terminal
   #:io/gen-impl/term)
  (:export
   ;; Re-exports from io/classes/terminal
   #:Terminal
   #:derive-terminal
   #:write
   #:write-line
   #:read-line

   ;; Re-exports from io/gen-impl/term
   #:implement-terminal
   ))
(in-package :io/terminal)
