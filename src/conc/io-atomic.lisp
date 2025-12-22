(cl:in-package :cl-user)
(defpackage :io/conc/atomic
  (:use
   #:io/gen-impl/conc/atomic)
  (:export
   ;; Re-exports from io/gen-impl/conc/atomic
   #:AtVar
   #:new-at-var
   #:read
   #:write
   #:modify
   #:modify-swap
   #:push
   #:pop
   ))
(in-package :io/conc/atomic)
