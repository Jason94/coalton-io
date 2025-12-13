(cl:in-package :cl-user)
(defpackage :io/atomic
  (:use
   #:io/gen-impl/atomic)
  (:export
   ;; Re-exports from io/gen-impl/atomic
   #:AtVar
   #:new-at-var
   #:read
   #:write
   #:modify
   #:modify-swap
   #:push
   #:pop
   ))
(in-package :io/atomic)
