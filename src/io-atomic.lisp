(cl:in-package :cl-user)
(defpackage :io/atomic
  (:use
   #:coalton
   #:coalton-prelude
   #:io/gen-impl/atomic)
  (:export
   #:AtVar
   #:unwrap-atvar
   #:MonadAtVar
   #:new-at-var
   #:read
   #:write
   #:modify
   #:modify-swap
   #:push
   #:pop

   #:derive-monad-at-var
   #:implement-monad-io-atomic))
(in-package :io/atomic)
