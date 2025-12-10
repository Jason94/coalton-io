(cl:in-package :cl-user)
(defpackage :io/atomic
  (:use
   #:io/classes/monad-at-var
   #:io/gen-impl/atomic)
  (:export
   ;; Re-exports from io/classes/monad-at-var
   #:AtVar
   #:unwrap-atvar
   #:MonadAtVar
   #:derive-monad-at-var
   #:new-at-var
   #:read
   #:write
   #:modify
   #:modify-swap
   #:push
   #:pop

   ;; Re-exports from io/gen-impl/atomic
   #:implement-monad-io-atomic))
(in-package :io/atomic)
