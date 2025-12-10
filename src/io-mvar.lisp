(cl:in-package :cl-user)
(defpackage :io/mvar
  (:use
   #:coalton
   #:coalton-prelude
   #:io/gen-impl/mvar)
  (:export
   #:MVar
   #:MonadIoMVar
   #:new-mvar
   #:new-empty-mvar
   #:take-mvar
   #:put-mvar
   #:try-take-mvar
   #:try-put-mvar
   #:read-mvar
   #:try-read-mvar
   #:swap-mvar
   #:is-empty-mvar

   #:derive-monad-io-mvar

   #:with-mvar
   #:with-mvar_
   #:do-with-mvar
   #:do-with-mvar_

   #:MChan
   #:new-empty-chan
   #:push-chan
   #:pop-chan

   #:implement-monad-io-mvar))
(in-package :io/mvar)
