(cl:in-package :cl-user)
(defpackage :io/mvar
  (:use
   #:io/classes/monad-io-mvar
   #:io/gen-impl/mvar)
  (:export
   ;; Re-exports from io/classes/monad-io-mvar
   #:MVar
   #:MonadIoMVar
   #:derive-monad-io-mvar
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

   #:with-mvar
   #:with-mvar_
   #:do-with-mvar
   #:do-with-mvar_

   ;; Re-exports from io/gen-impl/mvar
   #:MChan
   #:new-empty-chan
   #:push-chan
   #:pop-chan

   #:implement-monad-io-mvar))
(in-package :io/mvar)
