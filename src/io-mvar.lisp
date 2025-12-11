(cl:in-package :cl-user)

(defpackage :io/mvar
  (:use
   #:io/classes/monad-io-mvar
   #:io/io-impl/mvar
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
   #:do-with-mvar

   #:MChan
   #:new-empty-chan
   #:push-chan
   #:pop-chan

   ;; Re-exports from io/gen-impl/mvar
   #:with-mvar
   #:implement-monad-io-mvar

   ;; Re-exports from io/io-impl/mvar
   #:with-mvar_
   #:do-with-mvar_))

(in-package :io/mvar)
