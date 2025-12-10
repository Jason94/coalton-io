(cl:in-package :cl-user)
(defpackage :io/stm
  (:use
   #:io/classes/monad-io-stm
   #:io/gen-impl/stm)
  (:export
   ;; Re-exports from io/classes/monad-io-stm
   #:MonadIoSTM
   #:derive-monad-io-stm
   #:new-tvar
   #:read-tvar
   #:write-tvar
   #:modify-tvar
   #:retry
   #:or-else
   #:run-tx
   #:do-run-tx

   ;; Re-exports from io/gen-impl/stm
   #:TVar
   #:STM
   #:implement-monad-io-stm
   ))
(in-package :io/stm)
