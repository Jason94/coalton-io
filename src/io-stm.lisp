(cl:in-package :cl-user)
(defpackage :io/stm
  (:use
   #:coalton
   #:coalton-prelude
   #:io/gen-impl/stm)
  (:export
   ;;; Re-export from stm-impl
   #:TVar
   #:STM

   ;;; Re-export: io/classes/monad-io-stm
   #:MonadIoSTM
   #:new-tvar
   #:read-tvar
   #:write-tvar
   #:modify-tvar
   #:retry
   #:or-else
   #:run-tx

   ;;; Remaining exports
   #:do-run-tx

   #:derive-monad-io-stm
   #:implement-monad-io-stm
   )
  )
(in-package :io/stm)
