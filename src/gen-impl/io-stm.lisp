(cl:in-package :cl-user)
(defpackage :io/gen-impl/stm
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/experimental/do-control-core
   #:io/classes/monad-io-stm
   #:io/utils
   #:io/monad-io
   #:io/exception
   #:io/stm/stm-impl)
  (:import-from #:coalton-library/experimental/do-control-loops-adv
   #:LoopT)
  (:local-nicknames
   (:io #:io/simple-io)
   (:t #:coalton-threads/thread)
   (:st #:coalton-library/monad/statet)
   (:env #:coalton-library/monad/environment)
   (:c #:coalton-library/cell)
   (:a #:coalton-threads/atomic))
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
(in-package :io/gen-impl/stm)

(named-readtables:in-readtable coalton:coalton)

(cl:defmacro implement-monad-io-stm (monad)
  `(define-instance (MonadIoSTM ,monad)
     (define new-tvar new-tvar%)
     (define read-tvar read-tvar%)
     (define write-tvar write-tvar%)
     (define modify-tvar modify-tvar%)
     (define retry retry%)
     (define or-else or-else%)
     (define run-tx run-tx%)))

;; NOTE: All of these functions except new-tvar could be outside of the typeclass.
(cl:defmacro derive-monad-io-stm (monad-param monadT-form)
  "Automatically derive an instance of MonadIoSTM for a monad transformer.

Example:
  (derive-monad-io-stm :m (st:StateT :s :m))"
  `(define-instance (MonadIoSTM ,monad-param => MonadIoSTM ,monadT-form)
     (inline)
     (define new-tvar (compose lift new-tvar))
     (define read-tvar read-tvar)
     (define write-tvar write-tvar)
     (define modify-tvar modify-tvar)
     (define retry retry)
     (define or-else or-else%)
     (define run-tx run-tx)))

(coalton-toplevel

  ;;
  ;; Std. Library Transformer Instances
  ;;

  (derive-monad-io-stm :m (st:StateT :s :m))
  (derive-monad-io-stm :m (env:EnvT :e :m))
  (derive-monad-io-stm :m (LoopT :m))
  )

(cl:defmacro do-run-tx (cl:&body body)
  `(run-tx
    (do
     ,@body)))
