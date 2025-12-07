(cl:in-package :cl-user)
(defpackage :io/stm
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/experimental/do-control-core
   #:io/utils
   #:io/monad-io
   #:io/exception
   #:io/stm/stm-impl
   )
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

   ;;; Export STM interface
   #:MonadIoSTM
   #:new-tvar
   #:read-tvar
   #:write-tvar
   #:modify-tvar
   #:retry
   #:or-else
   #:run-tx
   #:do-run-tx

   #:derive-monad-io-stm
   #:implement-monad-io-stm
   )
  )
(in-package :io/stm)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (define-class (MonadIo :m => MonadIoSTM :m)
    "A MonadIo which can execute atomic transactions."
    (new-tvar
     "Create a new mutable variable that can be used inside an atomic transaction."
     (:a -> :m (TVar :a)))
    (read-tvar
     "Read a mutable variable inside an atomic transaction."
     (TVar :a -> STM :m :a))
    (write-tvar
     "Write to a mutable variable inside an atomic transaction."
     (TVar :a -> :a -> STM :m Unit))
    (modify-tvar
     "Modify a mutable variable inside an atomic transaction."
     (TVar :a -> (:a -> :a) -> STM :m :a))
    (retry
     "Retry the current operation because the observed state is invalid."
     (STM :m :a))
    (or-else
     "Run TX-A. If it signals a retry, run TX-b. If both transactions signal a
retry, then the entire transaction retries."
     (STM :m :a -> STM :m :a -> STM :m :a))
    (run-tx
     "Run an atomic transaction. If the transaction raises an exception,
the transaction is aborted and the exception is re-raised."
     (STM :m :a -> :m :a)))
  )

(cl:defmacro implement-monad-io-stm (monad)
  `(define-instance (MonadIoSTM ,monad)
     (define new-tvar new-tvar%)
     (define read-tvar read-tvar%)
     (define write-tvar write-tvar%)
     (define modify-tvar modify-tvar%)
     (define retry retry%)
     (define or-else or-else%)
     (define run-tx run-tx%)))

(coalton-toplevel
  (implement-monad-io-stm io:IO))

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
