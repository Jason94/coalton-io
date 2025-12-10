(cl:in-package :cl-user)
(defpackage :io/classes/monad-io-stm
  (:use
   #:coalton
   #:coalton-prelude
   #:io/classes/monad-io
   #:io/thread-impl/stm-impl)
  (:import-from #:coalton-library/experimental/do-control-loops-adv
   #:LoopT)
  (:local-nicknames
   (:st #:coalton-library/monad/statet)
   (:env #:coalton-library/monad/environment)
   (:c #:coalton-library/cell))
  (:export
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
   ))
(in-package :io/classes/monad-io-stm)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (define-class (MonadIo :m => MonadIoSTM :m)
    "A MonadIo which can execute atomic transactions.

The critical section of transaction commits is masked, so stopping a thread
during a transaction won't leave the STM in an inoperable state. Read-only
transactions never mask. Transactions are only masked during the brief commit
period; the thread is still stoppable during the bulk of the transaction
unless you mask it yourself."
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
     (STM :m :a -> :m :a))))

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
