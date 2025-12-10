(cl:in-package :cl-user)
(defpackage :io/classes/monad-io-stm
  (:use
   #:coalton
   #:coalton-prelude
   #:io/classes/monad-io
   #:io/thread-impl/stm-types)
  (:local-nicknames
   (:c #:coalton-library/cell))
  (:export
   #:MonadIoSTM
   #:new-tvar
   #:read-tvar
   #:write-tvar
   #:modify-tvar
   #:retry
   #:or-else
   #:run-tx
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
