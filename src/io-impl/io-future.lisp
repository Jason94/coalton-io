(cl:in-package :cl-user)
(in-package :io/future)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (declare fork-future_ ((MonadIoThread :m :t) (MonadIoMVar :m) (LiftTo io:IO :m)
                         => io:IO :a -> :m (Future :a)))
  (define fork-future_
    "Spawn a new future, which will run and eventually return the result
from TASK. The future is guaranteed to only ever run at most once, when
the produced :m is run."
    fork-future)
  )

(cl:defmacro do-fork-future_ (cl:&body body)
  `(fork-future_
    (do
     ,@body)))
