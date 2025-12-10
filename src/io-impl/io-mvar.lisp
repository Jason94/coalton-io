(cl:in-package :cl-user)
(in-package :io/mvar)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (declare with-mvar_ ((LiftTo io:IO :m) (MonadIoMVar :m) => MVar :a -> (:a -> io:IO :b) -> :m :b))
  (define with-mvar_ with-mvar)

  )

(cl:defmacro do-with-mvar_ ((sym mvar) cl:&body body)
  `(with-mvar_
     ,mvar
     (fn (,sym)
       (do
        ,@body))))
