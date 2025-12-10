(cl:in-package :cl-user)
(in-package :io/thread)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (declare fork_ ((MonadIoThread :m IoThread) (LiftTo io:IO :m)
                  => io:IO :a -> :m IoThread))
  (define fork_ fork)
  )
