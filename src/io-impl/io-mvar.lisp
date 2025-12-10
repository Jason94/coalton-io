(cl:in-package :cl-user)
(defpackage :io/io-impl/mvar
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/monad/classes
   #:io/thread-impl/runtime
   #:io/classes/monad-io-mvar
   #:io/gen-impl/mvar
   #:io/io-impl/simple-io
   )
  (:export
   #:with-mvar_
   #:do-with-mvar_
   ))
(in-package :io/io-impl/mvar)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (declare with-mvar_ ((LiftTo IO :m) (MonadIoMVar :m) => MVar :a -> (:a -> IO :b) -> :m :b))
  (define with-mvar_ with-mvar)

  )

(cl:defmacro do-with-mvar_ ((sym mvar) cl:&body body)
  `(with-mvar_
     ,mvar
     (fn (,sym)
       (do
        ,@body))))
