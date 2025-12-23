(cl:in-package :cl-user)
(defpackage :io/io-impl/mvar
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/monad/classes
   #:io/thread-impl/runtime
   #:io/classes/monad-io-thread
   #:io/gen-impl/conc/mvar
   #:io/io-impl/runtime
   #:io/io-impl/simple-io
   )
  (:export
   #:with-mvar_
   #:do-with-mvar_
   ))
(in-package :io/io-impl/mvar)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (declare with-mvar_ ((MonadIoThread IoRuntime IoThread :m) (LiftTo IO :m)
                       => MVar :a -> (:a -> IO :a) -> :m :a))
  (define (with-mvar_ mvar op)
    (with-mvar mvar op))

  )

(cl:defmacro do-with-mvar_ ((sym mvar) cl:&body body)
  `(with-mvar_
     ,mvar
     (fn (,sym)
       (do
        ,@body))))
