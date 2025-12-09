(cl:in-package :cl-user)
(defpackage :io/classes/monad-io
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/monad/classes
   #:io/classes/base-io)
  (:export
   ;; Library Public
   #:MonadIo
   #:wrap-io_))
(in-package :io/classes/monad-io)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (define-class (Monad :m => MonadIo :m)
    (wrap-io_
     "Wrap a (potentially) side-effectful function in the monad."
     ((Unit -> :a) -> :m :a))))
