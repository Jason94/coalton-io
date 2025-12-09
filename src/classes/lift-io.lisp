(cl:in-package :cl-user)
(defpackage :io/classes/lift-io
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/monad/classes
   #:io/classes/base-io)
  (:export
   ;; Library Public
   #:LiftIo
   #:lift-io))
(in-package :io/classes/lift-io)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (define-class ((Monad :m) (BaseIo :i) => LiftIo :i :m)
    (lift-io (BaseIo :i => :i :a -> :m :a))))
