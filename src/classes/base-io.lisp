(cl:in-package :cl-user)
(defpackage :io/classes/base-io
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/monad/classes)
  (:export
   ;; Library Public
   #:BaseIo
   #:run!))
(in-package :io/classes/base-io)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
