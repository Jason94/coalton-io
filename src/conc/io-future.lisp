(cl:in-package :cl-user)
(defpackage :io/conc/future
  (:use
   #:io/gen-impl/conc/future
   #:io/io-impl/future
   )
  (:export
   ;; Re-exports from io/gen-impl/conc/future
   #:Future
   #:fork-future
   #:try-read-future

   #:do-fork-future

   ;; Re-exports from io/io-impl/future
   #:fork-future_
   #:do-fork-future_
   ))
(in-package :io/conc/future)

(named-readtables:in-readtable coalton:coalton)
