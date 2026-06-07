(cl:in-package :cl-user)
(defpackage :io/simple-io/loops
  (:use
   #:io/io-impl/simple-io/loops)
  (:export
   ;; Re-exports from io/io-impl/simple-io/loops
   #:do-foreach-io
   #:do-map-into-io
   #:do-times-io
   ))

