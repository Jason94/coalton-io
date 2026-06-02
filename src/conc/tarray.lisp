(cl:in-package :cl-user)
(defpackage :io/conc/stm/tarray
  (:use
   #:io/gen-impl/conc/stm/tarray)
  (:export
   ;; Re-exports from io/gen-impl/conc/stm/tarray
   #:TArray
   #:new-tarray
   #:aref
   #:aref#
   #:set
   ))
(in-package :io/conc/stm/tarray)
