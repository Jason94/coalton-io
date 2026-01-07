(cl:in-package :cl-user)
(defpackage :io/conc/parking
  (:use
   #:io/gen-impl/conc/parking
   )
  (:export
   ;; Re-exports from io/gen-impl/parking
   #:ParkingQueue
   #:new-parking-queue
   #:park-in-queue-if
   #:unpark-queue
   ))
(in-package :io/conc/parking)

(named-readtables:in-readtable coalton:coalton)
