(cl:in-package :cl-user)
(defpackage :io/conc/queues/bounded-mpmc
  (:use
   #:io/gen-impl/conc/queues/bounded-mpmc)
  (:export
   ;; Re-exports from io/gen-impl/conc/queues/bounded-mpmc
   #:BoundedMpmcQueue

   #:new-bounded-mpmc-queue
   #:enqueue
   #:try-enqueue
   #:dequeue
   ))
