(cl:in-package :cl-user)
(defpackage :io/gen-impl/conc/unbounded-scheduler
  (:use
   #:coalton
   #:coalton-prelude
   #:io/classes/monad-io
   #:io/classes/thread
   #:io/classes/conc/scheduler
   #:io/gen-impl/conc/queues/unbounded-mpmc
   )
  (:export
   #:UnboundedScheduler
   #:new-unbounded-scheduler
   ))
(in-package :io/gen-impl/conc/unbounded-scheduler)

(cl:declaim (cl:optimize (cl:speed 3) (cl:debug 0) (cl:safety 1)))

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (repr :transparent)
  (define-type (UnboundedScheduler :a)
    "An `UnboundedScheduler` never blocks when submitting tasks, but can overflow memory
if too many tasks are submitted."
    (UnboundedScheduler% (UnboundedMpmcQueue :a)))

  (inline)
  (declare new-unbounded-scheduler (Threads :rt :t :m => :m (UnboundedScheduler :a)))
  (define new-unbounded-scheduler
    (map UnboundedScheduler% new-unbounded-mpmc-queue))

  (inline)
  (declare mchan% (UnboundedScheduler :a -> UnboundedMpmcQueue :a))
  (define (mchan% (UnboundedScheduler% mchan))
    mchan)

  (define-instance (Scheduler UnboundedScheduler)
    (inline)
    (define (submit item scheduler)
      (push-chan (mchan% scheduler) item))
    (inline)
    (define (submit-with item _strategy scheduler)
      (submit item scheduler))
    (inline)
    (define (try-submit item scheduler)
      (do
       (push-chan (mchan% scheduler) item)
       (pure True)))
    (inline)
    (define (take-item _ scheduler)
      (pop-chan (mchan% scheduler))))
  )
