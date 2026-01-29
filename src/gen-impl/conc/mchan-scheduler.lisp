(cl:in-package :cl-user)
(defpackage :io/gen-impl/conc/mchan-scheduler
  (:use
   #:coalton
   #:coalton-prelude
   #:io/classes/monad-io
   #:io/classes/threads
   #:io/classes/conc/scheduler
   #:io/gen-impl/conc/mvar
   )
  (:export
   #:MChanScheduler
   #:new-mchan-scheduler
   ))
(in-package :io/gen-impl/conc/mchan-scheduler)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (repr :transparent)
  (define-type (MChanScheduler :a)
    "An MChanScheduler uses a single MChan internally to manage tasks. Producers submit
tasks onto the end of the MChan. Worker threads pop items, one-by-one, from the front of
the MChan.

MChanScheduler is unbounded."
    (MChanScheduler% (MChan :a)))

  (inline)
  (declare new-mchan-scheduler (Threads :rt :t :m => :m (MChanScheduler :a)))
  (define new-mchan-scheduler
    (map MChanScheduler% new-empty-chan))

  (inline)
  (declare mchan% (MChanScheduler :a -> MChan :a))
  (define (mchan% (MChanScheduler% mchan))
    mchan)

  (define-instance (Scheduler MChanScheduler)
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
