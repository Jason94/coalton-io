(cl:in-package :cl-user)
(defpackage :io/gen-impl/conc/bounded-scheduler
  (:use
   #:coalton
   #:coalton-prelude
   #:io/classes/monad-io
   #:io/classes/exceptions
   #:io/classes/thread
   #:io/classes/conc/scheduler
   #:io/gen-impl/conc/queues/bounded-mpmc
   )
  (:export
   #:SchedulerError
   #:InvalidBoundedCapacityError

   #:BoundedScheduler
   #:new-bounded-scheduler
   ))
(in-package :io/gen-impl/conc/bounded-scheduler)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (define-type SchedulerError
    InvalidBoundedCapacityError)

  (define-instance (Signalable SchedulerError)
    (define (error e)
      (match e
        ((InvalidBoundedCapacityError)
         (error "Bounded scheduler queue must have non-zero capacity")))))

  (repr :transparent)
  (define-type (BoundedScheduler :a)
    "A `BoundedScheduler` has a limited capacity, and task submissions block if full."
    (BoundedScheduler% (BoundedMpmcQueue :a)))

  (inline)
  (declare new-bounded-scheduler ((Threads :rt :t :m) (Exceptions :m)
                                  => UFix -> :m (BoundedScheduler :a)))
  (define (new-bounded-scheduler capacity)
    (if (== 0 capacity)
     (raise InvalidBoundedCapacityError)
     (pure (BoundedScheduler% (new-bounded-mpmc-queue% capacity)))))

  (inline)
  (declare queue% (BoundedScheduler :a -> BoundedMpmcQueue :a))
  (define (queue% (BoundedScheduler% queue))
    queue)

  (define-instance (Scheduler BoundedScheduler)
    (inline)
    (define (submit item scheduler)
      (wrap-io-with-runtime (rt-prx)
        (enqueue!% rt-prx item (queue% scheduler))
        Unit))
    (inline)
    (define (submit-with item strategy scheduler)
      (wrap-io-with-runtime (rt-prx)
        (enqueue-with!% rt-prx item strategy (queue% scheduler))
        Unit))
    (inline)
    (define (try-submit item scheduler)
      (wrap-io-with-runtime (rt-prx)
        (try-enqueue!% rt-prx item (queue% scheduler))))
    (inline)
    (define (take-item _ scheduler)
      (wrap-io-with-runtime (rt-prx)
        (dequeue!% rt-prx (queue% scheduler)))))
  )
