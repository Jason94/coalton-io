(cl:in-package :cl-user)
(defpackage :io/gen-impl/conc/ring-buffer-scheduler
  (:use
   #:coalton
   #:coalton-prelude
   #:io/classes/monad-io
   #:io/classes/monad-exception
   #:io/classes/monad-io-thread
   #:io/classes/conc/scheduler
   #:io/gen-impl/conc/ring-buffer
   )
  (:export
   #:SchedulerError
   #:InvalidBoundedCapacityError

   #:RingBufferScheduler
   #:new-ring-buffer-scheduler
   ))
(in-package :io/gen-impl/conc/ring-buffer-scheduler)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (define-type SchedulerError
    InvalidBoundedCapacityError)

  (define-instance (Signalable SchedulerError)
    (define (error e)
      (match e
        ((InvalidBoundedCapacityError)
         (error "Scheduler queue must have non-zero capacity")))))

  (repr :transparent)
  (define-type (RingBufferScheduler :a)
    "A RingBufferScheduler uses a single RingBuffer internally to manage tasks.

MChanScheduler is bounded."
    (RingBufferScheduler% (RingBuffer :a)))

  (inline)
  (declare new-ring-buffer-scheduler ((MonadIoThread :rt :t :m) (MonadException :m)
                                      => UFix -> :m (RingBufferScheduler :a)))
  (define (new-ring-buffer-scheduler capacity)
    (if (== 0 capacity)
     (raise InvalidBoundedCapacityError)
     (pure (RingBufferScheduler% (new-ring-buffer% capacity)))))

  (inline)
  (declare queue% (RingBufferScheduler :a -> RingBuffer :a))
  (define (queue% (RingBufferScheduler% queue))
    queue)

  (define-instance (Scheduler RingBufferScheduler)
    (inline)
    (define (submit item scheduler)
      (wrap-io-with-runtime (rt-prx)
        (enqueue!% rt-prx item (queue% scheduler))))
    (inline)
    (define (submit-with item strategy scheduler)
      (wrap-io-with-runtime (rt-prx)
        (enqueue-with!% rt-prx item strategy (queue% scheduler))))
    (inline)
    (define (try-submit item scheduler)
      (wrap-io-with-runtime (rt-prx)
        (try-enqueue!% rt-prx item (queue% scheduler))))
    (inline)
    (define (take-item _ scheduler)
      (wrap-io-with-runtime (rt-prx)
        (dequeue!% rt-prx (queue% scheduler)))))
  )
