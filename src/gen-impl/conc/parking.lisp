(cl:in-package :cl-user)
(defpackage :io/gen-impl/conc/parking
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/types
   #:io/utils
   #:io/classes/monad-io
   #:io/classes/monad-io-thread
   )
  (:local-nicknames
   (:at #:io/thread-impl/atomics)
   )
  (:export
   ;; Library Public
   #:ParkingQueue
   #:new-parking-queue
   #:park-in-queue-if
   #:unpark-queue

   ;; Library Private
   #:new-parking-queue%
   #:park-in-queue-if%
   #:unpark-queue%
   ))
(in-package :io/gen-impl/conc/parking)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (repr :transparent)
  (define-type (ParkingQueue :t)
    "ParkingQueue is a thread-safe list of parked threads. A parking thread can atomically
subscibe and park on a Parking Queue, and a signalling thread can atomically unpark all
parkers on a ParkingQueue.

In general, ParkingQueue is the preferred way to park and unpark threads. The lower-level
parking functions exposed by the MonadIoThread and Runtime classes should only be used
if ParkingQueue doesn't provide enough functionality for the algorithm.

Concurrent:
  - ParkingQueue's algorithms are lock free, but individual threads can block for a very
    short window if contention on the parking queue is very high."
    (ParkingQueue% (at:Atomic (List (Tuple Generation :t)))))

  (inline)
  (declare new-parking-queue% (Unit -> ParkingQueue :t))
  (define (new-parking-queue%)
    (ParkingQueue% (at:new Nil)))

  (inline)
  (declare new-parking-queue (MonadIoThread :rt :t :m => :m (ParkingQueue :t)))
  (define new-parking-queue
    "Create a new ParkingQueue."
    (wrap-io_ new-parking-queue%))

  (inline)
  (declare get-queue% (ParkingQueue :t -> at:Atomic (List (Tuple Generation :t))))
  (define (get-queue% (ParkingQueue% atm))
    atm)

  (inline)
  (declare park-in-queue-if% (Runtime :rt :t
                              => Proxy :rt -> (Unit -> Boolean) -> ParkingQueue :t -> Unit))
  (define (park-in-queue-if% rt-prx should-park? pqueue)
    (park-current-thread-if!
     rt-prx
     (fn (gen)
       (at:atomic-push (get-queue% pqueue)
                       (Tuple gen (current-thread! rt-prx)))
       Unit)
     should-park?))

  (inline)
  (declare park-in-queue-if ((BaseIo :io) (MonadIoThread :rt :t :io) (MonadIo :m)
                             => :io Boolean -> ParkingQueue :t -> :m Unit))
  (define (park-in-queue-if should-park? pqueue)
    "Parks the current thread in PQUEUE if SHOULD-PARK? returns True. Will park the thread
until woken by an unpark from another thread. Upon an unpark, the thread will resume even
if SHOULD-PARK? is False! SHOULD-PARK? is only checked to determine if the thread should
park, *not* if it should resume.

Concurrent:
  - WARNING: SHOULD-PARK? must not block, or the thread could be left blocked in a masked
    state.
  - Can briefly block while trying to park the thread, if contended."
    (park-current-thread-if
     (fn (gen)
       (wrap-io-with-runtime (rt-prx)
         (at:atomic-push (get-queue% pqueue)
                         (Tuple gen (current-thread! rt-prx)))
         Unit))
     should-park?))

  (inline)
  (declare unpark-queue% (Runtime :rt :t => Proxy :rt -> ParkingQueue :t -> Unit))
  (define (unpark-queue% rt-prx pqueue)
    (let parked-entries = (at:atomic-swap (get-queue% pqueue) Nil))
    (for (Tuple gen thread) in parked-entries
      (unpark-thread! rt-prx gen thread))
    Unit)

  (inline)
  (declare unpark-queue (MonadIoThread :rt :t :m => ParkingQueue :t -> :m Unit))
  (define (unpark-queue pqueue)
    "Atomically reset PQUEUE, then attempt to unpark all threads parked on the queue.

Concurrent:
  - Can briefly block while trying to reset the queue or unpark a parked thread"
    (inject-runtime unpark-queue% pqueue))

 )
