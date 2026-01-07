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
   #:ParkingSet
   #:new-parking-set
   #:park-in-sets-if
   #:park-in-set-if
   #:unpark-set

   ;; Library Private
   #:new-parking-set%
   #:park-in-sets-if%
   #:park-in-set-if%
   #:unpark-set%
   ))
(in-package :io/gen-impl/conc/parking)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (repr :transparent)
  (define-type (ParkingSet :t)
    "ParkingSet is a thread-safe list of parked threads. A parking thread can atomically
subscibe and park on a Parking Set, and a signalling thread can atomically unpark all
parkers on a ParkingSet.

In general, ParkingSet is the preferred way to park and unpark threads. The lower-level
parking functions exposed by the MonadIoThread and Runtime classes should only be used
if ParkingSet doesn't provide enough functionality for the algorithm.

Concurrent:
  - ParkingSet's algorithms are lock free, but individual threads can block for a very
    short window if contention on the parking set is very high."
    (ParkingSet% (at:Atomic (List (Tuple Generation :t)))))

  (inline)
  (declare new-parking-set% (Unit -> ParkingSet :t))
  (define (new-parking-set%)
    (ParkingSet% (at:new Nil)))

  (inline)
  (declare new-parking-set (MonadIoThread :rt :t :m => :m (ParkingSet :t)))
  (define new-parking-set
    "Create a new ParkingSet."
    (wrap-io_ new-parking-set%))

  (inline)
  (declare get-set% (ParkingSet :t -> at:Atomic (List (Tuple Generation :t))))
  (define (get-set% (ParkingSet% atm))
    atm)

  (inline)
  (declare park-in-sets-if% (Runtime :rt :t
                              => Proxy :rt -> (Unit -> Boolean) -> List (ParkingSet :t) -> Unit))
  (define (park-in-sets-if% rt-prx should-park? psets)
    (park-current-thread-if!
     rt-prx
     (fn (gen)
       (for pset in psets
         (at:atomic-push (get-set% pset)
                         (Tuple gen (current-thread! rt-prx))))
       Unit)
     should-park?))

  (inline)
  (declare park-in-set-if% (Runtime :rt :t
                              => Proxy :rt -> (Unit -> Boolean) -> ParkingSet :t -> Unit))
  (define (park-in-set-if% rt-prx should-park? pset)
    (park-current-thread-if!
     rt-prx
     (fn (gen)
       (at:atomic-push (get-set% pset)
                       (Tuple gen (current-thread! rt-prx)))
       Unit)
     should-park?))

  (inline)
  (declare park-in-sets-if ((BaseIo :io) (MonadIoThread :rt :t :io) (MonadIo :m)
                             => :io Boolean -> List (ParkingSet :t) -> :m Unit))
  (define (park-in-sets-if should-park? psets)
    "Parks the current thread in PSETS if SHOULD-PARK? returns True. Will park the thread
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
         (for pset in psets
           (at:atomic-push (get-set% pset)
                           (Tuple gen (current-thread! rt-prx))))
         Unit))
     should-park?))

  (inline)
  (declare park-in-set-if ((BaseIo :io) (MonadIoThread :rt :t :io) (MonadIo :m)
                             => :io Boolean -> ParkingSet :t -> :m Unit))
  (define (park-in-set-if should-park? pset)
    "Parks the current thread in PSET if SHOULD-PARK? returns True. Will park the thread
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
         (at:atomic-push (get-set% pset)
                         (Tuple gen (current-thread! rt-prx)))
         Unit))
     should-park?))

  (inline)
  (declare unpark-set% (Runtime :rt :t => Proxy :rt -> ParkingSet :t -> Unit))
  (define (unpark-set% rt-prx pset)
    (let parked-entries = (at:atomic-swap (get-set% pset) Nil))
    (for (Tuple gen thread) in parked-entries
      (unpark-thread! rt-prx gen thread))
    Unit)

  (inline)
  (declare unpark-set (MonadIoThread :rt :t :m => ParkingSet :t -> :m Unit))
  (define (unpark-set pset)
    "Atomically reset PSET, then attempt to unpark all threads parked on the set.

Concurrent:
  - Can briefly block while trying to reset the set or unpark a parked thread"
    (inject-runtime unpark-set% pset))

 )
