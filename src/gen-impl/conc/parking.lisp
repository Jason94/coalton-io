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
  (define-type ParkingSet
    "ParkingSet is a thread-safe list of parked threads. A parking thread can atomically
subscibe and park on a Parking Set, and a signalling thread can atomically unpark all
parkers on a ParkingSet.

In general, ParkingSet is the preferred way to park and unpark threads. The lower-level
parking functions exposed by the MonadIoThread and Runtime classes should only be used
if ParkingSet doesn't provide enough functionality for the algorithm.

Concurrent:
  - ParkingSet's algorithms are lock free, but individual threads can block for a very
    short window if contention on the parking set is very high."
    (ParkingSet% (at:Atomic (List (Unit -> Unit)))))

  (inline)
  (declare new-parking-set% (Unit -> ParkingSet))
  (define (new-parking-set%)
    (ParkingSet% (at:new Nil)))

  (inline)
  (declare new-parking-set (MonadIo :m => :m ParkingSet))
  (define new-parking-set
    "Create a new ParkingSet."
    (wrap-io_ new-parking-set%))

  (inline)
  (declare get-set% (ParkingSet -> at:Atomic (List (Unit -> Unit))))
  (define (get-set% (ParkingSet% atm))
    atm)

  (inline)
  (declare park-in-sets-if% (Runtime :rt :t
                              => Proxy :rt -> (Unit -> Boolean) -> List ParkingSet -> Unit))
  (define (park-in-sets-if% rt-prx should-park? psets)
    (park-current-thread-if!
     rt-prx
     (fn (gen)
       (let unpark-action = (fn (_)
                              (unpark-thread! rt-prx gen (current-thread! rt-prx))))
       (for pset in psets
         (at:atomic-push (get-set% pset) unpark-action))
       Unit)
     should-park?))

  (inline)
  (declare park-in-set-if% (Runtime :rt :t
                              => Proxy :rt -> (Unit -> Boolean) -> ParkingSet -> Unit))
  (define (park-in-set-if% rt-prx should-park? pset)
    (park-current-thread-if!
     rt-prx
     (fn (gen)
       (let unpark-action = (fn (_)
                              (unpark-thread! rt-prx gen (current-thread! rt-prx))))
       (at:atomic-push (get-set% pset) unpark-action)
       Unit)
     should-park?))

  (inline)
  (declare park-in-sets-if ((BaseIo :io) (MonadIoThread :rt :t :io) (MonadIo :m)
                             => :io Boolean -> List ParkingSet -> :m Unit))
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
         ;; Need to set current-thread on parking thread, not on the unparking-thread!
         (let parked-thread = (current-thread! rt-prx))
         (let unpark-action = (fn ()
                                (unpark-thread! rt-prx gen parked-thread)))
         (for pset in psets
           (at:atomic-push (get-set% pset) unpark-action))
         Unit))
     should-park?))

  (inline)
  (declare park-in-set-if ((BaseIo :io) (MonadIoThread :rt :t :io) (MonadIo :m)
                             => :io Boolean -> ParkingSet -> :m Unit))
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
         ;; Need to set current-thread on parking thread, not on the unparking-thread!
         (let parked-thread = (current-thread! rt-prx))
         (let unpark-action = (fn ()
                                (unpark-thread! rt-prx gen parked-thread)))
         (at:atomic-push (get-set% pset) unpark-action)
         Unit))
     should-park?))

  (inline)
  (declare unpark-set% (ParkingSet -> Unit))
  (define (unpark-set% pset)
    (let parked-actions = (at:atomic-swap (get-set% pset) Nil))
    (for action in parked-actions
      (action))
    Unit)

  (inline)
  (declare unpark-set (MonadIo :m => ParkingSet -> :m Unit))
  (define (unpark-set pset)
    "Atomically reset PSET, then attempt to unpark all threads parked on the set.

Concurrent:
  - Can briefly block while trying to reset the set or unpark a parked thread"
    (wrap-io (unpark-set% pset)))

 )
