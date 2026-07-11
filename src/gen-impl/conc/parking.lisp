(cl:in-package :cl-user)
(defpackage :io/gen-impl/conc/parking
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton/types
   #:io/utils
   #:io/classes/monad-io
   #:io/classes/thread
   )
  (:local-nicknames
   (:l #:coalton/list)
   (:at #:io/threads-impl/atomics)
   )
  (:export
   ;; Library Public
   #:ParkingSet
   #:new-parking-set
   #:park-in-sets-if
   #:park-in-sets
   #:park-in-set-if
   #:park-in-set
   #:unpark-set
   #:unpark-one
   #:parking-set-empty?

   ;; Library Private
   #:new-parking-set%
   #:park-in-sets-if%
   #:park-in-sets-if-with%
   #:park-in-set-if%
   #:park-in-set-if-with%
   #:unpark-set%
   #:unpark-one%
   #:num-waiters
   #:parking-set-empty?%
   ))
(in-package :io/gen-impl/conc/parking)

;; TODO: Reorganize this file to cleanly separate the private and public APIs

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (repr :transparent)
  (define-type ParkingSet
    "ParkingSet is a thread-safe list of parked threads. A parking thread can atomically
subscibe and park on a Parking Set, and a signalling thread can atomically unpark all
parkers on a ParkingSet.

In general, ParkingSet is the preferred way to park and unpark threads. The lower-level
parking functions exposed by the Threads and Runtime classes should only be used
if ParkingSet doesn't provide enough functionality for the algorithm.

Concurrent:
  - ParkingSet's algorithms are lock free, but individual threads can block for a very
    short window if contention on the parking set is very high."
    (ParkingSet% (at:Atomic (List (Void -> Boolean)))))

  (inline)
  (declare new-parking-set% (Void -> ParkingSet))
  (define (new-parking-set%)
    (ParkingSet% (at:new Nil)))

  (inline)
  (declare new-parking-set (MonadIo :m => :m ParkingSet))
  (define new-parking-set
    "Create a new ParkingSet."
    (wrap-io_ new-parking-set%))

  (inline)
  (declare get-set% (ParkingSet -> at:Atomic (List (Void -> Boolean))))
  (define (get-set% (ParkingSet% atm))
    atm)

  (inline)
  (declare park-in-sets-if-with% (Runtime :rt :t
                                  => Proxy :rt
                                  * (Void -> Boolean)
                                  * TimeoutStrategy
                                  * List ParkingSet
                                  -> Void))
  (define (park-in-sets-if-with% rt-prx should-park? strategy psets)
    (park-current-thread-if!
     rt-prx
     (fn (gen)
       (let parked-thread = (current-thread! rt-prx))
       (let unpark-action = (fn ()
                              (unpark-thread! rt-prx gen parked-thread)))
       (foreach (pset psets)
         (at:atomic-push (get-set% pset) unpark-action))
       (values))
     should-park?
     :timeout strategy)
    (values))

  (inline)
  (declare park-in-sets-if% (Runtime :rt :t
                              => Proxy :rt * (Void -> Boolean) * List ParkingSet -> Void))
  (define (park-in-sets-if% rt-prx should-park? psets)
    (park-in-sets-if-with% rt-prx should-park? NoTimeout psets)
    (values))

  (inline)
  (declare park-in-set-if-with% (Runtime :rt :t
                                 => Proxy :rt
                                 * (Void -> Boolean)
                                 * TimeoutStrategy
                                 * ParkingSet
                                 -> Void))
  (define (park-in-set-if-with% rt-prx should-park? strategy pset)
    (park-current-thread-if!
     rt-prx
     (fn (gen)
       (let parked-thread = (current-thread! rt-prx))
       (let unpark-action = (fn ()
                              (unpark-thread! rt-prx gen parked-thread)))
       (at:atomic-push (get-set% pset) unpark-action)
       (values))
     should-park?
     :timeout
     strategy)
    (values))

  (inline)
  (declare park-in-set-if% (Runtime :rt :t
                              => Proxy :rt * (Void -> Boolean) * ParkingSet -> Void))
  (define (park-in-set-if% rt-prx should-park? pset)
    (park-in-set-if-with% rt-prx should-park? NoTimeout pset))

  (inline)
  (declare park-in-sets-if ((BaseIo :io) (Threads :rt :t :io) (MonadIo :m)
                             => :io Boolean * List ParkingSet
                             &key (:timeout TimeoutStrategy)
                             -> :m Unit))
  (define (park-in-sets-if should-park? psets &key (timeout NoTimeout))
    "Parks the current thread in PSETS if SHOULD-PARK? returns True. Will park the thread
until woken by an unpark from another thread. Upon an unpark, the thread will resume even
if SHOULD-PARK? is False! SHOULD-PARK? is only checked to determine if the thread should
park, *not* if it should resume. Can specify a timeout.

Concurrent:
  - WARNING: SHOULD-PARK? must not block, or the thread could be left blocked in a masked
    state."
    (park-current-thread-if
     (fn (gen)
       (wrap-io-with-runtime (rt-prx)
         ;; Need to set current-thread on parking thread, not on the unparking-thread!
         (let parked-thread = (current-thread! rt-prx))
         (let unpark-action = (fn ()
                                (unpark-thread! rt-prx gen parked-thread)))
         (foreach (pset psets)
           (at:atomic-push (get-set% pset) unpark-action))
         Unit))
     should-park?
     :timeout timeout))

  (inline)
  (declare park-in-sets (Threads :rt :t :m => List ParkingSet &key (:timeout TimeoutStrategy)
                                 -> :m Unit))
  (define (park-in-sets psets &key (timeout NoTimeout))
    "Parks the current thread in `psets`. Will park the thread until woken by an unpark
from another thread. Can specify a timeout."
    (wrap-io-with-runtime (rt-prx)
      (park-current-thread-if!
       rt-prx
       (fn (gen)
         (let parked-thread = (current-thread! rt-prx))
         (let unpark-action = (fn ()
                                (unpark-thread! rt-prx gen parked-thread)))
         (foreach (pset psets)
           (at:atomic-push (get-set% pset) unpark-action)))
       ƒ.True
       :timeout timeout)
      Unit))

  (inline)
  (declare park-in-set-if ((BaseIo :io) (Threads :rt :t :io) (MonadIo :m)
                           => :io Boolean * ParkingSet
                           &key (:timeout TimeoutStrategy)
                           -> :m Unit))
  (define (park-in-set-if should-park? pset &key (timeout NoTimeout))
    "Parks the current thread in PSET if SHOULD-PARK? returns True. Will park the thread
until woken by an unpark from another thread. Upon an unpark, the thread will resume even
if SHOULD-PARK? is False! SHOULD-PARK? is only checked to determine if the thread should
park, *not* if it should resume. Can specify a timeout.

Concurrent:
  - WARNING: SHOULD-PARK? must not block, or the thread could be left blocked in a masked
    state."
    (park-current-thread-if
     (fn (gen)
       (wrap-io-with-runtime (rt-prx)
         ;; Need to set current-thread on parking thread, not on the unparking-thread!
         (let parked-thread = (current-thread! rt-prx))
         (let unpark-action = (fn ()
                                (unpark-thread! rt-prx gen parked-thread)))
         (at:atomic-push (get-set% pset) unpark-action)
         Unit))
     should-park?
     :timeout timeout))

  (inline)
  (declare park-in-set (Threads :rt :t :m => ParkingSet &key (:timeout TimeoutStrategy)
                        -> :m Unit))
  (define (park-in-set pset &key (timeout NoTimeout))
    "Parks the current thread in `pset`. Will park the thread until woken by an unpark
from another thread. Can specify a timeout."
    (wrap-io-with-runtime (rt-prx)
      (park-current-thread-if!
       rt-prx
       (fn (gen)
         (let parked-thread = (current-thread! rt-prx))
         (let unpark-action = (fn ()
                                (unpark-thread! rt-prx gen parked-thread)))
         (at:atomic-push (get-set% pset) unpark-action)
         (values))
       ƒ.True
       :timeout timeout)
      Unit))

  (inline)
  (declare unpark-set% (Runtime :rt :t => ParkingSet * Proxy :rt -> Void))
  (define (unpark-set% pset rt-prx)
    ;; CONCURRENT:
    ;; - Masks before taking ownership of the existing pset.
    ;; - Unmasks after dispatching actions.
    (mask-current! rt-prx)
    (let parked-actions = (at:atomic-swap (get-set% pset) Nil))
    (foreach (action parked-actions)
      (action))
    (unmask-current! rt-prx)
    (values))

  (inline)
  (declare unpark-set (Threads :rt :t :m => ParkingSet -> :m Unit))
  (define (unpark-set pset)
    "Atomically reset PSET, then attempt to unpark all threads parked on the set."
    (wrap-io-with-runtime (rt-prx)
      (unpark-set% pset rt-prx)
      Unit))

  (declare unpark-one% (Runtime :rt :t => ParkingSet * Proxy :rt &key (:fair Boolean) -> Void))
  (define (unpark-one% pset rt-prx &key (fair False))
    ;; CONCURRENT:
    ;; - Masks before taking ownership of unparked action.
    ;; - Unmasks after dispatching action.
    ;; - Could technically unmask before recursion in stale case, but that would be
    ;;   inefficient because it would immediately remask.
    (mask-current! rt-prx)
    (rec % ()
      (let parked-actions = (at:atomic-update-swap (get-set% pset)
                                                   (if fair
                                                       l:init
                                                       ƒl.(l:drop 1 l))))
      (let parked-action? =
        (if fair
            (l:last parked-actions)
            (l:head parked-actions)))
      (match parked-action?
        ((None)
         (values))
        ((Some parked-action)
         (if (parked-action)
             (values)
             (%)))))
    (unmask-current! rt-prx))

  (inline)
  (declare unpark-one (Threads :rt :t :m => ParkingSet &key (:fair Boolean) -> :m Unit))
  (define (unpark-one pset &key (fair False))
    "Unpark one thread parked in `pset`. If no threads are parked, does nothing.

If `fair` is `True`, unparks the thread which has been parked the longest. If `fair` is
`False`, unparks an arbitrary parked thread."
    (wrap-io-with-runtime (rt-prx)
      (unpark-one% pset rt-prx :fair fair)
      Unit))

  (inline)
  (declare num-waiters (MonadIo :m => ParkingSet -> :m UFix))
  (define (num-waiters pset)
    "Get the number of waiters in PSET."
    (wrap-io
     (let (ParkingSet% at-waiters) = pset)
     (let waiters = (at:read at-waiters))
     (length waiters)))

  (inline)
  (declare parking-set-empty?% (ParkingSet -> Boolean))
  (define (parking-set-empty?% pset)
    "Check if `pset` is empty."
    (let (ParkingSet% at-waiters) = pset)
    (let waiters = (at:read at-waiters))
    (match waiters
      ((Nil) True)
      (_ False)))

  (inline)
  (declare parking-set-empty? (MonadIo :m => ParkingSet -> :m Boolean))
  (define (parking-set-empty? pset)
    "Check if `pset` is empty."
    (wrap-io
     (parking-set-empty?% pset)))
 )
