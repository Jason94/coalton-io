(cl:in-package :cl-user)
(defpackage :io/io-impl/conc/parking
  (:use
   #:coalton
   #:coalton-prelude
   #:io/classes/monad-io
   #:io/gen-impl/conc/parking
   #:io/thread-impl/runtime
   #:io/io-impl/simple-io
   )
  (:export
   #:park-in-sets-if_
   #:park-in-set-if_
   ))
(in-package :io/io-impl/conc/parking)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (inline)
  (declare park-in-sets-if_ (MonadIo :m
                               => IO Boolean -> List ParkingSet -> :m Unit))
  (define park-in-sets-if_
    "Parks the current thread in PSETS if SHOULD-PARK? returns True. Will park the thread
until woken by an unpark from another thread. Upon an unpark, the thread will resume even
if SHOULD-PARK? is False! SHOULD-PARK? is only checked to determine if the thread should
park, *not* if it should resume.

Concurrent:
  - WARNING: SHOULD-PARK? must not block, or the thread could be left blocked in a masked
    state.
  - Can briefly block while trying to park the thread, if contended."
    park-in-sets-if)

  (inline)
  (declare park-in-set-if_ (MonadIo :m => IO Boolean -> ParkingSet -> :m Unit))
  (define park-in-set-if_
    "Parks the current thread in PSET if SHOULD-PARK? returns True. Will park the thread
until woken by an unpark from another thread. Upon an unpark, the thread will resume even
if SHOULD-PARK? is False! SHOULD-PARK? is only checked to determine if the thread should
park, *not* if it should resume.

Concurrent:
  - WARNING: SHOULD-PARK? must not block, or the thread could be left blocked in a masked
    state.
  - Can briefly block while trying to park the thread, if contended."
    park-in-set-if)
  )
