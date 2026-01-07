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
   #:park-in-queues-if_
   #:park-in-queue-if_
   ))
(in-package :io/io-impl/conc/parking)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (inline)
  (declare park-in-queues-if_ (MonadIo :m
                               => IO Boolean -> List (ParkingQueue IoThread) -> :m Unit))
  (define park-in-queues-if_
    "Parks the current thread in PQUEUES if SHOULD-PARK? returns True. Will park the thread
until woken by an unpark from another thread. Upon an unpark, the thread will resume even
if SHOULD-PARK? is False! SHOULD-PARK? is only checked to determine if the thread should
park, *not* if it should resume.

Concurrent:
  - WARNING: SHOULD-PARK? must not block, or the thread could be left blocked in a masked
    state.
  - Can briefly block while trying to park the thread, if contended."
    park-in-queues-if)

  (inline)
  (declare park-in-queue-if_ (MonadIo :m => IO Boolean -> ParkingQueue IoThread -> :m Unit))
  (define park-in-queue-if_
    "Parks the current thread in PQUEUE if SHOULD-PARK? returns True. Will park the thread
until woken by an unpark from another thread. Upon an unpark, the thread will resume even
if SHOULD-PARK? is False! SHOULD-PARK? is only checked to determine if the thread should
park, *not* if it should resume.

Concurrent:
  - WARNING: SHOULD-PARK? must not block, or the thread could be left blocked in a masked
    state.
  - Can briefly block while trying to park the thread, if contended."
    park-in-queue-if)
  )
