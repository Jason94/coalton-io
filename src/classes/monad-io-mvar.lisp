(cl:in-package :cl-user)
(defpackage :io/classes/monad-io-mvar
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/functions
   #:io/classes/monad-io
   #:io/thread-impl/data-broadcast-pool)
  (:local-nicknames
   (:lk #:coalton-threads/lock)
   (:cv #:coalton-threads/condition-variable)
   (:at #:io/thread-impl/atomics))
  (:export
   #:MVar
   #:MonadIoMVar
   #:new-mvar
   #:new-empty-mvar
   #:take-mvar
   #:put-mvar
   #:try-take-mvar
   #:try-put-mvar
   #:read-mvar
   #:try-read-mvar
   #:swap-mvar
   #:is-empty-mvar
   ))
(in-package :io/classes/monad-io-mvar)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (define-struct (MVar :a)
    "A synchronized container that can be empty or hold one :a.
Can put data into the container, blocking until it is empty.
Can take data out of the container, blocking until it is full."
    (lock                lk:Lock)
    (read-broadcast-pool (DataBroadcastPool :a))
    (notify-full         cv:ConditionVariable)
    (notify-empty        cv:ConditionVariable)
    (data                (at:Atomic (Optional :a))))

  (define-class (Monad :m => MonadIoMVar :m)
    "Manage synchronized containers of mutable state.

All critical MVar operations are masked, so stopping a thread that's operating on
MVar's won't leave MVar's in an inoperable state. However, irresponsible stopping
could still cause deadlocks, race conditions, etc. if other threads were
depending on the stopped thread operating on an MVar to continue."
    (new-mvar
     "Create a new MVar containing an initial value."
     (:a -> :m (MVar :a)))
    (new-empty-mvar
     "Create a new empty MVar."
     (:m (MVar :a)))
    (take-mvar
     "Take a value from an MVar, blocking until one is available."
     (MVar :a -> :m :a))
    (put-mvar
     "Put a value into an MVar, blocking until it becomes empty."
     (MVar :a -> :a -> :m Unit))
    (try-take-mvar
     "Attempt to take a value from an MVar; returns None if empty."
     (MVar :a -> :m (Optional :a)))
    (try-put-mvar
     "Attempt to put a value into an MVar; returns False if full and the put fails,
True if the put succeeds."
     (MVar :a -> :a -> :m Boolean))
    (read-mvar
     "Read (without removing) the value from an MVar, blocking until one is available."
     (MVar :a -> :m :a))
    (try-read-mvar
     "Attempt to read (without removing) the value from an MVar; returns None if empty."
     (MVar :a -> :m (Optional :a)))
    (swap-mvar
     "Atomically replace the value in an MVar and return the old value."
     (MVar :a -> :a -> :m :a))
    (is-empty-mvar
     "Return True if the MVar is currently empty."
     (MVar :a -> :m Boolean))))
