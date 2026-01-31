(cl:in-package :cl-user)
(defpackage :io/classes/conc/scheduler
  (:use
   #:coalton
   #:coalton-prelude
   #:io/classes/thread
   )
  (:export
   #:Scheduler
   #:submit
   #:submit-with
   #:try-submit
   #:take-item
   ))
(in-package :io/classes/conc/scheduler)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

 ;; TODO: The definition here is simple, but the WorkerPool has to use
 ;; (Scheduler (Optional (:i Unit))). Specialize the functions on the class more so that
 ;; the type signature elsewhere is more simplified.
 (define-class (Scheduler :s)
   "A Scheduler distributes work from producer threads to worker threads. Generally, the
Scheduler is allowed to completely control the worker threads: it can sleep, block, spin,
etc., as dictated by the Scheduler algorithm. However, it must give producer threads the
choice of how to respond to the state of the Scheduler. For example, if the Scheduler is
bounded and its internal queue is full, the producer thread chooses whether to block or
fail based on its choice of which submit function to call.

The main purpose of a Scheduler is to pass into a WorkerPool to configure its scheduling
algorithm. A Scheduler could have uses in other contexts.

A Scheduler may be bounded or unbounded and still satisfy the Scheduler interface. For
unbounded schedulers, `submit` will never block and `try-submit` will always succeed.

Threads asking for work will submit a thread-index to the scheduler. The Scheduler can
choose to use this, particularly if it maintains a separate queue for each thread, or
choose to ignore it, if it only uses a single queue for all threads. Thread index values
should be 0-indexed, from [0, n-threads)."
   (submit
    "Submit a new item to the Scheduler.

Concurrent:
  - Blocks if the Scheduler is full. Only bounded Schedulers will ever be full."
    (Threads :rt :t :m => :a -> :s :a -> :m Unit))
   (submit-with
    "Submit a new item to the Scheduler.

Concurrent:
  - Blocks if the Scheduler is full, possibly timing out based on STRATEGY. Only bounded
Schedulers will ever be full."
    (Threads :rt :t :m => :a -> TimeoutStrategy -> :s :a -> :m Unit))
   (try-submit
    "Attempt to submit a new item to the Scheduler. Returns `True` if the item was added,
or `False` if the Scheduler was full. Only bounded Schedulers can be full."
    (Threads :rt :t :m => :a -> :s :a -> :m Boolean))
   (take-item
    "Take the next item from the Scheduler for the given thread.

Concurrent:
  - May block, sleep, spin, or do anything else to the requesting thread, except
    (1) leave it masked after returning, or (2) stop the thread."
    (Threads :rt :t :m => UFix -> :s :a -> :m :a)))

  )
