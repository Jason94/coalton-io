(cl:in-package :cl-user)
(defpackage :io/gen-impl/conc/queues
  (:use
   #:coalton
   #:coalton-prelude
   #:io/classes/thread
   )
  (:export
   ;; Library Public
   #:Queue
   #:enqueue
   #:try-enqueue
   #:dequeue
   #:try-dequeue
   ))
(in-package :io/gen-impl/conc/queues)

(cl:declaim (cl:optimize (cl:speed 3) (cl:debug 0) (cl:safety 1)))

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (define-class (Queue :q)
    "A First-in, First-out (FIFO) queue.

All `Queue` instances share the same API, but are allowed to have different invariants for
memory, synchronicity, etc. For example, a `Queue` is allowed to be either bounded or
unbounded; may be threadsafe to enqueue, dequeue, or both; etc.

There are some guarantees that must hold for certain types of `Queue`s. For example, if
a `Queue` is bounded then `enqueue` must block if it is full."
    (enqueue
     "Add a value to the end of the queue. If the queue is bounded and full, must block
until the value can be added.

Can specify a timeout."
     (Threads :rt :t :m => :a * :q :a &key (:timeout TimeoutStrategy) -> :m Unit))
    (try-enqueue
     "Attempt to add a value to the end of the queue. Returns `True` if it succeeded, or
`False` if it failed. If the queue is unbounded, must always return true."
     (Threads :rt :t :m => :a * :q :a -> :m Boolean))
    (dequeue
     "Pop the value at the front of the queue. If the queue is empty, must block until a
value is present.

Can specify a timeout."
     (Threads :rt :t :m => :q :a &key (:timeout TimeoutStrategy) -> :m :a))
    (try-dequeue
     "Try to pop the value at the front of the queue. Returns `None` if empty."
     (Threads :rt :t :m => :q :a -> :m (Optional :a))))
  )
