(cl:in-package :cl-user)
(defpackage :io/gen-impl/conc/ring-buffer
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/cell
   #:coalton-library/types
   #:io/classes/monad-io
   #:io/classes/thread
   #:io/classes/runtime-utils
   )
  (:local-nicknames
   (:opt #:coalton-library/optional)
   (:v #:coalton-library/vector)
   (:bt #:io/utilities/bt-compat)

   )
  (:export
   ;; Library Public
   #:RingBuffer

   #:new-ring-buffer
   #:enqueue
   #:try-enqueue
   #:dequeue

   ;; Library Private
   #:new-ring-buffer%
   #:enqueue!%
   #:enqueue-with!%
   #:try-enqueue!%
   #:dequeue!%
   #:dequeue-with!%
   ))
(in-package :io/gen-impl/conc/ring-buffer)

(cl:declaim (cl:optimize (cl:speed 3) (cl:debug 0) (cl:safety 1)))

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (define-struct (RingBuffer :a)
    "A bounded FIFO Multi-Producer, Multi-Consumer Queue implemented as a RingBuffer."
    (capacity         UFix)
    ;; Allow clearing the data so the buffer doesn't hold onto it and keep stale
    ;; data from being GC'd.
    (data             (Vector (Optional :a)))
    ;; It's required to track the count b/c insert-ptr == read-ptr in both the
    ;; empty and the full case
    (count            (Cell UFix))
    (insert-ptr       (Cell UFix))
    (read-ptr         (Cell UFix))
    (lock             bt:Lock)
    (notify-not-empty bt:ConditionVariable)
    (notify-not-full  bt:ConditionVariable))

  (inline)
  (declare increment% (RingBuffer :a * UFix -> UFix))
  (define (increment% buffer i)
    "Get the next index in BUFFER."
    (let next = (1+ i))
    (if (>= next (.capacity buffer))
        0
        next))

  (inline)
  (declare empty?% (RingBuffer :a -> Boolean))
  (define (empty?% buffer)
    (zero? (read (.count buffer))))

  (inline)
  (declare full?% (RingBuffer :a -> Boolean))
  (define (full?% buffer)
    (== (read (.count buffer)) (.capacity buffer)))

  (inline)
  (declare new-ring-buffer% (UFix -> RingBuffer :a))
  (define (new-ring-buffer% capacity)
    "Create a new ring buffer with the given capacity."
    (RingBuffer
     capacity
     (lisp (-> Vector :a) (capacity)
       (cl:make-array capacity :element-type cl:t :adjustable cl:nil :fill-pointer cl:nil))
     (new 0)
     (new 0)
     (new 0)
     (bt:new-lk)
     (bt:new-cv)
     (bt:new-cv)))

  (declare enqueue-with!% (Runtime :rt :t => Proxy :rt * :a * TimeoutStrategy * RingBuffer :a -> Void))
  (define (enqueue-with!% rt-prx elt strategy buffer)
    "Add ELT to BUFFER.

Concurrent:
  - Can block acquiring lock on buffer.
  - If full, blocks until BUFFER is not full, possibly timing out based on STRATEGY."
    ;; CONCURRENT:
    ;; - Masks before entering the critical region
    ;; - unmask-and-await-safely% unmasks and awaits, then wakes and re-masks in a
    ;;   catch block guaranteeing lock release.
    ;; - Unmasks before exiting the function.
    ;; - Broadcasts to wake all waiters, because can't guarantee there's only one
    ;;   waiter. Because of the optimization to only notify on empty->not-empty
    ;;   transition, if multiple enqueues race against the single woken dequeue,
    ;;   then subsequent dequeues can fail to get notified even if the buffer is
    ;;   non-empty.
    ;; - Notifying after release is valid because all waiter/notifiers evaluate guard
    ;;   condition and interpose lock acquisition before waiting/notifying.
    ;;   See https://stackoverflow.com/questions/21439359/signal-on-condition-variable-without-holding-lock
    (mask-current! rt-prx)
    (bt:acquire (.lock buffer))
    (rec % ()
      (if (full?% buffer)
          (progn
            (unmask-and-await-safely-with%
             rt-prx
             strategy
             (.notify-not-full buffer)
             (.lock buffer))
            (%))
          (progn
            (let should-notify = (empty?% buffer))
            (v:set! (read (.insert-ptr buffer))
                    (Some elt)
                    (.data buffer))
            (update! 1+ (.count buffer))
            (update! (fn (n)
                       (increment% buffer n))
                     (.insert-ptr buffer))
            (bt:release (.lock buffer))
            (when should-notify
              (bt:broadcast (.notify-not-empty buffer)))
            (unmask-current! rt-prx))
            )))

  (inline)
  (declare enqueue!% (Runtime :rt :t => Proxy :rt * :a * RingBuffer :a -> Void))
  (define (enqueue!% rt-prx elt buffer)
    "Add ELT to BUFFER.

Concurrent:
  - Can block acquiring lock on buffer.
  - If full, blocks until BUFFER is not full."
    (enqueue-with!% rt-prx elt NoTimeout buffer))

  (declare try-enqueue!% (Runtime :rt :t => Proxy :rt * :a * RingBuffer :a -> Boolean))
  (define (try-enqueue!% rt-prx elt buffer)
    "Attempt to add ELT to BUFFER. Returns True if equeue succeeded, False otherwise.

Concurrent: Can block acquiring lock on buffer."
    ;; CONCURRENT:
    ;; - Masks before entering the critical region
    ;; - Unmasks before exiting the function.
    ;; - Broadcasts to wake all waiters, because can't guarantee there's only one
    ;;   waiter. Because of the optimization to only notify on empty->not-empty
    ;;   transition, if multiple enqueues race against the single woken dequeue,
    ;;   then subsequent dequeues can fail to get notified even if the buffer is
    ;;   non-empty.
    ;; - Notifying after release is valid because all waiter/notifiers evaluate guard
    ;;   condition and interpose lock acquisition before waiting/notifying.
    ;;   See https://stackoverflow.com/questions/21439359/signal-on-condition-variable-without-holding-lock
    (mask-current! rt-prx)
    (bt:acquire (.lock buffer))
    (if (full?% buffer)
        (progn
          (bt:release (.lock buffer))
          (unmask-current! rt-prx)
          False)
        (progn
          (let should-notify = (empty?% buffer))
          (v:set! (read (.insert-ptr buffer))
                  (Some elt)
                  (.data buffer))
          (update! 1+ (.count buffer))
          (update! (fn (n)
                     (increment% buffer n))
                   (.insert-ptr buffer))
          (bt:release (.lock buffer))
          (when should-notify
            (bt:broadcast (.notify-not-empty buffer)))
          (unmask-current! rt-prx)
          True)
        ))

  (declare dequeue-with!% (Runtime :rt :t => Proxy :rt * TimeoutStrategy * RingBuffer :a -> :a))
  (define (dequeue-with!% rt-prx strategy buffer)
    "Pop an element from BUFFER.

Concurrent:
  - Can block briefly while acquiring lock on buffer.
  - If empty, blocks until BUFFER is not empty, possibly timing out based on STRATEGY."
    ;; CONCURRENT:
    ;; - Masks before entering the critical region
    ;; - unmask-and-await-safely% unmasks and awaits, then wakes and re-masks in a
    ;;   catch block guaranteeing lock release.
    ;; - Unmasks before exiting the function.
    ;; - Broadcasts to wake all waiters, because can't guarantee there's only one
    ;;   waiter. Because of the optimization to only notify on full->not-full
    ;;   transition, if multiple dequeues race against the single woken equeue,
    ;;   then subsequent enqueues can fail to get notified even if the buffer is
    ;;   non-full.
    ;; - Notifying after release is valid because all waiter/notifiers evaluate guard
    ;;   condition and interpose lock acquisition before waiting/notifying.
    ;;   See https://stackoverflow.com/questions/21439359/signal-on-condition-variable-without-holding-lock
    (mask-current! rt-prx)
    (bt:acquire (.lock buffer))
    (rec % ()
      (if (empty?% buffer)
          (progn
            (unmask-and-await-safely-with%
             rt-prx
             strategy
             (.notify-not-empty buffer)
             (.lock buffer))
            (%))
          (progn
            (let should-notify = (full?% buffer))
            (let read-i = (read (.read-ptr buffer)))
            (let elt = (opt:from-some "RingBuffer access error"
                                      (v:index-unsafe read-i (.data buffer))))
            ;; Allows stale data to be GC'd
            (v:set! read-i
                    None
                    (.data buffer))
            (update! 1- (.count buffer))
            (update! (fn (n)
                       (increment% buffer n))
                     (.read-ptr buffer))
            (bt:release (.lock buffer))
            (when should-notify
              (bt:broadcast (.notify-not-full buffer)))
            (unmask-current! rt-prx)
            elt)
          )))

  (inline)
  (declare dequeue!% (Runtime :rt :t => Proxy :rt * RingBuffer :a -> :a))
  (define (dequeue!% rt-prx buffer)
    "Pop an element from BUFFER.

Concurrent:
  - Can block briefly while acquiring lock on buffer.
  - If empty, blocks until BUFFER is not empty."
    (dequeue-with!% rt-prx NoTimeout buffer))
  )

;;;
;;; IO Wrappers
;;;

(coalton-toplevel

  (inline)
  (declare new-ring-buffer (Threads :rt :t :m => UFix -> :m (RingBuffer :a)))
  (define (new-ring-buffer capacity)
    "Create a new ring buffer with the given capacity."
    (wrap-io (new-ring-buffer% capacity)))

  (inline)
  (declare enqueue (Threads :rt :t :m
                    => :a * RingBuffer :a
                    &key (:timeout TimeoutStrategy)
                    -> :m Unit))
  (define (enqueue elt buffer &key (timeout NoTimeout))
    "Add ELT to BUFFER. Can specify a timeout.

Concurrent:
  - Can block acquiring lock on buffer.
  - If full, blocks until BUFFER is not full, possibly timing out based on TIMEOUT."
    (inject-runtime-unit enqueue-with!% elt timeout buffer))

  (inline)
  (declare try-enqueue (Threads :rt :t :m => :a * RingBuffer :a -> :m Boolean))
  (define (try-enqueue elt buffer)
    "Attempt to add ELT to BUFFER. Returns True if equeue succeeded, False otherwise.

Concurrent: Can block acquiring lock on buffer."
    (inject-runtime try-enqueue!% elt buffer))

  (inline)
  (declare dequeue (Threads :rt :t :m
                    => RingBuffer :a
                    &key (:timeout TimeoutStrategy)
                    -> :m :a))
  (define (dequeue buffer &key (timeout NoTimeout))
    "Pop an element from BUFFER. Can specify a timeout.

Concurrent:
  - Can block briefly while acquiring lock on buffer.
  - If empty, blocks until BUFFER is not empty, possibly timing out based on TIMEOUT."
    (inject-runtime dequeue-with!% timeout buffer))

  )
