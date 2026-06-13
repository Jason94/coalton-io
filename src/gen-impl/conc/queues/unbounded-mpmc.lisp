(cl:in-package :cl-user)
(defpackage :io/gen-impl/conc/queues/unbounded-mpmc
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/experimental/do-control-core
   #:io/classes/thread
   #:io/gen-impl/conc/mvar
   )
  (:local-nicknames
   )
  (:export
   ;; Library Public
   #:UnboundedMpmcQueue
   #:new-unbounded-mpmc-queue
   #:enqueue
   #:dequeue
   #:try-dequeue
   ))
(in-package :io/gen-impl/conc/queues/unbounded-mpmc)

(cl:declaim (cl:optimize (cl:speed 3) (cl:debug 0) (cl:safety 1)))

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (define-type (ChanNode :a)
    (ChanNode% :a (MVar (ChanNode :a))))

  (define-struct (UnboundedMpmcQueue :a)
    "A synchronized FIFO queue to pass data between threads."
    (head-var (MVar (MVar (ChanNode :a))))
    (tail-var (MVar (MVar (ChanNode :a)))))

  (declare new-unbounded-mpmc-queue (Threads :rt :t :m => :m (UnboundedMpmcQueue :a)))
  (define new-unbounded-mpmc-queue
    "Create a new empty queue."
    (do
      (cell <- new-empty-mvar)
      (head-var <- (new-mvar cell))
      (tail-var <- (new-mvar cell))
      (pure (UnboundedMpmcQueue head-var tail-var))))

  (declare enqueue (Threads :rt :t :m => :a * UnboundedMpmcQueue :a -> :m Unit))
  (define (enqueue val queue)
    "Add VAL to QUEUE."
    (do
     (new-tail-var <- new-empty-mvar)
     (old-tail-var <- (take-mvar-masked (.tail-var queue))) ;; Masks the thread after this returns
     (put-mvar old-tail-var (ChanNode% val new-tail-var))
     (put-mvar (.tail-var queue) new-tail-var)
     unmask-current-thread)) ;; Cleanup after take-mvar-masked

  (declare dequeue (Threads :rt :t :m
                    => UnboundedMpmcQueue :a &key (:timeout TimeoutStrategy)
                    -> :m :a))
  (define (dequeue queue &key (timeout NoTimeout))
    "Pop the front value in QUEUE. Blocks while QUEUE is empty. Can specify a timeout."
    (do
     (old-head-var <- (take-mvar-masked (.head-var queue))) ;; Masks the thread after this returns
     ((ChanNode% val new-head-var) <- (take-mvar old-head-var :timeout timeout))
     (put-mvar (.head-var queue) new-head-var)
     unmask-current-thread ;; Cleanup after take-mvar-masked
     (pure val)))

  (declare try-dequeue (Threads :rt :t :m => UnboundedMpmcQueue :a -> :m (Optional :a)))
  (define (try-dequeue queue)
    "Attempt to pop the front value in QUEUE. Does not block."
    (do-matchM (try-take-mvar-masked (.head-var queue)) ;; Masks the thread after this returns
      ((None)
       (pure None))
      ((Some old-head-var)
       ((ChanNode% val new-head-var) <- (take-mvar old-head-var))
       (put-mvar (.head-var queue) new-head-var)
       unmask-current-thread ;; Cleanup after take-mvar-masked
       (pure (Some val)))))

  )
