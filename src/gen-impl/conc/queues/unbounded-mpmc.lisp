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
   #:push-chan
   #:pop-chan
   #:try-pop-chan
   ))
(in-package :io/gen-impl/conc/queues/unbounded-mpmc)

(cl:declaim (cl:optimize (cl:speed 3) (cl:debug 0) (cl:safety 1)))

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (define-type (ChanNode :a)
    (ChanNode% :a (MVar (ChanNode :a))))

  (define-struct (UnboundedMpmcQueue :a)
    "A synchronized FIFO queue to pass data directionally between threads."
    (head-var (MVar (MVar (ChanNode :a))))
    (tail-var (MVar (MVar (ChanNode :a)))))

  (declare new-unbounded-mpmc-queue (Threads :rt :t :m => :m (UnboundedMpmcQueue :a)))
  (define new-unbounded-mpmc-queue
    "Create a new empty channel."
    (do
      (cell <- new-empty-mvar)
      (head-var <- (new-mvar cell))
      (tail-var <- (new-mvar cell))
      (pure (UnboundedMpmcQueue head-var tail-var))))

  (declare push-chan (Threads :rt :t :m => UnboundedMpmcQueue :a * :a -> :m Unit))
  (define (push-chan chan val)
    "Push VAL onto CHAN."
    (do
     (new-tail-var <- new-empty-mvar)
     (old-tail-var <- (take-mvar-masked (.tail-var chan))) ;; Masks the thread after this returns
     (put-mvar old-tail-var (ChanNode% val new-tail-var))
     (put-mvar (.tail-var chan) new-tail-var)
     unmask-current-thread)) ;; Cleanup after take-mvar-masked

  (declare pop-chan (Threads :rt :t :m
                          => UnboundedMpmcQueue :a &key (:timeout TimeoutStrategy)
                          -> :m :a))
  (define (pop-chan chan &key (timeout NoTimeout))
    "Pop the front value in CHAN. Blocks while CHAN is empty. Can specify a timeout."
    (do
     (old-head-var <- (take-mvar-masked (.head-var chan))) ;; Masks the thread after this returns
     ((ChanNode% val new-head-var) <- (take-mvar old-head-var :timeout timeout))
     (put-mvar (.head-var chan) new-head-var)
     unmask-current-thread ;; Cleanup after take-mvar-masked
     (pure val)))

  (declare try-pop-chan (Threads :rt :t :m => UnboundedMpmcQueue :a -> :m (Optional :a)))
  (define (try-pop-chan chan)
    "Attempt to pop the front value in CHAN. Does not block."
    (do-matchM (try-take-mvar-masked (.head-var chan)) ;; Masks the thread after this returns
      ((None)
       (pure None))
      ((Some old-head-var)
       ((ChanNode% val new-head-var) <- (take-mvar old-head-var))
       (put-mvar (.head-var chan) new-head-var)
       unmask-current-thread ;; Cleanup after take-mvar-masked
       (pure (Some val)))))

  )
