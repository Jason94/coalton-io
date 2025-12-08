(cl:in-package :cl-user)
(defpackage :io/benchmarks/mvar-contention
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/experimental/do-control-core
   #:coalton-library/experimental/do-control-loops
   #:io/utils
   #:io/monad-io
   #:io/simple-io
   #:io/mut
   #:io/thread
   #:io/future
   #:io/mvar)
  (:import-from #:io/term
   #:write-line)
  (:local-nicknames
   (:itr #:coalton-library/iterator)
   (:hm #:coalton-library/hashmap)
   (:opt #:coalton-library/optional)
   (:at #:io/atomic)))
(in-package :io/benchmarks/mvar-contention)

(named-readtables:in-readtable coalton:coalton)

;;; This benchmark simulates a high-contention scenario on a single MVar. It initiates
;;; batches of N threads where each batch attempts to do one of the functions on the MVar
;;; for S seconds. Because there's a good chance the entire threadpool will deadlock,
;;; a producer thread continuously retries `try-put-mvar` to fill it back up.
;;;
;;; Operations included:
;;; take-mvar
;;; put-mvar
;;; read-mvar
;;; swap-mvar
;;;
;;; Each thread logs how many operations it completed and returns them.
;;;
;;; Note that the performance isn't just determined by the MVar implemenation. It's also
;;; affected by the performance of the underlying IO monad, the cost of the library's
;;; thread runtime (masking/unmasking, etc), etc.

(coalton-toplevel

  (derive Eq)
  (repr :enum)
  (define-type Operation
    TakeVar
    PutVar
    ReadVar
    SwapVar)

  (define-instance (Hash Operation)
    (define (hash op)
      (hash
       (match op
         ((TakeVar) 1)
         ((PutVar) 2)
         ((ReadVar) 3)
         ((SwapVar) 4)))))

  (define operations (make-list
                      TakeVar
                      PutVar
                      ReadVar
                      ;; SwapVar
                      ))

  (define log-filename "~/data.csv")
  (declare n-threads-per-operation UFix)
  (define n-threads-per-operation 1000)
  (declare n-milliseconds UFix)
  (define n-milliseconds 2000)

  (declare benchmark (IO Unit))
  (define benchmark
    (do
     (finish-gate <- new-empty-mvar)
     (contention-mvar <- (new-mvar 0))
     (count-at-vars <- (at:new-at-var Nil))
     ;; Spawn the main benchmarking threads
     (do-foreach (op operations)
       (do-loop-times (_ n-threads-per-operation)
         (count <- (at:new-at-var 0))
         (at:modify count-at-vars (Cons (Tuple op count)))
         (do-fork_
           (do-loop-while
             (do-match op
               ((TakeVar)
                (take-mvar contention-mvar)
                (pure Unit))
               ((PutVar)
                (put-mvar contention-mvar 0))
               ((Readvar)
                (read-mvar contention-mvar)
                (pure Unit))
               ((SwapVar)
                (swap-mvar contention-mvar 10)
                (pure Unit)))
             (at:modify count 1+)
             (cont <- (is-empty-mvar finish-gate))
             (pure (not cont))))))
     ;; Spawn the looping producer
     ;; (do-fork_
     ;;   (do-loop-while
     ;;     (try-put-mvar contention-mvar 100)
     ;;     (is-empty-mvar finish-gate)))
     (sleep n-milliseconds)
     (put-mvar finish-gate Unit)
     ;; Sum up the results
     (sum-counts <- (new-var
                     (itr:collect!
                      (itr:into-iter (map (fn (op) (Tuple op 0))
                                          operations)))))
     (count-at-vars <- (at:read count-at-vars))
     (do-foreach ((Tuple op at-count) count-at-vars)
       (count <- (at:read at-count))
       (modify sum-counts
         (fn (hmap)
           (hm:modify_ hmap op (fn (x)
                                 (let _ = (the Integer x))
                                 (+ x count))))))
     (counts <- (read sum-counts))
     (write-line (force-string counts))
      )))

(coalton (run-io! benchmark))
