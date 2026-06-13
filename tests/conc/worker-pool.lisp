(defpackage :coalton-io/tests/conc/worker-pool
  (:use #:coalton #:coalton-prelude #:coalton-testing
     #:coalton-library/experimental/do-control-core
     #:io/tests/utils
     #:io/utils
     #:io/simple-io
     #:io/exceptions
     #:io/conc/mvar
     #:io/thread
     #:io/conc/future
     #:io/conc/scheduler
     #:io/conc/worker-pool)
  (:local-nicknames
   (:mt #:io/mut))
  )
(in-package :coalton-io/tests/conc/worker-pool)

(named-readtables:in-readtable coalton:coalton)

(fiasco:define-test-package #:coalton-io/tests/conc/worker-pool-fiasco)
(coalton-fiasco-init #:coalton-io/tests/conc/worker-pool-fiasco)

(define-test test-submit-one-job-and-shutdown ()
  (let result =
    (run-io!
     (do
      (result <- new-empty-mvar)
      (scheduler <- new-unbounded-scheduler)
      (pool <- (new-worker-pool_ 2 scheduler))
      (do-submit-job_ pool
        (put-mvar result True))
      (request-shutdown pool)
      (await pool)
      (try-take-mvar result))))
  (is (== (Some True) result)))

(define-test test-submit-indexed-chunks-even ()
  (let result =
    (run-io!
     (do
      (chunks <- (mt:new-var Nil))
      (scheduler <- new-unbounded-scheduler)
      (pool <- (new-worker-pool_ 1 scheduler))
      (do-submit-indexed-chunks_ pool (c (0 15 5))
        (mt:modify chunks ƒl.(Cons c l)))
      (request-shutdown pool)
      (await pool) 
      (map reverse (mt:read chunks)))))
  (is (== [(IndexChunk 0 5)
           (IndexChunk 5 10)
           (IndexChunk 10 15)]
          result)))

(define-test test-submit-indexed-chunks ()
  (let result =
    (run-io!
     (do
      (chunks <- (mt:new-var Nil))
      (scheduler <- new-unbounded-scheduler)
      (pool <- (new-worker-pool_ 1 scheduler))
      (do-submit-indexed-chunks_ pool (c (0 17 5))
        (mt:modify chunks ƒl.(Cons c l)))
      (request-shutdown pool)
      (await pool) 
      (map reverse (mt:read chunks)))))
  (is (== [(IndexChunk 0 5)
           (IndexChunk 5 10)
           (IndexChunk 10 15)
           (IndexChunk 15 17)]
          result)))

(define-test test-submit-indexed-empty ()
  (let result =
    (run-io!
     (do
      (chunks <- (mt:new-var Nil))
      (scheduler <- new-unbounded-scheduler)
      (pool <- (new-worker-pool_ 1 scheduler))
      (do-submit-indexed-chunks_ pool (c (0 0 5))
        (mt:modify chunks ƒl.(Cons c l)))
      (request-shutdown pool)
      (await pool) 
      (map reverse (mt:read chunks)))))
  (is (== []
          result)))

(define-test test-submit-indexed-invalid ()
  (let result =
    (run-io!
     (try-all
      (do
       (chunks <- (mt:new-var Nil))
       (scheduler <- new-unbounded-scheduler)
       (pool <- (new-worker-pool_ 1 scheduler))
       (do-submit-indexed-chunks_ pool (c (10 0 5))
         (mt:modify chunks ƒl.(Cons c l)))
       (request-shutdown pool)
       (await pool) 
       (map reverse (mt:read chunks))))))
  (is (== None
          result)))
