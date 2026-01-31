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
     #:io/conc/mchan-scheduler
     #:io/conc/worker-pool)
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
      (scheduler <- new-mchan-scheduler)
      (pool <- (new-worker-pool_ 2 scheduler))
      (do-submit-job_ pool
        (put-mvar result True))
      (request-shutdown pool)
      (await pool)
      (try-take-mvar result))))
  (is (== (Some True) result)))
