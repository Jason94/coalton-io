(defpackage :coalton-io/tests/io-impl/thread
  (:use #:coalton #:coalton-prelude #:coalton-testing
        #:io/utils
        #:io/monad-io
        #:io/simple-io
        #:io/exceptions
        #:io/thread
        #:io/mut
        #:io/conc/mvar
        #:io/tests/utils
        )
  )
(in-package :coalton-io/tests/io-impl/thread)

(named-readtables:in-readtable coalton:coalton)

(fiasco:define-test-package #:coalton-io/tests/io-impl/thread-fiasco)
(coalton-fiasco-init #:coalton-io/tests/io-impl/thread-fiasco)

;;;
;;; Tests for the IoThread Concurrent instance
;;;

(define-test test-unmask-finally-other-thread-runs-thunk-if-not-stopped ()
  (let result =
    (run-io!
     (do
      (started-gate <- s-new)
      (continue-gate <- s-new)
      (value <- (new-var None))
      (hit-finally <- (new-var None))
      (thread <-
        (do-fork-thread_
          (s-signal started-gate)
          (s-await continue-gate)
          (write value (Some 10))))
      ;; Wait for the thread to start before stopping it
      (s-await started-gate)
      (mask-thread thread)
      (unmask-finally_ thread (fn (mode)
                                (write hit-finally (Some mode))))
      (s-signal continue-gate)
      (sleep 2)
      (val <- (read value))
      (hit-finally? <- (read hit-finally))
      (pure (Tuple val hit-finally?)))))
  (is (== (Tuple (Some 10) (Some Running)) result)))
