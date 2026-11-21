(defpackage :coalton-io/tests/io-impl/delay
  (:use #:coalton #:coalton-prelude #:coalton-testing
   #:io/tests/utils
   #:io/simple-io
   #:io/thread
   #:io/exceptions
   #:io/delay
   #:io/tests/utils
   )
  )
(in-package :coalton-io/tests/io-impl/delay)

(named-readtables:in-readtable coalton:coalton)

(fiasco:define-test-package #:coalton-io/tests/io-impl/delay-fiasco)
(coalton-fiasco-init #:coalton-io/tests/io-impl/delay-fiasco)

(define-test test-delay-success ()
  (let result =
    (run-io!
     (do
      (d <- (fork-delay_ (pure 1)))
      (await d))))
  (is (== result 1)))

(define-test test-delay-exception-await-after-stop ()
  (let result =
    (run-io!
     (do
      (start-gate <- s-new)
      (wait-gate <- s-new)
      (d <- (do-fork-delay_
              (s-signal start-gate)
              (s-await wait-gate)
              (pure 10)))
      (s-await start-gate)
      (stop d)
      (try-all
       (await d)))))
  (is (== result None)))
  
(define-test test-delay-await-after-exception ()
  (let result =
    (run-io!
     (do
      (d <- (do-fork-delay_ :unhandled Swallow
              (raise (TE "Test Error"))
              (pure 1)))
      (try
       (await d)))))
  (is (== result (Err (TE "Test Error")))))
