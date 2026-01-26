(cl:in-package :cl-user)
(defpackage :io/io-impl/runtime
  (:use
   #:coalton
   #:coalton-prelude
   #:io/classes/monad-io-thread
   #:io/thread-impl/runtime
   )
  (:export
   #:IoRuntime
   ))
(in-package :io/io-impl/runtime)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (define-type IoRuntime)

  (define-instance (Runtime IoRuntime IoThread)
    (inline)
    (define (current-thread! _)
      (current-thread!%))
    (inline)
    (define (sleep! _) sleep!%)
    (inline)
    (define (fork! _) fork!%)
    (inline)
    (define (join! _) join!%)
    (inline)
    (define (stop! _) stop!%)
    (inline)
    (define (mask! _) mask!%)
    (inline)
    (define (unmask! _) unmask!%)
    (inline)
    (define (unmask-finally! _) unmask-finally!%)
    (inline)
    (define (park-current-thread-if! prx)
      (park-current-thread-if!% prx))
    (inline)
    (define (park-current-thread-if-with! prx)
      (park-current-thread-if-with!% prx))
    (inline)
    (define (unpark-thread! _) unpark-thread!%)
    )

  )
