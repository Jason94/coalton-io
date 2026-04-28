(cl:in-package :cl-user)
(defpackage :io/io-impl/runtime
  (:use
   #:coalton
   #:coalton-prelude
   #:io/classes/thread
   #:io/threads-impl/runtime
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
    (define (sleep! _ n)
      (sleep!% n))
    (inline)
    (define (fork! _ strategy thunk)
      (fork!% strategy thunk))
    (inline)
    (define (join! _ thread)
      (join!% thread))
    (inline)
    (define (stop! _ thread)
      (stop!% thread))
    (inline)
    (define (mask! _ thread)
      (mask!% thread))
    (inline)
    (define (unmask! _ thread)
      (unmask!% thread))
    (inline)
    (define (unmask-finally! _ thread thunk)
      (unmask-finally!% thread thunk))
    (inline)
    (define (park-current-thread-if! prx with-gen should-park? &key (timeout NoTimeout))
      (park-current-thread-if-with!% prx with-gen should-park? timeout))
    (inline)
    (define (unpark-thread! _ gen thread)
      (unpark-thread!% gen thread))
    )

  )
