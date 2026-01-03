(defpackage :coalton-io/tests/conc/ring-buffer
  (:use #:coalton #:coalton-prelude #:coalton-testing
   #:coalton-library/types
   #:io/io-impl/runtime
   #:io/gen-impl/conc/ring-buffer
   )
  )
(in-package :coalton-io/tests/conc/ring-buffer)

(named-readtables:in-readtable coalton:coalton)

(fiasco:define-test-package #:coalton-io/tests/conc/ring-buffer-fiasco)
(coalton-fiasco-init #:coalton-io/tests/conc/ring-buffer-fiasco)

(define-test test-enqueue-dequeue-once ()
  (let rt-prx = (the (Proxy IoRuntime) Proxy))
  (let buffer = (new-ring-buffer% 4))
  (enqueue!% rt-prx 1 buffer)
  (let result = (dequeue!% rt-prx buffer))
  (is (== 1 result)))

(define-test test-enqueue-dequeue-to-capacity ()
  (let rt-prx = (the (Proxy IoRuntime) Proxy))
  (let buffer = (new-ring-buffer% 4))
  (enqueue!% rt-prx 1 buffer)
  (enqueue!% rt-prx 2 buffer)
  (enqueue!% rt-prx 3 buffer)
  (enqueue!% rt-prx 4 buffer)
  (let a = (dequeue!% rt-prx buffer))
  (dequeue!% rt-prx buffer)
  (dequeue!% rt-prx buffer)
  (let b = (dequeue!% rt-prx buffer))
  (is (== 1 a))
  (is (== 4 b)))
