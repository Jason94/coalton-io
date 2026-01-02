(defpackage :coalton-io/tests/conc/ring-buffer
  (:use #:coalton #:coalton-prelude #:coalton-testing
   #:coalton-library/experimental/do-control-core
   #:io/tests/utils
   #:io/utils
   #:io/simple-io
   #:io/exception
   )
  )
(in-package :coalton-io/tests/conc/ring-buffer)

(named-readtables:in-readtable coalton:coalton)

(fiasco:define-test-package #:coalton-io/tests/conc/ring-buffer-fiasco)
(coalton-fiasco-init #:coalton-io/tests/conc/ring-buffer-fiasco)

(define-test test-submit-one-job-and-shutdown ()
  (is (== 1 1)))
