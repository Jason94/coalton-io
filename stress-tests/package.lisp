(defpackage #:io/stress
  (:use #:coalton #:coalton-prelude #:coalton-testing)
  (:export #:run-tests))
(in-package #:io/stress)

(named-readtables:in-readtable coalton:coalton)

(fiasco:define-test-package #:io/stress/fiasco-test-package)

(coalton-fiasco-init #:io/stress/fiasco-test-package)

(cl:defun run-tests ()
  (cl:multiple-value-bind (ok results)
      (fiasco:run-package-tests
       :packages '(
                   #:io/stress/queues-fiasco
                   #:io/stress/stm-fiasco
                   #:io/stress/mvar-fiasco
                   )
       :interactive cl:nil)
    (cl:declare (cl:ignore results))
    (cl:unless ok
      (cl:error "coalton-io stress tests failed."))
    ok))
