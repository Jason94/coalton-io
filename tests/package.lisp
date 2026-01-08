
(defpackage #:coalton-io/tests
  (:use #:coalton #:coalton-prelude #:coalton-testing)
  (:export #:run-tests))
(in-package #:coalton-io/tests)

(named-readtables:in-readtable coalton:coalton)

(fiasco:define-test-package #:coalton-io/fiasco-test-package)

(coalton-fiasco-init #:coalton-io/fiasco-test-package)

(cl:defun run-tests ()
  (fiasco:run-package-tests
   :packages '(
               #:coalton-io/tests/io-fiasco
               #:coalton-io/tests/exception-fiasco
               #:coalton-io/tests/mut-fiasco
               #:coalton-io/tests/random-fiasco
               #:coalton-io/tests/resource-fiasco
               #:coalton-io/tests/thread-fiasco
               #:coalton-io/tests/thread-async-boundary-fiasco
               #:coalton-io/tests/mvar-fiasco
               #:coalton-io/tests/future-fiasco
               #:coalton-io/tests/io-atomic-fiasco
               #:coalton-io/tests/io-impl/thread-fiasco
               #:coalton-io/tests/conc/parking-fiasco
               #:coalton-io/tests/conc/group-fiasco
               #:coalton-io/tests/conc/stm-fiasco
               #:coalton-io/tests/conc/worker-pool-fiasco
               #:coalton-io/tests/conc/ring-buffer-fiasco
               )
   :interactive cl:t))
