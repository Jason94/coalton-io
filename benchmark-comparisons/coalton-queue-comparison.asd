(asdf:defsystem "coalton-queue-comparison"
  :description "Matched queue, scheduler, and STM benchmarks for coalton-io and comparable Common Lisp libraries."
  :version "0.3.0"
  :license "MIT"
  :depends-on ("trivial-benchmark"
               "atomics"
               "cl-freelock"
               "coalton/threads"
               "lparallel"
               "stmx"
               "bordeaux-threads")
  :serial t
  :components ((:module "src"
                :components ((:file "benchmark-support")
                             (:file "package")
                             (:file "benchmarks")
                             (:file "schedulers")
                             (:file "stm")))))
