(defsystem "coalton-io"
  :long-name "coalton-io"
  :version "0.2"
  :author "Jason Walker"
  :maintainer "Jason Walker"
  :mailto "Jason0@pm.me"
  :license "MIT"
  :depends-on ("uiop" "coalton" "named-readtables" "atomics" "coalton-threads")
  :components ((:module "src"
                :serial t
                :components
                (;;;
                 ;;; Library Private Packages
                 ;;;
                 (:file "utils")
                 (:file "thread-exceptions")
                 ;; Load the two main "core classes" files first, before thread-impl/
                 (:file "classes/monad-exception")
                 (:file "classes/monad-io")
                 (:module "classes"
                  :serial t
                  :components ((:file "monad-io-term")
                               (:file "monad-io-file")
                               (:file "monad-io-thread")
                               (:file "monad-io-unique")
                               (:file "monad-io-random")
                               (:file "monad-io-var")
                               (:file "runtime-utils")
                               (:file "conc/scheduler")
                               ))
                 (:module "thread-impl"
                  :serial t
                  :components ((:file "atomics")
                               (:file "runtime")))
                 ;; TODO: Finish converting this to use Runtime, then move
                 ;; to gen-impl/conc
                 (:file "thread-impl/data-broadcast-pool")
                 ;;;
                 ;;; Library Public Packages
                 ;;;
                 ;; These files don't define new, full MonadIoX capability classes. They just
                 ;; do useful things with the capabilities provided by the classes in classes/
                 (:file "io-resource")

                 ;;;
                 ;;; Library Private Packages
                 ;;;
                 (:module "gen-impl"
                  :serial t
                  :components ((:file "io-term")
                               (:file "io-mut")
                               (:file "conc/parking")
                               (:file "conc/io-mvar")
                               (:file "conc/mchan-scheduler")
                               (:file "conc/io-future")
                               (:file "conc/io-atomic")
                               (:file "conc/concurrent-group")
                               (:file "conc/stm")
                               (:file "conc/worker-pool")
                               (:file "conc/ring-buffer")
                               (:file "conc/ring-buffer-scheduler")
                               (:file "io-thread")
                               (:file "io-file")
                               (:file "io-random")
                               (:file "io-unique")))
                 (:module "io-impl"
                  :serial t
                  :components ((:file "runtime")
                               (:file "simple-io")
                               (:file "io-thread")
                               (:file "io-mvar")
                               (:file "io-future")
                               (:file "io-file")
                               (:file "conc/parking")
                               (:file "conc/worker-pool")))
                 ;;;
                 ;;; Library Public Packages (Re-Export)
                 ;;;
                 (:file "io-exception")
                 (:file "monad-io")
                 (:file "simple-io")

                 (:file "io-mut")
                 (:file "io-term")
                 (:file "io-random")
                 (:file "io-thread")
                 (:file "io-file")
                 (:file "io-unique")
                 (:file "io-all")

                 (:module "conc"
                  :serial t
                  :components ((:file "parking")
                               (:file "scheduler")
                               (:file "mchan-scheduler")
                               (:file "ring-buffer")
                               (:file "io-future")
                               (:file "io-atomic")
                               (:file "io-mvar")
                               (:file "concurrent-group")
                               (:file "stm")
                               (:file "worker-pool")
                               ))

                 (:file "stubs/term")
                 )))
  :description "Functional IO interfaces and implementation for Coalton."
  :long-description "Functional IO interfaces and implementation for Coalton. Includes terminal IO, file system IO, random variables,
mutable variables, multithreading, and several data structures to safely share state between threads."
  :in-order-to ((test-op (test-op "coalton-io/tests"))))

(defsystem "coalton-io/tests"
  :author "Jason Walker"
  :license "MIT"
  :depends-on ("coalton-io"
               "coalton/testing"
               "fiasco")
  :components ((:module "tests"
                :serial t
                :components
                (
                 (:file "test-utils")
                 (:file "simple-io")
                 (:file "exception")
                 (:file "mut")
                 (:file "random")
                 (:file "thread")
                 (:file "thread-async-boundary")
                 (:file "resource")
                 (:file "io-atomic")
                 (:file "mvar")
                 (:file "future")
                 (:module "thread-impl"
                  :serial t
                  :components((:file "runtime--structured-conc")))
                 (:module "io-impl"
                  :serial t
                  :components ((:file "thread")))
                 (:module "conc"
                  :serial t
                  :components ((:file "parking")
                               (:file "concurrent-group")
                               (:file "stm")
                               (:file "worker-pool")
                               (:file "ring-buffer")
                               ))
                 (:file "package"))))
  :description "Test system for coalton-io"
  :perform (test-op (op c)
            (uiop:with-current-directory ((asdf:system-source-directory c))
              (uiop:symbol-call '#:coalton-io/tests '#:run-tests))))

(defsystem "coalton-io/docs"
  :author "Jason Walker"
  :license "MIT"
  :depends-on ("coalton-io" "coalton/doc")
  :pathname "pkg"
  :components ((:file "gen-docs"))
  :description "Generate HTML documentation for coalton-io."
  :perform (load-op (op c)
            (uiop:with-current-directory ((asdf:system-source-directory c))
              (uiop:symbol-call '#:coalton-io/docs '#:write-docs))))

(defsystem "coalton-io/examples"
  :author "Jason Walker"
  :license "MIT"
  :depends-on ("coalton-io")
  :components ((:module "examples"
                :components
                ((:file "channels-threading")
                 (:file "hangman")
                 (:file "fork-laws"))))
  :description "Example programs for coalton-io.")

(defsystem "coalton-io/benchmarks"
  :author "Jason Walker"
  :license "MIT"
  :depends-on ("coalton-io" "trivial-benchmark" "yason")
  :components ((:module "benchmarks"
                :components
                ((:file "benchmark-utils")
                 (:file "benchmarks")
                 (:file "simple-io")
                 (:file "schedulers")
                 )))
  :description "Benchmarks for coalton-io.")
