(defsystem "coalton-io"
  :long-name "coalton-io"
  :version "0.1"
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
                 (:module "thread-impl"
                  :serial t
                  :components ((:file "atomics")
                               (:file "runtime")
                               (:file "data-broadcast-pool")
                               (:file "stm-impl")))
                 (:module "classes"
                  :serial t
                  :components ((:file "monad-at-var")
                               (:file "monad-io-term")
                               (:file "monad-io-file")
                               (:file "monad-io-mvar")
                               (:file "monad-io-thread")
                               (:file "monad-io-unique")
                               (:file "monad-io-random")
                               (:file "monad-io-var")
                               (:file "monad-io-stm")))

                 ;;;
                 ;;; Library Public Packages
                 ;;;
                 ;; These files don't define new, full MonadIoX capability classes. They just
                 ;; do useful things with the capabilities provided by the classes in classes/
                 (:file "io-resource")
                 (:file "io-future")

                 ;;;
                 ;;; Library Private Packages
                 ;;;
                 (:module "gen-impl"
                  :serial t
                  :components ((:file "io-atomic")
                               (:file "io-term")
                               (:file "io-mut")
                               (:file "io-mvar")
                               (:file "io-thread")
                               (:file "io-file")
                               (:file "io-random")
                               (:file "io-unique")
                               (:file "io-stm")))
                 (:module "io-impl"
                  :serial t
                  :components ((:file "simple-io")
                               (:file "io-thread")
                               (:file "io-mvar")
                               (:file "io-future")
                               (:file "io-file")))
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
                 (:file "io-atomic")
                 (:file "io-mvar")
                 (:file "io-file")
                 (:file "io-unique")
                 (:file "io-stm")
                 (:file "io-all")
                 (:file "stubs/term"))))
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
                 (:file "stm")
                 (:file "package"))))
  :description "Test system for coalton-io"
  :perform (test-op (op c) (symbol-call '#:coalton-io/tests '#:run-tests)))

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
