(cl:in-package :cl-user)
(defpackage :io/io-all
  (:use #:coalton #:coalton-prelude)
  (:import-from #:io/monad-io
   #:derive-monad-io)
  (:import-from #:io/thread
   #:implement-threads
   #:derive-threads)
  (:import-from #:io/mut
   #:implement-mutable-var
   #:derive-mutable-var)
  (:import-from #:io/file
   #:implement-files
   #:derive-files)
  (:import-from #:io/random
   #:implement-random
   #:derive-random)
  (:import-from #:io/terminal
   #:implement-terminal
   #:derive-terminal)
  (:import-from #:io/unique
   #:implement-unique-gen
   #:derive-unique-gen)
  (:import-from #:io/network
   #:implement-sockets
   #:derive-sockets)
  (:export
   #:implement-io-all
   #:derive-monad-io-all))
(in-package :io/io-all)

(defmacro derive-monad-io-all (monad-param monadT-form)
  `(progn
     (derive-monad-io ,monad-param ,monadT-form)
     (derive-threads ,monad-param ,monadT-form)
     (derive-mutable-var ,monad-param ,monadT-form)
     (derive-files ,monad-param ,monadT-form)
     (derive-random ,monad-param ,monadT-form)
     (derive-terminal ,monad-param ,monadT-form)
     (derive-unique-gen ,monad-param ,monadT-form)
     (derive-sockets ,monad-param ,monadT-form)))

(defmacro implement-io-all (monad)
  `(progn
     (implement-threads ,monad)
     (implement-mutable-var ,monad)
     (implement-files ,monad)
     (implement-random ,monad)
     (implement-terminal ,monad)
     (implement-unique-gen ,monad)
     (implement-sockets ,monad)))
