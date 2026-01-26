(cl:in-package :cl-user)
(defpackage :io/io-all
  (:use #:coalton #:coalton-prelude)
  (:import-from #:io/monad-io
   #:derive-monad-io)
  (:import-from #:io/thread
   #:implement-monad-io-thread
   #:derive-monad-io-thread)
  (:import-from #:io/mut
   #:implement-monad-io-var
   #:derive-monad-var)
  (:import-from #:io/file
   #:implement-monad-io-file
   #:derive-monad-io-file)
  (:import-from #:io/random
   #:implement-monad-io-random
   #:derive-monad-io-random)
  (:import-from #:io/term
   #:implement-monad-io-term
   #:derive-monad-io-term)
  (:import-from #:io/unique
   #:implement-monad-io-unique
   #:derive-monad-io-unique)
  (:export
   #:implement-monad-io-all
   #:derive-monad-io-all))
(in-package :io/io-all)

(defmacro derive-monad-io-all (monad-param monadT-form)
  `(progn
     (derive-monad-io ,monad-param ,monadT-form)
     (derive-monad-io-thread ,monad-param ,monadT-form)
     (derive-monad-var ,monad-param ,monadT-form)
     (derive-monad-io-file ,monad-param ,monadT-form)
     (derive-monad-io-random ,monad-param ,monadT-form)
     (derive-monad-io-term ,monad-param ,monadT-form)
     (derive-monad-io-unique ,monad-param ,monadT-form)
     (derive-monad-io-network ,monad-param ,monadT-form)))

(defmacro implement-monad-io-all (monad)
  `(progn
     (implement-monad-io-thread ,monad)
     (implement-monad-io-var ,monad)
     (implement-monad-io-file ,monad)
     (implement-monad-io-random ,monad)
     (implement-monad-io-term ,monad)
     (implement-monad-io-unique ,monad)
     (implement-monad-io-network ,monad)))
