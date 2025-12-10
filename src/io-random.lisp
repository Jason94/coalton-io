(cl:in-package :cl-user)
(defpackage :io/random
  (:use
   #:coalton
   #:coalton-prelude
   #:io/gen-impl/random)
  (:export
   ;; Re-export: io/classes/monad-io-random
   #:RandomLimit
   #:RandomState
   #:MonadIoRandom
   #:make-random-state
   #:copy-random-state
   #:get-current-random-state
   #:set-current-random-state
   #:random
   #:random_

   ;; Remaining exports
   #:derive-monad-io-random
   #:implement-monad-io-random

   #:random-elt
   #:random-elt_
   #:random-elt#
   #:random-elt#_
   ))
(in-package :io/random)
