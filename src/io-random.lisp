(cl:in-package :cl-user)
(defpackage :io/random
  (:use
   #:io/classes/monad-io-random
   #:io/gen-impl/random)
  (:export
   ;; Re-exports from io/classes/monad-io-random
   #:RandomLimit
   #:RandomState
   #:MonadIoRandom
   #:derive-monad-io-random
   #:make-random-state
   #:copy-random-state
   #:get-current-random-state
   #:set-current-random-state
   #:random
   #:random_

   #:random-elt
   #:random-elt_
   #:random-elt#
   #:random-elt#_

   ;; Re-exports from io/gen-impl/random
   #:implement-monad-io-random
   ))
(in-package :io/random)
