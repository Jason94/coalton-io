(cl:in-package :cl-user)
(defpackage :io/classes/monad-io-random
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/functions
   #:coalton-library/monad/classes
   #:io/classes/monad-io)
  (:export
   #:RandomLimit
   #:RandomState
   #:MonadIoRandom
   #:make-random-state
   #:copy-random-state
   #:get-current-random-state
   #:set-current-random-state
   #:random
   #:random_
   ))
(in-package :io/classes/monad-io-random)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (define-class (Num :a => RandomLimit :a)
    "A number that can be used to bound a random number value.")

  (define-instance (RandomLimit UFix))
  (define-instance (RandomLimit Integer))
  (define-instance (RandomLimit F32))
  (define-instance (RandomLimit F64))

  (repr :native cl:random-state)
  (define-type RandomState)

  (define-class (Monad :m => MonadIoRandom :m)
    (make-random-state
     "Create a fresh random state."
     (:m RandomState))
    (copy-random-state
     "Create a copy of another random state, starting at the same seed."
     (RandomState -> :m RandomState))
    (get-current-random-state
     "Get the current thread's random state."
     (:m RandomState))
    (set-current-random-state
     "Set the current thread's random state."
     (RandomState -> :m Unit))
    (random
     "Generate a random value less than LIMIT using the given random state."
     (RandomLimit :a => RandomState -> :a -> :m :a))
    (random_
     "Generate a random value less than LIMIT using the current random state."
     (RandomLimit :a => :a -> :m :a))))
