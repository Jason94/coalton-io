(cl:in-package :cl-user)
(defpackage :io/classes/monad-io-random
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/functions
   #:coalton-library/monad/classes
   #:coalton-library/experimental/do-control-core
   #:io/utils
   #:io/classes/monad-io)
  (:import-from #:coalton-library/experimental/do-control-loops-adv
   #:LoopT)
  (:local-nicknames
   (:l #:coalton-library/list)
   (:opt #:coalton-library/optional)
   (:st  #:coalton-library/monad/statet)
   (:env #:coalton-library/monad/environment)
   )
  (:export
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
     (RandomLimit :a => :a -> :m :a)))

  ;;;
  ;;; Extra Functions
  ;;;

  (declare random-elt (MonadIoRandom :m => RandomState -> List :a -> :m (Optional :a)))
  (define (random-elt rs lst)
    "Get a random element from LST. Returns NONE if LST is empty."
    (let len = (length lst))
    (do-if (== 0 len)
        (pure None)
      (r <- (random rs len))
      (pure (l:index r lst))))

  (declare random-elt_ (MonadIoRandom :m => List :a -> :m (Optional :a)))
  (define (random-elt_ lst)
    "Get a random element from LST. Returns NONE if LST is empty."
    (let len = (length lst))
    (do-if (== 0 len)
        (pure None)
      (r <- (random_ len))
      (pure (l:index r lst))))

  (declare random-elt# (MonadIoRandom :m => RandomState -> List :a -> :m :a))
  (define (random-elt# rs lst)
    "Get a random element from LST. Errors if LST is empty."
    (map (opt:from-some "Cannot get random element from empty list")
         (random-elt rs lst)))

  (declare random-elt#_ (MonadIoRandom :m => List :a -> :m :a))
  (define (random-elt#_ lst)
    "Get a random element from LST. Errors if LST is empty."
    (map (opt:from-some "Cannot get random element from empty list")
         (random-elt_ lst)))
  )

(cl:defmacro derive-monad-io-random (monad-param monadT-form)
  "Automatically derive an instance of MonadIoRandom for a monad transformer.

Example:
  (derive-monad-io-random :m (st:StateT :s :m))"
  `(define-instance (MonadIoRandom ,monad-param => MonadIoRandom ,monadT-form)
     (define make-random-state (lift make-random-state))
     (define copy-random-state (compose lift copy-random-state))
     (define get-current-random-state (lift get-current-random-state))
     (define set-current-random-state (compose lift set-current-random-state))
     (define random (compose2 lift random))
     (define random_ (compose lift random_))))

(coalton-toplevel
  ;;
  ;; Std. Library Transformer Instances
  ;;

  (derive-monad-io-random :m (st:StateT :s :m))
  (derive-monad-io-random :m (env:EnvT :e :m))
  (derive-monad-io-random :m (LoopT :m)))
