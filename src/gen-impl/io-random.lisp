(cl:in-package :cl-user)
(defpackage :io/gen-impl/random
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/functions
   #:coalton-library/experimental/do-control-core
   #:io/classes/monad-io-random
   #:io/utils
   #:io/monad-io)
  (:import-from #:coalton-library/experimental/do-control-loops-adv
   #:LoopT)
  (:local-nicknames
   (:l #:coalton-library/list)
   (:opt #:coalton-library/optional)
   (:st  #:coalton-library/monad/statet)
   (:env #:coalton-library/monad/environment)
   (:io #:io/simple-io))
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
(in-package :io/gen-impl/random)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (declare make-random-state% (MonadIo :m => :m RandomState))
  (define make-random-state%
    (wrap-io (lisp :a ()
               (cl:make-random-state cl:t))))

  (declare copy-random-state% (MonadIo :m => RandomState -> :m RandomState))
  (define (copy-random-state% rs)
    (wrap-io (lisp :a (rs)
               (cl:make-random-state rs))))

  (declare get-current-random-state% (MonadIo :m => :m RandomState))
  (define get-current-random-state%
    (wrap-io (lisp :a ()
               cl:*random-state*)))

  (declare set-current-random-state% (MonadIo :m => RandomState -> :m Unit))
  (define (set-current-random-state% rs)
    (wrap-io
      (lisp :a (rs)
        (cl:setf cl:*random-state* rs))
      Unit))

  (declare random% ((RandomLimit :a) (MonadIo :m) => RandomState -> :a -> :m :a))
  (define (random% rs limit)
    (wrap-io (lisp :a (rs limit)
               (cl:random limit rs))))

  (declare random_% ((RandomLimit :a) (MonadIo :m) => :a -> :m :a))
  (define (random_% limit)
    (wrap-io (lisp :a (limit)
               (cl:random limit))))

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

(cl:defmacro implement-monad-io-random (monad)
  `(define-instance (MonadIoRandom ,monad)
     (define make-random-state make-random-state%)
     (define copy-random-state copy-random-state%)
     (define get-current-random-state get-current-random-state%)
     (define set-current-random-state set-current-random-state%)
     (define random random%)
     (define random_ random_%)))

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

;;
;; Simple IO Implementation
;;
