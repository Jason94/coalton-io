(cl:in-package :cl-user)
(defpackage :io/classes/monad-io-unique
  (:use
   #:coalton
   #:coalton-prelude
   #:io/classes/monad-io)
  (:import-from #:coalton-library/experimental/do-control-loops-adv
   #:LoopT)
  (:local-nicknames
   (:st #:coalton-library/monad/statet)
   (:env #:coalton-library/monad/environment)
   )
  (:export
   ;; Library Public
   #:Unique
   #:MonadIoUnique
   #:derive-monad-io-unique
   #:new-unique
   #:to-int

   ;; Library Private
   #:Unique%))
(in-package :io/classes/monad-io-unique)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (derive Eq)

  (repr :transparent)
  (define-type Unique
    (Unique% Integer))

  (define-instance (Ord Unique)
    (define (<=> (Unique% a) (Unique% b))
      (<=> a b)))

  (inline)
  (declare to-int (Unique -> Integer))
  (define (to-int (Unique% i))
    "Convert a unique value to an integer.

It is guaranteed that: (/= (to-int a) (to-int b))
for any two different Unique instances."
    i)

  (define-class (Monad :m => MonadIoUnique :m)
    (new-unique
     "Generate a value that will be unique within this run of the program.
Threadsafe - calling from different threads will still result in unique
values across all threads."
     (:m Unique))))

(defmacro derive-monad-io-unique (monad-param monadT-form)
  "Automatically derive an instance of MonadIoUnique for a monad transformer.

Example:
  (derive-monad-io-unique :m (st:StateT :s :m))"
  `(define-instance (MonadIoUnique ,monad-param => MonadIoUnique ,monadT-form)
     (define new-unique (lift new-unique))))

(coalton-toplevel
  ;;
  ;; Std. Library Transformer Instances
  ;;

  (derive-monad-io-unique :m (st:StateT :s :m))
  (derive-monad-io-unique :m (env:EnvT :e :m))
  (derive-monad-io-unique :m (LoopT :m)))
