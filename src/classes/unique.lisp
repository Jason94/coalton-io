(cl:in-package :cl-user)
(defpackage :io/classes/unique
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
   #:UniqueGen
   #:derive-unique-gen
   #:new-unique
   #:to-int

   ;; Library Private
   #:Unique%))
(in-package :io/classes/unique)

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

  (define-class (Monad :m => UniqueGen :m)
    (new-unique
     "Generate a value that will be unique within this run of the program.
Threadsafe - calling from different threads will still result in unique
values across all threads."
     (:m Unique))))

(defmacro derive-unique-gen (monad-param monadT-form)
  "Automatically derive an instance of UniqueGen for a monad transformer.

Example:
  (derive-unique-gen :m (st:StateT :s :m))"
  `(define-instance (UniqueGen ,monad-param => UniqueGen ,monadT-form)
     (define new-unique (lift new-unique))))

(coalton-toplevel
  ;;
  ;; Std. Library Transformer Instances
  ;;

  (derive-unique-gen :m (st:StateT :s :m))
  (derive-unique-gen :m (env:EnvT :e :m))
  (derive-unique-gen :m (LoopT :m)))
