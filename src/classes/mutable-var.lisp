(cl:in-package :cl-user)
(defpackage :io/classes/mutable-var
  (:use
   #:coalton
   #:coalton-prelude
   #:io/utils
   #:io/classes/monad-io)
  (:import-from #:coalton-library/experimental/do-control-loops-adv
   #:LoopT)
  (:local-nicknames
   (:st #:coalton-library/monad/statet)
   (:env #:coalton-library/monad/environment)
   (:c #:coalton-library/cell))
  (:export
   ;; Library Public
   #:Var
   #:MutableVar
   #:derive-mutable-var
   #:new-var
   #:read
   #:write
   #:modify

   ;; Library Private
   #:Var%))
(in-package :io/classes/mutable-var)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (derive Eq)
  (repr :transparent)
  (define-type (Var :a)
    (Var% (c:Cell :a)))

  (define-class (Monad :m => MutableVar :m)
    (new-var
     "Create a new variable with an initial value."
     (:a -> :m (Var :a)))
    (read
     "Read the current value stored in a variable."
     (Var :a -> :m :a))
    (write
     "Set the value in a variable and return the old value."
     (Var :a -> :a -> :m :a))
    (modify
     "Modify the value in a variable by applying F, and return the old value."
     (Var :a -> (:a -> :a) -> :m :a))))

(defmacro derive-mutable-var (monad-param monadT-form)
  "Automatically derive an instance of MutableVar for a monad transformer.

Example:
  (derive-mutable-var :m (st:StateT :s :m))"
  `(define-instance (MutableVar ,monad-param => MutableVar ,monadT-form)
     (define new-var (compose lift new-var))
     (define read (compose lift read))
     (define write (compose2 lift write))
     (define modify (compose2 lift modify))))

(coalton-toplevel

  ;;
  ;; Std. Library Transformer Instances
  ;;

  (derive-mutable-var :m (st:StateT :s :m))
  (derive-mutable-var :m (env:EnvT :e :m))
  (derive-mutable-var :m (LoopT :m)))
