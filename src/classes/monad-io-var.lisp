(cl:in-package :cl-user)
(defpackage :io/classes/monad-io-var
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
   #:MonadIoVar
   #:new-var
   #:read
   #:write
   #:modify

   ;; Library Private
   #:Var%))
(in-package :io/classes/monad-io-var)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (derive Eq)
  (repr :transparent)
  (define-type (Var :a)
    (Var% (c:Cell :a)))

  (define-class (Monad :m => MonadIoVar :m)
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

(cl:defmacro derive-monad-var (monad-param monadT-form)
  "Automatically derive an instance of MonadIoVar for a monad transformer.

Example:
  (derive-monad-var :m (st:StateT :s :m))"
  `(define-instance (MonadIoVar ,monad-param => MonadIoVar ,monadT-form)
     (define new-var (compose lift new-var))
     (define read (compose lift read))
     (define write (compose2 lift write))
     (define modify (compose2 lift modify))))

(coalton-toplevel

  ;;
  ;; Std. Library Transformer Instances
  ;;

  (derive-monad-var :m (st:StateT :s :m))
  (derive-monad-var :m (env:EnvT :e :m))
  (derive-monad-var :m (LoopT :m)))
