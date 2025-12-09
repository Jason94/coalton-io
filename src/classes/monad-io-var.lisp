(cl:in-package :cl-user)
(defpackage :io/classes/monad-io-var
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/functions
   #:coalton-library/monad/classes
   #:io/classes/monad-io)
  (:local-nicknames
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
