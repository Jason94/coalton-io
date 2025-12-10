(cl:in-package :cl-user)
(defpackage :io/classes/monad-at-var
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
   (:at #:io/thread-impl/atomics))
  (:export
   ;; Library Public
   #:AtVar
   #:unwrap-atvar
   #:MonadAtVar
   #:new-at-var
   #:read
   #:write
   #:modify
   #:modify-swap
   #:push
   #:pop

   #:derive-monad-at-var

   ;; Library Private
   #:AtVar%))
(in-package :io/classes/monad-at-var)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (repr :transparent)
  (define-type (AtVar :a)
    (AtVar% (at:Atomic :a)))

  (inline)
  (declare unwrap-atvar (AtVar :a -> at:Atomic :a))
  (define (unwrap-atvar (AtVar% atm))
    atm)

  (define-class (Monad :m => MonadAtVar :m)
    (new-at-var
     "Create a new atomic variable with an initial value."
     (:a -> :m (AtVar :a)))
    (read
     "Read the value from an atomic variable."
     (AtVar :a -> :m :a))
    (write
     "Write a new value to an atomic variable."
     (AtVar :a -> :a -> :m Unit))
    (modify
     "Atomically modify by applying F, then return the new
value of the atomic variable. F may be called multiple times,
and must be a pure function. If F errors, it will be raised
in (:m :a) as an UnhandledError exception, and the atomic
variable will not be modified."
     (AtVar :a -> (:a -> :a) -> :m :a))
    (modify-swap
     "Atomically modify by applying F, then return the old
value of the variable. F may be called multiple times, and
must be a pure function. If F errors, it will be raised
in (:m :a) as an UnhandledError exception, and the atomic
variable will not be modified."
     (AtVar :a -> (:a -> :a) -> :m :a))
    (push
     "Atomically push a value onto an atomic list."
     (AtVar (List :a) -> :a -> :m (List :a)))
    (pop
     "Atomically pop and retrieve the head of an atomic list."
     (AtVar (List :a) -> :m (Optional :a)))))

(cl:defmacro derive-monad-at-var (monad-param monadT-form)
  "Automatically derive an instance of MonadAtVar for a monad transformer.

Example:
  (derive-monad-at-var :m (st:StateT :s :m))"
  `(define-instance (MonadAtVar ,monad-param => MonadAtVar ,monadT-form)
     (define new-at-var (compose lift new-at-var))
     (define read (compose lift read))
     (define write (compose2 lift write))
     (define modify (compose2 lift modify))
     (define modify-swap (compose2 lift modify-swap))
     (define push (compose2 lift push))
     (define pop (compose lift pop))))

(coalton-toplevel

  ;;
  ;; Std. Library Transformer Instances
  ;;

  (derive-monad-at-var :m (st:StateT :s :m))
  (derive-monad-at-var :m (env:EnvT :e :m))
  (derive-monad-at-var :m (LoopT :m)))
