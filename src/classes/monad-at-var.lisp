(cl:in-package :cl-user)
(defpackage :io/classes/monad-at-var
  (:use
   #:coalton
   #:coalton-prelude
   #:io/classes/monad-io)
  (:local-nicknames
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

