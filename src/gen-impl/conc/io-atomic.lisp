(cl:in-package :cl-user)
(defpackage :io/gen-impl/conc/atomic
  (:use
   #:coalton
   #:coalton-prelude
   #:io/utils
   #:io/classes/monad-io
   )
  (:local-nicknames
   (:at #:io/thread-impl/atomics)
   )
  (:export
   ;; Library Public
   #:AtVar
   #:new-at-var
   #:read
   #:write
   #:modify
   #:modify-swap
   #:push
   #:pop

   ;; Library Private
   #:unwrap-atvar
   ))
(in-package :io/gen-impl/conc/atomic)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (repr :transparent)
  (define-type (AtVar :a)
    (AtVar% (at:Atomic :a)))

  (inline)
  (declare unwrap-atvar (AtVar :a -> at:Atomic :a))
  (define (unwrap-atvar (AtVar% atm))
    atm)

  (inline)
  (declare new-at-var (MonadIo :m => :a -> :m (AtVar :a)))
  (define (new-at-var val)
    "Create a new atomic variable with an initial value."
    (wrap-io (AtVar% (at:new val))))

  (inline)
  (declare read (MonadIo :m => AtVar :a -> :m :a))
  (define (read atm)
    "Read the value from an atomic variable."
    (wrap-io (at:read (unwrap-atvar atm))))

  (inline)
  (declare write (MonadIo :m => AtVar :a -> :a -> :m Unit))
  (define (write atm val)
    "Write a new value to an atomic variable."
    (wrap-io (at:atomic-write (unwrap-atvar atm) val)))

  (inline)
  (declare modify (MonadIo :m => AtVar :a -> (:a -> :a) -> :m :a))
  (define (modify atm f)
    "Atomically modify by applying F, then return the new
value of the atomic variable. F may be called multiple times,
and must be a pure function. If F errors, it will be raised
in (:m :a) as an UnhandledError exception, and the atomic
variable will not be modified."
    (wrap-io (at:atomic-update (unwrap-atvar atm) f)))

  (inline)
  (declare modify-swap (MonadIo :m => AtVar :a -> (:a -> :a) -> :m :a))
  (define (modify-swap atm f)
    "Atomically modify by applying F, then return the old
value of the variable. F may be called multiple times, and
must be a pure function. If F errors, it will be raised
in (:m :a) as an UnhandledError exception, and the atomic
variable will not be modified."
    (wrap-io (at:atomic-update-swap (unwrap-atvar atm) f)))

  (inline)
  (declare push (MonadIo :m => AtVar (List :a) -> :a -> :m (List :a)))
  (define (push atm elt)
    "Atomically push a value onto an atomic list."
    (wrap-io (at:atomic-push (unwrap-atvar atm) elt)))

  (inline)
  (declare pop (MonadIo :m => AtVar (List :a) -> :m (Optional :a)))
  (define (pop atm)
    "Atomically pop and retrieve the head of an atomic list."
    (wrap-io (at:atomic-pop (unwrap-atvar atm)))))

