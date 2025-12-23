(cl:in-package :cl-user)
(defpackage :io/gen-impl/conc/atomic
  (:use
   #:coalton
   #:coalton-prelude
   #:io/utils
   #:io/classes/monad-io
   #:io/classes/monad-exception
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
    "A container that can be read and modified atomically."
    ;; CONCURRENT:
    ;; In general, atomic algorithms don't require masking.
    (AtVar% (at:Atomic :a)))

  (inline)
  (declare unwrap-atvar (AtVar :a -> at:Atomic :a))
  (define (unwrap-atvar (AtVar% atm))
    atm)

  (inline)
  (declare new-at-var (MonadIo :m => :a -> :m (AtVar :a)))
  (define (new-at-var val)
    "Create a new AtVar containing `val`."
    (wrap-io (AtVar% (at:new val))))

  (inline)
  (declare read (MonadIo :m => AtVar :a -> :m :a))
  (define (read atm)
    "Atomically read the value from `atm`."
    (wrap-io (at:read (unwrap-atvar atm))))

  (inline)
  (declare write (MonadIo :m => AtVar :a -> :a -> :m Unit))
  (define (write atm val)
    "Atomically write a new value to `atm`."
    (wrap-io (at:atomic-write (unwrap-atvar atm) val)))

  (inline)
  (declare modify ((MonadIo :m) (MonadException :m) => AtVar :a -> (:a -> :a) -> :m :a))
  (define (modify atm f)
    "Atomically modify `atm` by applying `f` and return the new value. `f` must be a pure
function. If `f` throws an error, `atm` will be unchanged and the error will be handleable
via `MonadException`.

Concurrent:
  - WARNING: `f` will be retried each time the calling thread loses the race to update
    `atm`, so `f` must be pure."
    (wrap-io (at:atomic-update (unwrap-atvar atm) f)))

  (inline)
  (declare modify-swap (MonadIo :m => AtVar :a -> (:a -> :a) -> :m :a))
  (define (modify-swap atm f)
    "Atomically modify `atm` by applying `f` and return the old value. `f` must be a pure
function. If `f` throws an error, `atm` will be unchanged and the error will be handleable
via `MonadException`.

Concurrent:
  - WARNING: `f` will be retried each time the calling thread loses the race to update
    `atm`, so `f` must be pure."
    (wrap-io (at:atomic-update-swap (unwrap-atvar atm) f)))

  (inline)
  (declare push (MonadIo :m => AtVar (List :a) -> :a -> :m (List :a)))
  (define (push atm elt)
    "Atomically push a value onto an atomic list. Returns the new list."
    (wrap-io (at:atomic-push (unwrap-atvar atm) elt)))

  (inline)
  (declare pop (MonadIo :m => AtVar (List :a) -> :m (Optional :a)))
  (define (pop atm)
    "Atomically pop and retrieve the head of an atomic list."
    (wrap-io (at:atomic-pop (unwrap-atvar atm)))))

