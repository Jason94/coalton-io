(cl:in-package :cl-user)
(defpackage :io/gen-impl/atomic
  (:use
   #:coalton
   #:coalton-prelude
   #:io/utils
   #:io/classes/monad-io
   #:io/classes/monad-at-var
   )
   (:local-nicknames
    (:at #:io/thread-impl/atomics)
    )
  (:export
   ;; Library Public
   #:implement-monad-at-var

   ;; Library Private
   #:new-at-var%
   #:read%
   #:write%
   #:modify%
   #:modify-swap%
   #:push%
   #:pop%
   ))
(in-package :io/gen-impl/atomic)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (inline)
  (declare new-at-var% (MonadIo :m => :a -> :m (AtVar :a)))
  (define (new-at-var% val)
    (wrap-io (AtVar% (at:new val))))

  (inline)
  (declare read% (MonadIo :m => AtVar :a -> :m :a))
  (define (read% atm)
    (wrap-io (at:read (unwrap-atvar atm))))

  (inline)
  (declare write% (MonadIo :m => AtVar :a -> :a -> :m Unit))
  (define (write% atm val)
    (wrap-io (at:atomic-write (unwrap-atvar atm) val)))

  (inline)
  (declare modify% (MonadIo :m => AtVar :a -> (:a -> :a) -> :m :a))
  (define (modify% atm f)
    (wrap-io (at:atomic-update (unwrap-atvar atm) f)))

  (inline)
  (declare modify-swap% (MonadIo :m => AtVar :a -> (:a -> :a) -> :m :a))
  (define (modify-swap% atm f)
    (wrap-io (at:atomic-update-swap (unwrap-atvar atm) f)))

  (inline)
  (declare push% (MonadIo :m => AtVar (List :a) -> :a -> :m (List :a)))
  (define (push% atm elt)
    (wrap-io (at:atomic-push (unwrap-atvar atm) elt)))

  (inline)
  (declare pop% (MonadIo :m => AtVar (List :a) -> :m (Optional :a)))
  (define (pop% atm)
    (wrap-io (at:atomic-pop (unwrap-atvar atm)))))

(cl:defmacro implement-monad-at-var (monad)
  `(define-instance (MonadAtVar ,monad)
     (define new-at-var new-at-var%)
     (define read read%)
     (define write write%)
     (define modify modify%)
     (define modify-swap modify-swap%)
     (define push push%)
     (define pop pop%)))

