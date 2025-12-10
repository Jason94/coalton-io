(cl:in-package :cl-user)
(defpackage :io/gen-impl/mut
  (:use
   #:coalton
   #:coalton-prelude
   #:io/classes/monad-io
   #:io/classes/monad-io-var
   )
  (:local-nicknames
   (:c #:coalton-library/cell)
   )
  (:export
   ;; Library Public
   #:implement-monad-io-var

   ;; Library Private
   #:new-var%
   #:read%
   #:write%
   #:modify%
   ))
(in-package :io/gen-impl/mut)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (inline)
  (declare new-var% (MonadIo :m => :a -> :m (Var :a)))
  (define (new-var% val)
    (wrap-io (Var% (c:new val))))

  (inline)
  (declare read% (MonadIo :m => Var :a -> :m :a))
  (define (read% (Var% cel))
    (wrap-io (c:read cel)))

  (inline)
  (declare write% (MonadIo :m => Var :a -> :a -> :m :a))
  (define (write% (Var% cel) val)
    "Set the value in an Var and return the old value."
    (wrap-io
      (c:swap! cel val)))

  (inline)
  (declare modify% (MonadIo :m => Var :a -> (:a -> :a) -> :m :a))
  (define (modify% (Var% cel) f)
    "Modify the value in an Var and return the old value."
    (wrap-io (c:update-swap! f cel))))

(cl:defmacro implement-monad-io-var (monad)
  `(define-instance (MonadIoVar ,monad)
     (define new-var new-var%)
    (define read read%)
    (define write write%)
    (define modify modify%)))

