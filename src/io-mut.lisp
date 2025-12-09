(cl:in-package :cl-user)
(defpackage :io/mut
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/functions
   #:io/classes/monad-io-var
   #:io/utils
   #:io/monad-io)
  (:import-from #:coalton-library/experimental/loops
   #:dolist)
  (:import-from #:coalton-library/experimental/do-control-loops-adv
   #:LoopT)
  (:local-nicknames
   (:at #:io/atomics_)
   (:io #:io/simple-io)
   (:it #:coalton-library/iterator)
   (:c #:coalton-library/cell)
   (:st #:coalton-library/monad/statet)
   (:env #:coalton-library/monad/environment))
  (:export
   ;; Re-export: io/classes/monad-io-var
   #:Var
   #:MonadIoVar
   #:new-var
   #:read
   #:write
   #:modify

   ;; Remaining exports
   #:derive-monad-var
   #:implement-monad-io-var
   ))
(in-package :io/mut)

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

;;
;; Simple IO Implementation
;;

(coalton-toplevel

  (implement-monad-io-var io:IO))
