(cl:in-package :cl-user)
(defpackage :io/unique
  (:use
   #:coalton
   #:coalton-prelude
   #:io/monad-io
   #:io/classes/monad-io-unique)
  (:import-from #:coalton-library/experimental/do-control-loops-adv
   #:LoopT)
  (:local-nicknames
   (:at #:io/atomics_)
   (:st #:coalton-library/monad/statet)
   (:env #:coalton-library/monad/environment))
  (:export
   ;; Re-exports from io/classes/monad-io-unique
   #:MonadIoUnique
   #:Unique
   #:new-unique
   #:to-int

   ;; Remaining exports
   #:derive-monad-io-unique
   #:implement-monad-io-unique
   ))
(in-package :io/unique)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (declare counter% (at:Atomic Integer))
  (define counter%
    (at:new 0))

  (declare new-unique% (MonadIo :m => :m Unique))
  (define new-unique%
    (wrap-io (Unique% (at:atomic-update-swap counter% (+ 1))))))

(cl:defmacro implement-monad-io-unique (monad)
  `(define-instance (MonadIoUnique ,monad)
     (define new-unique new-unique%)))

(cl:defmacro derive-monad-io-unique (monad-param monadT-form)
  "Automatically derive an instance of MonadIoUnique for a monad transformer.

Example:
  (derive-monad-io-unique :m (st:StateT :s :m))"
  `(define-instance (MonadIoUnique ,monad-param => MonadIoUnique ,monadT-form)
     (define new-unique (lift new-unique))))

(coalton-toplevel
  ;;
  ;; Std. Library Transformer Instances
  ;;

  (derive-monad-io-unique :m (st:StateT :s :m))
  (derive-monad-io-unique :m (env:EnvT :e :m))
  (derive-monad-io-unique :m (LoopT :m)))
