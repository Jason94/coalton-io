(cl:in-package :cl-user)
(defpackage :io/gen-impl/unique
  (:use
   #:coalton
   #:coalton-prelude
   #:io/classes/monad-io
   #:io/classes/monad-io-unique)
  (:local-nicknames
   (:at #:io/thread-impl/atomics)
   )
  (:export
   #:implement-monad-io-unique
   ))
(in-package :io/gen-impl/unique)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  ;; TODO: Update to use at:AtomicInteger
  (declare counter% (at:Atomic Integer))
  (define counter%
    (at:new 0))

  (declare new-unique% (MonadIo :m => :m Unique))
  (define new-unique%
    (wrap-io (Unique% (at:atomic-update-swap counter% (+ 1))))))

(defmacro implement-monad-io-unique (monad)
  `(define-instance (MonadIoUnique ,monad)
     (define new-unique new-unique%)))

