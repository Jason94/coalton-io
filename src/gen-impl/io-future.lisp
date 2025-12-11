(cl:in-package :cl-user)
(defpackage :io/gen-impl/future
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/monad/classes
   #:coalton-library/experimental/do-control-core
   #:io/utils
   #:io/classes/monad-exception
   #:io/classes/monad-io
   #:io/classes/monad-io-thread
   #:io/classes/monad-io-mvar
   )
  (:export
   #:Future
   #:fork-future
   #:await
   #:try-read-future

   #:do-fork-future
   ))
(in-package :io/gen-impl/future)

(named-readtables:in-readtable coalton:coalton)

;;;
;;; Futures - Note: Unlike most other packages, Futures can be defined
;;; using MVars and Threads. Therefore, Future doesn't need a separate
;;; capability class or deriving/implementing macros.
;;;

(coalton-toplevel
  (repr :transparent)
  (define-type (Future :a)
    (Future% (MVar (Result Dynamic :a))))

  (inline)
  (declare value-mvar (Future :a -> MVar (Result Dynamic :a)))
  (define (value-mvar (Future% mvar))
    mvar)

  (declare fork-future ((MonadIoThread :m :t) (MonadIoMVar :m) (MonadIoMVar :r)
                        (UnliftIo :r :i) (LiftTo :r :m) (MonadException :r)
                        => :r :a -> :m (Future :a)))
  (define (fork-future task)
    "Spawn a new future, which will run and eventually return the result
from TASK. The future is guaranteed to only ever run at most once, when
the produced :m is run."
    (do
     (value-var <- new-empty-mvar)
     (do-fork
       (result <- (try-dynamic task))
       (put-mvar value-var result))
     (pure (Future% value-var))))


  (inline)
  (declare await ((MonadIoMVar :m) (MonadException :m) => Future :a -> :m :a))
  (define (await future)
    "Read the value from FUTURE, blocking until it is available.
Raises any exceptions in the awaiting thread that were raised in
the future thread."
    (matchM (read-mvar (value-mvar future))
      ((Ok a)
       (pure a))
      ((Err dyn-e)
       (raise-dynamic dyn-e))))

  (inline)
  (declare try-read-future ((MonadIoMvar :m) (MonadException :m)
                            => Future :a -> :m (Optional :a)))
  (define (try-read-future future)
    "Try to read the current value from FUTURE, returning NONE
if it is not available. Raises any exceptions in the awaiting thread
that were raised in the future thread."
    (do-matchM (try-read-mvar (value-mvar future))
      ((None)
       (pure None))
      ((Some result?)
       (match result?
         ((Ok a)
          (pure (Some a)))
         ((Err dyn-e)
          (raise-dynamic dyn-e))))))
  )


(cl:defmacro do-fork-future (cl:&body body)
  `(fork-future
    (do
     ,@body)))
