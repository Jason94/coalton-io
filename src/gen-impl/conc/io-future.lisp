(cl:in-package :cl-user)
(defpackage :io/gen-impl/conc/future
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/types
   #:coalton-library/monad/classes
   #:coalton-library/experimental/do-control-core
   #:io/utils
   #:io/thread-exceptions
   #:io/classes/monad-exception
   #:io/classes/monad-io
   #:io/classes/monad-io-thread
   #:io/gen-impl/conc/mvar
   )
  (:export
   #:Future
   #:fork-future
   #:try-read-future

   #:do-fork-future
   ))
(in-package :io/gen-impl/conc/future)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  ;; NOTE: Unfortunately we have to store all of the methods of the underlying Concurrent in here as
  ;; thunks. The only alternative is to store the underlying Concurrent type in Future.
  ;;
  ;; TODO: Once Coalton gets GADTs, we can scrap all of this!
  (define-struct (Future :a)
    (value-mvar (MVar (Result Dynamic :a)))
    (stop-callback (Unit -> Unit))
    (mask-callback (Unit -> Unit))
    (unmask-callback (Unit -> Unit))
    (unmask-finally-callback ((UnmaskFinallyMode -> Unit) -> Unit)))

  (inline)
  (declare fork-future ((MonadException :r) (LiftTo :r :m) (UnliftIo :r :i)
                        (MonadIoThread :rt :t :r) (MonadIoThread :rt :t :m)
                        => :r :a -> :m (Future :a)))
  (define (fork-future task)
    "Spawn a new future, which will run and eventually return the result
from TASK. The future is guaranteed to only ever run at most once, when
the produced :m is run."
    (let m-prx = Proxy)
    (let rt-prx = (runtime-for m-prx))
    (as-proxy-of
     (do
      (value-var <- new-empty-mvar)
      (thread <-
       (do-fork-thread
         (result <- (try-dynamic task))
         (put-mvar value-var result)))
      (pure (Future value-var
                    (fn ()
                      (stop! rt-prx thread))
                    (fn ()
                      (mask! rt-prx thread))
                    (fn ()
                      (unmask! rt-prx thread))
                    (fn (callback)
                      (unmask-finally! rt-prx thread callback)))))
     m-prx))

  (inline)
  (declare await% ((MonadIoThread :rt :t :m) (MonadException :m) => Future :a -> :m :a))
  (define (await% future)
    (matchM (read-mvar (.value-mvar future))
      ((Ok a)
       (pure a))
      ((Err dyn-e)
       (raise-dynamic dyn-e))))

  (inline)
  (declare try-read-future ((MonadIoThread :rt :t :m) (MonadException :m)
                            => Future :a -> :m (Optional :a)))
  (define (try-read-future future)
    "Try to read the current value from FUTURE, returning NONE
if it is not available. Raises any exceptions in the awaiting thread
that were raised in the future thread."
    (do-matchM (try-read-mvar (.value-mvar future))
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

;;;
;;; Future Concurrent Instance
;;;

(coalton-toplevel

  (define-instance (Concurrent (Future :a) :a)
    (inline)
    (define (stop fut)
      (wrap-io ((.stop-callback fut))))
    (inline)
    (define await await%)
    (inline)
    (define (mask fut)
      (wrap-io ((.mask-callback fut))))
    (inline)
    (define (unmask fut)
      (wrap-io ((.unmask-callback fut))))
    (inline)
    (define (unmask-finally fut callback)
      (lift-to
       (with-run-in-io
           (fn (run)
             (wrap-io
               ((.unmask-finally-callback fut) (compose (const Unit) (fn (x)
                                                                       (run! (run (callback x))))))))))))
  )
