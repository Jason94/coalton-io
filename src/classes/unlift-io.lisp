(cl:in-package :cl-user)
(defpackage :io/classes/unlift-io
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/monad/classes
   #:io/classes/monad-io
   #:io/classes/lift-io)
  (:import-from #:coalton-library/monad/environment
   #:EnvT)
  (:local-nicknames
   (:e #:coalton-library/monad/environment))
  (:export
   ;; Library Public
   #:UnliftIo
   #:with-run-in-io))
(in-package :io/classes/unlift-io)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  ;; NOTE: Defining a "wrapper" around with-run-in-io so that we can specialize on it.
  (define-class ((MonadIo :m) (LiftIo :i :m) => UnliftIo :m :i (:m -> :i))
    (with-run-in-io (((:m :a -> :i :a) -> :i :b) -> :m :b)))

  (define-instance ((BaseIo :r) (UnliftIo :m :r) => UnliftIo (EnvT :env :m) :r)
    (inline)
    (define (with-run-in-io enva->ioa-->iob)
      (EnvT
       (fn (env)
         (with-run-in-io
           (fn (ma->ioa-->iob)
             (enva->ioa-->iob
              (fn (m-env)
               (ma->ioa-->iob
                (e:run-envT m-env env))))))))))
  )
