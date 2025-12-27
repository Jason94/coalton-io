(cl:in-package :cl-user)
(defpackage :io/classes/monad-io
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/types
   #:coalton-library/monad/classes
   #:io/utils
   )
  (:import-from #:coalton-library/experimental/loops
   #:dolist)
  (:import-from #:coalton-library/experimental/do-control-loops-adv
   #:LoopT)
  (:local-nicknames
   (:c #:coalton-library/cell)
   (:it #:coalton-library/iterator)
   (:st #:coalton-library/monad/statet)
   (:e #:coalton-library/monad/environment))
  (:export
   #:MonadIo
   #:derive-monad-io
   #:wrap-io_
   #:wrap-io

   #:BaseIo
   #:run!
   #:run-handled!
   #:run-as!

   #:LiftIo
   #:derive-lift-io
   #:lift-io

   #:UnliftIo
   #:with-run-in-io
   #:base-io-prx-for
   #:unlift-io-prx-for

   #:map-into-io
   #:do-map-into-io
   #:foreach-io
   #:do-foreach-io))
(in-package :io/classes/monad-io)

(named-readtables:in-readtable coalton:coalton)

(cl:declaim (cl:optimize (cl:speed 3) (cl:debug 0) (cl:safety 0)))

(coalton-toplevel

  ;;;
  ;;; Define the Core Classes
  ;;;

  (define-class (Monad :m => MonadIo :m)
    (wrap-io_
     "Wrap a (potentially) side-effectful function in the monad."
     ((Unit -> :a) -> :m :a)))

  (define-class (MonadIo :m => BaseIo :m)
    "A 'base' IO implementation, which can be run to execute some
(potentially side-effectful) operation."
    (run!
     "Run a (potentially) side-effectful operation. Throws any unhandled
exceptions."
     (:m :a -> :a))
    (run-handled!
     "Run a (potentially) side-effectful operation. Returns any unhandled
exceptions as an (Err e)."
     (:m :a -> Result Dynamic :a)))

  (define-class ((Monad :m) (BaseIo :i) => LiftIo :i :m)
    (lift-io (BaseIo :i => :i :a -> :m :a)))

  (define-class ((MonadIo :m) (LiftIo :i :m) => UnliftIo :m :i (:m -> :i))
    (with-run-in-io (((:m :a -> :i :a) -> :i :b) -> :m :b)))

  ;;;
  ;;; Define Core Instances
  ;;;

  (define-instance (BaseIo :i => LiftIo :i :i)
    (inline)
    (define lift-io id))
  )

;;;
;;; Derive Macros
;;;

(defmacro derive-monad-io (monad-param monadT-form)
  "Automatically derive an instance of MonadIo for a monad transformer.

Example:
  (derive-monad-io :m (st:StateT :s :m))"
  `(define-instance (MonadIo ,monad-param => MonadIo ,monadT-form)
     (define wrap-io_ (compose lift wrap-io_))))

(defmacro derive-lift-io (monad-param monadT-form)
  "Automatically derive an instance of LiftIo for a monad transformer.

Example:
  (derive-lift-io :m (e:EnvT :e :m))"
  `(define-instance ((LiftIo :i ,monad-param) => LiftIo :i ,monadT-form)
     (define lift-io (compose lift lift-io))))

(coalton-toplevel

  ;;;
  ;;; Define Standard Library Instances
  ;;;

  (derive-monad-io :m (st:StateT :s :m))
  (derive-monad-io :m (e:EnvT :env :m))
  (derive-monad-io :m (LoopT :m))

  (derive-lift-io :m (st:StateT :s :m))
  (derive-lift-io :m (e:EnvT :env :m))
  (derive-lift-io :m (LoopT :m))

  (define-instance ((BaseIo :r) (UnliftIo :m :r) => UnliftIo (e:EnvT :env :m) :r)
    (inline)
    (define (with-run-in-io enva->ioa-->iob)
      (e:EnvT
       (fn (env)
         (with-run-in-io
           (fn (ma->ioa-->iob)
             (enva->ioa-->iob
              (fn (m-env)
               (ma->ioa-->iob
                (e:run-envT m-env env))))))))))

  (inline)
  (declare base-io-prx-for (UnliftIo :r :i => Proxy (:r :a) -> Proxy (:i :b)))
  (define (base-io-prx-for _)
    "For an UnliftIo, get a proxy for its BaseIo."
    Proxy)

  (inline)
  (declare unlift-io-prx-for (UnliftIo :r :i => Proxy (:i :a) -> Proxy (:r :b)))
  (define (unlift-io-prx-for _)
    "For an BaseIo, get a proxy for one of its UnliftIo's."
    Proxy)
  )

;;;
;;; Monad Helpers
;;;

(defmacro wrap-io (cl:&body body)
  "Wrap the execution of BODY in the IO monad.
Supports any MonadIo instance.

Example:
  (wrap-io
    (lisp :a (str)
      (cl:print str))"
  `(wrap-io_ (fn () ,@body)))

(defmacro run-as! (m-type m-op)
  "Run M-OP using the concrete RunIo M-TYPE. Useful for situations where
you want to create a generic MonadIo operation and immediately run it,
so the compiler can't infer the type of the actual monad you want to use.

Example:
  (run-as! (IO Unit) (pure Unit))

NOTE: Unfortunately, there seems to be a type inference bug that requires
putting in the full type of M-OP, not just (IO :a).
"

  ;; NOTE: This should be fine, until Coalton gets scoped type variables.
  ;; Then we'll need to use a gensym to construct the keyword.
  ;; NOTE: This *should* work. See above.
  ;; `(run! (the (,m-type :a) ,m-op)))
  `(run! (the ,m-type ,m-op)))

;;;
;;; Efficient Iteration Ops
;;;

(coalton-toplevel
  (declare map-into-io ((UnliftIo :r :io) (LiftTo :r :m) (it:IntoIterator :i :a)
                         => :i -> (:a -> :r :b) -> :m (List :b)))
  (define (map-into-io itr a->rb)
    "Efficiently perform a monadic operation for each element of an iterator
and return the results. If you're having inference issues, try map-into-io_"
    (lift-to
     (with-run-in-io
         (fn (run)
           (wrap-io
             (let results = (c:new (make-list)))
             (for a in (it:into-iter itr)
               (c:push! results (run! (run (a->rb a)))))
             (reverse (c:read results)))))))

  (declare foreach-io ((UnliftIo :r :io) (LiftTo :r :m) (it:IntoIterator :i :a)
                       => :i -> (:a -> :r :b) -> :m Unit))
  (define (foreach-io itr a->mb)
    "Efficiently perform a monadic operation for each element of an iterator.
If you're having inference issues, try foreach-io_."
    (lift-to
     (with-run-in-io
       (fn (run)
         (wrap-io
           (for a in (it:into-iter itr)
             (run! (run (a->mb a))))
           Unit)))))
  )

;;
;; Syntactic Sugar Macros
;;

(defmacro do-map-into-io ((var lst) cl:&body body)
  `(map-into-io ,lst
     (fn (,var)
       (do
        ,@body))))

(defmacro do-foreach-io ((var into-itr) cl:&body body)
  `(foreach-io ,into-itr
     (fn (,var)
       (do
        ,@body))))
