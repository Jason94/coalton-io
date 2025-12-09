(cl:in-package :cl-user)
(defpackage :io/monad-io
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/functions
   #:coalton-library/monad/classes
   #:io/classes/base-io
   #:io/classes/monad-io
   #:io/classes/lift-io
   #:io/classes/unlift-io
   #:io/utils)
  (:import-from #:coalton-library/experimental/loops
   #:dolist)
  (:import-from #:coalton-library/experimental/do-control-loops-adv
   #:LoopT)
  (:local-nicknames
   (:t #:coalton-library/types)
   (:st #:coalton-library/monad/statet)
   (:e #:coalton-library/monad/environment)
   (:it #:coalton-library/iterator)
   (:c #:coalton-library/cell))
  (:export
   ;;; Re-export: BaseIo
   #:BaseIo
   #:run!

   ;;; Re-export: MonadIo
   #:MonadIo
   #:wrap-io_
   #:wrap-io
   #:derive-monad-io
   #:run-as!
   ;;; UnliftIo & LiftIo
   ;;; Re-export: LiftIo
   #:LiftIo
   #:lift-io_
   #:lift-io
   #:derive-lift-io

   ;;; Re-export: UnliftIo
   #:UnliftIo
   #:with-run-in-io

   ;;; Fused Helpers
   #:map-into-io
   #:foreach-io
   #:do-map-into-io
   #:do-foreach-io
   ))
(in-package :io/monad-io)

(named-readtables:in-readtable coalton:coalton)

(cl:defmacro wrap-io (cl:&body body)
  "Wrap the execution of BODY in the IO monad.
Supports any MonadIo instance.

Example:
  (wrap-io
    (lisp :a (str)
      (cl:print str)))"
  `(wrap-io_ (fn () ,@body)))

(cl:defmacro derive-monad-io (monad-param monadT-form)
  "Automatically derive an instance of MonadIo for a monad transformer.

Example:
  (derive-monad-io :m (st:StateT :s :m))"
  `(define-instance (MonadIo ,monad-param => MonadIo ,monadT-form)
     (define wrap-io_ (compose lift wrap-io_))))

(cl:defmacro run-as! (m-type m-op)
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

(coalton-toplevel

  ;;
  ;; Std. Library Transformer Instances
  ;;

  (derive-monad-io :m (st:StateT :s :m))
  (derive-monad-io :m (e:EnvT :env :m))
  (derive-monad-io :m (LoopT :m))
  )

;;;
;;; Lift IO
;;;

(coalton-toplevel

  (define-instance (BaseIo :i => LiftIo :i :i)
    (inline)
    (define lift-io id)))

(cl:defmacro derive-lift-io (monad-param monadT-form)
  "Automatically derive an instance of LiftIo for a monad transformer.

Example:
  (derive-lift-io :m (e:EnvT :e :m))"
  `(define-instance ((LiftIo :i ,monad-param) => LiftIo :i ,monadT-form)
     (define lift-io (compose lift lift-io))))

(coalton-toplevel

  ;;
  ;; Std. Library Transformer Instances
  ;;

  (derive-lift-io :m (st:StateT :s :m))
  (derive-lift-io :m (e:EnvT :env :m))
  (derive-lift-io :m (LoopT :m))
  )

;;;
;;; Unlift IO
;;;

;;
;; Efficient Iteration Ops
;;

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

(cl:defmacro do-map-into-io ((var lst) cl:&body body)
  `(map-into-io ,lst
     (fn (,var)
       (do
        ,@body))))

(cl:defmacro do-foreach-io ((var into-itr) cl:&body body)
  `(foreach-io ,into-itr
     (fn (,var)
       (do
        ,@body))))
