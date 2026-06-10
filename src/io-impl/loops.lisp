(cl:in-package :cl-user)
(defpackage :io/io-impl/simple-io/loops
  (:use
   #:coalton
   #:coalton-prelude
   #:io/utils
   #:io/classes/monad-io
   #:io/io-impl/simple-io
   )
  (:import-from #:coalton-library/experimental/loops
   #:dotimes)
  (:local-nicknames
   (:dcc #:coalton/experimental/do-control-core)
   (:it #:coalton-library/iterator)
   (:c #:coalton-library/cell)
   )
  (:export
   ;; Library Public
   #:do-foreach-io
   #:do-map-into-io
   #:do-times-io
   #:do-repeat-io
   #:do-while-io
   #:do-while-val-io

   ;; Library Private
   ))
(in-package :io/io-impl/simple-io/loops)

(cl:declaim (cl:optimize (cl:speed 3) (cl:debug 0) (cl:safety 0)))

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (inline)
  (declare map-into-io ((LiftIo IO :m) (it:IntoIterator :i :a)
                         => :i * (:a -> IO :b) -> :m (List :b)))
  (define (map-into-io itr a->mb)
    (lift-io
     (the
      (IO (List :a))
      (wrap-io
        (let results = (c:new (make-list)))
        (foreach (a itr)
          (c:push! results (run-io-unhandled! (a->mb a))))
        (reverse (c:read results))))))

  (inline)
  (declare foreach-io ((LiftIo IO :m) (it:IntoIterator :i :a)
                        => :i * (:a -> IO :b) -> :m Unit))
  (define (foreach-io coll a->mb)
    (lift-io
     (the
      (IO Unit)
      (wrap-io
       (let itr = (it:into-iter coll))
       (foreach (a itr)
         (run-io-unhandled! (a->mb a)))
       Unit))))

  (inline)
  (declare times-io (LiftIo IO :m => UFix * (UFix -> IO :a) -> :m Unit))
  (define (times-io n x->io-op)
    (lift-io
     (the
      (IO Unit)
      (wrap-io
       (dotimes (i n)
         (run-io-unhandled! (x->io-op i)))
       Unit))))

  (inline)
  (declare repeat-io (LiftIo IO :m => UFix * IO :a -> :m Unit))
  (define (repeat-io n io-op)
    (lift-io
     (the
      (IO Unit)
      (wrap-io
       (dotimes (_ n)
         (run-io-unhandled! io-op))
       Unit))))

  (inline)
  (declare while-io (LiftIo IO :m => IO Boolean -> :m Unit))
  (define (while-io io-op)
    (lift-io
     (the
      (IO Unit)
      (wrap-io
       (for ((next? (run-io-unhandled! io-op)
                    (run-io-unhandled! io-op)))
            :while next?)
       Unit))))

  (inline)
  (declare while-val-io ((LiftIo IO :m) (dcc::Yielder :y) => IO (:y :a) * (:a -> IO :b) -> :m Unit))
  (define (while-val-io io-val io-body)
    (lift-io
     (the
      (IO Unit)
      (wrap-io
       (for ()
         (match (dcc::yield (run-io-unhandled! io-val))
           ((None)
            (break))
           ((Some a)
            (run-io-unhandled! (io-body a))
            (values))))
       Unit))))
  )

(defmacro do-map-into-io ((var lst) cl:&body body)
  "Efficiently perform an IO operation for each element of an iterator and return the
results."
  `(map-into-io ,lst
     (fn (,var)
       (do
        ,@body))))

(defmacro do-foreach-io ((var-sym into-itr) cl:&body body)
  "Efficiently perform an IO operation for each element of an iterator."
  `(foreach-io ,into-itr
    (fn (,var-sym)
      (do
       ,@body))))

(defmacro do-times-io ((count-sym n) cl:&body body)
  "Efficiently perform an IO operation N times with a counter."
  `(times-io ,n
    (fn (,count-sym)
      (do
      ,@body))))

(defmacro do-repeat-io (n cl:&body body)
  "Efficiently perform an IO operation N times."
  `(repeat-io ,n
    (do
     ,@body)))

(defmacro do-while-io (cl:&body body)
  "Efficiently loop an IO operation until it returns `False`."
  `(while-io
    (do
     ,@body)))

(defmacro do-while-val-io ((var var-op) cl:&body body)
  "Efficiently run `body` with `var` so long as `var-op` produces a value."
  `(while-val-io
    ,var-op
    (fn (,var)
      (do
       ,@body))))
