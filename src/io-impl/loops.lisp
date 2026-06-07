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
   (:it #:coalton-library/iterator)
   (:c #:coalton-library/cell)
   )
  (:export
   ;; Library Public
   #:do-foreach-io
   #:do-map-into-io
   #:do-times-io

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
    "Efficiently perform a monadic operation for each element of an iterator
and return the results. More efficient than map-into-io, if you can run your
effect in a BaseIo."
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
                        => :i * (c:Cell :a -> IO :b) -> :m Unit))
  (define (foreach-io coll a->mb)
    "Efficiently perform a monadic operation for each element of an iterator.
More efficient than foreach-io, if your effect can run in IO. The next element of the
iterator is passed into the operation via a cell."
    (lift-io
     (the
      (IO Unit)
      (wrap-io
       (let itr = (it:into-iter coll))
       (let fst = (it:next! itr))
       (match fst
         ((None)
          Unit)
         ((Some initial-val)
          (let c = (c:new initial-val))
          (let monad-op = (a->mb c))
          (run-io-unhandled! monad-op)
          (foreach (a itr)
                   (c:write! c a)
                   (run-io-unhandled! monad-op))
          Unit))))))

  (inline)
  (declare times-io (LiftIo IO :m => UFix * IO :a -> :m Unit))
  (define (times-io n io-op)
    "Efficiently perform an IO operation N times."
    (lift-io
     (the
      (IO Unit)
      (wrap-io_
       (fn ()
         (dotimes (_ n)
           (run-io-unhandled! io-op))
         Unit)))))
  )

(defmacro do-map-into-io ((var lst) cl:&body body)
  `(map-into-io ,lst
     (fn (,var)
       (do
        ,@body))))

(defmacro do-foreach-io ((var-sym into-itr) cl:&body body)
  "Efficiently perform a monadic operation for each element of an iterator.
More efficient than foreach-io, if your effect can run in IO. VAR-SYM is bound
to the value of the element in the iterator."
  (cl:let ((cell-sym (cl:gensym "iteration-val")))
    `(foreach-io ,into-itr
      (fn (,cell-sym)
        (do
         (,var-sym <- (wrap-io (c:read ,cell-sym)))
         ,@body)))))

(defmacro do-times-io (n cl:&body body)
  "Efficiently perform an IO operation N times."
  `(times-io ,n
    (do
     ,@body)))
