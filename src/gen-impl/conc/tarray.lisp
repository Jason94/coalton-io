(cl:in-package :cl-user)
(defpackage :io/gen-impl/conc/stm/tarray
  (:use
   #:coalton
   #:coalton-prelude
   #:io/classes/monad-io
   #:io/gen-impl/conc/stm
   )
  (:local-nicknames
   (:lp #:coalton/experimental/loops)
   (:la #:coalton/lisparray)
   )
  (:export
   ;; Library Public
   #:TArray
   #:new-tarray
   #:aref
   #:aref#
   #:set
   ))
(in-package :io/gen-impl/conc/stm/tarray)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (repr :transparent)
  (define-type (TArray :a)
    (TArray% (la:LispArray (TVar :a))))

  (inline)
  (declare tarr% (TArray :a -> la:LispArray (TVar :a)))
  (define (tarr% (TArray% tarr))
    tarr)

  (declare new-tarray (MonadIo :m => UFix * :a -> :m (TArray :a)))
  (define (new-tarray length init-elem)
    "Create a new `TArray` with size `length` and all values set to `init-elem`."
    (wrap-io
     (let arr = (la:make-uninitialized length))
     (lp:dotimes (i length)
       (la:set! arr i (new-tvar% init-elem)))
     (TArray% arr)))

  (inline)
  (declare aref (TArray :a * UFix -> STM (Optional :a)))
  (define (aref tarr i)
    (if (< i (la:length (tarr% tarr)))
        (STM%
         (fn (tx-data)
           (map Some
                (inner-read-tvar% (la:aref (tarr% tarr) i)
                                  tx-data))))
        (pure None)))

  (inline)
  (declare aref# (TArray :a * UFix -> STM :a))
  (define (aref# tarr i)
    (read-tvar (la:aref (tarr% tarr) i)))

  (inline)
  (declare set (TArray :a * UFix * :a -> STM Unit))
  (define (set tarr i elem)
    (write-tvar (la:aref (tarr% tarr) i)
                elem))
 )
