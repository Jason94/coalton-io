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

  (declare new-tarray (MonadIo :m => UFix * :a -> :m (TArray :a)))
  (define (new-tarray length init-elem)
    "Create a new `TArray` with size `length` and all values set to `init-elem`."
    (wrap-io
     (let arr = (la:make-uninitialized length))
     (lp:dotimes (i length)
       (la:set! arr i (new-tvar% init-elem)))
     (TArray% arr)))

  (inline)
  (declare aref (MonadIo :m => TArray :a * UFix -> STM :m (Optional :a)))
  (define (aref (TArray% tarr) i)
    (if (< i (la:length tarr))
        (STM%
         (fn (tx-data)
           (wrap-io
            (map Some
                 (inner-read-tvar% (la:aref tarr i)
                                   tx-data)))))
        (pure None)))

  (inline)
  (declare aref# (MonadIo :m => TArray :a * UFix -> STM :m :a))
  (define (aref# (TArray% tarr) i)
    (read-tvar (la:aref tarr i)))

  (inline)
  (declare set (MonadIo :m => TArray :a * UFix * :a -> STM :m Unit))
  (define (set (TArray% tarr) i elem)
    (write-tvar (la:aref tarr i)
                elem))
 )
