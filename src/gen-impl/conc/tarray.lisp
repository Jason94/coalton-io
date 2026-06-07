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
   #:at
   #:at#
   #:set
   #:modify
   #:modify-swap
   ))
(in-package :io/gen-impl/conc/stm/tarray)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (repr :transparent)
  (define-type (TArray :a)
    "A transactional array, where each element is a synchronized transactional variable."
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
  (declare at (TArray :a * UFix -> STM (Optional :a)))
  (define (at tarr i)
    "Read the value in `tarr` at index `i`."
    (if (< i (la:length (tarr% tarr)))
        (STM%
         (fn (tx-data)
           (Some
            (inner-read-tvar% (la:aref (tarr% tarr) i)
                              tx-data))))
        (pure None)))

  (inline)
  (declare at# (TArray :a * UFix -> STM :a))
  (define (at# tarr i)
    "Read the value in `tarr` at index `i`. Errors if out of bounds."
    (read-tvar (la:aref (tarr% tarr) i)))

  (inline)
  (declare set (TArray :a * UFix * :a -> STM Unit))
  (define (set tarr i elem)
    "Set the value in `tarr` at index `i` to `elem`."
    (write-tvar (la:aref (tarr% tarr) i)
                elem))

  (inline)
  (declare modify (TArray :a * UFix * (:a -> :a) -> STM :a))
  (define (modify tarr i f)
    "Update the value in `tarr` at index `i` with `f`. Returns the new value."
    (modify-tvar (la:aref (tarr% tarr) i)
                 f))

  (inline)
  (declare modify-swap (TArray :a * UFix * (:a -> :a) -> STM :a))
  (define (modify-swap tarr i f)
    "Update the value in `tarr` at index `i` with `f`. Returns the old value."
    (modify-swap-tvar (la:aref (tarr% tarr) i)
                      f))
 )
