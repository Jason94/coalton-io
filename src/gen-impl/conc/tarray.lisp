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
   (:m-lp #:coalton/experimental/do-control-loops)
   (:la #:coalton/lisparray)
   (:t #:coalton/types)
   )
  (:export
   ;; Library Public
   #:TArray
   #:new-tarray
   #:new-tarray-apply
   #:to-arr
   #:at
   #:at#
   #:tvar-at
   #:tvar-at#
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

  (declare new-tarray-apply (MonadIo :m => UFix * (UFix -> :m :a) -> :m (TArray :a)))
  (define (new-tarray-apply length factory)
    "Create a new `TArray` with size `length` and all values populated by applying
`factory` with the index."
    (do
     (let arr = (la:make-uninitialized length))
     (m-lp:do-loop-times (i length)
       (val <- (factory i))
       (wrap-io
        (la:set! arr i (new-tvar% val))
        Unit))
     (pure (TArray% arr))))

  (declare to-arr (t:RuntimeRepr :a => TArray :a -> STM (la:LispArray :a)))
  (define (to-arr tarr)
    "Extract synchronized values in `tarr` into a `LispArray`."
    (STM%
     (fn (tx-data)
       (let length = (la:length (tarr% tarr)))
       (let output = (la:make-uninitialized length))
       (lp:dotimes (i length)
         (la:set! output i (inner-read-tvar% (la:aref (tarr% tarr) i)
                                             tx-data)))
       output)))

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
  (declare tvar-at (TArray :a * UFix -> Optional (TVar :a)))
  (define (tvar-at tarr i)
    "Get the synchronized variable at `i` in `tarr`."
    (if (< i (la:length (tarr% tarr)))
        (Some (la:aref (tarr% tarr) i))
        None))

  (inline)
  (declare tvar-at# (TArray :a * UFix -> TVar :a))
  (define (tvar-at# tarr i)
    "Get the synchronized variable at `i` in `tarr`. Errors if out of bounds."
    (la:aref (tarr% tarr) i))

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
