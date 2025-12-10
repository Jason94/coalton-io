(cl:in-package :cl-user)
(defpackage :io/thread-impl/stm-types
  (:use
   #:coalton
   #:coalton-prelude
   #:io/classes/monad-io)
  (:local-nicknames
   (:a #:coalton-threads/atomic)
   (:c #:coalton-library/cell))
  (:export
   ;; Library Public
   #:TVar

   #:STM

   ;; Library Private
   #:TVar%
   #:unwrap-tvar%
   #:set-tvar%
   #:tvar-value%

   #:TxResult%
   #:TxSuccess
   #:TxRetryAfterWrite
   #:TxFailed

   #:ReadEntry%
   #:WriteHashTable%
   #:TxData%

   #:unwrap-stm%
   #:run-stm%
   ))
(in-package :io/thread-impl/stm-types)

(named-readtables:in-readtable coalton:coalton)

;;; Provide early definition of the core STM types.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;          Main STM Types           ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(coalton-toplevel

  (derive Eq)
  (repr :transparent)
  (define-type (TVar :a)
    (TVar% (c:Cell :a)))

  (inline)
  (declare unwrap-tvar% (TVar :a -> c:Cell :a))
  (define (unwrap-tvar% (TVar% a))
    a)

  (inline)
  (declare set-tvar% (TVar :a -> :a -> Unit))
  (define (set-tvar% tvar val)
    (c:write! (unwrap-tvar% tvar) val)
    Unit)

  (inline)
  (declare tvar-value% (TVar :a -> :a))
  (define (tvar-value% tvar)
    (c:read (unwrap-tvar% tvar)))

  (define-type (TxResult% :a)
    (TxSuccess :a)
    TxRetryAfterWrite
    TxFailed)

  (define-instance (Functor TxResult%)
    (inline)
    (define (map f result)
      (match result
        ((TxSuccess a)
         (TxSuccess (f a)))
        ((TxRetryAfterWrite)
         TxRetryAfterWrite)
        ((TxFailed)
         TxFailed))))

  (repr :native cl:cons)
  (define-type ReadEntry%)

  (repr :native cl:hash-table)
  (define-type WriteHashTable%)

  (define-struct TxData%
    (lock-snapshot (c:cell a::Word))
    (read-log (c:cell (List ReadEntry%)))
    (write-log WriteHashTable%)
    (parent-tx (c:cell (Optional TxData%))))

  (repr :transparent)
  (define-type (STM :io :a)
    (STM% (TxData% -> :io (TxResult% :a))))

  (inline)
  (declare unwrap-stm% (STM :io :a -> (TxData% -> :io (TxResult% :a))))
  (define (unwrap-stm% (STM% f-tx))
    f-tx)

  (inline)
  (declare run-stm% (TxData% -> STM :io :a -> :io (TxResult% :a)))
  (define (run-stm% tx-data tx)
    ((unwrap-stm% tx) tx-data))
  )
