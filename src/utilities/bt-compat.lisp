(cl:in-package :cl-user)
(defpackage :io/utilities/bt-compat
  (:use
   #:coalton
   #:coalton-prelude
   )
  (:import-from #:coalton-library/system #:LispCondition)
  (:local-nicknames
   )
  (:export
   ;; Atomics
   #:AtomicInteger
   #:new-at
   #:cas!
   #:decf!
   #:incf!
   #:read
   ;; Locks
   #:Lock
   #:with-lock-held
   #:new-lk
   #:acquire
   #:acquire-no-wait
   #:release
   ))
(in-package :io/utilities/bt-compat)

(cl:declaim (cl:optimize (cl:speed 3) (cl:debug 0) (cl:safety 0)))

(named-readtables:in-readtable coalton:coalton)

;;;; This code is derived from coalton-threads:
;;;; https://github.com/garlic0x1/coalton-threads/tree/master
;;;;
;;;; coalton-threads is licensed under the MIT license, and the original
;;;; upstream LICENSE.txt is reproduced in src/utilities/coalton-threads-LICENSE.txt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                    Atomics                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(coalton-toplevel
  (define-type-alias Word #+32-bit U32 #+64-bit U64
    "An integer that fits in a CPU word.")

  (repr :native bt2:atomic-integer)
  (define-type AtomicInteger
    "An unsigned machine word that allows atomic increment, decrement and swap.")

  (declare new-at (Word -> AtomicInteger))
  (define (new-at value)
    "Creates an `AtomicInteger' with initial value `value'."
    (lisp (-> AtomicInteger) (value)
      (bt2:make-atomic-integer :value value)))

  (declare cas! (AtomicInteger * Word * Word -> Boolean))
  (define (cas! atomic old new)
    "If the current value of `atomic' is equal to `old', replace it with `new'.
Returns True if the replacement was successful, otherwise False."
    (lisp (-> Boolean) (atomic old new)
      (bt2:atomic-integer-compare-and-swap atomic old new)))

  (declare decf! (AtomicInteger * Word -> Word))
  (define (decf! atomic delta)
    "Decrements the value of `atomic' by `delta'."
    (lisp (-> Word) (atomic delta)
      (bt2:atomic-integer-decf atomic delta)))

  (declare incf! (AtomicInteger * Word -> Word))
  (define (incf! atomic delta)
    "Increments the value of `atomic' by `delta'."
    (lisp (-> Word) (atomic delta)
      (bt2:atomic-integer-incf atomic delta)))

  (declare read (AtomicInteger -> Word))
  (define (read atomic)
    "Returns the current value of `atomic'."
    (lisp (-> Word) (atomic)
      (bt2:atomic-integer-value atomic))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     Locks                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(coalton-toplevel
  (repr :native bt2:lock)
  (define-type Lock
    "Wrapper for a native non-recursive lock.")

  (declare new-lk (Void -> Lock))
  (define (new-lk)
    "Creates a non-recursive lock."
    (lisp (-> Lock) ()
      (bt2:make-lock)))

  (declare acquire (Lock -> Boolean))
  (define (acquire lock)
    "Acquire `lock' for the calling thread."
    (lisp (-> Boolean) (lock)
      (bt2:acquire-lock lock)))

  (declare acquire-no-wait (Lock -> Boolean))
  (define (acquire-no-wait lock)
    "Acquire `lock' for the calling thread.
Returns Boolean immediately, True if `lock' was acquired, False otherwise."
    (lisp (-> Boolean) (lock)
      (bt2:acquire-lock lock :wait nil)))

  (declare release (Lock -> (Result LispCondition Lock)))
  (define (release lock)
    "Release `lock'. It is an error to call this unless
the lock has previously been acquired (and not released) by the same
thread. If other threads are waiting for the lock, the
`acquire-lock' call in one of them will now be able to continue.

Returns the lock."
    (lisp (-> Result LispCondition Lock) (lock)
      (cl:handler-case (Ok (bt2:release-lock lock))
        (cl:error (c) (Err c)))))

  (declare with-lock-held (Lock * (Void -> :a) -> :a))
  (define (with-lock-held lock thunk)
    (acquire lock)
    (let ((result (thunk)))
      (release lock)
      result)))
