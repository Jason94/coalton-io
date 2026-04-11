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

   ;; Semaphores
   #:Semaphore
   #:new-sm
   #:signal
   #:await

   ;; Condition Variables
   #:ConditionVariable
   #:new-cv
   #:await
   #:notify
   #:broadcast
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                   Semaphore                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(coalton-toplevel
  (repr :native bt2:semaphore)
  (define-type Semaphore
    "Wrapper for a native semaphore.")

  (declare new-sm (Void -> Semaphore))
  (define (new-sm)
    "Creates a semaphore with initial count 0."
    (lisp (-> Semaphore) ()
      (bt2:make-semaphore)))

  (declare signal (Semaphore * UFix -> Unit))
  (define (signal sem count)
    "Increment `sem' by `count'.
If there are threads awaiting this semaphore, then `count' of them are woken up."
    (lisp (-> Unit) (sem count)
      (bt2:signal-semaphore sem :count count)
      Unit))

  (declare await (Semaphore -> Unit))
  (define (await sem)
    "Decrement the count of `sem' by 1 if the count is larger than zero.
If the count is zero, blocks until `sem' can be decremented."
    (lisp (-> Unit) (sem)
      (bt2:wait-on-semaphore sem)
      Unit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;              Condition Variables               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(coalton-toplevel
  (repr :native bt2:condition-variable)
  (define-type ConditionVariable
    "Wrapper for a native condition variable.")

  (declare new-cv (Void -> ConditionVariable))
  (define (new-cv)
    "Creates a condition variable."
    (lisp (-> ConditionVariable) ()
      (bt2:make-condition-variable)))

  (declare await (ConditionVariable * lock:Lock -> Unit))
  (define (await cv lock)
    "Atomically release `lock' and enqueue the calling thread waiting for `cv'.
The thread will resume when another thread has notified it using `notify-cv';
it may also resume if interrupted by some external event or in other
implementation-dependent circumstances: the caller must always test on waking
that there is threading to be done, instead of assuming that it can go ahead."
    (lisp (-> Unit) (cv lock)
      (bt2:condition-wait cv lock)
      Unit))

  (declare notify (ConditionVariable -> Unit))
  (define (notify cv)
    "Notify one of the threads waiting for `cv'."
    (lisp (-> Unit) (cv)
      (bt2:condition-notify cv)
      Unit))

  (declare broadcast (ConditionVariable -> Unit))
  (define (broadcast cv)
    "Notify all of the threads waiting for `cv'."
    (lisp (-> Unit) (cv)
      (bt2:condition-broadcast cv)
      Unit)))
