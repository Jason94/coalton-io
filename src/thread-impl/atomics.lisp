(defpackage #:io/threads-impl/atomics
  (:use
   #:coalton
   #:coalton-prelude
   #:io/utils)
  (:local-nicknames
   (:itr #:coalton-library/iterator)
   (:c #:coalton-library/cell)
   (#:at #:atomics))
  (:export
   #:Atomic
   #:new
   #:read
   #:compare-and-swap
   #:atomic-pop
   #:atomic-push
   #:atomic-swap
   #:atomic-update
   #:atomic-update-swap
   #:atomic-write

   #:AtomicInteger
   #:new-at-int
   #:read-at-int
   #:int-cas
   #:atomic-inc
   #:atomic-inc1
   #:atomic-dec
   #:atomic-dec1
   #:atomic-int-write

   #:AtomicStack
   #:new-atomic-stack
   #:at-st-push-front!
   #:at-st-pop-front!
   #:at-st-peek
   ))

(in-package #:io/threads-impl/atomics)

(named-readtables:in-readtable coalton:coalton)

;;
;; This file wraps the Atomics library to Coalton.
;; https://github.com/Shinmera/atomics
;;

(cl:defun nil-to-opt (x)
  (cl:if x
         (coalton (Some (lisp :a () x)))
         (coalton None)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;          Generic Atomics          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:declaim (cl:inline make-atomic-internal))

(cl:defstruct atomic-internal
  (inner (cl:error "") :type cl:t))

(cl:defmethod cl:print-object ((self atomic-internal) stream)
  (cl:declare (cl:stream stream))
  (cl:format stream "#.(ATOMIC ~A)" (atomic-internal-inner self))
  self)

#+sbcl
(cl:declaim (sb-ext:freeze-type atomic-internal))

(coalton-toplevel

  (repr :native atomic-internal)
  (define-type (Atomic :a)
    "Thread-safe mutable cell")

  (inline)
  (declare new (:a -> Atomic :a))
  (define (new data)
    "Create a new atomic cell containing `data`."
    (lisp (Atomic :a) (data)
      (make-atomic-internal :inner data)))

  (inline)
  (declare read (Atomic :a -> :a))
  (define (read atm)
    "Read the value of an atomic cell `atm`."
    (lisp :a (atm)
      (atomic-internal-inner atm)))

  (inline)
  (declare compare-and-swap (Atomic :a -> :a -> :a -> Boolean))
  (define (compare-and-swap atm old new)
    "Attempt to swap the contents of `atm` from OLD to NEW. Returns
TRUE if the swap succeeded, FALSE otherwise. Does not repeat."
    (lisp Boolean (atm old new)
      (at:cas (atomic-internal-inner atm) old new)))

  (inline)
  (declare atomic-pop (Atomic (List :a) -> Optional :a))
  (define (atomic-pop atm)
    "Atomically pop from the list inside `atm` until it succeedes,
and return the popped value. Returns None if the list was empty."
    (lisp (Optional :a) (atm)
      (nil-to-opt
       (at:atomic-pop (atomic-internal-inner atm)))))

  (inline)
  (declare atomic-push (Atomic (List :a) -> :a -> List :a))
  (define (atomic-push atm elt)
    "Atomically push ELT onto the list inside `atm` until it succeedes.
Returns the new list, with the element included."
    (lisp (List :a) (atm elt)
      (at:atomic-push elt (atomic-internal-inner atm))))

  (inline)
  (declare atomic-swap (Atomic :a -> :a -> :a))
  (define (atomic-swap atm new-val)
    "Atomically swap the value in ATM with NEW-VAL. Returns the old value."
    (lisp :a (atm new-val)
      (cl:labels ((lp ()
                    (cl:let ((old-val (atomic-internal-inner atm)))
                      (cl:if (at:cas (atomic-internal-inner atm)
                                     old-val
                                     new-val)
                             old-val
                             (lp)))))
        (lp))))

  (declare atomic-update (Atomic :a -> (:a -> :a) -> :a))
  (define (atomic-update atm f)
    "Atomically update the value in `atm` by applying F until it succeedes.
Returns the new value stored in `atm` after applying F."
    (lisp :a (atm f)
      (cl:let ((update-fn (cl:lambda (x)
                            (call-coalton-function f x))))
        (at:atomic-update (atomic-internal-inner atm) update-fn))))

  (declare atomic-update-swap (Atomic :a -> (:a -> :a) -> :a))
  (define (atomic-update-swap atm f)
    "Atomically update the value in `atm` by applying F until it succeedes.
Returns the old value stored in `atm` after applying F."
    (lisp :a (atm f)
      (cl:let* ((old-val cl:nil)
                (update-fn (cl:lambda (x)
                             (cl:setf old-val x)
                             (call-coalton-function f x))))
        (at:atomic-update (atomic-internal-inner atm) update-fn)
        old-val)))

  (inline)
  (declare atomic-write (Atomic :a -> :a -> Unit))
  (define (atomic-write atm val)
    "Atomically set the value in `atm` to `val`."
    (atomic-update atm (const val))
    Unit)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;          Atomic Integers          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(coalton-toplevel

  (repr :native bt2:atomic-integer)
  (define-type AtomicInteger)

  (inline)
  (declare new-at-int (Word -> AtomicInteger))
  (define (new-at-int n)
    (lisp :a (n)
      (bt2:make-atomic-integer :value n)))

  (inline)
  (declare read-at-int (AtomicInteger -> Word))
  (define (read-at-int atm)
    (lisp Word (atm)
      (bt2:atomic-integer-value atm)))

  (inline)
  (declare int-cas (AtomicInteger -> Word -> Word -> Boolean))
  (define (int-cas atm old new)
    "Attempt to swap the contents of `atm` from OLD to NEW. Returns
TRUE if the swap succeeded, FALSE otherwise. Does not repeat."
    (lisp Boolean (atm old new)
      (bt2:atomic-integer-compare-and-swap atm old new)))

  (inline)
  (declare atomic-inc (AtomicInteger -> Word -> Word))
  (define (atomic-inc atm n)
    "Atomically increment `atm` by `n`. Returns the new value."
    (lisp :a (atm n)
      (bt2:atomic-integer-incf atm n)))

  (inline)
  (declare atomic-inc1 (AtomicInteger -> Word))
  (define (atomic-inc1 atm)
    "Atomically increment `atm` by 1. Returns the new value."
    (lisp :a (atm)
      (bt2:atomic-integer-incf atm)))

  (inline)
  (declare atomic-dec (AtomicInteger -> Word -> Word))
  (define (atomic-dec atm n)
    "Atomically decrement `atm` by `n`. Returns the new value."
    (lisp :a (atm n)
      (bt2:atomic-integer-decf atm n)))

  (inline)
  (declare atomic-dec1 (AtomicInteger -> Word))
  (define (atomic-dec1 atm)
    "Atomically decrement `atm` by 1. Returns the new value."
    (lisp :a (atm)
      (bt2:atomic-integer-decf atm)))

  (inline)
  (declare atomic-int-write (AtomicInteger -> Word -> Word))
  (define (atomic-int-write atm new)
    "Atomically set ATM to NEW. Returns the old value."
    (let old = (read-at-int atm))
    (lisp Word (atm old new)
      (cl:if (bt2:atomic-integer-compare-and-swap atm old new)
             old
             (call-coalton-function atomic-int-write atm new))))

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;           Atomic Queue            ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(coalton-toplevel

  (define-struct (AtomicStackEntry :a)
    (data :a)
    (next? (Optional (AtomicStackEntry :a))))

  (define-struct (AtomicStack :a)
    "The AtomicStack is a LinkedList queue of atomic references
to a data cell and the next item in the queue. The queue can be
iterated, front-popped, and front-pushed without contention."
    (head? (Atomic (Optional (AtomicStackEntry :a)))))

  (inline)
  (declare new-atomic-stack (Unit -> AtomicStack :a))
  (define (new-atomic-stack)
    (AtomicStack (new None)))

  (inline)
  (declare at-st-push-front! (:a -> AtomicStack :a -> AtomicStack :a))
  (define (at-st-push-front! val stack)
    "Atomically push val to the front of stack. Returns the stack
for convenience."
    (atomic-update (.head? stack)
                   (fn (head?)
                     (Some (AtomicStackEntry val head?))))
    stack)

  (inline)
  (declare at-st-pop-front! (AtomicStack :a -> AtomicStack :a))
  (define (at-st-pop-front! stack)
    "Atomically pop the front of the stack. Returns the stack for
convenience."
    (atomic-update (.head? stack)
                   (fn (head?)
                     (match head?
                       ((None) None)
                       ((Some head) (.next? head)))))
    stack)

  (inline)
  (declare at-st-length% (Optional (AtomicStackEntry :a) -> UFix))
  (define (at-st-length% head?)
    (rec % ((current head?)
            (len 0))
      (match current
        ((None) len)
        ((Some entry)
         (% (.next? (the (AtomicStackEntry :a) entry)) (1+ len))))))

  (define-instance (itr:IntoIterator (AtomicStack :a) :a)
    (define (itr:into-iter stack)
      (let head? = (read (.head? stack)))
      (let current-entry? = (c:new head?))
      (itr:with-size
        (fn ()
          (map (fn (current-entry)
                 (c:write! current-entry? (.next? current-entry))
                 (.data current-entry))
               (c:read current-entry?)))
        (at-st-length% head?))))

  (inline)
  (declare at-st-peek (AtomicStack :a -> Optional :a))
  (define (at-st-peek stack)
    "Atomically peek at the first value in the stack."
    (map .data (read (.head? stack))))
  )
