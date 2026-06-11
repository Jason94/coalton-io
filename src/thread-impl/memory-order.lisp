(cl:in-package #:cl-user)
(defpackage #:io/threads-impl/memory-order
  (:use
   #:coalton
   #:coalton-prelude)
  (:local-nicknames
   (:bt2 #:bordeaux-threads-2))
  (:export
   ;; Coalton wrappers
   #:read-barrier
   #:write-barrier
   #:memory-barrier

   ;; Lisp backend functions
   #:read-barrier%
   #:write-barrier%
   #:memory-barrier%))

(in-package #:io/threads-impl/memory-order)

(cl:declaim (cl:inline read-barrier%
                       write-barrier%
                       memory-barrier%))

;; Memory barrier package that is portable across all targeted Lisp implementations.
;; For more details, see:
;; https://www.sbcl.org/manual/#Barriers
;;
;; SBCL exposes explicit read/write/full barriers.
;;
;; CCL does not expose a barrier API, so the CCL backend constructs a conservative
;; full barrier by forcing a successful atomic CAS on a private dummy atomic
;; integer.

#+sbcl
(cl:progn
  (cl:defun read-barrier% ()
    (sb-thread:barrier (:read))
    cl:nil)

  (cl:defun write-barrier% ()
    (sb-thread:barrier (:write))
    cl:nil)

  (cl:defun memory-barrier% ()
    (sb-thread:barrier (:memory))
    cl:nil))

#+(and ccl x86-target)
(cl:progn
  (cl:defvar *memory-barrier-cell* (bt2:make-atomic-integer :value 0)
    "Private dummy word used to synthesize memory barriers on CCL.")

  (cl:defun %ccl-atomic-rmw-memory-barrier% ()
    "Force a successful atomic RMW operation on CCL.

Bordeaux Threads v2 implements ATOMIC-INTEGER-COMPARE-AND-SWAP on CCL using
CCL::CONDITIONAL-STORE, so a successful no-op CAS on a private word establishes
a full memory barrier (on x86 platforms)."
    (cl:unless (bt2:atomic-integer-compare-and-swap
                *memory-barrier-cell* 0 0)
      (cl:error "Internal memory barrier cell invariant violated."))
    cl:nil)

  (cl:defun read-barrier% ()
    (%ccl-atomic-rmw-memory-barrier%))

  (cl:defun write-barrier% ()
    (%ccl-atomic-rmw-memory-barrier%))

  (cl:defun memory-barrier% ()
    (%ccl-atomic-rmw-memory-barrier%)))

#-(or sbcl (and ccl x86-target))
(cl:progn
  (cl:defun %unsupported-memory-order-backend ()
    (cl:error "coalton-io memory barriers are implemented only for SBCL and CCL x86/x86-64."))

  (cl:defun read-barrier% ()
    (%unsupported-memory-order-backend))

  (cl:defun write-barrier% ()
    (%unsupported-memory-order-backend))

  (cl:defun memory-barrier% ()
    (%unsupported-memory-order-backend)))

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (inline)
  (declare read-barrier (Void -> Unit))
  (define (read-barrier)
    "Prevent reads before/after this point from being reordered across it."
    (lisp (-> Unit) ()
      (read-barrier%)
      Unit))

  (inline)
  (declare write-barrier (Void -> Unit))
  (define (write-barrier)
    "Prevent writes before/after this point from being reordered across it."
    (lisp (-> Unit) ()
      (write-barrier%)
      Unit))

  (inline)
  (declare memory-barrier (Void -> Unit))
  (define (memory-barrier)
    "Prevent memory accesses before/after this point from being reordered across it."
    (lisp (-> Unit) ()
      (memory-barrier%)
      Unit))
  )
