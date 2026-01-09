(cl:in-package :cl-user)
(defpackage :io/io-impl/vm-io-impl
  (:use :cl)
  (:export
   ))
(in-package :io/io-impl/vm-io-impl)

;; -------------------------------- ;;
;;        Bytecode Definition       ;;
;; -------------------------------- ;;

(deftype u32 () '(unsigned-byte 32))
(deftype u24 () '(unsigned-byte 24))
(deftype op8 () '(unsigned-byte 8))

(declaim (inline instruction% op% arg%))
(defun instruction% (op arg)
  (declare (type op8 op)
           (type u24 arg)
           (values u32))
  (logior (the u32 (ash (the u32 op) 24))
          (the u32 (logand (the u32 arg) #xFFFFFF))))

(defun op% (instruction)
  (declare (type u32 instruction)
           (values op8))
  (ldb (byte 8 24) instruction))

(defun arg% (instruction)
  (declare (type u32 instruction)
           (values u24))
  (logand instruction #xFFFFFF))

(defconstant +op-const+ 0)
(defconstant +op-thunk+ 1)
(defconstant +op-run+   2)
(defconstant +op-map+   3)
(defconstant +op-bind+  4)
(defconstant +op-halt+  5)

(defun instruction-to-string% (instruction)
  "Convert an instruction to a human readable format. Mostly used for debugging."
  (declare (type u32 instruction))
  (let* ((op-str (ecase (op% instruction)
                  (0 "op-const")
                  (1 "op-thunk")
                  (2 "op-run")
                  (3 "op-map")
                  (4 "op-bind")
                  (5 "op-halt")))
         (arg (arg% instruction))
         (arg-str (if (eql 0 arg)
                      ""
                      (format nil " (~a)" arg))))
    (concatenate 'string op-str arg-str)))

(defstruct io-bytecode
  (instructions (error "instructions required") :type (simple-array u32 (*)))
  (consts (error "consts required") :type simple-vector))

(declaim (inline io-bytecode-code io-bytecode-consts))

;; -------------------------------- ;;
;;           IO Operations          ;;
;; -------------------------------- ;;

(defparameter *code-pure*
  (let ((v (make-array 2 :element-type 'u32)))
    (setf (aref v 0) (instruction% +op-const+ 0))
    (setf (aref v 1) (instruction% +op-halt+ 0))
    v))

(defparameter *code-delay*
  (let ((v (make-array 2 :element-type 'u32)))
    (setf (aref v 0) (instruction% +op-thunk+ 0))
    (setf (aref v 1) (instruction% +op-halt+ 0))
    v))
(type-of *code-delay*)
(defparameter *code-map*
  (let ((v (make-array 3 :element-type 'u32)))
    (setf (aref v 0) (instruction% +op-run+ 0))
    (setf (aref v 1) (instruction% +op-map+ 1))
    (setf (aref v 2) (instruction% +op-halt+ 0))
    v))

(defparameter *code-bind*
  (let ((v (make-array 2 :element-type 'u32)))
    (setf (aref v 0) (instruction% +op-run+ 0))
    (setf (aref v 1) (instruction% +op-bind+ 1))
    v))

(declaim (inline io-pure io-delay io-map io-bind))

(defun io-pure (x)
  (declare (values io-bytecode))
  (make-io-bytecode *code-pure* (vector x)))

(defun io-delay (thunk)
  (declare (type function thunk)
           (values io-bytecode))
  (make-io-bytecode *code-delay* (vector thunk)))

(defun io-map (f io)
  (declare (type function f)
           (type io-bytecode io)
           (values io-bytecode))
  (make-io-bytecode *code-map* (vector io f)))

(defun io-bind (io k)
  (declare (type io-bytecode io)
           (type function k)
           (values io-bytecode))
  (make-io-bytecode *code-bind* (vector io k)))
        
;; -------------------------------- ;;
;;             IO Runner            ;;
;; -------------------------------- ;;

(declaim (inline ensure-stack-capacity%))

(defun ensure-stack-capacity% (instrss constss pcs top)
  "Grows stacks if needed. The runner keeps and reuses them across nested RUNs within
a single top-level run."
  (declare (type simple-vector instrss constss)
           (type (simple-array fixnum (*)) pcs)
           (type fixnum top)
           (values simple-vector simple-vector (simple-array fixnum (*))))
  (let ((old-length (length instrss)))
    (if (< top old-length)
        (values instrss constss pcs)
        (let* ((new-length (the fixnum (max 16 (* 2 old-length))))
               (new-instrss (make-array new-length))
               (new-constss (make-array new-length))
               (new-pcs    (make-array new-length :element-type 'fixnum)))
          (replace new-instrss instrss)
          (replace new-constss constss)
          (replace new-pcs pcs)
          (values new-instrss new-constss new-pcs)))))

(defun io-run! (io)
  (declare (type io-bytecode io))
  (let* ((instrs (io-bytecode-instructions io))
         (consts (io-bytecode-consts io))
         (stack-ptr fixnum 0)
         (register nil)
         ;; Continuation stacks for nested RUNs
         (instrs-stack (make-array 32))
         (consts-stack (make-array 32))
         (stack-ptr-stack (make-array 32 :element-type 'fixnum))
         (top 0))
    (declare (type fixnum stack-ptr top))
    (labels ((push-cont (next-stack-ptr next-instrs next-consts)
               (multiple-value-setq (instrs-stack consts-stack stack-ptr-stack)
                 (ensure-stack-capacity% instrs-stack consts-stack stack-ptr-stack
                                         top))
               
    

         
