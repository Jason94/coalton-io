(cl:in-package :cl-user)
(defpackage :io/io-impl/vm-reg-io-impl
  (:use :cl)
  (:export
   ))
(in-package :io/io-impl/vm-reg-io-impl)

(deftype register () #+32-bit '(unsigned-byte 32) #+64-bit '(unsigned-byte 64))

;; ------------------------------ ;;
;;      Register Program AST      ;;
;; ------------------------------ ;;

(defstruct op-end
  (return-register (error "return-register required") :type register))

(defstruct op-load-literal
  (value (error "value required"))
  (write-register (error "register required") :type register))

(defstruct op-jump
  (code-ptr (error "code-ptr required") :type register))

(defstruct op-null-op
  (null-op (error "null-op required"))
  (write-register (error "write-register required") :type register))

(defstruct op-unary-op
  (unary-op (error "unary-op required"))
  (read-register (error "read-register required") :type register)
  (write-register (error "write-register required") :type register))

;; ------------------------------ ;;
;;         AST Compilation        ;;
;; ------------------------------ ;;

(defstruct register-state
  (num-registers (error "num-registers required") :type register)
  (code (error "code required") :type cons)
  (hoisted (error "hoisted required") :type cons))

