(cl:in-package :cl-user)
(defpackage :io/gen-impl/term
  (:use
   #:coalton
   #:coalton-prelude
   #:io/classes/monad-io
   #:io/classes/term)
  (:local-nicknames
   (:f #:coalton/format))
  (:export
   #:implement-terminal
   #:write-format
   ))
(in-package :io/gen-impl/term)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (declare write% ((Into :a String) (MonadIo :m) => :a -> :m Unit))
  (define (write% obj)
    (let str = (the String (into obj)))
    (wrap-io
      (lisp (-> :a) (str)
        (cl:format cl:t "~a" str))
      Unit))

  (declare write-line% ((Into :a String) (MonadIo :m) => :a -> :m Unit))
  (define (write-line% obj)
    (let str = (the String (into obj)))
    (wrap-io
      (lisp (-> :a) (str)
        (cl:format cl:t "~a~%" str))
      Unit))

  (declare read-line% (MonadIo :m => :m String))
  (define read-line%
    (wrap-io (lisp (-> :a) ()
               (cl:read-line)))))

(defmacro write-format (control-string cl:&rest rest)
  "Write formatted string to the terminal."
  `(wrap-io
    (f:format f:Out ,control-string ,@rest)
    Unit))

(defmacro implement-terminal (monad)
  `(define-instance (Terminal ,monad)
     (define write write%)
     (define write-line write-line%)
     (define read-line read-line%)))
