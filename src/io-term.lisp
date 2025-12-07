(cl:in-package :cl-user)
(defpackage :io/term
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/functions
   #:io/monad-io)
  (:import-from #:coalton-library/monad/statet
   #:StateT)
  (:import-from #:coalton-library/monad/environment
   #:EnvT)
  (:import-from #:coalton-library/experimental/do-control-loops-adv
   #:LoopT)
  (:local-nicknames
   (:io #:io/simple-io))
  (:export
   #:MonadIoTerm
   #:derive-monad-io-term

   #:write
   #:write-line
   #:read-line
   #:implement-monad-io-term
   ))
(in-package :io/term)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (define-class (MonadIo :m => MonadIoTerm :m)
    (write
     "Write a string to standard output."
     (Into :a String => :a -> :m Unit))
    (write-line
     "Write a string to standard output followed by a newline."
     (Into :a String => :a -> :m Unit))
    (read-line
     "Read a line from standard input."
     (:m String)))

  (declare write% ((Into :a String) (MonadIo :m) => :a -> :m Unit))
  (define (write% obj)
    (let str = (the String (into obj)))
    (wrap-io
      (lisp :a (str)
        (cl:format cl:t "~a" str))
      Unit))

  (declare write-line% ((Into :a String) (MonadIo :m) => :a -> :m Unit))
  (define (write-line% obj)
    (let str = (the String (into obj)))
    (wrap-io
      (lisp :a (str)
        (cl:format cl:t "~a~%" str))
      Unit))

  (declare read-line% (MonadIo :m => :m String))
  (define read-line%
    (wrap-io (lisp :a ()
               (cl:read-line)))))

(cl:defmacro implement-monad-io-term (monad)
  `(define-instance (MonadIoTerm ,monad)
     (define write write%)
     (define write-line write-line%)
     (define read-line read-line%)))

(cl:defmacro derive-monad-io-term (monad-param monadT-form)
  "Automatically derive an instance of MonadIoTerm for a monad transformer.

Example:
  (derive-monad-io-term :m (st:StateT :s :m))"
  `(define-instance (MonadIoTerm ,monad-param => MonadIoTerm ,monadT-form)
     (define write (compose lift write))
     (define write-line (compose lift write-line))
     (define read-line (lift read-line))))

;;
;; Std. Library Transformer Instances
;;

(coalton-toplevel
  (derive-monad-io-term :m (StateT :s :m))
  (derive-monad-io-term :m (EnvT :e :m))
  (derive-monad-io-term :m (LoopT :m)))

;;
;; Simple IO Implementation
;;

(coalton-toplevel

  (implement-monad-io-term io:IO))
