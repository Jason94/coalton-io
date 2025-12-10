(cl:in-package :cl-user)
(defpackage :io/classes/monad-io-term
  (:use
   #:coalton
   #:coalton-prelude
   #:io/classes/monad-io)
  (:import-from #:coalton-library/monad/statet
   #:StateT)
  (:import-from #:coalton-library/monad/environment
   #:EnvT)
  (:import-from #:coalton-library/experimental/do-control-loops-adv
   #:LoopT)
  (:export
   #:MonadIoTerm
   #:derive-monad-io-term
   #:write
   #:write-line
   #:read-line))
(in-package :io/classes/monad-io-term)

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
     (:m String))))

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
  (derive-monad-io-term :m (loopt :m)))
