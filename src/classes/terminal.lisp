(cl:in-package :cl-user)
(defpackage :io/classes/terminal
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
   #:Terminal
   #:derive-terminal
   #:write
   #:write-line
   #:read-line))
(in-package :io/classes/terminal)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (define-class (MonadIo :m => Terminal :m)
    (write
     "Write a string to standard output."
     (Into :a String => :a -> :m Unit))
    (write-line
     "Write a string to standard output followed by a newline."
     (Into :a String => :a -> :m Unit))
    (read-line
     "Read a line from standard input."
     (:m String))))

(defmacro derive-terminal (monad-param monadT-form)
  "Automatically derive an instance of Terminal for a monad transformer.

Example:
  (derive-terminal :m (st:StateT :s :m))"
  `(define-instance (Terminal ,monad-param => Terminal ,monadT-form)
     (define write (compose lift write))
     (define write-line (compose lift write-line))
     (define read-line (lift read-line))))

;;
;; Std. Library Transformer Instances
;;

(coalton-toplevel
  (derive-terminal :m (StateT :s :m))
  (derive-terminal :m (EnvT :e :m))
  (derive-terminal :m (loopt :m)))
