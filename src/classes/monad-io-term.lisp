(cl:in-package :cl-user)
(defpackage :io/classes/monad-io-term
  (:use
   #:coalton
   #:coalton-prelude
   #:io/classes/monad-io)
  (:export
   #:MonadIoTerm
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
