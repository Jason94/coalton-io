(cl:in-package :cl-user)
(defpackage :io/gen-impl/thread
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/types
   #:coalton-library/monad/classes
   #:coalton-library/types
   #:io/utils
   #:io/classes/monad-io
   #:io/classes/monad-io-thread
   #:io/classes/monad-io-term
   #:io/thread-impl/runtime
   )
  (:export
   #:implement-monad-io-thread
   ))
(in-package :io/gen-impl/thread)

(named-readtables:in-readtable coalton:coalton)

(cl:defmacro implement-monad-io-thread (monad runtime thread)
  `(define-instance (MonadIoThread ,runtime ,thread ,monad)
     ))
