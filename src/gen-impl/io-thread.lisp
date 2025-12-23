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
   #:write-line-sync
   ))
(in-package :io/gen-impl/thread)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (declare write-line-sync ((Into :s String) (MonadIoTerm :m) => :s -> :m Unit))
  (define (write-line-sync msg)
    "Perform a synchrozied write-line to the terminal. Not performant - mainly useful
for debugging."
    (wrap-io (write-line-sync% msg) Unit))
  )

(defmacro implement-monad-io-thread (monad runtime thread)
  `(define-instance (MonadIoThread ,runtime ,thread ,monad)
     ))
