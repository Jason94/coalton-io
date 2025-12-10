(cl:in-package :cl-user)
(defpackage :io/gen-impl/thread
  (:use
   #:coalton
   #:coalton-prelude
   #:io/classes/monad-io
   #:io/classes/monad-io-term
   #:io/thread-impl/runtime
   )
  (:export
   #:write-line-sync
   #:implement-monad-io-thread
   ))
(in-package :io/gen-impl/thread)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (declare write-line-sync ((Into :s String) (MonadIoTerm :m) => :s -> :m Unit))
  (define (write-line-sync msg)
    "Perform a synchrozied write-line to the terminal. Not performant - mainly useful
for debugging."
    (wrap-io (write-line-sync% msg) Unit)))

(cl:defmacro implement-monad-io-thread (monad)
  `(define-instance (MonadIoThread ,monad IoThread)
     (define current-thread current-thread%)
     (define fork fork%)
     (define sleep sleep%)
     (define mask mask%)
     (define mask-current mask-current-thread%)
     (define unmask unmask%)
     (define unmask-finally unmask-finally%)
     (define unmask-current unmask-current-thread%)
     (define unmask-current-finally unmask-current-thread-finally%)
     (define stop stop%)))
