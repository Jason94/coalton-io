(cl:in-package :cl-user)
(defpackage :io/gen-impl/thread
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/types
   #:io/classes/monad-io
   #:io/classes/monad-io-thread
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
    (wrap-io (write-line-sync% msg) Unit))

  ;; (declare sleep_ ((Runtime :rt :t) (MonadIoThread :m :rt) => UFix -> :m Unit))
  (define (sleep_ msec)
    (let m-prx = Proxy)
    (as-proxy-of
     (wrap-io (sleep! (runtime-for m-prx) msec))
     m-prx))
  )

(cl:defmacro implement-monad-io-thread (monad runtime)
  `(define-instance (MonadIoThread ,monad ,runtime)
     (inline)
     (define current-thread
       (let m-prx = Proxy)
       (as-proxy-of
        (wrap-io (current-thread! (runtime-for m-prx)))
        m-prx))
     (define sleep sleep%)
     (define mask-thread mask%)
     (define mask-current-thread mask-current-thread%)
     (define unmask-thread unmask%)
     (define unmask-current-thread unmask-current-thread%)
     (define stop-thread stop%)))
