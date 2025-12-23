(cl:in-package :cl-user)
(defpackage :io/gen-impl/stm
  (:use
   #:coalton
   #:coalton-prelude
   #:io/classes/monad-io-stm
   #:io/thread-impl/stm-impl)
  (:export
   #:implement-monad-io-stm
   )
  )
(in-package :io/gen-impl/stm)

(named-readtables:in-readtable coalton:coalton)

(defmacro implement-monad-io-stm (monad)
  `(define-instance (MonadIoSTM ,monad)
     (define new-tvar new-tvar%)
     (define read-tvar read-tvar%)
     (define write-tvar write-tvar%)
     (define modify-tvar modify-tvar%)
     (define retry retry%)
     (define or-else or-else%)
     (define run-tx run-tx%)))
