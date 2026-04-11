(defpackage :io/tests/utils
  (:use
   #:coalton
   #:coalton-prelude
   #:io/monad-io
   )
  (:local-nicknames
   (:bt #:io/utilities/bt-compat)
   )
  (:export
   #:s-new
   #:s-signal
   #:s-await
   ))
(in-package :io/tests/utils)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (declare s-new (MonadIo :m => :m bt:Semaphore))
  (define s-new
    (wrap-io (bt:new-sm)))

  (define (s-signal s)
    (wrap-io (bt:signal s 1)))

  (define (s-await s)
    (wrap-io (bt:await-sm s)))
  )
