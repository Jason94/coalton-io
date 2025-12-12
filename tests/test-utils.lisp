(defpackage :io/tests/utils
  (:use
   #:coalton
   #:coalton-prelude
   #:io/monad-io
   )
  (:local-nicknames
   (:s #:coalton-threads/semaphore)
   )
  (:export
   #:s-new
   #:s-signal
   #:s-await
   )
  )
(in-package :io/tests/utils)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (declare s-new (MonadIo :m => :m s:Semaphore))
  (define s-new
    (wrap-io (s:new)))

  (define (s-signal s)
    (wrap-io (s:signal s 1)))

  (define (s-await s)
    (wrap-io (s:await s)))
  )
