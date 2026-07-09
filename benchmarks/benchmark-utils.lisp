(cl:in-package :cl-user)
(defpackage :benchmark-utils
  (:use
   #:coalton
   #:coalton-prelude
   #:io/monad-io
   )
  (:local-nicknames
   (:bt #:io/utilities/bt-compat)
   (:b #:org.shirakumo.trivial-benchmark))
  (:export
   #:Timer
   #:current-timer
   #:start
   #:stop
   #:commit
   #:s-new
   #:s-signal
   #:s-await
   ))
(in-package :benchmark-utils)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (repr :native b:Timer)
  (define-type Timer)

  (declare current-timer (Void -> Timer))
  (define (current-timer)
    (lisp (-> Timer) ()
      b::*current-timer*))

  (declare start (Timer -> Unit))
  (define (start timer)
    (lisp (-> :a) (timer)
      (b:start timer))
    Unit)

  (declare stop (Timer -> Unit))
  (define (stop timer)
    (lisp (-> :a) (timer)
      (b::stop timer))
    Unit)

  (declare commit (Timer -> Unit))
  (define (commit timer)
    (lisp (-> :a) (timer)
      (b::commit timer))
    Unit)
  )

(coalton-toplevel
  (declare s-new (MonadIo :m => :m bt:Semaphore))
  (define s-new
    (wrap-io (bt:new-sm)))

  (define (s-signal s)
    (wrap-io (bt:signal s 1)))

  (define (s-await s)
    (wrap-io (bt:await-sm s)))
  )
