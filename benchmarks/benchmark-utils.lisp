(cl:in-package :cl-user)
(defpackage :benchmark-utils
  (:use
   #:coalton
   #:coalton-prelude
   )
  (:local-nicknames
   (:b #:org.shirakumo.trivial-benchmark))
  (:export
   #:Timer
   #:current-timer
   #:start
   #:stop
   #:commit
   ))
(in-package :benchmark-utils)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (repr :native b:Timer)
  (define-type Timer)

  (declare current-timer (Unit -> Timer))
  (define (current-timer)
    (lisp Timer ()
      b::*current-timer*))

  (declare start (Timer -> Unit))
  (define (start timer)
    (lisp :a (timer)
      (b:start timer))
    Unit)

  (declare stop (Timer -> Unit))
  (define (stop timer)
    (lisp :a (timer)
      (b::stop timer))
    Unit)

  (declare commit (Timer -> Unit))
  (define (commit timer)
    (lisp :a (timer)
      (b::commit timer))
    Unit)
  )
