(cl:in-package :cl-user)
(defpackage :io/io-impl/io-runtime/delay
  (:use
   #:coalton
   #:coalton-prelude
   #:io/utils
   #:io/threads-exceptions
   #:io/classes/monad-io
   #:io/classes/exceptions
   #:io/classes/thread
   #:io/io-impl/io-runtime/runtime
   )
  (:local-nicknames
   (:opt #:coalton-library/optional)
   (:c #:coalton-library/cell)
   )
  (:export
   ;; Library Public
   #:IoDelay

   ;; Library Private
   #:IoDelay%
   ))
(in-package :io/io-impl/io-runtime/delay)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (repr :transparent)
  (define-type (IoDelay :a)
    (IoDelay% IoThread))

  (inline)
  (declare delay-thread% (IoDelay :a -> IoThread))
  (define (delay-thread% (IoDelay% thread))
    thread)
  
  (inline)
  (declare join-val!% (forall (:a) IoDelay :a -> Result Dynamic :a))
  (define (join-val!% delay)
    (let native-thread = (opt:from-some "Error: IoThread leaked without setting native thread handle"
                                        (c:read (.handle (delay-thread% delay)))))
    (match (lisp (-> ThreadResult :a) (native-thread)
             (bt2:join-thread native-thread))
      ((ThreadComplete val)
       (Ok val))
      ((ThreadStoppedEarly)
       (Err (to-dynamic UnexpectedStop)))
      ((ThreadExcepted dyn)
       (Err dyn)))) 
  
  (define-instance (Concurrent (IoDelay :a) :a)
    (inline)
    (define (stop (IoDelay% th))
      (stop th))
    (inline)
    (define (await delay)
      (raise-result-dynamic (wrap-io (join-val!% delay))))
    (inline)
    (define (mask (IoDelay% th))
      (mask th))
    (inline)
    (define (unmask (IoDelay% th))
      (unmask th))
    (inline)
    (define (unmask-finally (IoDelay% th) thunk)
      (unmask-finally th thunk)))
  )
