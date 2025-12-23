(cl:in-package :cl-user)
(defpackage :io/gen-impl/random
  (:use
   #:coalton
   #:coalton-prelude
   #:io/utils
   #:io/classes/monad-io
   #:io/classes/monad-io-random
   )
  (:export
   #:implement-monad-io-random
   ))
(in-package :io/gen-impl/random)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (declare make-random-state% (MonadIo :m => :m RandomState))
  (define make-random-state%
    (wrap-io (lisp :a ()
               (cl:make-random-state cl:t))))

  (declare copy-random-state% (MonadIo :m => RandomState -> :m RandomState))
  (define (copy-random-state% rs)
    (wrap-io (lisp :a (rs)
               (cl:make-random-state rs))))

  (declare get-current-random-state% (MonadIo :m => :m RandomState))
  (define get-current-random-state%
    (wrap-io (lisp :a ()
               cl:*random-state*)))

  (declare set-current-random-state% (MonadIo :m => RandomState -> :m Unit))
  (define (set-current-random-state% rs)
    (wrap-io
      (lisp :a (rs)
        (cl:setf cl:*random-state* rs))
      Unit))

  (declare random% ((RandomLimit :a) (MonadIo :m) => RandomState -> :a -> :m :a))
  (define (random% rs limit)
    (wrap-io (lisp :a (rs limit)
               (cl:random limit rs))))

  (declare random_% ((RandomLimit :a) (MonadIo :m) => :a -> :m :a))
  (define (random_% limit)
    (wrap-io (lisp :a (limit)
               (cl:random limit))))
  )

(defmacro implement-monad-io-random (monad)
  `(define-instance (MonadIoRandom ,monad)
     (define make-random-state make-random-state%)
     (define copy-random-state copy-random-state%)
     (define get-current-random-state get-current-random-state%)
     (define set-current-random-state set-current-random-state%)
     (define random random%)
     (define random_ random_%)))
