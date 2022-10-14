(cl:in-package :cl-user)
(defpackage :coalton-io.io-monad
  (:use #:coalton #:coalton-prelude)
  (:export
    #:IO
    #:run!))
(in-package :coalton-io.io-monad)

(coalton-toplevel
  ;;
  ;; IO Monad
  ;;
  (repr :transparent)
  (define-type (IO :a)
    (IO (Unit -> :a)))

  (declare run! (IO :a -> :a))
  (define (run! io)
    (let (IO funit->a) = io)
    (funit->a))

  (define-instance (Functor IO)
    (define (map fb->c io-b)
      (IO
        (fn ()
          (let (IO funit->b) = io-b)
          (fb->c (funit->b))))))

  (define-instance (Applicative IO)
    (define (pure x)
      (IO
        (fn () x)))
    (define (liftA2 fa->b->c io-a io-b)
      (IO
        (fn ()
          (let (IO f->a) = io-a)
          (let (IO f->b) = io-b)
          (fa->b->c (f->a) (f->b))))))

  (define-instance (Monad IO)
    (define (>>= io-a fa->io-b)
      (IO
        (fn ()
          (let (IO f->a) = io-a)
          (run! (fa->io-b (f->a))))))))
