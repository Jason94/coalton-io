(cl:in-package :cl-user)
(defpackage :io/stubs/term
  (:use
   #:coalton
   #:coalton-prelude
   #:io/monad-io
   #:io/term)
  (:local-nicknames
   (:l #:coalton-library/list)
   (:f #:coalton-library/monad/free)
   (:ft #:coalton-library/monad/freet)
   (:id #:coalton-library/monad/identity)
   (:io-rand #:io/random)
   (:io-mvar #:io/mvar)
   (:io-atom #:io/atomic)
   (:io-file #:io/file)
   (:io-mut #:io/mut)
   (:io-thd #:io/thread)
   (:io-unq #:io/unique)
   )
  (:export
   #:TermStubM
   #:TermStub
   #:run-term-stub
   #:run-term-stubM
   ))
(in-package :io/stubs/term)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (define-type (TermStubF :next)
    (Write% String :next)
    (WriteLine% String :next)
    (ReadLine% (String -> :next)))

  (define-instance (Functor TermStubF)
    (define (map f stb)
      (match stb
        ((Write% str next)
         (Write% str (f next)))
        ((WriteLine% str next)
         (WriteLine% str (f next)))
        ((ReadLine% cont)
         (ReadLine% (map f cont))))))

  (define-type-alias TermStubM (ft:FreeT TermStubF))
  (define-type-alias TermStub (TermStubM id:Identity))

  (define-instance (MonadIo :m => MonadIoTerm (TermStubM :m))
    (inline)
    (define (write into-str)
      (f:liftF (Write% (into into-str) Unit)))
    (inline)
    (define (write-line into-str)
      (f:liftF (WriteLine% (into into-str) Unit)))
    (inline)
    (define read-line
      (f:liftF (ReadLine% id))))

  ;; These implementations allow MonadIoTerm to sit on top of the normal IO
  ;; monad. MonadIoTerm will "intercept" any MonadIoTerm behavior, and pass
  ;; any other effects down the transformer stack to IO.
  ;;
  ;; I *think* the one problem with this will be any calls that require IO's,
  ;; like foreach-io.
  (derive-monad-io :m (TermStubM :m))
  (io-rand:derive-monad-io-random :m (TermStubM :m))
  (io-mvar:derive-monad-io-mvar :m (TermStubM :m))
  (io-atom:derive-monad-at-var :m (TermStubM :m))
  (io-file:derive-monad-io-file :m (TermStubM :m))
  (io-mut:derive-monad-var :m (termstubm :m))
  (io-thd:derive-monad-io-thread :m (TermStubM :m))
  (io-unq:derive-monad-io-unique :m (TermStubM :m))

  (declare run-term-stubM (Monad :m => TermStubM :m :a -> List String -> :m (Tuple (List String) :a)))
  (define (run-term-stubM opm read-line-inputs)
    (rec % ((written-lines (make-list))
            (rem-line-inputs read-line-inputs)
            (opm opm))
      (do
       (step <- (ft:unwrap-freeT opm))
       (match step
         ((ft:Val a)
          (let norm-written-lines =
            (l:reverse
             (match written-lines
               ((Cons "" rest)
                rest)
               (_ written-lines))))
          (pure (Tuple norm-written-lines a)))
         ((ft:FReeF op)
          (match op
            ((Write% str next)
             (let next-written-lines =
               (match written-lines
                 ((Nil) (make-list str))
                 ((Cons s rest)
                  (Cons (<> s str) rest))))
             (% next-written-lines rem-line-inputs next))
            ((WriteLine% str next)
             (let next-written-lines =
               (match written-lines
                 ((Nil) (make-list "" str))
                 ((Cons s rest)
                  (Cons "" (Cons (<> s str) rest)))))
             (% next-written-lines rem-line-inputs next))
            ((ReadLine% cont)
             (match rem-line-inputs
               ((Nil)
                (match read-line-inputs
                  ((Nil) (error "Must supply read-line data to terminal stub."))
                  ((Cons s rest)
                   (% written-lines rest (cont s)))))
               ((Cons s rest)
                (% written-lines rest (cont s)))))))))))

  (declare run-term-stub (TermStub :a -> List String -> Tuple (List String) :a))
  (define (run-term-stub stub-op read-line-inputs)
    (id:run-identity (run-term-stubM stub-op read-line-inputs)))
  )
