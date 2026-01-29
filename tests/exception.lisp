(defpackage coalton-io/tests/exception
  (:use #:coalton #:coalton-prelude #:coalton-testing
        #:coalton-library/types
        #:coalton-library/monad/statet
        #:coalton-library/experimental/do-control-core
        #:io/tests/utils
        #:io/utils
        #:io/simple-io
        #:io/exceptions
        #:io/monad-io
        #:io/threads)
  (:local-nicknames
   (:m #:io/mutable-var))
  )
(in-package :coalton-io/tests/exception)

(named-readtables:in-readtable coalton:coalton)

(fiasco:define-test-package #:coalton-io/tests/exception-fiasco)

(coalton-fiasco-init #:coalton-io/tests/exception-fiasco)

;;;
;;; Exception Utility Tests
;;;

(coalton-toplevel
  (derive Eq)
  (repr :lisp)
  (define-type TestException
    (TE String))

  (define-instance (Signalable TestException)
    (define (error (TE s))
      (error s)))

  (derive Eq)
  (repr :lisp)
  (define-type TestException2
    (TE2 String))

  (define-instance (Signalable TestException2)
    (define (error (TE2 s))
      (error s))))

(define-test test-try-ok ()
  (let result =
    (the (Result String Integer)
         (run-io!
          (try (wrap-io 10)))))
  (is (== (Ok 10) result)))

(define-test test-try-fail ()
  (let result =
    (run-io!
     (try (raise-io_ (TE "Error!")))))
  (is (== (Err (TE "Error!")) result)))

(define-test test-wrap-error-coalton-error ()
  (let result =
    (run-io!
     (try-all (wrap-error
               (error "Unhandled Coalton error!")
               1))))
  (is (== None result)))

(define-test test-wrap-error-lisp-error ()
  (let result =
    (run-io!
     (try-all (wrap-error
               (lisp Void ()
                 (cl:error "Unhandled lisp error!"))
               1))))
  (is (== None result)))

(define-test test-wrap-error-success ()
  (let result =
    (run-io!
     (try-all (wrap-error
               1))))
  (is (== (Some 1) result)))

(coalton-toplevel
  (declare pop# (List Integer -> IO Integer))
  (define (pop# ints)
    (match ints
      ((Cons x _)
       (pure x))
      ((Nil)
       (raise (TE "No ints left"))))))

(define-test test-handle-all ()
  (let result =
    (run-io!
     (do
      (ints <- (wrap-io Nil))
      (b <- (handle-all (do
                         (x <- (pop# ints))
                         (pure (+ 1 x)))
                        (fn ()
                          (wrap-io -10))))
      (pure b))))
  (is (== -10 result)))

;;;
;;; Multithreaded Tests
;;;

(define-test test-handle-all-doesnt-catch-thread-stops ()
  (let result =
    (run-io!
     (do
      (s-start <- s-new)
      (s-stopped <- s-new)
      (m-result <- (m:new-var None))
      (thread <-
        (do-fork-thread_
          (handle-all
           (do
            (s-signal s-start)
            (s-await s-stopped)
            (m:write m-result (Some True)))
           (const (m:write m-result (Some False))))))
      (s-await s-start)
      (stop thread)
      (s-signal s-stopped)
      (sleep 2)
      (m:read m-result))))
  (is (== None result)))

(define-test test-try-dynamic-doesnt-catch-thread-stops ()
  (let result =
    (run-io!
     (do
      (s-start <- s-new)
      (s-stopped <- s-new)
      (m-result <- (m:new-var None))
      (thread <-
        (do-fork-thread_
          (try-dynamic
           (do
            (s-signal s-start)
            (s-await s-stopped)))
          (m:write m-result (Some True))))
      (s-await s-start)
      (stop thread)
      (s-signal s-stopped)
      (sleep 2)
      (m:read m-result))))
  (is (== None result)))

;;;
;;; StateT instance tests
;;;

(coalton-toplevel

  (declare pop-return (StateT (List Integer) IO Integer))
  (define pop-return
     (do-matchM get
       ((Nil)
        (raise (TE "No ints left")))
       ((Cons x rem)
        (put rem)
        (pure x))))

  (declare add-three-ints (StateT (List Integer) IO Integer))
  (define add-three-ints
    (do
     (a <- pop-return)
     (b <- pop-return)
     (c <- pop-return)
     (pure (+ a (+ b c)))))

  (declare run-test (List Integer -> StateT (List Integer) IO :a -> (Tuple (List Integer) :a)))
  (define (run-test ints op)
    (run-io! (run-stateT op ints))))

(define-test test-statet-no-exceptions ()
  (let result = (run-test (make-list 1 2 3 4) add-three-ints))
  (is (== (Tuple (make-list 4) 6) result)))

(define-test test-statet-try ()
  (let result =
    (run-test (make-list 1) (try add-three-ints)))
  (is (== (Tuple (make-list 1) (Err (TE "No ints left")))
          result)))

(define-test test-statet-handle-all ()
  (let result =
    (run-test (make-list 1)
              (do-handle-all add-three-ints
                (modify (Cons 2))
                (pure 10))
              ))
  (is (== (Tuple (make-list 2 1) 10)
          result)))

(define-test test-statet-handle-type ()
  (let result =
    (run-test (make-list 1)
              (do-handle add-three-ints (e)
                (let _ = (the TestException e))
                (modify (Cons 2))
                (pure 10))
              ))
  (is (== (Tuple (make-list 2 1) 10)
          result)))

(define-test test-statet-handle-wrong-type ()
  (let result =
    (run-test (make-list 1)
              (try
               (do-handle add-three-ints (e)
                 (let _ = (the TestException2 e))
                 (modify (Cons 2))
                 (pure 10))
               )))
  (is (== (Tuple (make-list 1) (Err (TE "No ints left")))
          result)))
