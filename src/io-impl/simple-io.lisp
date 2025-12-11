(cl:in-package :cl-user)
(defpackage :io/io-impl/simple-io
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/functions
   #:coalton-library/types
   #:coalton-library/experimental/do-control-core
   #:io/utils
   #:io/thread-exceptions
   #:io/classes/monad-exception
   #:io/classes/monad-io
   #:io/io-impl/runtime
   #:io/thread-impl/runtime
   )
  (:import-from #:coalton-library/experimental/loops
   #:dolist)
  (:import-from #:coalton-library/experimental/do-control-loops-adv
   #:LoopT)
  (:import-from #:coalton-library/types
   #:RuntimeRepr)
  (:local-nicknames
   (:r #:coalton-library/result)
   (:st #:coalton-library/monad/statet)
   (:env #:coalton-library/monad/environment)
   (:it #:coalton-library/iterator)
   (:c #:coalton-library/cell))
  (:export
   #:IO
   #:run-io!

   #:raise-io
   #:raise-io_
   #:raise-dynamic-io
   #:reraise-io
   #:handle-io
   #:handle-all-io
   #:try-dynamic-io

   #:with-run-in-io_
   #:foreach-io_
   #:do-foreach-io_
   #:map-into-io_
   #:do-map-into-io_
   ))
(in-package :io/io-impl/simple-io)

(named-readtables:in-readtable coalton:coalton)

;;;
;;; Debugging Helpers
;;;

(cl:defmacro compile-debug-sleep (sleep-ms)
  "If environmental variable SIMPLE_IO_DEBUG_SLEEP = 'y', sleep for SLEEP-MS.
See >>="
  (cl:if (cl:equalp (uiop:getenv "SIMPLE_IO_DEBUG_SLEEP") "y")
         `(lisp Void ()
            (cl:sleep ,(cl:/ sleep-ms 1000.0)))
         `Unit))

;;;
;;; Simple IO
;;;

(coalton-toplevel

  ;;
  ;; IO Monad
  ;;
  (repr :transparent)
  (define-type (IO :a)
    (IO% (Unit -> Result Dynamic :a)))

  (inline)
  (declare wrap-io%_ ((Unit -> :a) -> IO :a))
  (define (wrap-io%_ f)
    (IO%
     (fn ()
       (inline
        (Ok (f))))))

  (inline)
  (declare run-io-handled!% (IO :a -> Result Dynamic :a))
  (define (run-io-handled!% (IO% f->a?))
    "Run an IO, but instead of raising, pass on any exceptions. Used internally to
implement MonadException and handle asynchronous exception signals."
    (match (catch-thunk f->a?)
      ((Err unh-err)
       (let (UnhandledError e) = unh-err)
       (Err
        (cond
          ((lisp Boolean (e) (cl:typep e 'ThreadingException))
           (force-dynamic (the (Proxy ThreadingException) Proxy) e))
          (True
           (to-dynamic unh-err)))))
      ((Ok a)
       a)))

  (inline)
  (declare run-io! (IO :a -> :a))
  (define (run-io! io-op)
    "Top-level run-io! that raises any unhandled exceptions."
    (match (run-io-handled!% io-op)
      ((Ok a)
       a)
      ((Err dyn-e)
       (match (cast dyn-e)
         ((Some (UnhandledError e))
          (let _ = (the MockException e))
          (throw e))
         ((None)
          (throw-dynamic dyn-e))))))

  (define-instance (Functor IO)
    (define (map fb->c io-op)
      (match io-op
        ((IO% funit->b)
         (IO%
          (fn ()
            (map fb->c (funit->b))))))))

  (define-instance (Applicative IO)
    (inline)
    (define (pure x) (IO% (fn () (Ok x))))
    (inline)
    (define (liftA2 fa->b->c (IO% f->a?) (IO% f->b?))
      (IO%
       (fn ()
        (match (f->a?)
          ((Err e1)
           (Err e1))
          ((Ok a)
           (match (f->b?)
             ((Err e2)
              (Err e2))
             ((Ok b)
              (Ok (fa->b->c a b))))))))))

  (define-instance (Monad IO)
    (inline)
    (define (>>= (IO% f->a?) fa->io-b)
      (IO%
       (fn ()
        ;; It's VERY difficult to determinalistically test the thread runtime's
        ;; behavior for managing async exceptions outside of the wrap-io boundary.
        ;; To support those test cases, IO provides a conditional compilation
        ;; mechanism to insert a 5 MS sleep during the >>= operation, which is
        ;; guaranteed to be outside of wrap-io.
        ;;
        ;; To trigger this, set SIMPLE_IO_DEBUG_SLEEP = 'y' and compile simple-io.lisp
        (match (f->a?)
          ((Err e)
           (Err e))
          ((Ok a)
           (compile-debug-sleep 5)
           (run-io-handled!% (fa->io-b a))))))))

  (inline)
  (declare raise-io ((RuntimeRepr :e) (Signalable :e) => :e -> IO :a))
  (define (raise-io e)
    (IO% (fn () (Err (to-dynamic e)))))

  (inline)
  (declare raise-dynamic-io (Dynamic -> IO :a))
  (define (raise-dynamic-io dyn)
    (IO% (fn () (Err dyn))))

  (inline)
  (declare raise-io_ ((RuntimeRepr :e) (Signalable :e) => :e -> IO Unit))
  (define raise-io_ raise-io)

  (inline)
  (declare reraise-io (IO :a -> (Unit -> IO :b) -> IO :a))
  (define (reraise-io op catch-op)
    (IO%
     (fn ()
       (let result = (run-io-handled!% op))
       (do-match result
         ((Ok _)
          result)
         ((Err _)
          (let result2 = (run-io-handled!% (catch-op)))
          (match result2
            ((Ok _)
             result)
            ((Err e)
             (Err e))))))))

  (inline)
  (declare handle-io (RuntimeRepr :e => IO :a -> (:e -> IO :a) -> IO :a))
  (define (handle-io io-op handle-op)
    (IO%
     (fn ()
      (let ((result (run-io-handled!% io-op)))
        (match result
          ((Ok a)
           (Ok a))
          ((Err e?)
           (match (cast e?)
             ((Some e)
              (run-io-handled!% (handle-op e)))
             ((None)
              result))))))))

  ;; BUG: This handles thread interrupt exceptions, which it *definitely* shouldn't
  (inline)
  (declare handle-all-io (IO :a -> (Unit -> IO :a) -> IO :a))
  (define (handle-all-io io-op handle-op)
    "Run IO-OP, and run HANDLE-OP to handle exceptions of any type thrown by IO-OP."
    (IO%
     (fn ()
      (let ((result (run-io-handled!% io-op)))
        (match result
          ((Ok a)
           (Ok a))
          ((Err _)
           (run-io-handled!% (handle-op))))))))

  (inline)
  (declare try-dynamic-io (IO :a -> IO (Result Dynamic :a)))
  (define (try-dynamic-io io-op)
    (IO%
     (fn ()
       (Ok
        (run-io-handled!% io-op)))))

  ;;
  ;; MonadException Instance
  ;;

  (define-instance (MonadException IO)
    (define raise raise-io)
    (define raise-dynamic raise-dynamic-io)
    (define reraise reraise-io)
    (define handle handle-io)
    (define handle-all handle-all-io)
    (define try-dynamic try-dynamic-io))

  (define-instance (BaseIo IO)
    (define run! run-io!))

  (define-instance (UnliftIo IO IO)
    (inline)
    (define (with-run-in-io inner)
      (inner id)))

  (declare with-run-in-io_ (UnliftIo :m IO => (((:m :a -> IO :a) -> IO :b) -> :m :b)))
  (define with-run-in-io_
    "`with-run-in-io`, but pegged to the simple-io implementation. Useful when you
need to unlift, run, then immediately re-run a function. See, e.g., io-file:with-open-file%."
    with-run-in-io)

  ;;
  ;; MonadIo Instances
  ;;

  (define-instance (MonadIo IO)
    (inline)
    (define wrap-io_ wrap-io%_)))

;;;
;;; Extra Functions
;;;

(coalton-toplevel

  ;; TODO: This might not be more efficient, if the inliner is able to eliminate
  ;; the (run) call in `map-into-io` for the (UnliftIo IO IO) case. Test with
  ;; a benchmark. But even if they're not more efficient, they still solve some
  ;; inference issues.
  (declare map-into-io_ ((LiftIo IO :m) (it:IntoIterator :i :a)
                         => :i -> (:a -> IO :b) -> :m (List :b)))
  (define (map-into-io_ itr a->mb)
    "Efficiently perform a monadic operation for each element of an iterator
and return the results. More efficient than map-into-io, if you can run your
effect in a BaseIo."
    (let io-prx = Proxy)
    (lift-io
     (as-proxy-of
      (wrap-io
        (let results = (c:new (make-list)))
        (for a in (it:into-iter itr)
          (c:push! results (run! (as-proxy-of
                                  (a->mb a)
                                  io-prx))))
        (reverse (c:read results)))
      (proxy-swap-inner io-prx))))

  (declare foreach-io_ ((LiftIo IO :m) (it:IntoIterator :i :a)
                        => :i -> (:a -> IO :b) -> :m Unit))
  (define (foreach-io_ itr a->mb)
    "Efficiently perform a monadic operation for each element of an iterator.
More efficient than foreach-io, if you can run your effect in a BaseIo."
    (let io-prx = Proxy)
    (lift-io
     (as-proxy-of
      (wrap-io
        (for a in (it:into-iter itr)
          (run! (as-proxy-of
                 (a->mb a)
                 io-prx)))
        Unit)
      (proxy-swap-inner io-prx))))

  )

(cl:defmacro do-map-into-io_ ((var lst) cl:&body body)
  `(map-into-io_ ,lst
     (fn (,var)
       (do
        ,@body))))

(cl:defmacro do-foreach-io_ ((var into-itr) cl:&body body)
  `(foreach-io_ ,into-itr
     (fn (,var)
       (do
        ,@body))))

;;;
;;; IO Capability Implementations
;;;

(coalton-toplevel
  (io/gen-impl/thread:implement-monad-io-thread IO IoRuntime IoThread)
  (io/gen-impl/atomic:implement-monad-at-var IO)
  (io/gen-impl/mut:implement-monad-io-var IO)
  (io/gen-impl/mvar:implement-monad-io-mvar IO)
  (io/gen-impl/file:implement-monad-io-file IO)
  (io/gen-impl/random:implement-monad-io-random IO)
  (io/gen-impl/term:implement-monad-io-term IO)
  (io/gen-impl/stm:implement-monad-io-stm IO))
