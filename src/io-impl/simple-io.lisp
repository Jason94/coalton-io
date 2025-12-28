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
   #:dolist
   #:dotimes)
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
   #:run-io-handled!

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
   #:times-io_
   #:do-times-io_
   ))
(in-package :io/io-impl/simple-io)

(cl:declaim (cl:optimize (cl:speed 3) (cl:debug 0) (cl:safety 0)))

(named-readtables:in-readtable coalton:coalton)

;;;
;;; Debugging Helpers
;;;

(defmacro compile-debug-sleep (sleep-ms)
  "If environmental variable SIMPLE_IO_DEBUG_SLEEP = 'y', sleep for SLEEP-MS.
See >>="
  (cl:if (cl:equalp (uiop:getenv "SIMPLE_IO_DEBUG_SLEEP") "y")
         `(lisp Void ()
            (cl:format cl:t "~%Entering compile debug sleep~%")
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
    (IO% (Unit -> :a)))

  (inline)
  (declare wrap-io%_ ((Unit -> :a) -> IO :a))
  (define (wrap-io%_ f)
    (IO% f))

  ;; NOTE: Catching prevents SBCL from optimizing tail calls because it needs to protect
  ;; the stack. Therefore run-io-handled!% can *only* be called in exception combinators,
  ;; where we'll eat the cost (in theory looping those could blow the stack) and at
  ;; the top-level run boundary. Usually use run-io-unhandled! or run-io!
  (inline)
  (declare run-io-handled!% (IO :a -> Result IoError :a))
  (define (run-io-handled!% (IO% f->a?))
    "Run an IO, but instead of raising, pass on any exceptions. Used internally to
implement MonadException and handle asynchronous exception signals."
    (catch-thunk f->a?))

  (inline)
  (declare run-io-handled! (IO :a -> Result Dynamic :a))
  (define (run-io-handled! io)
    (r:map-err (fn (io-err)
                 (match io-err
                   ((UnhandledError _ _)
                    (to-dynamic io-err))
                   ((HandledError dyn-err _)
                    dyn-err)))
               (run-io-handled!% io)))

  (inline)
  (declare run-io-unhandled! (IO :a -> :a))
  (define (run-io-unhandled! (IO% f->a?))
    (f->a?))

  (inline)
  (declare run-io! (IO :a -> :a))
  (define (run-io! io-op)
    "Top-level run-io! that raises any unhandled exceptions."
    (match (run-io-handled!% io-op)
      ((Ok a)
       a)
      ((Err io-err)
       (match io-err
         ((UnhandledError _ throw-thunk)
          (throw-thunk)
          (error "Malformed UnhandledError throw-thunk"))
         ((HandledError _ err-thunk)
          (err-thunk)
          (error "Malformed HandledError err-thunk"))))))

  (define-instance (Functor IO)
    (inline)
    (define (map fa->b io-a)
      (match io-a
        ((IO% funit->a)
         (IO%
          (fn ()
            (fa->b (funit->a))))))))

  (define-instance (Applicative IO)
    (inline)
    (define (pure x) (IO% (fn () x)))
    (inline)
    (define (liftA2 fa->b->c (IO% f->a) (IO% f->b))
      (IO%
       (fn ()
         (fa->b->c
          (f->a)
          (f->b))))))

  (define-instance (Monad IO)
    (inline)
    (define (>>= (IO% f->a) fa->io-b)
      (IO%
       (fn ()
         ;; It's VERY difficult to determinalistically test the thread runtime's
         ;; behavior for managing async exceptions outside of the wrap-io boundary.
         ;; To support those test cases, IO provides a conditional compilation
         ;; mechanism to insert a 5 MS sleep during the >>= operation, which is
         ;; guaranteed to be outside of wrap-io.
         ;;
         ;; To trigger this, set SIMPLE_IO_DEBUG_SLEEP = 'y' and compile simple-io.lisp
         (let a = (f->a))
         (compile-debug-sleep 5)
         (run-io-unhandled! (fa->io-b a))))))

  (inline)
  (declare raise-io ((RuntimeRepr :e) (Signalable :e) => :e -> IO :a))
  (define (raise-io e)
    (IO% (fn () (throw (HandledError (to-dynamic e)
                                     (fn () (error e)))))))

  (inline)
  (declare raise-dynamic-io (Dynamic -> IO :a))
  (define (raise-dynamic-io dyn)
    (IO% (fn () (throw (HandledError dyn
                                     (fn () (throw-dynamic dyn)))))))

  (inline)
  (declare raise-io_ ((RuntimeRepr :e) (Signalable :e) => :e -> IO Unit))
  (define raise-io_ raise-io)

  (inline)
  (declare reraise-io (IO :a -> (Unit -> IO :b) -> IO :a))
  (define (reraise-io op catch-op)
    (IO%
     (fn ()
       (let result = (run-io-handled!% op))
       (match result
         ((Ok a)
          a)
         ((Err e)
          (run-io-unhandled! (catch-op))
          (throw e))))))

  (inline)
  (declare handle-io (RuntimeRepr :e => IO :a -> (:e -> IO :a) -> IO :a))
  (define (handle-io io-op handle-op)
    (IO%
     (fn ()
      (let result = (run-io-handled!% io-op))
      (match result
        ((Ok a)
         a)
        ((Err io-err)
         (match io-err
           ((UnhandledError _ _)
            (throw io-err))
           ((HandledError e? _)
            (let casted = (cast e?))
            (match casted
              ((Some e)
               (run-io-unhandled! (handle-op e)))
              ((None)
               (throw io-err))))))))))

  (inline)
  (declare handle-all-io (IO :a -> (Unit -> IO :a) -> IO :a))
  (define (handle-all-io io-op handle-op)
    "Run IO-OP, and run HANDLE-OP to handle exceptions of any type thrown by IO-OP."
    (IO%
     (fn ()
      (let result = (run-io-handled!% io-op))
      (match result
        ((Ok a)
         a)
        ((Err io-err)
         ;; Don't allow handle-all to accidentally mask the thread!
         (if (is-threading-exception io-err)
             (throw io-err)
             (run-io-unhandled! (handle-op))))))))

  (inline)
  (declare try-dynamic-io (IO :a -> IO (Result Dynamic :a)))
  (define (try-dynamic-io io-op)
    (IO%
     (fn ()
       (let result = (run-io-handled!% io-op))
       (match result
         ((Ok a)
          (Ok a))
         ((Err io-err)
          (if (is-threading-exception io-err)
              (throw io-err)
              (match io-err
                ((UnhandledError _ _)
                 (throw io-err))
                ((HandledError e _)
                 (Err e)))))))))

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
    (inline)
    (define run! run-io!)
    (inline)
    (define run-handled! run-io-handled!))

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
          (c:push! results (run-io-unhandled! (as-proxy-of
                                               (a->mb a)
                                               io-prx))))
        (reverse (c:read results)))
      (proxy-swap-inner io-prx))))

  (declare foreach-io_ ((LiftIo IO :m) (it:IntoIterator :i :a)
                        => :i -> (c:Cell :a -> IO :b) -> :m Unit))
  (define (foreach-io_ coll a->mb)
    "Efficiently perform a monadic operation for each element of an iterator.
More efficient than foreach-io, if your effect can run in IO. The next element of the
iterator is passed into the operation via a cell."
    (lift-io
     (wrap-io%_
      (fn ()
        (let itr = (it:into-iter coll))
        (let fst = (it:next! itr))
        (match fst
          ((None)
           Unit)
          ((Some initial-val)
           (let c = (c:new initial-val))
           (let monad-op = (a->mb c))
           (run-io-unhandled! monad-op)
           (for a in itr
             (c:write! c a)
             (run-io-unhandled! monad-op))))))))

  (declare times-io_ (LiftIo IO :m => UFix -> IO :a -> :m Unit))
  (define (times-io_ n io-op)
    "Efficiently perform an IO operation N times."
    (lift-io
     (wrap-io%_
      (fn ()
        (dotimes (_ n)
          (run-io-unhandled! io-op))))))
  )

(defmacro do-map-into-io_ ((var lst) cl:&body body)
  `(map-into-io_ ,lst
     (fn (,var)
       (do
        ,@body))))

(defmacro do-foreach-io_ ((var-sym into-itr) cl:&body body)
  "Efficiently perform a monadic operation for each element of an iterator.
More efficient than foreach-io, if your effect can run in IO. VAR-SYM is bound
to the value of the element in the iterator."
  (cl:let ((cell-sym (cl:gensym "iteration-val")))
    `(foreach-io_ ,into-itr
      (fn (,cell-sym)
        (do
         ,@(cl-maptree (cl:lambda (sym)
                         (cl:if (cl:eq sym var-sym)
                            `(c:read ,cell-sym)
                            sym))
                       body))))))

(defmacro do-times-io_ (n cl:&body body)
  "Efficiently perform an IO operation N times."
  `(times-io_ ,n
    (do
     ,@body)))

;;;
;;; IO Capability Implementations
;;;

(coalton-toplevel
  (io/gen-impl/thread:implement-monad-io-thread IO IoRuntime IoThread)
  (io/gen-impl/mut:implement-monad-io-var IO)
  (io/gen-impl/file:implement-monad-io-file IO)
  (io/gen-impl/random:implement-monad-io-random IO)
  (io/gen-impl/term:implement-monad-io-term IO)
  )
