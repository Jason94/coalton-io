(cl:in-package :cl-user)
(defpackage :io/gen-impl/conc/future
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton/monad/classes
   #:coalton/experimental/do-control-core
   #:io/utils
   #:io/classes/exceptions
   #:io/classes/monad-io
   #:io/classes/thread
   #:io/gen-impl/conc/mvar
   )
  (:export
   #:Future
   #:Promise
   
   #:new-future
   #:fork-future
   #:satisfy-future
   #:await-future
   #:fail-future
   #:try-read-future

   #:do-fork-future
   ))
(in-package :io/gen-impl/conc/future)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (repr :transparent)
  (define-type (Future :a)
    "A concurrent value that starts empty, and can be satisfied or failed by the
associated `Promise`."
    (Future% (MVar (Result Dynamic :a))))

  (inline)
  (declare f-mvar% (Future :a -> MVar (Result Dynamic :a)))
  (define (f-mvar% (Future% mvar))
    mvar)

  (repr :transparent)
  (define-type (Promise :a)
    "Used to satisfy or fail the associated `Future`."
    (Promise% (MVar (Result Dynamic :a))))

  (inline)
  (declare p-mvar% (Promise :a -> MVar (Result Dynamic :a)))
  (define (p-mvar% (Promise% mvar))
    mvar)

  (inline)
  (declare new-future (Threads :rt :t :m => :m (Tuple (Future :a) (Promise :a))))
  (define new-future
    "Create a new `Future` and an associated `Promise`."
    (wrap-io
     (let mvar = (new-empty-mvar%))
     (Tuple (Future% mvar) (Promise% mvar))))

  (inline)
  (declare await-future ((Threads :rt :t :m) (Exceptions :m) => Future :a -> :m :a))
  (define (await-future f)
    "Await for `f` to be satisfied. Raises an exception if `f` has failed.

Concurrent:
  - If `f` is not satisfied, blocks until `f` is satisfied."
    (raise-result-dynamic
     (read-mvar (f-mvar% f))))

  (inline)
  (declare satisfy-future (Threads :rt :t :m => Promise :a * :a -> :m Boolean))
  (define (satisfy-future p val)
    "Satisfy the future corresponding to `p` with `val`."
    (try-put-mvar (p-mvar% p) (Ok val)))

  (inline)
  (declare fail-future ((Threads :rt :t :m) (Exception :e) => Promise :a * :e -> :m Boolean))
  (define (fail-future p exc)
    "Fail the future corresponding to `p` with `exc`."
    (try-put-mvar (p-mvar% p) (Err (to-dynamic exc))))

  (inline)
  (declare try-read-future ((Threads :rt :t :m) (Exceptions :m) => Future :a -> :m (Optional :a)))
  (define (try-read-future f)
    "Attempt to immediately read from `f`, returning `None` if it is empty. Raises an
exception if `f` has failed."
    (matchM (try-read-mvar (f-mvar% f))
      ((None)
       (pure None))
      ((Some res)
       (match res
         ((Err dyn-e)
          (raise-dynamic dyn-e))
         ((Ok a)
          (pure (Some a)))))))
    
  (inline)
  (declare fork-future ((UnliftIo :r :i) (LiftTo :r :m) (Threads :rt :t :r) (Threads :rt :t :m)
                        => (Promise :a -> :r :b)
                        &key
                        (:unhandled UnhandledExceptionStrategy)
                        (:scope (ForkScope :t))
                        -> :m (Tuple (Future :a) :t)))
  (define (fork-future thunk &key (unhandled LogAndSwallow) (scope Structured))
    "Create a future-promise pair. Start a new thread with the `Promise`. Return the
`Future` and a handle to the running thread.

If the thread raises an unhandled exception, the default behavior is to log it to
*ERROR-OUTPUT* and swallow it until/if the thread is joined. The default fork scope is
structured."
    (do
     ((Tuple f p) <- new-future)
     (t <- (fork-thread (thunk p) :unhandled unhandled :scope scope))
     (pure (Tuple f t))))
  )

(defmacro do-fork-future ((promise-sym) cl:&body forms)
  (cl:multiple-value-bind (unhandled scope body)
      (parse-fork-keywords% forms)
    `(fork-future
      (fn (,promise-sym)
        (do
         ,@body))
      :scope ,scope :unhandled ,unhandled)))
