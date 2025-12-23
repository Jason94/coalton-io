(cl:in-package :cl-user)
(defpackage :io/classes/monad-io-thread
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/types
   #:coalton-library/monad/classes
   #:coalton-library/experimental/do-control-core
   #:io/utils
   #:io/thread-exceptions
   #:io/classes/monad-exception
   #:io/classes/monad-io
   #:io/classes/monad-io-term
   )
   ;; #:io/thread-impl/runtime)
  (:import-from #:coalton-library/experimental/do-control-loops-adv
   #:LoopT)
  (:local-nicknames
   (:st #:coalton-library/monad/statet)
   (:env #:coalton-library/monad/environment)
   )
  (:export
   ;; Library Public
   #:Runtime
   #:current-thread!
   #:sleep!
   #:fork!
   #:fork-throw!
   #:join!
   #:stop!
   #:mask!
   #:unmask!
   #:unmask-finally!
   #:mask-current!
   #:unmask-current!

   #:Concurrent
   #:stop
   #:await
   #:mask
   #:unmask
   #:unmask-finally

   #:MonadIoThread
   #:derive-monad-io-thread
   #:current-thread
   #:fork-thread
   #:fork-thread-throw
   #:join-thread
   #:sleep
   #:mask-thread
   #:mask-current-thread
   #:unmask-thread
   #:unmask-thread-finally
   #:unmask-current-thread
   #:unmask-current-thread-finally
   #:stop-thread
   #:do-fork-thread
   #:do-fork-thread-throw

   #:runtime-for
   #:as-runtime-prx
   #:get-runtime-for

   ;; Library Private
   #:inject-runtime
   #:wrap-io-with-runtime
   #:prxs-same-runtime
   #:concurrent-value-prx
   #:value-concurrent-prx
   ))
(in-package :io/classes/monad-io-thread)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (define-class (Runtime :r :t (:r -> :t))
    "This class doesn't represent data, but the type tells a Concurrent and
a MonadIoThread how to hook into the native threading implementations that
a runtime provides.  A runtime has a 'base' concurrent, which is the underlying
thread/fiber/etc. that the runtime produces to run concurrently. All other
Concurrents are built by composing on the base concurrent somehow.

Runtime is a low-level type that operates inside the normal MonadIo layer.
It should not be used by normal application code. Its two main purposes are:
(1) to make MonadIoThread generic over the type of thread it forks, and
(2) to build low-level, efficient concurrency tools that are generic
over the underlying thread type."
    (current-thread!
     "Get a handle for the current thread."
     (Proxy :r -> :t))
    (sleep!
     "Sleep the current thread for MSECS milliseconds."
     (Proxy :r -> UFix -> Unit))
    (fork!
     "Spawn a new thread, which starts running immediately.
Returns the handle to the thread."
     (Proxy :r -> (Unit -> Result Dynamic :a) -> :t))
    (fork-throw!
     "Spawn a new thread, which starts running immediately. Returns
the handle to the thread. If the thread raises an unhandled exception,
throws immediately. The underlying system determines the result of the
throw, but it could include terminating the whole program."
     (Proxy :r -> (Unit -> Result Dynamic :a) -> :t))
    (join!
     "Block the current thread until the target thread is completed.
Does not a retrieve value. Raises an exception if the target thread
raised an unhandled exception, wrapping the target thread's raised
exception. JOIN! is the lowest level operation to block on another
thread's termination, and most code should use AWAIT instead."
     (Proxy :r -> :t -> Result Dynamic Unit))
    (stop!
     "Stop a :t. If the thread has already stopped, does nothing.
If the :t is masked, this will pend a stop on the :t. When/if
the :t becomes completely unmaksed, it will stop iself. Regardless
of whether the target :t is masked, STOP does not block or wait for
the target thread to complete."
     (Proxy :r -> :t -> Unit))
    (mask!
     "Mask the thread so it can't be stopped."
     (Proxy :r -> :t -> Unit))
    (unmask!
     "Unmask the thread so it can be stopped. Unmask respects
nested masks - if the thread has been masked N times, it can only be
stopped after being unmasked N times. When the thread unmasks, if
there are any pending stops, it will immediately be stopped."
     (Proxy :r -> :t -> Unit))
    (unmask-finally!
     "Unmask the thread, run the provided action, and then honor any pending stop for that
thread after the action finishes.

Warning: There is a very small chance that the UnmaskFinallyMode passed to the callback could be
inconsistent with whether the Concurrent is ultimately stopped. Regardless of the input, the
callback should leave any resources in a valid state. An example of a valid callback: closing a log
file if the thread is stopped, or closing the log file with a final message if the thread is
continuing."
      (Proxy :r -> :t -> (UnmaskFinallyMode -> :a) -> Unit)))

  (inline)
  (declare mask-current! (Runtime :rt :t => Proxy :rt -> Unit))
  (define (mask-current! rt-prx)
    "Mask the current thread."
    (mask! rt-prx (current-thread! rt-prx)))

  (inline)
  (declare unmask-current! (Runtime :rt :t => Proxy :rt -> Unit))
  (define (unmask-current! rt-prx)
    "Unmask the current thread."
    (unmask! rt-prx (current-thread! rt-prx)))

  (define-class (Concurrent :c :a (:c -> :a))
    "A Concurrent has thread-like semantics. It can be stopped, masked, unmasked, and await-ed.
Concurrents don't have a uniform fork function, becasue they require different initialization
input."
    (stop
     "Stop a Concurrent. If the Concurrent has already stopped, does nothing. If the Concurrent is
masked, this will pend a stop on the Concurrent. When/if the Concurrent becomes completely unmaksed,
it will stop iself. Regardless of whether the target Concurrent is masked, STOP does not block or
wait for the target to complete."
     ((MonadException :m) (MonadIoThread :rt :t :m) => :c -> :m Unit))
    (await
     "Block the current thread until the target Concurrent is completed, and retrieve its value.
Re-raises if the target Concurrent raised an unhandled exception"
     ((MonadException :m) (MonadIoThread :rt :t :m) => :c -> :m :a))
    (mask
     "Mask the Concurrent so it can't be stopped."
     ((MonadException :m) (MonadIoThread :rt :t :m) => :c -> :m Unit))
    (unmask
     "Unmask the Concurrent so it can be stopped. Unmask respects nested masks - if the
Concurrent has been masked N times, it can only be stopped after being unmasked N times. When the
Concurrent unmasks, if there are any pending stops, it will immediately stop itself."
     ((MonadException :m) (MonadIoThread :rt :t :m) => :c -> :m Unit))
    (unmask-finally
     "Unmask the thread, run the provided action, and then honor any pending stop for that
thread after the action finishes.

Warning: There is a very small chance that the UnmaskFinallyMode passed to the callback could be
inconsistent with whether the Concurrent is ultimately stopped. Regardless of the input, the
callback should leave any resources in a valid state. An example of a valid callback: closing a log
file if the thread is stopped, or closing the log file with a final message if the thread is
continuing."
     ((UnliftIo :r :io) (LiftTo :r :m) (MonadIoThread :rt :t :r) (MonadException :m)
      (MonadIoThread :rt :t :m)
      => :c -> (UnmaskFinallyMode -> :r :b) -> :m Unit)))

  (inline)
  (declare concurrent-value-prx (Concurrent :c :a => :c -> Proxy :a))
  (define (concurrent-value-prx _)
    Proxy)

  (inline)
  (declare value-concurrent-prx (Concurrent :c :a => Proxy :a -> Proxy :c))
  (define (value-concurrent-prx _)
    Proxy)

  (define-class ((MonadIo :m) (Runtime :rt :t) => MonadIoThread :rt :t :m (:m -> :rt))
    "A MonadIo which can spawn :t's. Other :t's error
separately. A spawned :t erroring will not cause the parent
:t to fail. :t can be any 'thread-like' object, depending on the
underlying implementation - system threads, software-managed green
threads, etc."
    )

  (inline)
  (declare runtime-for (MonadIoThread :rt :t :m => Proxy (:m :a) -> Proxy :rt))
  (define (runtime-for _)
    "Get the Runtime type for a MonadIoThread type."
    Proxy)

  (inline)
  (declare as-runtime-prx (MonadIoThread :rt :t :m => :m :a -> Proxy :rt -> :m :a))
  (define (as-runtime-prx op _rt-prx)
    "Get the Runtime type for a MonadIoThread operation."
    op)

  (inline)
  (declare get-runtime-for (MonadIoThread :rt :t :m => :m :a -> Proxy :rt))
  (define (get-runtime-for op)
    "Get the Runtime type for a MonadIoThread operation."
    (runtime-for (proxy-of op))))

(defmacro derive-monad-io-thread (monad-param monadT-form)
  "Automatically derive an instance of MonadIoThread for a monad transformer.

Example:
  (derive-monad-io-thread :m (st:StateT :s :m))"
  `(define-instance (MonadIoThread :runtime :thread ,monad-param => MonadIoThread :runtime :thread ,monadT-form)
     ))

(coalton-toplevel

  ;;
  ;; Std. Library Transformer Instances
  ;;

  (derive-monad-io-thread :m (st:statet :s :m))
  (derive-monad-io-thread :m (env:EnvT :e :m))
  (derive-monad-io-thread :m (LoopT :m))
  )

(defmacro inject-runtime (f cl:&rest args)
  "Weave proxies to inject the runtime proxy as the first argument of f.
Assumes the output has type :m :a for some MonadIoThread :m."
  `(progn
     (let m-prx = Proxy)
     (as-proxy-of
      (wrap-io (,f (runtime-for m-prx) ,@args))
      m-prx)))

(defmacro wrap-io-with-runtime ((rt-prx-sym) cl:&body body)
  "Wrap the body in a wrap-io and pass a proxy to the runtime with RT-PRX-SYM."
  (cl:let ((m-prx (cl:gensym)))
    `(progn
       (let ,m-prx = Proxy)
       (let ,rt-prx-sym = (runtime-for ,m-prx))
       (as-proxy-of
        (wrap-io
          ,@body)
        ,m-prx))))

;;; TODO: Once the inference bug gets fixed, move these back into the typeclass and
;;; put the implementations into gen-impl/io-thread
(coalton-toplevel
  (inline)
  (declare fork-thread ((UnliftIo :r :i) (LiftTo :r :m) (MonadIoThread :rt :t :r)
                        => :r :a -> :m :t))
  (define (fork-thread op)
    "Spawn a new thread, which starts running immediately. Returns
the handle to the thread. If the thread raises an unhandled exception,
it will be logged to *ERROR-OUTPUT* and swallowed, until/if the thread
is joined.

This version can accept
any underlying BaseIo, which can be useful, but causes inference
issues in some cases."
    (lift-to
     (with-run-in-io
         (fn (run)
           (wrap-io
             (fork! (get-runtime-for op)
                    (fn (_)
                      (run-handled! (run op)))))))))

  (inline)
  (declare fork-thread-throw ((UnliftIo :r :i) (LiftTo :r :m) (MonadIoThread :rt :t :r)
                              => :r :a -> :m :t))
  (define (fork-thread-throw op)
  "Spawn a new thread, which starts running immediately. Returns
the handle to the thread. If the thread raises an unhandled exception,
throws immediately. The underlying system determines the result of the
throw, but it could include terminating the whole program."
    (lift-to
     (with-run-in-io
       (fn (run)
         (wrap-io
          (fork-throw! (get-runtime-for op)
                       (fn (_)
                         (run-handled! (run op)))))))))

  (inline)
  (declare join-thread ((MonadIoThread :rt :t :m) (MonadException :m) => :t -> :m Unit))
  (define (join-thread thread)
    "Block the current thread until the target thread is completed.
Does not a retrieve value. Raises an exception if the target thread
raised an unhandled exception, wrapping the target thread's raised
exception. JOIN-THREAD is the lowest level operation to block on another
thread's termination."
    (let m-prx = Proxy)
    (let rt-prx = (runtime-for m-prx))
    (as-proxy-of
     (do-matchM (wrap-io (join! rt-prx thread))
       ((Ok _)
        (pure Unit))
       ((Err e)
        (raise (JoinedFailedThread e))))
     m-prx))

  (inline)
  (declare stop-thread (MonadIoThread :rt :t :m => :t -> :m Unit))
  (define (stop-thread thread)
    "Stop a thread. If the thread has already stopped, does nothing."
    (inject-runtime stop! thread))

  (inline)
  (declare sleep (MonadIoThread :rt :t :m => UFix -> :m Unit))
  (define (sleep msec)
    "Sleep the current thread for MSECS milliseconds."
    (inject-runtime sleep! msec))

  (inline)
  (declare current-thread (MonadIoThread :rt :t :m => :m :t))
  (define current-thread
    "Get the current thread."
    (inject-runtime current-thread!))

  (inline)
  (declare mask-thread (MonadIoThread :rt :t :m => :t -> :m Unit))
  (define (mask-thread thread)
     "Mask the thread so it can't be stopped."
    (inject-runtime mask! thread))

  (inline)
  (declare mask-current-thread (MonadIoThread :rt :t :m => :m Unit))
  (define mask-current-thread
    "Mask the current thread so it can't be stopped."
    (let m-prx = Proxy)
    (let runtime-prx = (runtime-for m-prx))
    (as-proxy-of
     (wrap-io
       (let current-thread = (current-thread! runtime-prx))
       (mask! runtime-prx current-thread))
     m-prx))

  (inline)
  (declare unmask-thread (MonadIoThread :rt :t :m => :t -> :m Unit))
  (define (unmask-thread thread)
    "Unmask the thread so it can be stopped. Unmask respects
nested masks - if the thread has been masked N times, it can only be
stopped after being unmasked N times."
    (inject-runtime unmask! thread))

  (inline)
  (declare unmask-current-thread (MonadIoThread :rt :t :m => :m Unit))
  (define unmask-current-thread
    "Unmask the current thread so it can be stopped. Unmask respects
nested masks - if the thread has been masked N times, it can only be
stopped after being unmasked N times."
    (let m-prx = Proxy)
    (as-proxy-of
     (wrap-io
       (let current-thread = (current-thread! (runtime-for m-prx)))
       (unmask! (runtime-for m-prx) current-thread))
     m-prx))

  (inline)
  (declare unmask-thread-finally ((UnliftIo :r :io) (LiftTo :r :m) (MonadIoThread :rt :t :r)
                                  => :t -> (UnmaskFinallyMode -> :r :b) -> :m Unit))
  (define (unmask-thread-finally thread op-finally)
    "Unmask the thread, run the provided action, and then honor any
 pending stop for that thread after the action finishes.

Warning: There is a very small chance that the UnmaskFinallyMode passed to the callback could be
inconsistent with whether the Concurrent is ultimately stopped. Regardless of the input, the
callback should leave any resources in a valid state. An example of a valid callback: closing a log
file if the thread is stopped, or closing the log file with a final message if the thread is
continuing."
    (lift-to
     (with-run-in-io
         (fn (run)
           (wrap-io (unmask-finally! (runtime-for (proxy-result-of op-finally))
                                     thread
                                     (fn (m) (run! (run (op-finally m))))))))))

  (inline)
  (declare unmask-current-thread-finally ((UnliftIo :r :io) (LiftTo :r :m) (MonadIoThread :rt :t :r)
                                          => (UnmaskFinallyMode -> :r Unit) -> :m Unit))
  (define (unmask-current-thread-finally op-finally)
    "Unmask the current thread, run the provided action, and then honor any
 pending stop for that thread after the action finishes.

Warning: There is a very small chance that the UnmaskFinallyMode passed to the callback could be
inconsistent with whether the Concurrent is ultimately stopped. Regardless of the input, the
callback should leave any resources in a valid state. An example of a valid callback: closing a log
file if the thread is stopped, or closing the log file with a final message if the thread is
continuing."
    (lift-to
     (with-run-in-io
       (fn (run)
         (wrap-io
           (let runtime-prx = (runtime-for (proxy-result-of op-finally)))
           (unmask-finally! runtime-prx
                            (current-thread! runtime-prx)
                            (fn (m) (run! (run (op-finally m))))))))))

  )

(defmacro do-fork-thread (cl:&body body)
  `(fork-thread
    (do
     ,@body)))

(defmacro do-fork-thread-throw (cl:&body body)
  `(fork-thread-throw
    (do
     ,@body)))

(coalton-toplevel
  (inline)
  (declare prxs-same-runtime ((MonadIoThread :rt :t :m1) (MonadIoThread :rt :t :m2)
                              => Proxy (:m1 :a) -> Proxy (:m2 :b) -> Unit))
  (define (prxs-same-runtime _ _)
    "Force two MonadIoThread's to have the same runtime and thread type."
    Unit))
