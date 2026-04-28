(cl:in-package :cl-user)
(defpackage :io/classes/thread
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/types
   #:coalton-library/monad/classes
   #:coalton-library/experimental/do-control-core
   #:io/utils
   #:io/threads-exceptions
   #:io/classes/exceptions
   #:io/classes/monad-io
   #:io/classes/term
   )
   ;; #:io/threads-impl/runtime)
  (:import-from #:coalton-library/experimental/do-control-loops-adv
   #:LoopT)
  (:local-nicknames
   (:st #:coalton-library/monad/statet)
   (:env #:coalton-library/monad/environment)
   )
  (:export
   ;; Library Public
   #:Generation
   #:TimeoutStrategy
   #:Timeout
   #:NoTimeout

   #:UnhandledExceptionStrategy
   #:ThrowException
   #:LogAndSwallow
   #:Swallow

   #:ForkScope
   #:Structured
   #:Detached
   #:StructuredIn

   #:ForkStrategy

   #:Runtime
   #:current-thread!
   #:sleep!
   #:fork!
   #:join!
   #:stop!
   #:mask!
   #:unmask!
   #:unmask-finally!
   #:mask-current!
   #:unmask-current!
   #:park-current-thread-if!
   #:unpark-thread!

   #:Concurrent
   #:stop
   #:await
   #:mask
   #:unmask
   #:unmask-finally

   #:Threads
   #:derive-threads
   #:current-thread
   #:fork-thread
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
   #:park-current-thread-if
   #:unpark-thread

   #:runtime-for
   #:as-runtime-prx
   #:get-runtime-for

   ;; Library Private
   #:inject-runtime
   #:inject-runtime-unit
   #:wrap-io-with-runtime
   #:prxs-same-runtime
   #:concurrent-value-prx
   #:value-concurrent-prx
   #:atomic-set-generation%!
   )
  (:local-nicknames
   (:bt #:io/utilities/bt-compat)
   )
  )
(in-package :io/classes/thread)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (derive Eq)
  (repr :transparent)
  (define-type Generation
    (Generation bt::Word))

  (derive Eq)
  (define-type TimeoutStrategy
    "Controls whether blocking IO operations use a timeout in milliseconds."
    (Timeout Double-Float)
    NoTimeout)

  (derive Eq)
  (repr :enum)
  (define-type UnhandledExceptionStrategy
    "Controls what happens when a forked thread raises an exception that is not a
ThreadingException.

  - ThrowException: Immediately throws the Dynamic value in the child thread.
  - LogAndSwallow:  Logs the exception to *ERROR-OUTPUT* and ignore it until/if joined.
  - Swallow:        Ignore the error until/if joined."
    ThrowException
    LogAndSwallow
    Swallow)

  (define-type (ForkScope :t)
    "Controls whether a forked thread is attached to a scope, and if so which one.

  - Structured:   Attach to the current thread's scope.
  - StructuredIn: Attach to the provided thread handle's scope.
  - Detached:     Attach to the global scope; the thread will end with the toplevel run!."
    Structured
    Detached
    (StructuredIn :t))

  (define-struct (ForkStrategy :t)
    "Strategy object controlling fork behavior (exception semantics + structured concurrency)."
    (unhandled UnhandledExceptionStrategy)
    (scope (ForkScope :t)))

  (define-instance (Ord Generation)
    (inline)
    (define (<=> (Generation a) (Generation b))
      (<=> a b)))

  ;; TODO: Convert the generation stuff to use my own AtomicInteger, not coalton-thread's,
  ;; so I can use the same CAS loop algorithms.
  (declare atomic-set-generation%! (Generation * bt:AtomicInteger -> Void))
  (define (atomic-set-generation%! (Generation gen) atm)
    "Set the value of ATM to GEN."
    (rec % ()
      (if (bt:cas! atm (bt:read atm) gen)
          (values)
          (%))))

  (define-class (Runtime :r :t (:r -> :t))
    "This class doesn't represent data, but the type tells a Concurrent and
a Threads how to hook into the native threading implementations that
a runtime provides.  A runtime has a 'base' concurrent, which is the underlying
thread/fiber/etc. that the runtime produces to run concurrently. All other
Concurrents are built by composing on the base concurrent somehow.

Runtime is a low-level type that operates inside the normal MonadIo layer.
It should not be used by normal application code. Its two main purposes are:
(1) to make Threads generic over the type of thread it forks, and
(2) to build low-level, efficient concurrency tools that are generic
over the underlying thread type."
    (current-thread!
     "Get a handle for the current thread."
     (Proxy :r -> :t))
    (sleep!
     "Sleep the current thread for MSECS milliseconds."
     (Proxy :r * UFix -> Void))
    (fork!
     "Spawn a new thread, which starts running immediately.
Returns the handle to the thread.

The ForkStrategy controls both:
  - how unhandled exceptions behave, and
  - whether the fork is structured (and which thread's scope owns it)."
     (Proxy :r * (ForkStrategy :t) * (Void -> Result Dynamic :a) -> :t))
    (join!
     "Block the current thread until the target thread is completed.
Does not a retrieve value. Raises an exception if the target thread
raised an unhandled exception, wrapping the target thread's raised
exception. JOIN! is the lowest level operation to block on another
thread's termination, and most code should use AWAIT instead."
     (Proxy :r * :t -> Result Dynamic Unit))
    (stop!
     "Stop a :t. If the thread has already stopped, does nothing.
If the :t is masked, this will pend a stop on the :t. When/if
the :t becomes completely unmaksed, it will stop iself. Regardless
of whether the target :t is masked, STOP does not block or wait for
the target thread to complete."
     (Proxy :r * :t -> Void))
    (mask!
     "Mask the thread so it can't be stopped."
     (Proxy :r * :t -> Void))
    (unmask!
     "Unmask the thread so it can be stopped. Unmask respects
nested masks - if the thread has been masked N times, it can only be
stopped after being unmasked N times. When the thread unmasks, if
there are any pending stops, it will immediately be stopped."
     (Proxy :r * :t -> Void))
    (unmask-finally!
     "Unmask the thread, run the provided action, and then honor any pending stop for that
thread after the action finishes.

Warning: There is a very small chance that the UnmaskFinallyMode passed to the callback could be
inconsistent with whether the Concurrent is ultimately stopped. Regardless of the input, the
callback should leave any resources in a valid state. An example of a valid callback: closing a log
file if the thread is stopped, or closing the log file with a final message if the thread is
continuing."
      (Proxy :r * :t * (UnmaskFinallyMode -> Void) -> Void))
    (park-current-thread-if!
     "Parks the current thread if SHOULD-PARK? returns True. Will park the thread until
woken by an unpark from another thread. Upon an unpark, the thread will resume even if
SHOULD-PARK? is False! SHOULD-PARK? is only checked to determine if the thread should
park, *not* if it should resume. Can specify a timeout.

Concurrent:
  - WARNING: SHOULD-PARK? must not block, or the thread could be left blocked in a masked
    state.
  - Can briefly block while trying to park the thread, if contended."
     (Proxy :r * (Generation -> Void) * (Void -> Boolean)
      &key (:timeout TimeoutStrategy)
      -> Void))
    (unpark-thread!
     "Unparks the thread if it is still waiting on the generation. Attempting to unpark
the thread with a stale generation has no effect. A generation will be stale if the thread
has unparked and re-parked since the initial park.

Concurrent:
  - Can briefly block while trying to unpark the thread, if contended."
     (Proxy :r * Generation * :t -> Void))
     )

  (inline)
  (declare mask-current! (Runtime :rt :t => Proxy :rt -> Void))
  (define (mask-current! rt-prx)
    "Mask the current thread."
    (mask! rt-prx (current-thread! rt-prx)))

  (inline)
  (declare unmask-current! (Runtime :rt :t => Proxy :rt -> Void))
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
     ((Exceptions :m) (Threads :rt :t :m) => :c -> :m Unit))
    (await
     "Block the current thread until the target Concurrent is completed, and retrieve its value.
Re-raises if the target Concurrent raised an unhandled exception"
     ((Exceptions :m) (Threads :rt :t :m) => :c -> :m :a))
    (mask
     "Mask the Concurrent so it can't be stopped."
     ((Exceptions :m) (Threads :rt :t :m) => :c -> :m Unit))
    (unmask
     "Unmask the Concurrent so it can be stopped. Unmask respects nested masks - if the
Concurrent has been masked N times, it can only be stopped after being unmasked N times. When the
Concurrent unmasks, if there are any pending stops, it will immediately stop itself."
     ((Exceptions :m) (Threads :rt :t :m) => :c -> :m Unit))
    (unmask-finally
     "Unmask the thread, run the provided action, and then honor any pending stop for that
thread after the action finishes.

Warning: There is a very small chance that the UnmaskFinallyMode passed to the callback could be
inconsistent with whether the Concurrent is ultimately stopped. Regardless of the input, the
callback should leave any resources in a valid state. An example of a valid callback: closing a log
file if the thread is stopped, or closing the log file with a final message if the thread is
continuing."
     ((UnliftIo :r :io) (LiftTo :r :m) (Threads :rt :t :r) (Exceptions :m)
      (Threads :rt :t :m)
      => :c * (UnmaskFinallyMode -> :r Unit) -> :m Unit)))

  (inline)
  (declare concurrent-value-prx (Concurrent :c :a => :c -> Proxy :a))
  (define (concurrent-value-prx _)
    Proxy)

  (inline)
  (declare value-concurrent-prx (Concurrent :c :a => Proxy :a -> Proxy :c))
  (define (value-concurrent-prx _)
    Proxy)

  (define-class ((MonadIo :m) (Runtime :rt :t) => Threads :rt :t :m (:m -> :rt))
    "A MonadIo which can spawn :t's. Other :t's error
separately. A spawned :t erroring will not cause the parent
:t to fail. :t can be any 'thread-like' object, depending on the
underlying implementation - system threads, software-managed green
threads, etc."
    )

  (inline)
  (declare runtime-for (Threads :rt :t :m => Proxy (:m :a) -> Proxy :rt))
  (define (runtime-for _)
    "Get the Runtime type for a Threads type."
    Proxy)

  (inline)
  (declare as-runtime-prx (Threads :rt :t :m => :m :a * Proxy :rt -> :m :a))
  (define (as-runtime-prx op _rt-prx)
    "Get the Runtime type for a Threads operation."
    op)

  (inline)
  (declare get-runtime-for (Threads :rt :t :m => :m :a -> Proxy :rt))
  (define (get-runtime-for op)
    "Get the Runtime type for a Threads operation."
    (runtime-for (proxy-of op))))

(defmacro derive-threads (monad-param monadT-form)
  "Automatically derive an instance of Threads for a monad transformer.

Example:
  (derive-threads :m (st:StateT :s :m))"
  `(define-instance (Threads :runtime :thread ,monad-param => Threads :runtime :thread ,monadT-form)
     ))

(coalton-toplevel

  ;;
  ;; Std. Library Transformer Instances
  ;;

  (derive-threads :m (st:statet :s :m))
  (derive-threads :m (env:EnvT :e :m))
  (derive-threads :m (LoopT :m))
  )

(defmacro inject-runtime (f cl:&rest args)
  "Weave proxies to inject the runtime proxy as the first argument of f.
Assumes the output has type :m :a for some Threads :m."
  `(progn
     (let m-prx = Proxy)
     (as-proxy-of
      (wrap-io (,f (runtime-for m-prx) ,@args))
      m-prx)))

(defmacro inject-runtime-unit (f cl:&rest args)
  "Weave proxies to inject the runtime proxy as the first argument of f.
Assumes the output has type :m :a for some Threads :m. Returns Unit."
  `(progn
     (let m-prx = Proxy)
     (as-proxy-of
      (wrap-io
       (,f (runtime-for m-prx) ,@args)
       Unit)
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
  (declare fork-thread ((UnliftIo :r :i) (LiftTo :r :m) (Threads :rt :t :r)
                        => :r :a
                        &key
                        (:unhandled UnhandledExceptionStrategy)
                        (:scope (ForkScope :t))
                        -> :m :t))
  (define (fork-thread op &key (unhandled LogAndSwallow) (scope Structured))
    "Spawn a new thread, which starts running immediately. Returns
the handle to the thread. Can specify an unhandled exception strategy and fork scope.

If the thread raises an unhandled exception, the default behavior is to log it to
*ERROR-OUTPUT* and swallow it until/if the thread is joined. The default fork scope is
structured."
    (lift-to
     (with-run-in-io
       (fn (run)
         (wrap-io
           (fork! (get-runtime-for op)
                  (ForkStrategy unhandled scope)
                  (fn ()
                    (run-handled! (run op)))))))))

  (inline)
  (declare join-thread ((Threads :rt :t :m) (Exceptions :m) => :t -> :m Unit))
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
  (declare stop-thread (Threads :rt :t :m => :t -> :m Unit))
  (define (stop-thread thread)
    "Stop a thread. If the thread has already stopped, does nothing."
    (inject-runtime-unit stop! thread))

  (inline)
  (declare sleep (Threads :rt :t :m => UFix -> :m Unit))
  (define (sleep msec)
    "Sleep the current thread for MSECS milliseconds."
    (inject-runtime-unit sleep! msec))

  (inline)
  (declare current-thread (Threads :rt :t :m => :m :t))
  (define current-thread
    "Get the current thread."
    (inject-runtime current-thread!))

  (inline)
  (declare mask-thread (Threads :rt :t :m => :t -> :m Unit))
  (define (mask-thread thread)
     "Mask the thread so it can't be stopped."
    (inject-runtime-unit mask! thread))

  (inline)
  (declare mask-current-thread (Threads :rt :t :m => :m Unit))
  (define mask-current-thread
    "Mask the current thread so it can't be stopped."
    (let m-prx = Proxy)
    (let runtime-prx = (runtime-for m-prx))
    (as-proxy-of
     (wrap-io
       (let current-thread = (current-thread! runtime-prx))
       (mask! runtime-prx current-thread)
       Unit)
     m-prx))

  (inline)
  (declare unmask-thread (Threads :rt :t :m => :t -> :m Unit))
  (define (unmask-thread thread)
    "Unmask the thread so it can be stopped. Unmask respects
nested masks - if the thread has been masked N times, it can only be
stopped after being unmasked N times."
    (inject-runtime-unit unmask! thread))

  (inline)
  (declare unmask-current-thread (Threads :rt :t :m => :m Unit))
  (define unmask-current-thread
    "Unmask the current thread so it can be stopped. Unmask respects
nested masks - if the thread has been masked N times, it can only be
stopped after being unmasked N times."
    (let m-prx = Proxy)
    (as-proxy-of
     (wrap-io
       (let current-thread = (current-thread! (runtime-for m-prx)))
       (unmask! (runtime-for m-prx) current-thread)
       Unit)
     m-prx))

  (inline)
  (declare unmask-thread-finally ((UnliftIo :r :io) (LiftTo :r :m) (Threads :rt :t :r)
                                  => :t * (UnmaskFinallyMode -> :r Unit) -> :m Unit))
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
                                     (fn (m)
                                       (run! (run (op-finally m)))
                                       (values)))
                    Unit)))))

  ;; BUG: These kinds of inner wrap-io nested run! calls may not propogate errors
  ;; correctly, or might have other problems.
  (inline)
  (declare unmask-current-thread-finally ((UnliftIo :r :io) (LiftTo :r :m) (Threads :rt :t :r)
                                          => (UnmaskFinallyMode -> :r Unit) -> :m Unit))
  (define (unmask-current-thread-finally op-finally)
    "Unmask the current thread, run the provided action, and then honor any pending stop
for that thread after the action finishes.

Warning: There is a very small chance that the UnmaskFinallyMode passed to the callback
could be inconsistent with whether the Concurrent is ultimately stopped. Regardless of the
input, the callback should leave any resources in a valid state.

An example of a valid callback: closing a log file if the thread is stopped, or closing
the log file with a final message if the thread is continuing."
    (lift-to
     (with-run-in-io
       (fn (run)
         (wrap-io
           (let runtime-prx = (runtime-for (proxy-result-of op-finally)))
           (unmask-finally! runtime-prx
                            (current-thread! runtime-prx)
                            (fn (m)
                              (run! (run (op-finally m)))
                              (values)))
           Unit)))))

  ;; TODO: Currently unlifting :r :a with different :a's is difficult, maybe impossible
  ;; without existential qualifiers for the with-run-in-io type. Figure that out and
  ;; replace BaseIo here.
  (inline)
  (declare park-current-thread-if ((BaseIo :io) (Threads :rt :t :io) (MonadIo :m)
                                   => (Generation -> :io Unit) * :io Boolean
                                   &key (:timeout TimeoutStrategy)
                                   -> :m Unit))
  (define (park-current-thread-if with-gen should-park? &key (timeout NoTimeout))
    "Parks the current thread if SHOULD-PARK? returns True. Will park the thread until
woken by an unpark from another thread. Upon an unpark, the thread will resume even if
SHOULD-PARK? is False! SHOULD-PARK? is only checked to determine if the thread should
park, *not* if it should resume. Can specify a timeout.

Concurrent:
  - WARNING: SHOULD-PARK? must not block, or the thread could be left blocked in a masked
    state.
  - Can briefly block while trying to park the thread, if contended."
     (wrap-io
      (let runtime-prx = (get-runtime-for should-park?))
      (park-current-thread-if! runtime-prx
                               (fn (gen)
                                 (run! (with-gen gen))
                                 (values))
                               (fn ()
                                 (run! should-park?))
                               :timeout timeout)
      Unit))

  (inline)
  (declare unpark-thread (Threads :rt :t :m => Generation * :t -> :m Unit))
  (define (unpark-thread gen thread)
    "Unparks the thread if it is still waiting on the generation. Attempting to unpark
the thread with a stale generation has no effect. A generation will be stale if the thread
has unparked and re-parked since the initial park.

Concurrent:
  - Can briefly block while trying to unpark the thread, if contended."
    (inject-runtime-unit unpark-thread! gen thread))
  )

(cl:defun parse-fork-keywords% (forms)
  (cl:let ((unhandled 'LogAndSwallow)
           (scope 'Structured))
    (cl:loop :while (cl:and forms (cl:keywordp (cl:first forms)))
             :do (cl:let ((key (cl:pop forms)))
                   (cl:unless forms
                     (cl:error "Missing value for ~S in DO-FORK-THREAD" key))
                   (cl:ecase key
                     (:unhandled (cl:setf unhandled (cl:pop forms)))
                     (:scope (cl:setf scope (cl:pop forms))))))
    (cl:values unhandled scope forms)))

(defmacro do-fork-thread (cl:&body forms)
  (cl:multiple-value-bind (unhandled scope body)
      (parse-fork-keywords% forms)
    `(fork-thread
      (do
       ,@body)
      :unhandled ,unhandled
      :scope ,scope)))

(coalton-toplevel
  (inline)
  (declare prxs-same-runtime ((Threads :rt :t :m1) (Threads :rt :t :m2)
                              => Proxy (:m1 :a) * Proxy (:m2 :b) -> Void))
  (define (prxs-same-runtime _ _)
    "Force two Threads's to have the same runtime and thread type."
    (values)))
