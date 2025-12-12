(cl:in-package :cl-user)
(defpackage :io/classes/monad-io-thread
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/types
   #:coalton-library/monad/classes
   #:io/utils
   #:io/thread-exceptions
   #:io/classes/monad-io
   #:io/classes/monad-io-term
   #:io/thread-impl/runtime)
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
   #:stop!
   #:mask!
   #:unmask!
   #:unmask-finally!
   #:mask-current!
   #:unmask-current!

   #:MonadIoThread
   #:derive-monad-io-thread
   #:current-thread
   #:fork-thread
   #:sleep
   #:mask-thread
   #:mask-current-thread
   #:unmask-thread
   #:unmask-thread-finally
   #:unmask-current-thread
   #:unmask-current-thread-finally
   #:stop-thread
   #:do-fork-thread

   #:write-line-sync

   #:runtime-for
   #:as-runtime-prx
   #:get-runtime-for

   ;; Library Private
   #:inject-runtime
   #:wrap-io-with-runtime
   #:prxs-same-runtime
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
     (Proxy :r -> (Unit -> :a) -> :t))
    (stop!
     "Stop a :t. If the thread has already stopped, does nothing.
If the :t is masked, this will pend a stop on the :t. When/if
the :t becomes completely unmaksed, it will stop iself. Regardless
of whether the target :t is masked, STOP does not block or wait for
the target thread to complete."
     (Proxy :r -> :t -> Unit))
    (mask!
     "Mask the given thread so it can't be stopped."
     (Proxy :r -> :t -> Unit))
    (unmask!
     "Unmask the given thread so it can be stopped. Unmask respects
nested masks - if the thread has been masked N times, it can only be
stopped after being unmasked N times. When the thread unmasks, if
there are any pending stops, it will immediately be stopped."
     (Proxy :r -> :t -> Unit))
    (unmask-finally!
     "Unmask the given thread, run the provided action, and then honor any
 pending stop for that thread after the action finishes."
      (Proxy :r -> :t -> (UnmaskFinallyMode -> :a) -> Unit)))

  (inline)
  (declare mask-current! (Runtime :rt :t => Proxy :rt -> Unit))
  (define (mask-current! rt-prx)
    "Mask the current thread."
    (mask! rt-prx (current-thread! rt-prx)))

  (inline)
  (declare unmask-current! (Runtime :rt :t => Proxy :rt -> Unit))
  (define (unmask-current! rt-prx)
    "Mask the current thread."
    (mask! rt-prx (current-thread! rt-prx)))

  (define-class (Concurrent :c :a (:c -> :a))
    "A Concurrent is a type that has thread-like semantics. It can be
stopped, masked, unmasked, and await-ed. Concurrents don't have a uniform
fork function, becasue they might require different initialization input.
The most important property of Concurrents is that they can be composed."
    )
;;     (stop__
;;      "Stop a Concurrent. If the thread has already stopped, does nothing.
;; If the Concurrent is masked, this will pend a stop on the Concurrent. When/if
;; the Concurrent becomes completely unmaksed, it will stop iself. Regardless
;; of whether the target Concurrent is masked, STOP does not block or wait for
;; the target thread to complete."
     ;; ((MonadIoThread :rt :t :m) => :c -> :m Unit)))

  ;; TODO: To solve all the problems where you can never guarantee re-masking
  ;; in time after handling a thread interrupt, we should actually *re-mask*
  ;; after getting interrupted! Then, any code-path that wants to handle
  ;; an interrupt will need to be aware of that and is responsible for
  ;; un-masking the re-mask, if it wants the thread to keep going.
  ;; ACTUALLY, wait. That might be required for some Concurrent's to work
  ;; properly. Like a thread group, for example. What we could do is pack some
  ;; more data into the IoThread word. We could set aside some bits for pending
  ;; unmasks from other threads, and then treat those unmasks sort of like we
  ;; treat pending kills. That could help solve some of the race conditions.
  ;; TODO: Hopefully remove :t from this definition when this issue is fixed:
  ;; https://github.com/coalton-lang/coalton/issues/1717
  (define-class ((MonadIo :m) (Runtime :rt :t) => MonadIoThread :rt :t :m (:m -> :rt) (:m -> :t))
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

(cl:defmacro derive-monad-io-thread (monad-param monadT-form)
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

;;; TODO: Once the inference bug gets fixed, move these back into the typeclass and
;;; put the implementations into gen-impl/io-thread

(cl:defmacro inject-runtime (f cl:&rest args)
  "Weave proxies to inject the runtime proxy as the first argument of f.
Assumes the output has type :m :a for some MonadIoThread :m."
  `(progn
     (let m-prx = Proxy)
     (as-proxy-of
      (wrap-io (,f (runtime-for m-prx) ,@args))
      m-prx)))

(cl:defmacro wrap-io-with-runtime ((rt-prx-sym) cl:&body body)
  "Wrap the body in a wrap-io and pass a proxy to the runtime with RT-PRX-SYM."
  (cl:let ((m-prx (cl:gensym)))
    `(progn
       (let ,m-prx = Proxy)
       (let ,rt-prx-sym = (runtime-for ,m-prx))
       (as-proxy-of
        (wrap-io
          ,@body)
        ,m-prx))))

(coalton-toplevel
  (declare write-line-sync ((Into :s String) (MonadIoTerm :m) => :s -> :m Unit))
  (define (write-line-sync msg)
    "Perform a synchrozied write-line to the terminal. Not performant - mainly useful
for debugging."
    (wrap-io (write-line-sync% msg) Unit))

  )

(coalton-toplevel
  (inline)
  (declare fork-thread ((UnliftIo :r :i) (LiftTo :r :m) (MonadIoThread :rt :t :r)
                        => :r :a -> :m :t))
  (define (fork-thread op)
    "Spawn a new thread, which starts running immediately.
Returns the handle to the thread. This version can accept
any underlying BaseIo, which can be useful, but causes inference
issues in some cases."
    (lift-to
     (with-run-in-io
         (fn (run)
           (wrap-io
             (fork! (get-runtime-for op)
                    (fn (_)
                      (run! (run op)))))))))

  (inline)
  ;; (:t -> :m Unit)))
  (define (stop-thread thread)
    "Stop a thread. If the thread has already stopped, does nothing."
    (inject-runtime stop! thread))

  (inline)
  ;; (declare sleep_ ((Runtime :rt :t) (MonadIoThread :rt :t :m) => UFix -> :m Unit))
  (define (sleep_ msec)
    "Sleep the current thread for MSECS milliseconds."
    (inject-runtime sleep! msec))

  (inline)
  (declare current-thread (MonadIoThread :rt :t :m => :m :t))
  (define current-thread
    "Get the current thread."
    (inject-runtime current-thread!))

  (inline)
  ;; (:t -> :m Unit))
  (define (mask-thread thread)
     "Mask the given thread so it can't be stopped."
    (inject-runtime mask! thread))

  (inline)
  ;; TODO: This can probably not have to be a (Unit -> X) when the bug is fixed
  ;; (declare mask-current-thread (MonadIoThread :rt :t :m => :m Unit))
  (define (mask-current-thread)
    "Mask the current thread so it can't be stopped."
    (let m-prx = Proxy)
    (let runtime-prx = (runtime-for m-prx))
    (as-proxy-of
     (wrap-io
       (let current-thread = (current-thread! runtime-prx))
       (mask! runtime-prx current-thread))
     m-prx))

  (inline)
  ;; (:t -> :m Unit))
  (define (unmask-thread thread)
    "Unmask the given thread so it can be stopped. Unmask respects
nested masks - if the thread has been masked N times, it can only be
stopped after being unmasked N times."
    (inject-runtime unmask! thread))

  (inline)
  ;; (declare unmask-current-thread (MonadIoThread :rt :t :m => :m Unit))
  (define (unmask-current-thread)
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
  ;; (declare unmask-finally ((UnliftIo :r :io) (LiftTo :r :m) (Runtime :rt :t) (MonadIoThread :rt :t :m)
  ;;                          => :t -> (UnmaskFinallyMode -> :r Unit) -> :m Unit))
  (define (unmask-thread-finally thread op-finally)
    "Unmask the given thread, run the provided action, and then honor any
 pending stop for that thread after the action finishes."
    (lift-to
     (with-run-in-io
         (fn (run)
           (wrap-io (unmask-finally! (get-runtime-for (proxy-result-of op-finally))
                                     thread
                                     (fn (m) (run! (run (op-finally m))))))))))

  (inline)
  ;; (declare unmask-current-finally ((UnliftIo :r :io) (LiftTo :r :m)
  ;;                                  => (UnmaskFinallyMode -> :r Unit) -> :m Unit))
  (define (unmask-current-thread-finally op-finally)
    "Unmask the current thread, run the provided action, and then honor any
 pending stop for that thread after the action finishes."
    (lift-to
     (with-run-in-io
       (fn (run)
         (wrap-io
           (unmask-finally! (get-runtime-for (proxy-result-of op-finally))
                            (current-thread! (proxy-result-of op-finally))
                            (fn (m) (run! (run op-finally m)))))))))

  )

(cl:defmacro do-fork-thread (cl:&body body)
  `(fork-thread
    (do
     ,@body)))

(coalton-toplevel
  (inline)
  (declare prxs-same-runtime ((MonadIoThread :rt :t :m1) (MonadIoThread :rt :t :m2)
                              => Proxy (:m1 :a) -> Proxy (:m2 :b) -> Unit))
  (define (prxs-same-runtime _ _)
    "Force two MonadIoThread's to have the same runtime and thread type."
    Unit))
