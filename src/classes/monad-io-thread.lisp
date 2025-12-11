(cl:in-package :cl-user)
(defpackage :io/classes/monad-io-thread
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/monad/classes
   #:coalton-library/types
   #:io/classes/monad-io
   #:io/thread-impl/runtime)
  (:import-from #:coalton-library/experimental/do-control-loops-adv
   #:LoopT)
  (:local-nicknames
   (:st #:coalton-library/monad/statet)
   (:env #:coalton-library/monad/environment)
   )
  (:export
   #:MonadIoThread
   #:derive-monad-io-thread
   #:current-thread
   #:fork
   #:sleep
   #:mask
   #:mask-current
   #:unmask
   #:unmask-finally
   #:unmask-current
   #:unmask-current-finally
   #:stop

   #:do-fork))
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
      (Proxy :r -> (UnmaskFinallyMode -> Unit) -> :t -> Unit)))

  ;; TODO: Decide if this should have mask/unmask or not. See below.
  ;; For now, docstrings are written assuming we'll make masking available.
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
     ;; ((MonadIoThread :m :rt) => :c -> :m Unit)))

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
  (define-class (MonadIo :m => MonadIoThread :m :rt (:m -> :rt))
    "A MonadIo which can spawn :t's. Other :t's error
separately. A spawned :t erroring will not cause the parent
:t to fail. :t can be any 'thread-like' object, depending on the
underlying implementation - system threads, software-managed green
threads, etc."
    (current-thread
     "Get the current thread."
     (:m :t))
    (fork
     "Spawn a new thread, which starts running immediately.
Returns the handle to the thread. This version can accept
any underlying BaseIo, which can be useful, but causes inference
issues in some cases."
     ((UnliftIo :r :i) (LiftTo :r :m) => :r :a -> :m :t))
    (sleep
     "Sleep the current thread for MSECS milliseconds."
     (UFix -> :m Unit))
    (mask
     "Mask the given thread so it can't be stopped."
     (:t -> :m Unit))
    (mask-current
     "Mask the current thread so it can't be stopped."
     (:m Unit))
    ;; TODO: Functions unmasking *other* threads need to be removed because
    ;; they aren't safe. There's no way to prevent thread stop race conditions
    ;; if other threads can unmask the current thread, because unmaks itself can
    ;; potentially stop the thread being unmasked.
    (unmask
     "Unmask the given thread so it can be stopped. Unmask respects
nested masks - if the thread has been masked N times, it can only be
stopped after being unmasked N times."
     (IoThread -> :m Unit))
    (unmask-finally
     "Unmask the given thread, run the provided action, and then honor any
 pending stop for that thread after the action finishes."
     ((UnliftIo :r :io) (LiftTo :r :m)
      => IoThread -> (UnmaskFinallyMode -> :r Unit) -> :m Unit))
    (unmask-current
     "Unmask the current thread so it can be stopped. Unmask respects
nested masks - if the thread has been masked N times, it can only be
stopped after being unmasked N times."
     (:m Unit))
    (unmask-current-finally
     "Unmask the current thread, run the provided action, and then honor any
 pending stop for that thread after the action finishes."
     ((UnliftIo :r :io) (LiftTo :r :m) => (UnmaskFinallyMode -> :r Unit) -> :m Unit))
    (stop
     "Stop a thread. If the thread has already stopped, does nothing."
     (:t -> :m Unit))))

(cl:defmacro derive-monad-io-thread (monad-param monadT-form)
  "Automatically derive an instance of MonadIoThread for a monad transformer.

Example:
  (derive-monad-io-thread :m (st:StateT :s :m))"
  `(define-instance (MonadIoThread ,monad-param IoThread => MonadIoThread ,monadT-form IoThread)
     (define current-thread (lift current-thread))
     (define fork fork%)
     (define sleep (compose lift sleep))
     (define mask (compose lift mask))
     (define mask-current (lift mask-current))
     (define unmask (compose lift unmask))
     (define unmask-finally unmask-finally%)
     (define unmask-current (lift unmask-current))
     (define unmask-current-finally unmask-current-thread-finally%)
     (define stop (compose lift stop))))

(coalton-toplevel

  ;;
  ;; Std. Library Transformer Instances
  ;;

  (derive-monad-io-thread :m (st:StateT :s :m))
  (derive-monad-io-thread :m (env:EnvT :e :m))
  (derive-monad-io-thread :m (LoopT :m)))

(cl:defmacro do-fork (cl:&body body)
  `(fork
    (do
     ,@body)))
