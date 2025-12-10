(cl:in-package :cl-user)
(defpackage :io/classes/monad-io-thread
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/monad/classes
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

  (define-class (MonadIo :m => MonadIoThread :m :t (:m -> :t))
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
