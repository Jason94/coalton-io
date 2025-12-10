(cl:in-package :cl-user)
(defpackage :io/classes/monad-io-thread
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/monad/classes
   #:io/classes/monad-io
   #:io/thread-impl/runtime)
  (:export
   #:MonadIoThread
   #:current-thread
   #:fork
   #:sleep
   #:mask
   #:mask-current
   #:unmask
   #:unmask-finally
   #:unmask-current
   #:unmask-current-finally
   #:stop))
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
