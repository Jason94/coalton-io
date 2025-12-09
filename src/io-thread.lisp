(cl:in-package :cl-user)
(defpackage :io/thread
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/functions
   #:coalton-library/monad/classes
   #:io/utils
   #:io/monad-io
   #:io/exception
   #:io/resource
   #:io/term
   #:io/thread-impl/runtime
   )
  (:import-from #:coalton-library/experimental/do-control-loops-adv
   #:LoopT)
  (:local-nicknames
   (:st #:coalton-library/monad/statet)
   (:env #:coalton-library/monad/environment)
   (:io #:io/simple-io)
   (:t/l #:coalton-threads/lock)
   )
  (:export
   ;; Re-export from the runtime
   #:IoThread
   #:ThreadingException
   #:InterruptCurrentThread

   #:UnmaskFinallyMode
   #:Stopped
   #:Running

   ;; MonadIoThread interface
   #:MonadIoThread
   #:derive-monad-io-thread
   #:current-thread
   #:fork
   #:fork_
   #:do-fork
   #:do-fork_
   #:sleep
   #:mask
   #:mask-current
   #:unmask
   #:unmask-finally
   #:unmask-current
   #:unmask-current-finally
   #:stop
   #:with-mask
   #:do-with-mask

   #:write-line-sync
   
   #:implement-monad-io-thread
   ))
(in-package :io/thread)

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
     (:t -> :m Unit)))

  ;;
  ;; Thread Masking Helpers
  ;;

  (declare with-mask ((MonadIoThread :m IoThread) (MonadException :m)
                      => :m :a -> :m :a))
  (define (with-mask op)
    "Mask the current thread while running OP, automatically unmasking
afterward."
    (bracket-io_ mask-current
                 (const unmask-current)
                 (fn (_) op)))

  ;;
  ;; Other Threading Utilities
  ;;

  (declare write-line-sync ((Into :s String) (MonadIoTerm :m) => :s -> :m Unit))
  (define (write-line-sync msg)
    "Perform a synchrozied write-line to the terminal. Not performant - mainly useful
for debugging."
    (wrap-io (write-line-sync% msg) Unit))
  )

(cl:defmacro do-with-mask (cl:&body body)
  "Evaluate BODY with the current thread masked, automatically unmasking
afterward."
  `(with-mask
     (do
      ,@body)))

(cl:defmacro implement-monad-io-thread (monad)
  `(define-instance (MonadIoThread ,monad IoThread)
     (define current-thread current-thread%)
     (define fork fork%)
     (define sleep sleep%)
     (define mask mask%)
     (define mask-current mask-current-thread%)
     (define unmask unmask%)
     (define unmask-finally unmask-finally%)
     (define unmask-current unmask-current-thread%)
     (define unmask-current-finally unmask-current-thread-finally%)
     (define stop stop%)))

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

(cl:defmacro do-fork_ (cl:&body body)
  `(fork_
    (do
     ,@body)))

;;
;; Simple IO Implementation
;;

(coalton-toplevel

  (implement-monad-io-thread io:IO)

  (declare fork_ ((MonadIoThread :m IoThread) (LiftTo io:IO :m)
                  => io:IO :a -> :m IoThread))
  (define fork_ fork)
  )
