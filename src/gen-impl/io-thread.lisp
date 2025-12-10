(cl:in-package :cl-user)
(defpackage :io/gen-impl/thread
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/functions
   #:coalton-library/monad/classes
   #:io/classes/monad-io-thread
   #:io/utils
   #:io/monad-io
   #:io/thread-impl/runtime)
  (:import-from #:coalton-library/experimental/do-control-loops-adv
   #:LoopT)
  (:local-nicknames
   (:st #:coalton-library/monad/statet)
   (:env #:coalton-library/monad/environment)
   (:io #:io/simple-io)
   (:t/l #:coalton-threads/lock))
  (:export
   ;; Re-export from the runtime
   #:IoThread
   #:ThreadingException
   #:InterruptCurrentThread

   #:UnmaskFinallyMode
   #:Stopped
   #:Running

   ;; Re-exports from io/classes/monad-io-thread
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
   #:stop

   ;; Remaining exports
   #:derive-monad-io-thread
   #:fork_
   #:do-fork
   #:do-fork_

   #:write-line-sync

   #:implement-monad-io-thread
   ))
(in-package :io/gen-impl/thread)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  ;;
  ;; Thread Masking Helpers
  ;;


  ;;
  ;; Other Threading Utilities
  ;;

  (declare write-line-sync ((Into :s String) (MonadIoTerm :m) => :s -> :m Unit))
  (define (write-line-sync msg)
    "Perform a synchrozied write-line to the terminal. Not performant - mainly useful
for debugging."
    (wrap-io (write-line-sync% msg) Unit))
  )


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

