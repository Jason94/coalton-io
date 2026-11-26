(cl:in-package :cl-user)
(defpackage :io/classes/delay
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton/types
   #:coalton/monad/classes
   #:io/utils
   #:io/classes/monad-io
   #:io/classes/thread
   )
  (:export
   ;; Library Public
   #:DelayRuntime
   #:fork-delay!

   #:Delays
   #:fork-delay
   #:do-fork-delay
   
   ;; Library Private
   #:delay-runtime-for
   #:get-delay-runtime-for
   )
  )
(in-package :io/classes/delay)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (define-class (Runtime :r :t => DelayRuntime :r :d (:r -> :d))
    "A runtime that can fork a delayed computation onto another thread and await the
result of that computation later.

Runtime is a low-level type that operates inside the normal MonadIo layer.
It should not be used by normal application code. Its two main purposes are:
(1) to make Delays generic over the type of thread it forks, and
(2) to build low-level, efficient concurrency tools that are generic
over the underlying thread type."
    (fork-delay!
     "Spawn a new thread to calculate a value, which starts running immediately.
Returns the handle to the delay, which can manage the lifecycle of the thread.

The ForkStrategy controls both:
  - how unhandled exceptions behave, and
  - whether the fork is structured (and which thread's scope owns it)."
     (Proxy :r * ForkStrategy :t * (Void -> :a) -> :d :a)))
  )

(coalton-toplevel
  (define-class ((Threads :rt :t :m) (DelayRuntime :rt :d) => Delays :rt :m (:m -> :rt))
    "A MonadIo which can fork a delayed computation.")

  (inline)
  (declare delay-runtime-for (Delays :rt :m => Proxy (:m :a) -> Proxy :rt))
  (define (delay-runtime-for _)
    Proxy)

  (inline)
  (declare get-delay-runtime-for (Delays :rt :m => :m :a -> Proxy :rt))
  (define (get-delay-runtime-for op)
    "Get the DelayRuntime type for a Delays operation."
    (delay-runtime-for (proxy-of op))))

(coalton-toplevel
  (inline)
  (declare fork-delay ((UnliftIo :r :i) (LiftTo :r :m) (Delays :rt :r) (DelayRuntime :rt :d)
                       => :r :a
                       &key
                       (:unhandled UnhandledExceptionStrategy)
                       (:scope (ForkScope :t))
                       -> :m (:d :a)))
  (define (fork-delay op &key (unhandled LogAndSwallow) (scope Structured))
    "Start calculation in a new thread, which starts running immediately. Returns
the handle to the thread. Can specify an unhandled exception strategy and fork scope.

If the thread raises an unhandled exception, the default behavior is to log it to
*ERROR-OUTPUT* and swallow it until/if the thread is joined. The default fork scope is
structured."
    (lift-to
     (with-run-in-io
       (fn (run)
         (wrap-io
          (fork-delay! (get-delay-runtime-for op)
                       (ForkStrategy unhandled scope)
                       (fn ()
                         (run-unsafe! (run op))))))))))

(defmacro do-fork-delay (cl:&body forms)
  (cl:multiple-value-bind (unhandled scope body)
      (parse-fork-keywords% forms)
    `(fork-delay
      (do
       ,@body)
      :unhandled ,unhandled
      :scope ,scope)))
