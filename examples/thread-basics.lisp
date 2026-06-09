(cl:in-package :cl-user)
(defpackage :io/examples/thread-basics
  (:use
   #:coalton
   #:coalton-prelude
   #:io/simple-io
   #:io/thread)
  (:import-from #:io/simple-io/loops
   #:do-while-io)
  (:export
   #:run-example
   ))
(in-package :io/examples/thread-basics)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  
  ;;; An (IO Unit) is a program that is capable of performing IO operations, like terminal
  ;;; IO or multithreading, and returns a "Unit" type value when it is run.
  ;;;
  ;;; Here, we build the main program, but we don't run it! In normal Coalton, this
  ;;; function would have to be a (Void -> Unit) function. If it was just a (Unit) value,
  ;;; then compiling it would ask the user for their name at compile time! But when we
  ;;; create an (IO Unit) value, we're just assembling a program that we can run later.
  (declare multithreaded-program (IO Unit))
  (define multithreaded-program
    ;; `do` syntax allows us to chain multiple IO programs together into one bigger program.
    (do
     ;; Normal Common Lisp terminal operations aren't threadsafe. `write-line-sync` is an
     ;; IO program that allows multiple threads to write to the terminal.
     (write-line-sync "Main thread starting")

     ;; `do-fork-thread_` is an IO that takes another IO, and spawns a thread running that
     ;; program. Normally, just calling `do-fork-thread_` on its own will construct an IO
     ;; value but won't run it, so it won't actually start a thread until it's run with
     ;; `run-io!`. However, because we're using it in `do` notation, we're running it as
     ;; part of the `multithreaded-program` IO program.
     ;;
     ;; In `do` notation, we capture this return value using the `<-` command. Here, we
     ;; store the handle to the spawned thread, which we could use to stop it later.
     (_thread <-
      (do-fork-thread_
        ;; `do-while-io` takes an inner IO program that returns a `Boolean` when run. It returns
        ;; another IO program that runs the IO program while it returns `True`, then stops.
        ;;
        ;; Note that all coalton-io macros that start with `do-`, like `do-fork-thread_` and
        ;; `do-while-io` automatically use `do` syntax, so we don't need to add another `do` to
        ;; construct the program we pass to `do-while-io`, like the top of `multithreaded-program`.
        (do-while-io
          (write-line-sync "Inner thread looping...")
          (sleep 100)
          ;; `pure` constructs an IO that returns a constant value. In this case, because
          ;; the `do-while-io` loop body returns `True`, it will loop forever until the thread
          ;; is stopped.
          (pure True))))

     (write-line-sync "Main thread sleeping")
     (sleep 2000)

     (write-line-sync "Main thread done")

     ;; coalton-io's structured concurrency runtime manages thread lifecycles to prevent
     ;; orphaned threads. By default, when a thread finishes, it stops all child threads.
     ;; Because we forked `_thread` with the default settings, we don't need to stop it
     ;; ourselves. In addition, even if threads are forked outside of their parent's scope,
     ;; all threads are cleaned up when `run-io!` exits.
     ;;
     ;; For more on how to use structured concurrency, see:
     ;; https://jason94.github.io/coalton-io/#io-classes-thread-forkscope-type
     ;; https://jason94.github.io/coalton-io/#io-classes-thread-fork-thread-value
     ;;
     ;; For more on the implementation details of structured concurrency in coalton-io, see:
     ;; https://github.com/Jason94/coalton-io/blob/master/docs/runtime.md#structured-concurrency
     ;;
     ;;
     ;; If you want to stop threads manually, either to make it explicit or as part of a more
     ;; complex program, use `stop` and `await` like so:
     ;;
     ;; `stop` immediately tells the target thread to stop, and `await` blocks the current
     ;; thread until the given thread has finished running completely:
     ;; (stop _thread)
     ;; (await _thread)
     ))) 
        
     

(cl:defun run-example ()
  ;; run-io! runs the multithreaded-program we created above.
  (coalton (run-io! multithreaded-program)))
