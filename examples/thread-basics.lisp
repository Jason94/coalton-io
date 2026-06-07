(cl:in-package :cl-user)
(defpackage :io/examples/thread-basics
  (:use
   #:coalton
   #:coalton-prelude
   ;; #:io/monad-io
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
  ;;; Here, we build the greeter program, but we don't run it! In normal Coalton, this
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
     ;; store the handle to the spawned thread so we can stop it later.
     (thread <-
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

     ;; Stop the inner thread and exit
     (stop thread)
     (write-line-sync "Main thread done")))) 
        
     

(cl:defun run-example ()
  ;; run-io! runs the multithreaded-program we created above.
  (coalton (run-io! multithreaded-program)))
