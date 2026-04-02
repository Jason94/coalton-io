(cl:in-package :cl-user)
(defpackage :io/examples/greeter
  (:use
   #:coalton
   #:coalton-prelude
   #:io/simple-io)
  (:local-nicknames
   (:tm #:io/term))
  (:export
   #:run-example))

(in-package :io/examples/greeter)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  ;;; An (IO Unit) is a program that is capable of performing IO operations, like terminal
  ;;; IO or multithreading, and returns a "Unit" type value when it is run.
  ;;;
  ;;; Here, we build the greeter program, but we don't run it! In normal Coalton, this
  ;;; function would have to be a (Void -> Unit) function. If it was just a (Unit) value,
  ;;; then compiling it would ask the user for their name at compile time! But when we
  ;;; create an (IO Unit) value, we're just assembling a program that we can run later.
  (declare greeter-program (IO Unit))
  (define greeter-program
    ;; `do` syntax allows us to chain multiple IO programs together into one bigger program.
    (do
     ;; tm:write-line takes a String and returns a program (with type `IO Unit`) that prints
     ;; out the input String when it is run.
     (tm:write-line "Please enter your name:")
     ;; tm:read-line is a program (with type `IO String`). When the program is run, it reads
     ;; a line in from the terminal and returns it as a String value.
     ;;
     ;; In `do` notation, we capture this return value using the `<-` command.
     (user-name <- tm:read-line)
     ;; Now, we execute another tm:write-line program that prints out a customized greeting.
     (tm:write-line (<> (<> "Hello, " user-name) "!")))))

(cl:defun run-example ()
  ;; run-io! runs the greeter program we created above.
  (coalton (run-io! greeter-program)))
