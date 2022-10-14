(cl:in-package :cl-user)
(defpackage :coalton-io.examples
  (:use #:coalton #:coalton-prelude #:coalton-io.io-monad
        #:coalton-io.streams)
  (:local-nicknames
    (:str #:coalton-library/string)))
(in-package :coalton-io.examples)

(coalton-toplevel
  ;;
  ;; IO Examples. Helper functions to run these defined at the end of the file.
  ;;
  (define (greet-impure)
    (lisp :a ()
      (cl:print "Hello! What is your name?"))
    (let input = (lisp String ()
                   (cl:format nil "~a" (cl:read-line))))
    (let greeting = (str:concat "Hello, " input))
    (lisp :a (greeting)
      (cl:print greeting))
    Unit)

  (define greet-pure
    (do
      (print-line "Hello! What is your name?")
      (name <- read-line)
      (let greeting = (str:concat "Hello, " name))
      (print-line greeting)))

  (declare greet-without-do (IO Unit))
  (define greet-without-do
    (progn
      (let a = (print-line "Hello! What is your name?"))
      (let io-name = (>>= a
                          (fn (_)
                            read-line)))
      (let io-greeting = (map (fn (name)
                                (str:concat "Hello, " name))
                              io-name))
      (>>= io-greeting print-line)))

  ;; I think there is a more efficient way to do this using foldl
  (declare discard-none (List (Optional :a) -> List :a))
  (define (discard-none lst)
    (let ((inner (fn (acc rem)
                   (match rem
                     ((Cons head tail)
                      (match head
                        ((Some elt)
                         (inner (Cons elt acc)
                                tail))
                        ((None)
                         (inner acc tail))))
                     ((Nil)
                      (reverse acc))))))
      (inner nil lst)))

  (declare sum-numbers (Unit -> IO Unit))
  (define (sum-numbers)
    (match (open-file "numbers.txt" Optionally)
      ((Some stream)
       (do
         (lines <- (read-lines stream))
         (close stream)
         (let integers = (discard-none (map str:parse-int lines)))
         (let sum = (fold + 0 integers))
         (print-line (str:concat "Total: " (show sum)))))
      ((None)
       (print-line "Could not open file"))))

  (declare show (:a -> String))
  (define (show x)
    (lisp String (x)
      (cl:write-to-string x))))

(cl:defun test-print-line ()
  (coalton (run! (print-line "Hello"))))

(cl:defun test-impure-greet ()
  (coalton (greet-impure)))

(cl:defun test-pure-greet ()
  (coalton (run! greet-pure)))

(cl:defun test-sum-numbers ()
  (coalton (run! (sum-numbers))))
