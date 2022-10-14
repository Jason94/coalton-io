(cl:in-package :cl-user)
(defpackage :coalton-io.examples
  (:use #:coalton #:coalton-prelude
        #:coalton-library/monad/io)
  (:import-from #:coalton-library/char-stream-pure
    #:read-line-std
    #:write-line-std)
  (:import-from #:coalton-library/char-stream
    #:standard-input
    #:standard-output
    #:write-line!)
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
    Unit))

(coalton-toplevel
  (define greet-pure
    (do
      (write-line-std "Hello! What is your name?")
      (name <- (read-line-std))
      (let greeting = (str:concat "Hello, " name))
      (write-line-std greeting))))

(coalton-toplevel
  (declare greet-without-do (IO Unit))
  (define greet-without-do
    (progn
      (let a = (write-line-std "Hello! What is your name?"))
      (let io-name = (>>= a
                          (fn (_)
                            (read-line-std))))
      (let io-greeting = (map (fn (name)
                                (str:concat "Hello, " name))
                              io-name))
      (>>= io-greeting write-line-std))))

(coalton-toplevel
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

  ;; This will have to wait until we're opening file streams
  ; (declare sum-numbers (Unit -> IO Unit))
  ; (define (sum-numbers)
  ;   (match (open-file "numbers.txt" Optionally)
  ;     ((Some stream)
  ;      (do
  ;        (lines <- (read-lines stream))
  ;        (close stream)
  ;        (let integers = (discard-none (map str:parse-int lines)))
  ;        (let sum = (fold + 0 integers))
  ;        (write-line (str:concat "Total: " (show sum)))))
  ;     ((None)
  ;      (write-line "Could not open file"))))

  (declare show (:a -> String))
  (define (show x)
    (lisp String (x)
      (cl:write-to-string x))))

; (cl:defun test-write-line ()
;   (coalton (run! (write-line 1 "Hello"))))

; (cl:defun test-write-line2 ()
;   (coalton (write-line! (stardard-output Unit) "Hello")))
;
; (cl:defun test-impure-greet ()
;   (coalton (greet-impure)))
;
; (cl:defun test-pure-greet ()
;   (coalton (run! greet-pure)))

; (cl:defun test-sum-numbers ()
;   (coalton (run! (sum-numbers))))
