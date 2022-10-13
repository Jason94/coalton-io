(in-package :coalton-io)

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

  ;;
  ;; LispStream datatype and a basic wrapper around the stream API
  ;;
  (repr :native cl:stream)
  (define-type LispStream)

  ;; TODO: Find a better way to encode stream properties/subtypes in an ADT
  ; (define-type Stream
  ;   (InputStringStream stream)
  ;   (OutputStringStream stream)
  ;   (BidirectionalStringStream stream)
  ;   (InputByteStream stream)
  ;   (OutputByteStream stream)
  ;   (BidirectionalByteStream stream))

  (declare *standard-input* LispStream)
  (define *standard-input* (lisp :a ()
                             cl:*standard-input*))

  (repr :enum)
  (define-type OpenFileOption
    ThrowError
    Create
    Optionally)

  (declare open-file (String -> OpenFileOption -> Optional LispStream))
  (define (open-file fname open-opt)
    (match open-opt
      ((ThrowError)
       (Some
         (lisp LispStream (fname)
           (cl:open fname :if-does-not-exist :error))))
      ((Create)
       (Some
         (lisp LispStream (fname)
           (cl:open fname :if-does-not-exist :create))))
      ((Optionally)
       (lisp (Optional LispStream) (fname)
         (cl:let ((stream (cl:open fname :if-does-not-exist nil)))
           (cl:if stream
             (coalton-prelude:Some stream)
             coalton-prelude:None))))))

  (declare close (LispStream -> IO Unit))
  (define (close stream)
    (IO
      (fn ()
        (lisp Unit (stream)
          (cl:close stream)
          Unit))))

  (declare show (:a -> String))
  (define (show x)
    (lisp String (x)
      (cl:write-to-string x)))

  ;; Had some trouble getting this to complie for the Num typeclass
  ; (define-class (Show :a)
  ;    "Types that can be converted to a string."
  ;    (show (:a -> String)))
  ;
  ; (define-instance (Show String)
  ;   (define (show str)
  ;     str))
  ;
  ; (define-instance (Show Boolean)
  ;   (define (show b)
  ;     (if b
  ;       "True"
  ;       "False")))
  ;; for some reason this doesn't work??
  ; (define-instance (Num :a => Show :a)
  ;   (define (show x)
  ;     (lisp String (x)
  ;       (cl:write-to-string x))))

  ;;
  ;; IO Monad
  ;;
  (define-type (IO :a)
    (IO (Unit -> :a)))

  (declare run! (IO :a -> :a))
  (define (run! io)
    (let (IO funit->a) = io)
    (funit->a))

  (define-instance (Functor IO)
    (define (map fb->c io-b)
      (IO
        (fn ()
          (let (IO funit->b) = io-b)
          (fb->c (funit->b))))))

  (define-instance (Applicative IO)
    (define (pure x)
      (IO
        (fn () x)))
    (define (liftA2 fa->b->c io-a io-b)
      (IO
        (fn ()
          (let (IO f->a) = io-a)
          (let (IO f->b) = io-b)
          (fa->b->c (f->a) (f->b))))))

  (define-instance (Monad IO)
    (define (>>= io-a fa->io-b)
      (IO
        (fn ()
          (let (IO f->a) = io-a)
          (run! (fa->io-b (f->a)))))))

  ;;
  ;; Simple IO stream API
  ;;

  (declare print-line (String -> IO Unit))
  (define (print-line str)
    (IO
      (fn ()
        (lisp :a (str)
          (cl:print str))
        Unit)))

  (declare read-line (IO String))
  (define read-line
    (IO
      (fn ()
        (lisp String ()
          (cl:multiple-value-bind (result)
              (cl:read-line
                cl:*standard-input*
                nil
                "")
            result)))))

  (declare read-lines (LispStream -> IO (List String)))
  (define (read-lines stream)
    (let ((declare inner (List String -> IO (List String)))
          (inner (fn (lines)
                   (do
                     (next <- (read-line-stream stream))
                     (match next
                       ((Some line)
                        (inner (cons line lines)))
                       ((None)
                        (pure lines)))))))
      (inner nil)))

  (declare read-line-stream (LispStream -> IO (Optional String)))
  (define (read-line-stream stream)
    (IO
      (fn ()
        (lisp (Optional String) (stream)
          (cl:multiple-value-bind (result is-end)
              (cl:read-line
                stream
                nil)
            (cl:if is-end
              coalton-library/classes:None
              (coalton-library/classes:Some result))))))))

(cl:defun test-print-line ()
  (coalton (run! (print-line "Hello"))))

(cl:defun test-impure-greet ()
  (coalton (greet-impure)))

(cl:defun test-pure-greet ()
  (coalton (run! greet-pure)))

(cl:defun test-sum-numbers ()
  (coalton (run! (sum-numbers))))
