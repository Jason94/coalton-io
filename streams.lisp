(cl:in-package :cl-user)
(defpackage :coalton-io.streams
  (:use
    #:coalton
    #:coalton-prelude
    #:coalton-library/monad/io)
    ;#:coalton-io.io-monad)
  (:export
    #:close
    #:print-line
    #:read-line
    #:read-lines
    #:open-file
    #:ThrowError
    #:Create
    #:Optionally))
(in-package :coalton-io.streams)

;;
;; LispStream datatype.
;;
;; Getting a stream type doesn't require IO, but actually doing anything with
;; it does. This is contrary to Haskell, where all streams are wrapped in IO.
;; (see fromList https://hackage.haskell.org/package/io-streams-1.5.2.2/docs/System-IO-Streams-List.html)
;;
;; From a purity standpoint there isn't any reason to not wrap a stream in an
;; IO, I suppose. But I think having a non-monadic, impure IO library is also
;; important, and I'd like to reuse the same stream types between the two.
;;
(coalton-toplevel
  (repr :native cl:stream)
  (define-type LispStream)

  (repr :transparent)
  (define-type (InputStream :t) (InputStream LispStream))
  (repr :transparent)
  (define-type (OutputStream :t) (OutputStream LispStream))
  (repr :transparent)
  (define-type (BidirectionalStream :t) (BidirectionalStream LispStream))

  (define-class (Stream :t))

  (define-instance (Stream (InputStream :t)))
  (define-instance (Stream (OutputStream :t)))
  (define-instance (Stream (BidirectionalStream :t))))

;;
;; Define the ways to get stream objects
;;
(coalton-toplevel
  (declare *standard-input* (InputStream Char))
  (define *standard-input* (lisp :a ()
                             cl:*standard-input*))

  (repr :enum)
  (define-type OpenFileOption
    ThrowError
    Create
    Optionally)

  (declare open-file (String -> OpenFileOption -> Optional (InputStream Char)))
  (define (open-file fname open-opt)
    (match open-opt
      ((ThrowError)
       (Some
         (lisp (InputStream Char) (fname)
           (cl:open fname :if-does-not-exist :error))))
      ((Create)
       (Some
         (lisp (InputStream Char) (fname)
           (cl:open fname :if-does-not-exist :create))))
      ((Optionally)
       (lisp (Optional (InputStream Char)) (fname)
         (cl:let ((stream (cl:open fname :if-does-not-exist nil)))
           (cl:if stream
             (coalton-prelude:Some stream)
             coalton-prelude:None)))))))

(coalton-toplevel
  ;;
  ;; Simple IO stream API
  ;;
  (declare close (Stream :s => :s -> IO Unit))
  (define (close stream)
    (IO
      (fn ()
        (lisp Unit (stream)
          (cl:close stream)
          Unit))))

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

  ;; There might be a more efficient way to implement this.
  (declare read-lines (InputStream Char -> IO (List String)))
  (define (read-lines stream)
    (let ((declare inner (List String -> IO (List String)))
          (inner (fn (lines)
                   (do
                     (next <- (read-line-stream stream))
                     (match next
                       ((Some line)
                        (inner (cons line lines)))
                       ((None)
                        (pure (reverse lines))))))))
      (inner nil)))

  (declare read-line-stream (InputStream Char -> IO (Optional String)))
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
