(cl:in-package :cl-user)

(defpackage :io/examples/redis/cli
  (:use
   #:coalton
   #:coalton-prelude
   #:io/simple-io
   #:io/examples/redis/protocol
   )
  (:local-nicknames
   (:tm #:io/term)
   )
  (:export
   #:is-help-line
   #:print-help
   #:parse-cli-line
   ))

(in-package :io/examples/redis/cli)

(named-readtables:in-readtable coalton:coalton)

;;;;
;;;; This file defines some helper functions for the client-side CLI. It's mostly string
;;;; parsing, much of it using Common Lisp under the hood, and some terminal input/output
;;;; that does use coalton-io a little bit.
;;;;
;;;; But it's mostly here to have a functioning mini-Redis client. The other files in
;;;; the example will be more instructive on using coalton-io itself!
;;;;

(coalton-toplevel

  (declare str-upcase (String -> String))
  (define (str-upcase s)
    (lisp String (s)
      (cl:string-upcase s)))

  (declare join-with-space (List String -> String))
  (define (join-with-space xs)
    (lisp String (xs)
      (cl:format cl:nil "~{~A~^ ~}" xs)))

  (declare trim (String -> String))
  (define (trim s)
    (lisp String (s)
      (cl:string-trim '(#\Space #\Tab #\Return #\Newline) s)))

  (declare print-help (IO Unit))
  (define print-help
    (do
     (tm:write-line "Commands:")
     (tm:write-line "  PING [message]         Send ping (no message => server replies PONG)")
     (tm:write-line "  GET <key>              Fetch a key (missing => (nil))")
     (tm:write-line "  SET <key> <value>      Set a key (value may be multiple words or quoted)")
     (tm:write-line "  RENAME <key> <new-key> Rename a key")
     (tm:write-line "  QUIT                   Close the connection")
     (tm:write-line "")
     (tm:write-line "Input rules:")
     (tm:write-line "  - Whitespace separates tokens.")
     (tm:write-line "  - Use double quotes for a token with spaces:  SET a \"hello world\"")
     (tm:write-line "  - Escapes inside quotes: \\\\  \\\"  \\n  \\r  \\t")
     (tm:write-line "")
     (tm:write-line "Help:")
     (tm:write-line "  (empty line), \"\", -h, --help")))

  (declare is-help-line (String -> Boolean))
  (define (is-help-line line)
    (let t = (trim line))
    (cond
      ((== t "") True)
      ((== t "\"\"") True)
      ((== t "-h") True)
      ((== t "--help") True)
      (True False)))

  ;; Tokenizer:
  ;; - splits on whitespace
  ;; - supports double-quoted tokens:  SET k "hello world"
  ;; - supports backslash escapes inside quotes: \" \\ \n \r \t
  (declare tokenize-line (String -> (List String)))
  (define (tokenize-line line)
    (lisp (List String) (line)
      (cl:labels
          ((ws? (c)
             (cl:or (cl:char= c #\Space)
                    (cl:char= c #\Tab)
                    (cl:char= c #\Return)
                    (cl:char= c #\Newline)))

           (skip-ws (s i len)
             (cl:loop
               (cl:when (cl:>= i len) (cl:return i))
               (cl:if (ws? (cl:char s i))
                      (cl:incf i)
                      (cl:return i))))

           (escape-char (c)
             (cl:case c
               (#\n #\Newline)
               (#\r #\Return)
               (#\t #\Tab)
               (cl:t c)))

           (read-bare (s i len)
             (cl:let ((start i))
               (cl:loop
                 (cl:when (cl:>= i len) (cl:return))
                 (cl:when (ws? (cl:char s i)) (cl:return))
                 (cl:incf i))
               (cl:values (cl:subseq s start i) i)))

           ;; Assumes s[i] == #\"
           (read-quoted (s i len)
             ;; Consume opening quote
             (cl:incf i)
             (cl:let ((buf (cl:make-string-output-stream)))
               (cl:loop
                 (cl:when (cl:>= i len)
                   (cl:return))
                 (cl:let ((c (cl:char s i)))
                   (cl:cond
                     ;; Closing quote
                     ((cl:char= c #\")
                      (cl:incf i)
                      (cl:return))
                     ;; Escaping backslacsh
                     ((cl:char= c #\\)
                      (cl:incf i)
                      (cl:when (cl:< i len)
                        (cl:write-char (escape-char (cl:char s i)) buf)
                        (cl:incf i)))
                     (cl:t
                      (cl:write-char c buf)
                      (cl:incf i)))))
               (cl:values (cl:get-output-stream-string buf) i))))

        (cl:let* ((len (cl:length line))
                  (i (skip-ws line 0 len))
                  (out '()))
          (cl:loop
            (cl:when (cl:>= i len)
              (cl:return (cl:nreverse out)))

            (cl:multiple-value-bind (tok next-i)
                (cl:if (cl:char= (cl:char line i) #\")
                       (read-quoted line i len)
                       (read-bare line i len))
              (cl:push tok out)
              (cl:setf i (skip-ws line next-i len))))))))

  (declare parse-cli-line (String -> Result String (Optional Command)))
  (define (parse-cli-line line)
    (let tline = (trim line))
    (if (== tline "")
        (Ok None)
        (match (tokenize-line tline)
          ((Nil)
           (Ok None))
          ((Cons cmd rest)
           (let cmdU = (str-upcase cmd))
           (cond
             ((== cmdU "PING")
              (Ok (Some (Ping (match rest
                                ((Nil)
                                 None)
                                (_
                                 (Some (join-with-space rest))))))))
             ((== cmdU "GET")
              (match rest
                ((Cons key (Nil))
                 (Ok (Some (GetKey key))))
                (_
                 (Err "Usage: GET <key>"))))
             ((== cmdU "SET")
              (match rest
                ((Cons key more)
                 (match more
                   ((Nil)
                    (Err "Usage: SET <key> <value>"))
                   (_
                    (Ok (Some (SetKey key (join-with-space more)))))))
                (_
                 (Err "Usage: SET <key> <value>"))))
             ((== cmdU "RENAME")
              (match rest
                ((Cons key (Cons new-key (Nil)))
                 (Ok (Some (RenameKey key new-key))))
                (_
                 (Err "Usage: RENAME <key> <new-key>"))))
             ((== cmdU "QUIT")
              (match rest
                ((Nil) (Ok (Some Quit)))
                (_     (Err "Usage: QUIT"))))
             (True
              (Err (<> "Unknown command: " cmd))))))))
  )
