(cl:in-package :cl-user)

(defpackage :io/examples/redis
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/experimental/do-control-core
   #:coalton-library/experimental/do-control-loops
   #:io/utils
   #:io/monad-io
   #:io/simple-io
   #:io/threads
   #:io/conc/stm
   #:io/examples/redis/protocol
   )
  (:local-nicknames
   (:s #:coalton-library/string)
   (:v #:coalton-library/vector)
   (:hm #:coalton-library/hashmap)
   (:tm #:io/terminal)
   (:mt #:io/mutable-var)
   (:nt  #:io/sockets))
  (:export
   #:run-server
   #:run-client))

(in-package :io/examples/redis)

(named-readtables:in-readtable coalton:coalton)

;;;
;;; Small Utilities
;;;

(coalton-toplevel
  (declare mod-hash (Hash :h => :h -> UFix -> UFix))
  (define (mod-hash the-hash n)
    ;; Hash is implementation dependent, but will always be some kind of underlying integer.
    (lisp UFix (the-hash n)
      (cl:mod the-hash n)))
  )

(coalton-toplevel
  (define hostname "127.0.0.1")
  (define port (the UFix 5555))
  )

;;;
;;; Client code
;;;

(coalton-toplevel

  ;;;
  ;;; Response Printing
  ;;;

  (declare print-response (Resp -> IO Unit))
  (define (print-response resp)
    (do-match resp
      ((RespSimpleString str)
       (tm:write-line str))
      ((RespBulkString str)
       (tm:write-line str))
      ((RespInt n)
       (tm:write-line (as String n)))
      ((RespError msg)
       (tm:write-line (<> "Server returned an error: " msg)))
      ((RespNull)
       (tm:write-line "(nil)"))
      ((RespArray xs)
       (tm:write-line (<> "(array) " (force-string xs))))))

  (declare do-command (Command -> nt:ByteConnectionSocket -> IO Unit))
  (define (do-command cmd conn)
    (do
     (write-resp (command->resp cmd) conn)
     (resp? <- (read-resp conn))
     (do-match resp?
       ((Err e)
        (tm:write-line (<> "Protocol error: " e)))
       ((Ok resp)
        (print-response resp)))))

  ;;;
  ;;; CLI Parsing Helpers
  ;;;

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

  ;; --- Help ---

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
             ;; PING [payload...]
             ((== cmdU "PING")
              (Ok (Some (Ping (join-with-space rest)))))

             ;; GET key
             ((== cmdU "GET")
              (match rest
                ((Cons key (Nil))
                 (Ok (Some (GetKey key))))
                (_
                 (Err "Usage: GET <key>"))))

             ;; SET key value...
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

             ;; RENAME old new
             ((== cmdU "RENAME")
              (match rest
                ((Cons key (Cons new-key (Nil)))
                 (Ok (Some (RenameKey key new-key))))
                (_
                 (Err "Usage: RENAME <key> <new-key>"))))

             ;; QUIT
             ((== cmdU "QUIT")
              (match rest
                ((Nil) (Ok (Some Quit)))
                (_     (Err "Usage: QUIT"))))

             (True
              (Err (<> "Unknown command: " cmd))))))))

  ;;;
  ;;; Prompt + REPL
  ;;;

  (declare print-prompt (IO Unit))
  (define print-prompt
    (do
     (tm:write "redis> ")
     (wrap-io
      (lisp Unit ()
        (cl:finish-output)
        Unit))))

  (declare client-repl (nt:ByteConnectionSocket -> IO Unit))
  (define (client-repl conn)
    (rec % ()
      (do
       print-prompt
       (line <- tm:read-line)
       (if (is-help-line line)
           (do
            print-help
            (%))
           (do
            (cmd? <- (pure (parse-cli-line line)))
            (do-match cmd?
              ((Err e)
               (tm:write-line e)
               (%))
              ((Ok maybe-cmd)
               (match maybe-cmd
                 ((None)
                  (%))
                 ((Some cmd)
                  (do
                   (do-command cmd conn)
                   (match cmd
                     ((Quit)
                      (pure Unit))
                     (_
                      (%)))))))))))))

  (declare client-main (IO Unit))
  (define client-main
    (nt:do-byte-socket-connect-with (conn (hostname port))
     (tm:write-line (<> "connected to " (<> hostname (<> ":" (as String port)))))
     (tm:write-line "Type --help for commands.")
     (client-repl conn))))

(cl:defun run-client ()
  (coalton (run-io! client-main)))

;;;
;;; Server code
;;;

(coalton-toplevel

  (define-struct (TStripedMap :k :v)
    "A TStripedMap contains a set of N buckets. Each bucket is an immutable hashmap from
:k -> :v, synchronized in a transaction variable (TVar). Key/value pairs are stored in
the (hash :k MOD N-BUCKETS)'th bucket.

Splitting the map into buckets reduces contention. If the map were stored in one
monolithic TVar, then any write to the map would force all other read and writes to retry.
Two threads trying to write to the map will only have a 1/N chance of conflicting, and
when a write does occur it will only force other transactions operating on *that bucket*
to retry."
    ;; NOTE: Uses a mutable vector internally for efficient access. Once initialized, the
    ;; vector should never be mutated!
    (buckets (Vector (TVar (hm:HashMap :k :v)))))

  (define-type-alias Database (TStripedMap String String))

  (declare new-database (UFix -> IO Database))
  (define (new-database n-buckets)
    "Creates a new database with N-BUCKETS buckets. (Will always create at least 1 bucket.)"
    (do
     (vector <- (wrap-io (v:with-capacity (max n-buckets 1))))
     (do-loop-times (_ (max n-buckets 1))
       (bucket <- (new-tvar hm:empty))
       (wrap-io
        (v:push! bucket vector)))
     (pure (TStripedMap vector))))

  (inline)
  (declare db-length (Database -> UFix))
  (define (db-length db)
    (v:length (.buckets db)))

  (inline)
  (declare bucket-for (String -> Database -> TVar (hm:HashMap String String)))
  (define (bucket-for key db)
    (v:index-unsafe (mod-hash (hash key) (db-length db))
                    (.buckets db)))

  (declare write-key-tx (String -> String -> Database -> STM IO Unit))
  (define (write-key-tx key val db)
    (let bucket = (bucket-for key db))
    (do
     (bucket-map <- (read-tvar bucket))
     (write-tvar bucket (hm:insert bucket-map key val))))

  (declare read-key (String -> Database -> IO (Optional String)))
  (define (read-key key db)
    (do
     (let bucket = (bucket-for key db))
     ;; NOTE: To minimize the time in the transaction, just read the bucket inside the
     ;; transaction, and descend to grab the value after the transaction completes.
     (bucket-map <- (run-tx (read-tvar bucket)))
     (pure (hm:lookup bucket-map key))))

  (declare rename-key (String -> String -> Database -> STM IO Boolean))
  (define (rename-key key new-key db)
    "Rename a key in the database. Returns True if the original key was found, False if
it was missing."
    (let bucket = (bucket-for key db))
    (do
     (bucket-map <- (read-tvar bucket))
     (let (Tuple bucket-map2 val?) =
       (hm:update bucket-map key
                  (fn (k?)
                    (match k?
                      ((None)
                       (Tuple None None))
                      ((Some val)
                       (Tuple None (Some val)))))))
     (do-match val?
       ((None)
        (pure False))
       ((Some val)
        (write-tvar bucket bucket-map2)
        (write-key-tx new-key val db)
        (pure True))))))

(coalton-toplevel

  (declare handle-ping (String -> IO Resp))
  (define (handle-ping ping-str)
    (if (== ping-str "")
        (pure (RespSimpleString "PONG"))
        (pure (RespBulkString ping-str))))

  (declare handle-client (nt:ByteConnectionSocket -> Database -> IO Unit))
  (define (handle-client conn db)
    (do
     (cmd? <- (read-command conn))
     (tm:write-line (<> "Received command: " (force-string cmd?)))
     (do-match cmd?
       ((Err _)
        (handle-client conn db))
       ((Ok cmd)
        (resp <-
          (do-match cmd
            ((Ping ping-str)
             (handle-ping ping-str))
            ((GetKey key)
             (value? <- (read-key key db))
             (match value?
               ((None)
                (pure RespNull))
               ((Some val)
                (pure (RespBulkString val)))))
            ((SetKey key val)
             (run-tx (write-key-tx key val db))
             (pure (RespSimpleString "OK")))
            ((RenameKey key new-key)
             (result <- (run-tx (rename-key key new-key db)))
             (if result
                 (pure (RespSimpleString "OK"))
                 (pure (RespError (<> "Key not found: " key)))))
            ((Quit)
             (pure (RespSimpleString "OK")))))
        (write-resp resp conn)
        (match cmd
          ((Quit)
           (pure Unit))
          (_
           (handle-client conn db)))))))

  (declare accept-loop (nt:ByteServerSocket -> Database -> IO Unit))
  (define (accept-loop server db)
    (do
      (nt:do-byte-socket-accept-fork-with (conn (server))
        (the (IO Unit) (tm:write-line "client connected"))
        (handle-client conn db))
      (accept-loop server db)))

  (declare server-main (IO Unit))
  (define server-main
    (do
      (db <- (new-database 16))
      (nt:do-byte-socket-listen-with (server (hostname port))
        (tm:write-line (<> "listening on " (<> hostname (<> ":" (as String port)))))
        (accept-loop server db))))
  )

(cl:defun run-server ()
  (coalton (run-io! server-main)))
