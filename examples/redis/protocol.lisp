(cl:in-package :cl-user)

(defpackage :io/examples/redis/protocol
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/experimental/do-control-core
   #:coalton-library/experimental/do-control-loops
   #:io/utils
   #:io/monad-io
   #:io/simple-io
   #:io/thread
   #:io/conc/stm
   )
  (:local-nicknames
   (:v #:coalton-library/vector)
   (:s #:coalton-library/string)
   (:hm #:coalton-library/hashmap)
   (:tm #:io/term)
   (:mt #:io/mut)
   (:nt  #:io/network))
  (:export
   #:Resp
   #:RespArray
   #:RespInt
   #:RespSimpleString
   #:RespError
   #:RespBulkString
   #:RespNull

   #:Command
   #:Ping
   #:SetKey
   #:GetKey
   #:RenameKey
   #:Quit

   #:read-resp
   #:read-command
   #:write-resp
   #:command->resp
   ))

(in-package :io/examples/redis/protocol)

(named-readtables:in-readtable coalton:coalton)

;; NOTE There are more elegant ways to implement parsers like this, but for the sake
;; of the example, this uses a simple - but pretty verbose - style of just manually
;; unpacking a lot of options and results, and lots of iterative writes and reads from
;; the socket connection.

;;;
;;; Protocol (Used on Client & Server)
;;;

(coalton-toplevel
  ;; Implement a (small) subset of the RESP3 protocol.
  ;; See https://redis.io/docs/latest/develop/reference/protocol-spec/#sending-commands-to-a-redis-server
  ;;
  ;; The biggest difference is that this only implements ASCII characters, not full UTF-8 strings. It
  ;; would be easy to convert this to use UTF-8 using the Babel Common Lisp package.

  (declare char->u8 (Char -> U8))
  (define (char->u8 c)
    (lisp U8 (c)
      (cl:the (cl:unsigned-byte 8) (cl:char-code c))))

  (declare bytes->int (Vector U8 -> Integer))
  (define (bytes->int v)
    ;; "This type is a CRLF-terminated string that represents a signed, base-10, 64-bit integer."
    ;; https://redis.io/docs/latest/develop/reference/protocol-spec/#integers
    (lisp Integer (v)
      (cl:parse-integer
       (cl:map 'cl:string (cl:lambda (b) (cl:code-char b)) v))))

  (declare bytes->str (Vector U8 -> String))
  (define (bytes->str v)
    (lisp String (v)
      (cl:coerce
       (cl:map 'cl:list (cl:lambda (b) (cl:code-char b)) v)
       'cl:string)))

  (define term-1 (char->u8 #\Return))
  (define term-2 (char->u8 #\Newline))

  (define array-type-char (char->u8 #\*))
  (define simple-string-type-char (char->u8 #\+))
  (define simple-error-type-char (char->u8 #\-))
  (define int-type-char (char->u8 #\:))
  (define bulk-str-type-char (char->u8 #\$))
  (define null-type-char (char->u8 #\_))

  (declare read-one-byte (nt:ByteConnectionSocket -> IO U8))
  (define (read-one-byte conn)
    (do
     (buffer <- (nt:read-exactly 1 conn))
     (pure (v:index-unsafe 0 buffer))))

  (declare read-until-terminator (nt:ByteConnectionSocket -> IO (Vector U8)))
  (define (read-until-terminator conn)
    (do
     (buffer <- (wrap-io (v:new)))
     (bits-12 <- (nt:read-exactly 2 conn))
     (rec % ((c1-val (v:index-unsafe 0 bits-12))
             (c2-val (v:index-unsafe 1 bits-12)))
       (if (and (== c1-val term-1)
                (== c2-val term-2))
           (pure buffer)
           (do
            (wrap-io (v:push! c1-val buffer))
            (next-byte <- (nt:read-exactly 1 conn))
            (% c2-val (v:index-unsafe 0 next-byte)))))))

  (define-type Resp
    (RespArray (Vector Resp))
    (RespInt Integer)
    (RespSimpleString String)
    (RespError String)
    (RespBulkString String)
    RespNull)

  (declare resp->resp-arry (Resp -> Optional (Vector Resp)))
  (define (resp->resp-arry resp)
    "Get the (possible) array out of a RespArray."
    (match resp
      ((RespArray data)
       (Some data))
      (_
       None)))

  (declare resp->resp-bulk-str (Resp -> Optional String))
  (define (resp->resp-bulk-str resp)
    "Get the (possible) stringh out of a RespBulkStr."
    (match resp
      ((RespBulkString str)
       (Some str))
      (_
       None)))

  (declare read-resp-arry (nt:ByteConnectionSocket -> IO (Result String Resp)))
  (define (read-resp-arry conn)
    "Read a resp-array. Assumes the first byte, signifying the type, has already been read.

Example stream input, where the '*' type byte has already been read:
  *2\r\n$5\r\nhello\r\n$5\r\nworld\r\n"
    (do
     (num-elements-bytes <- (read-until-terminator conn))
     (do-match (tryinto (bytes->int num-elements-bytes))
       ((Err e)
        (pure (Err e)))
       ((Ok num-elements)
        (buffer <- (wrap-io (v:with-capacity num-elements)))
        (result? <-
          (rec % ()
            (if (< (v:length buffer) num-elements)
                (do
                 (elt? <- (read-resp conn))
                 (do-match elt?
                   ((Err e)
                    (pure (Err e)))
                   ((Ok elt)
                    (wrap-io (v:push! elt buffer))
                    (%))))
                (pure (Ok Unit)))))
        (match result?
          ((Err e)
           (pure (Err e)))
          ((Ok _)
           (pure (Ok (RespArray buffer)))))))))

  (declare read-simple-string (nt:ByteConnectionSocket -> IO (Result String Resp)))
  (define (read-simple-string conn)
    "Read a resp-simple-string. Assumes the first byte, signifying the type, has been read.

Example stream input, where the '+' type byte has already been read:
  +OK\r\n"
    (do
     (input <- (read-until-terminator conn))
     (pure (Ok (RespSimpleString (bytes->str input))))))

  (declare read-simple-error (nt:ByteConnectionSocket -> IO (Result String Resp)))
  (define (read-simple-error conn)
    "Read a resp-simple-error. Assumes the first byte, signifying the type, has been read.

Example stream input, where the '-' type byte has already been read:
  -Error Message: Unknown Command 'lngth'\r\n"
    (do
     (input <- (read-until-terminator conn))
     (pure (Ok (RespError (bytes->str input))))))

  (declare read-integer (nt:ByteConnectionSocket -> IO (Result String Resp)))
  (define (read-integer conn)
    "Read a resp-integer. Assumes the first byte, signifying the type, has been read.

Example stream input, where the ':' type byte has already been read:
  :-1243\r\n"
    (do
     (input <- (read-until-terminator conn))
     (wrap-io
      (catch (Ok (RespInt (bytes->int input)))
        (_ (Err "Malformed integer"))))))

  (declare read-bulk-string (nt:ByteConnectionSocket -> IO (Result String Resp)))
  (define (read-bulk-string conn)
    "Read a length-defined bulk string that can include \r\n terminators.
Assumes the first byte, signifying the type, has been read.

Example stream input, where the '$' type byte has already been read:
  $5\r\nhello\r\n"
    (do
     (length-bytes <- (read-until-terminator conn))
     (do-match (tryinto (bytes->int length-bytes))
       ((Err e)
        (pure (Err (<> "Error parsing bulk string length: " e))))
       ((Ok length)
        (str-bytes <- (nt:read-exactly length conn))
        ;; Consume the trailing \r\n delimiter
        (read-until-terminator conn)
        (pure (Ok (RespBulkString (bytes->str str-bytes))))))))

  (declare read-null (nt:ByteConnectionSocket -> IO (Result String Resp)))
  (define (read-null conn)
    "Read a resp-null. Assumes the first byte, signifying the type, has been read.

Example stream input, where the '_' type byte has already been read:
  _\r\n"
    (do
     (read-until-terminator conn)
     (pure (Ok RespNull))))

  (declare read-resp (nt:ByteConnectionSocket -> IO (Result String Resp)))
  (define (read-resp conn)
    (do
     (type-byte <- (read-one-byte conn))
     (cond
       ((== array-type-char type-byte)
        (read-resp-arry conn))
       ((== simple-string-type-char type-byte)
        (read-simple-string conn))
       ((== simple-error-type-char type-byte)
        (read-simple-error conn))
       ((== int-type-char type-byte)
        (read-integer conn))
       ((== bulk-str-type-char type-byte)
        (read-bulk-string conn))
       ((== null-type-char type-byte)
        (read-null conn))
       (True
        (pure (Err (build-str "Unknown type byte: " (force-string type-byte))))))))
  )

(coalton-toplevel
  (declare int->bytes (Integer -> (Vector U8)))
  (define (int->bytes n)
    (lisp (Vector U8) (n)
      (cl:let* ((s (cl:format cl:nil "~D" n))
                (len (cl:length s))
                (out (cl:make-array len :element-type '(cl:unsigned-byte 8))))
        (cl:dotimes (i len out)
          (cl:setf (cl:aref out i)
                   (cl:the (cl:unsigned-byte 8)
                           (cl:char-code (cl:aref s i))))))))

  (declare str->bytes (String -> (Vector U8)))
  (define (str->bytes s)
    (lisp (Vector U8) (s)
      (cl:let* ((len (cl:length s))
                (out (cl:make-array len :element-type '(cl:unsigned-byte 8))))
        (cl:dotimes (i len out)
          (cl:setf (cl:aref out i)
                   (cl:the (cl:unsigned-byte 8)
                           (cl:char-code (cl:aref s i))))))))

  (declare write-terminator (nt:ByteConnectionSocket -> IO Unit))
  (define (write-terminator conn)
    (nt:write-bytes (v:make term-1 term-2) conn))

  (declare write-resp-array (Vector Resp -> nt:ByteConnectionSocket -> IO Unit))
  (define (write-resp-array data conn)
    (do
     (nt:write-bytes (v:make array-type-char) conn)
     (nt:write-bytes (int->bytes (into (v:length data)))
                     conn)
     (write-terminator conn)
     (do-foreach-io_ (x data)
       (write-resp x conn))))

  (declare write-resp-int (Integer -> nt:ByteConnectionSocket -> IO Unit))
  (define (write-resp-int x conn)
    (do
     (nt:write-bytes (v:make int-type-char) conn)
     (nt:write-bytes (int->bytes x) conn)
     (write-terminator conn)))

  (declare write-resp-simple-string (String -> nt:ByteConnectionSocket -> IO Unit))
  (define (write-resp-simple-string str conn)
    (do
     (nt:write-bytes (v:make simple-string-type-char) conn)
     (nt:write-bytes (str->bytes str) conn)
     (write-terminator conn)))

  (declare write-resp-simple-error (String -> nt:ByteConnectionSocket -> IO Unit))
  (define (write-resp-simple-error str conn)
    (do
     (nt:write-bytes (v:make simple-error-type-char) conn)
     (nt:write-bytes (str->bytes str) conn)
     (write-terminator conn)))

  (declare write-resp-bulk-string (String -> nt:ByteConnectionSocket -> IO Unit))
  (define (write-resp-bulk-string str conn)
    (do
     (nt:write-bytes (v:make bulk-str-type-char) conn)
     (nt:write-bytes (int->bytes (into (s:length str)))
                     conn)
     (write-terminator conn)
     (nt:write-bytes (str->bytes str) conn)
     (write-terminator conn)))

  (declare write-resp-null (nt:ByteConnectionSocket -> IO Unit))
  (define (write-resp-null conn)
    (do
     (nt:write-bytes (v:make null-type-char) conn)
     (write-terminator conn)))

  (declare write-resp (Resp -> nt:ByteConnectionSocket -> IO Unit))
  (define (write-resp resp conn)
    (do
     (match resp
       ((RespArray data)
        (write-resp-array data conn))
       ((RespInt x)
        (write-resp-int x conn))
       ((RespSimpleString str)
        (write-resp-simple-string str conn))
       ((RespError str)
        (write-resp-simple-error str conn))
       ((RespBulkString str)
        (write-resp-bulk-string str conn))
       ((RespNull)
        (write-resp-null conn)))))
  )

(coalton-toplevel
  (define-type Command
    (Ping String)
    Quit
    (GetKey String)
    (SetKey String String)
    (RenameKey String String)
    )

  (define ping-command-str "PING")
  (define quit-command-str "QUIT")
  (define get-command-str "GET")
  (define set-command-str  "SET")
  (define rename-command-str "RENAME")

  (declare parse-ping (Vector Resp -> Result String Command))
  (define (parse-ping data)
    (match (v:index 1 data)
      ((None)
       (Err "Ping missing string payload."))
      ((Some str-payload)
       (match (resp->resp-bulk-str str-payload)
         ((None)
          (Err "Bad type: Ping expuects bulk-str payload."))
         ((Some str)
          (Ok (Ping str)))))))

  (declare parse-get (Vector Resp -> Result String Command))
  (define (parse-get data)
    (match (v:index 1 data)
      ((None)
       (Err "Get missing string payload."))
      ((Some str-payload)
       (match (resp->resp-bulk-str str-payload)
         ((None)
          (Err "Bad type: Get expects bulk-str payload."))
         ((Some str)
          (Ok (GetKey str)))))))

  (declare parse-set (Vector Resp -> Result String Command))
  (define (parse-set data)
    (match (Tuple (v:index 1 data) (v:index 2 data))
      ((Tuple (Some key-resp) (Some val-resp))
       (match (Tuple (resp->resp-bulk-str key-resp)
                     (resp->resp-bulk-str val-resp))
         ((Tuple (Some key) (Some val))
          (Ok (SetKey key val)))
         (_
          (Err "Bad type: Set expects bulk-str payloads."))))
      (_
       (Err "Set missing payloads"))))

  (declare parse-rename (Vector Resp -> Result String Command))
  (define (parse-rename data)
    (match (Tuple (v:index 1 data) (v:index 2 data))
      ((Tuple (Some key-resp) (Some new-key-resp))
       (match (Tuple (resp->resp-bulk-str key-resp)
                     (resp->resp-bulk-str new-key-resp))
         ((Tuple (Some key) (Some new-key))
          (Ok (RenameKey key new-key)))
         (_
          (Err "Bad type: Set expects bulk-str payloads."))))
      (_
       (Err "Set missing payloads"))))

  (declare parse-quit (Vector Resp -> Result String Command))
  (define (parse-quit _)
    (Ok Quit))

  (declare parse-command (Resp -> Result String Command))
  (define (parse-command resp)
    "Parse a RespArray into a Command to run on the server."
    (match (resp->resp-arry resp)
      ((None)
       (Err (<> "Expected RespArray, received Resp data: " (force-string resp))))
      ((Some data)
       (match (v:index 0 data)
         ((None)
          (Err "Command data was empty."))
         ((Some resp-1)
          (match (resp->resp-bulk-str resp-1)
            ((None)
             (Err "Expected command string in initil position."))
            ((Some command-str)
             (cond
               ((== ping-command-str command-str)
                (parse-ping data))
               ((== get-command-str command-str)
                (parse-get data))
               ((== set-command-str command-str)
                (parse-set data))
               ((== rename-command-str command-str)
                (parse-rename data))
               ((== quit-command-str command-str)
                (parse-quit data))
               (True
                (Err (<> "Received unknown command string: " command-str)))))))))))

  (declare read-command (nt:ByteConnectionSocket -> IO (Result String Command)))
  (define (read-command conn)
    (do
     (resp <- (read-resp conn))
     (do-match resp
       ((Err e)
        (pure (Err e)))
       ((Ok resp)
        (pure (parse-command resp))))))
  )

(coalton-toplevel
  (declare command->resp (Command -> Resp))
  (define (command->resp cmd)
    (match cmd
      ((Ping ping-str)
       (RespArray (v:make (RespBulkString ping-command-str)
                          (RespBulkString ping-str))))
      ((GetKey key)
       (RespArray (v:make (RespBulkString get-command-str)
                          (RespBulkString key))))
      ((SetKey key val)
       (RespArray (v:make (RespBulkString set-command-str)
                          (RespBulkString key)
                          (RespBulkString val))))
      ((RenameKey key new-key)
       (RespArray (v:make (RespBulkString rename-command-str)
                          (RespBulkString key)
                          (RespBulkString new-key))))
      ((Quit)
       (RespArray (v:make (RespBulkString quit-command-str))))))
  )
