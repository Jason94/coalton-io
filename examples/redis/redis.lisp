(cl:in-package :cl-user)

(defpackage :io/examples/redis
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/experimental/do-control-core
   #:io/utils
   #:io/exceptions
   #:io/monad-io
   #:io/simple-io
   #:io/resource
   #:io/thread
   #:io/conc/stm
   #:io/examples/redis/protocol
   #:io/examples/redis/cli
   #:io/examples/redis/rw-lock
   )
  (:import-from #:coalton/experimental/do-control-loops
   #:do-loop-times
   #:do-collect
   #:do-loop-while-valM
   )
  (:local-nicknames
   (:l #:coalton-library/list)
   (:opt #:coalton-library/optional)
   (:s #:coalton-library/string)
   (:v #:coalton-library/vector)
   (:hm #:coalton-library/hashmap)
   (:f #:coalton-library/file)
   (:tm #:io/term)
   (:mt #:io/mut)
   (:io-f #:io/file)
   (:nt  #:io/network))
  (:export
   #:run-server
   #:run-client))

(in-package :io/examples/redis)

(named-readtables:in-readtable coalton:coalton)

;;;
;;; Small Utilities
;;;

(coalton-toplevel
  (declare contains? (Eq :a => :a * List :a -> Boolean))
  (define (contains? elt lst)
    (opt:some? (l:elemindex elt lst)))

  (declare mod-hash (Hash :h => :h * UFix -> UFix))
  (define (mod-hash the-hash n)
    ;; Hash is implementation dependent, but will always be some kind of underlying integer.
    (lisp (-> UFix) (the-hash n)
      (cl:mod the-hash n)))
  )

(coalton-toplevel
  (define hostname "127.0.0.1")
  (define port (the UFix 5556))
  ;; NOTE: The real Redis program uses "dump.rdb", but because our file format is simpler, we use a more honest file extension.
  (define filename "dump.resp")
  (define OK-Response (RespSimpleString "OK"))
  )

;;; ---------------------------------------------- ;;;
;;;                   Client code                  ;;;
;;; ---------------------------------------------- ;;;

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

  (declare do-command (Command * nt:ByteConnectionSocket -> IO Boolean))
  (define (do-command cmd conn)
    "Returns True if the client should continue, False if it should terminate."
    (do
     (write-resp (command->resp cmd) conn)
     (handle
      (do
       (resp? <- (read-resp conn))
       (do-match resp?
         ((Err e)
          (tm:write-line (<> "Protocol error: " e)))
         ((Ok resp)
          (print-response resp)))
       (pure True))
      (fn (e)
        (do-match e
          ((nt:EndOfFileException _)
           (tm:write-line "Error: Server closed the connection")
           (pure False)))))))

  ;;;
  ;;; Prompt + REPL
  ;;;

  (declare print-prompt (IO Unit))
  (define print-prompt
    (do
     (tm:write "redis> ")
     (wrap-io
      (lisp (-> Unit) ()
        (cl:finish-output)
        Unit))))

  (declare client-repl (nt:ByteConnectionSocket -> IO Unit))
  (define (client-repl conn)
    (do
     print-prompt
     (line <- tm:read-line)
      (if (is-help-line line)
          (do
           print-help
           (client-repl conn))
          (do
           (cmd? <- (pure (parse-cli-line line)))
           (do-match cmd?
             ((Err e)
              (tm:write-line e)
              (client-repl conn))
             ((Ok maybe-cmd)
              (match maybe-cmd
                ((None)
                 (client-repl conn))
                ((Some cmd)
                 (do-whenM (do-command cmd conn)
                   (match cmd
                     ((Quit)
                      (pure Unit))
                     (_
                      (client-repl conn))))))))))))

  (declare client-main (IO Unit))
  (define client-main
    (nt:do-byte-socket-connect-with (conn (hostname port))
     (tm:write-line (<> "connected to " (<> hostname (<> ":" (as String port)))))
     (tm:write-line "Type --help for commands.")
     (client-repl conn))))

(cl:defun run-client ()
  (coalton (run-io! client-main)))

;;; ---------------------------------------------- ;;;
;;;                   Server code                  ;;;
;;; ---------------------------------------------- ;;;

;;;
;;; Implement the database in a custom STM data structure.
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

  (define-struct Database
    (data (TStripedMap String String))
    (lock TRWLock))

  (declare new-database (UFix -> IO Database))
  (define (new-database n-buckets)
    "Creates a new database with N-BUCKETS buckets. (Will always create at least 1 bucket.)"
    (do
     (vector <- (wrap-io (v:with-capacity (max n-buckets 1))))
     (do-loop-times (_ (max n-buckets 1))
       (bucket <- (new-tvar hm:empty))
       (wrap-io
        (v:push! bucket vector)))
     (lock <- new-trwlock)
     (pure (Database (TStripedMap vector)
                     lock))))

  (define-type-alias Bucket (hm:HashMap String String))

  (declare copy-buckets (Database -> IO (List Bucket)))
  (define (copy-buckets db)
    "Retrieve a synchronized list of all the buckets in the database."
    (run-tx
     (do-collect (bucket-tvar (.buckets (.data db)))
       (read-tvar bucket-tvar))))

  (declare write-buckets (Database * Vector (TVar Bucket) -> IO Unit))
  (define (write-buckets db new-buckets)
    "Update the data in DB with the data in NEW-BUCKETS."
    (do
     (let buckets = (.buckets (.data db)))
     (run-tx
      (do-loop-times (i (v:length buckets))
        (new-bucket <- (read-tvar (v:index-unsafe i new-buckets)))
        (write-tvar (v:index-unsafe i buckets)
                    new-bucket)))))

  (inline)
  (declare db-length (Database -> UFix))
  (define (db-length db)
    (v:length (.buckets (.data db))))

  (inline)
  (declare bucket-for (String * Database -> TVar Bucket))
  (define (bucket-for key db)
    (v:index-unsafe (mod-hash (hash key) (db-length db))
                    (.buckets (.data db))))

  (declare write-key-tx (String * String * Database -> STM IO Unit))
  (define (write-key-tx key val db)
    (let bucket = (bucket-for key db))
    (do
     (bucket-map <- (read-tvar bucket))
     (write-tvar bucket (hm:insert bucket-map key val))))

  (declare read-key (String * Database -> IO (Optional String)))
  (define (read-key key db)
    (do
     (let bucket = (bucket-for key db))
     ;; NOTE: To minimize the time in the transaction, just read the bucket inside the
     ;; transaction, and descend to grab the value after the transaction completes.
     (bucket-map <- (run-tx (read-tvar bucket)))
     (pure (hm:lookup bucket-map key))))

  (declare rename-key (String * String * Database -> STM IO Boolean))
  (define (rename-key key new-key db)
    "Rename a key in the database. Returns True if the original key was found, False if
it was missing."
    (let bucket = (bucket-for key db))
    (do
     (bucket-map <- (read-tvar bucket))
     (let (values bucket-map2 val?) =
       (hm:update bucket-map key
                  (fn (k?)
                    (match k?
                      ((None)
                       (values None None))
                      ((Some val)
                       (values None (Some val)))))))
     (do-match val?
       ((None)
        (pure False))
       ((Some val)
        (write-tvar bucket bucket-map2)
        (write-key-tx new-key val db)
        (pure True)))))

  (declare save-buckets (String * List Bucket -> IO Unit))
  (define (save-buckets filename buckets)
    "Dump the contents of BUCKETS into FILENAME as a RESP-encoded snapshot.

Each key/value pair is written as a RESP array of two bulk strings: the key and the value.

This is a custom file format and is not compatible with Redis RDB files.

For more information on the real Redis file format, for the curious:
https://rdb.fnordig.de/file_format.html"
    (io-f:do-with-open-file_ (filename fs :direction f:Output
                                          :if-exists f:Overwrite)
      ;; Peg type of FS to (FileStream U8)
      (let _ = (the (f:FileStream U8) fs))
      ;; For efficiency, allocate one buffer containing two Resp elements. Each loop,
      ;; reuse it to create a new RespArray.
      ;;
      ;; Even though IO doesn't have a builtin mutable vector type, it's easy to use wrap-io
      ;; to perform normal, side-effectul Coalton code.
      (buffer <- (wrap-io (v:make RespNull RespNull)))
      (do-foreach-io_ (bucket buckets)
        (do-foreach-io_ ((Tuple key val) bucket)
          (wrap-io
           (v:set! 0 (RespBulkString key) buffer)
           (v:set! 1 (RespBulkString val) buffer)
           Unit)
          (write-resp (RespArray buffer) fs)))))

  (declare next-resp-item (f:FileStream U8 -> IO (Optional Resp)))
  (define (next-resp-item fs)
    "Try to read the next response item from a file stream."
    (do
     (result? <- (try-all (read-resp fs)))
     (match result?
       ((None)
        (pure None))
       ((Some (Err _))
        (pure None))
       ((Some (Ok val))
        (pure (Some val))))))

  (declare load-dump-file (String * UFix -> IO (Result String Database)))
  (define (load-dump-file filename n-buckets)
    "Load the contents of a dump file into a fresh database with N-BUCKETS buckets."
    (do
     (db <- (new-database n-buckets))
     (io-f:do-with-open-file_ (filename fs)
       (do-loop-while-valM (resp-item (next-resp-item fs))
         (do-when-match resp-item (RespArray data)
           (do-when (== (v:length data) 2)
             (do-when-match (Tuple (v:index-unsafe 0 data) (v:index-unsafe 1 data))
                            (Tuple (RespBulkString key) (RespBulkString val))
               (run-tx
                (write-key-tx key val db)))))))
     (pure (Ok db))))
  )

;; Helper macros to use the DB's lock

(defmacro do-with-reader-lock (db cl:&body body)
  `(with-reader-lock (.lock ,db)
     (do
      ,@body)))

(defmacro do-with-writer-lock (db cl:&body body)
  `(with-writer-lock (.lock ,db)
     (do
      ,@body)))

;;;
;;; Implement the server that talks with the client and manages the database.
;;;

(coalton-toplevel

  (declare handle-ping (Optional String -> IO Resp))
  (define (handle-ping ping-str?)
    (match ping-str?
      ((None)
       (pure (RespSimpleString "PONG")))
      ((Some ping-str)
       (pure (RespBulkString ping-str)))))

  (declare handle-get-key (String * Database -> IO Resp))
  (define (handle-get-key key db)
    (do
     (value? <-
       (do-with-reader-lock db
         (read-key key db)))
     (match value?
       ((None)
        (pure RespNull))
       ((Some val)
        (pure (RespBulkString val))))))

  (declare handle-set-key (String * String * Database -> IO Resp))
  (define (handle-set-key key val db)
    (do
     (do-with-writer-lock db
       (run-tx (write-key-tx key val db)))
     (pure OK-Response)))

  (declare handle-rename-key (String * String * Database -> IO Resp))
  (define (handle-rename-key key new-key db)
    (do
     (result <-
       (do-with-writer-lock db
         (run-tx (rename-key key new-key db))))
     (if result
         (pure OK-Response)
         (pure (RespError (<> "Key not found: " key))))))

  (declare handle-save (Database -> IO Resp))
  (define (handle-save db)
    (do
     (buckets <-
       (do-with-reader-lock db
         (copy-buckets db)))
     (save-buckets filename buckets)
     (pure OK-Response)))

  (declare handle-load (Database -> IO Resp))
  (define (handle-load db)
    (do
     (new-buckets? <-
       (load-dump-file filename (db-length db)))
     (do-match new-buckets?
       ((Err e)
        (pure (RespError e)))
       ((Ok new-buckets)
        (do-with-writer-lock db
          (write-buckets db (.buckets (.data new-buckets))))
        (pure Ok-Response)))))

  (declare handle-client (nt:ByteConnectionSocket * Database -> IO Unit))
  (define (handle-client conn db)
    (do
     (rec % ()
       (do
        (cmd? <- (read-command conn))
        (do-match cmd?
          ((Err _)
           (%))
          ((Ok cmd)
           (tm:write-line (<> "Received command: " (force-string cmd)))
           (resp <-
                 (do-match cmd
                   ((Ping ping-str?)
                    (handle-ping ping-str?))
                   ((GetKey key)
                    (handle-get-key key db))
                   ((SetKey key val)
                    (handle-set-key key val db))
                   ((RenameKey key new-key)
                    (handle-rename-key key new-key db))
                   ((Quit)
                    (pure OK-Response))
                   ((Save)
                    (handle-save db))
                   ((Load)
                    (handle-load db))
                   ))
           (tm:write-line (<> "Sending response: " (force-string resp)))
           (write-resp resp conn)
           (match cmd
             ((Quit)
              (pure Unit))
             (_
              (%)))))))
     (tm:write-line "client disconnected")))

  (declare accept-loop (nt:ByteServerSocket * Database -> IO Unit))
  (define (accept-loop server db)
    (do
      (nt:do-byte-socket-accept-fork-with (conn (server))
        (the (IO Unit) (tm:write-line "client connected"))
        (handle-client conn db))
      (accept-loop server db)))

  (declare server-main (IO Unit))
  (define server-main
    (do
     (do-fork-thread
      (db <- (new-database 16))
      (nt:do-byte-socket-listen-with (server (hostname port))
        (tm:write-line (<> "listening on " (<> hostname (<> ":" (as String port)))))
        (accept-loop server db)))
     (tm:write-line "Press enter to close the server...")
     tm:read-line
     (pure Unit)))
  )

(cl:defun run-server ()
  (coalton (run-io! server-main)))
