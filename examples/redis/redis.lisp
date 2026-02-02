(cl:in-package :cl-user)

(defpackage :io/examples/redis
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/experimental/do-control-core
   #:coalton-library/experimental/do-control-loops
   #:io/utils
   #:io/exceptions
   #:io/monad-io
   #:io/simple-io
   #:io/resource
   #:io/thread
   #:io/conc/stm
   #:io/examples/redis/protocol
   #:io/examples/redis/cli
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
  (declare contains? (Eq :a => :a -> List :a -> Boolean))
  (define (contains? elt lst)
    (opt:some? (l:elemindex elt lst)))

  (declare mod-hash (Hash :h => :h -> UFix -> UFix))
  (define (mod-hash the-hash n)
    ;; Hash is implementation dependent, but will always be some kind of underlying integer.
    (lisp UFix (the-hash n)
      (cl:mod the-hash n)))
  )

(coalton-toplevel
  (define hostname "127.0.0.1")
  (define port (the UFix 5558))
  (define filename "dump.rdb")
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

  (declare do-command (Command -> nt:ByteConnectionSocket -> IO Boolean))
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
      (lisp Unit ()
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
;;; Implement an STM readers/writer lock.
;;;
;;; One of the Redis commands (SAVE) specifically instructs that other commands should block
;;; while it's writing to the disk, which needs to be done outside of the transaction.
;;; To solve this, we implement a readers/writer lock *in the STM* using a (TVar Boolean). Any
;;; commands that don't need to synchronize database access check the lock out as a "reader".
;;; Any commands that need to guarantee exclusive access to the database (just SAVE in this
;;; example) check the lock out as a "writer." The RW lock guarantees that any number of
;;; "reader" commands can run simultaneously, but if any single "writer" command is running,
;;; it guarantees it has exclusive access.
;;;
;;; Note: A reader/writer lock is the standard name for this kind of lock. It's a bit of a
;;; misnomer in our case, because several of the commands that are "reader" commands actually
;;; *do* write to the database. That's because the STM handles actually synchronizing the
;;; data automatically, so the purpose of this lock is to handle a different level of access
;;; control. But we're following the standard naming conventions for the kind of data structure
;;; implemented here.
;;;
;;; This demonstrates one of the great benefits of an STM: it's surprisingly good at expressing
;;; concurrency control constructs like locks, queues, etc., not just sychronizing shared memory.
;;; Although the Redis program doesn't make full use of this, the retry and or-else mechanisms
;;; in the STM make these kind of tools surprisingly powerful. For example, a transaction could
;;; try to acquire a lock before entering sub-transaction A, but if acquiring the lock fails,
;;; it could enter sub-transaction B instead.
;;;

(coalton-toplevel

  (define-struct TRWLock
    ""
    (n-readers (TVar UFix))
    (n-writers-waiting (TVar UFix))
    (writer-active? (TVar Boolean)))

  (declare new-trwlock (IO TRWLock))
  (define new-trwlock
    "Create a new TRWLock."
    (do
      (n-readers-tvar <- (new-tvar 0))
      (n-writers-tvar <- (new-tvar 0))
      (active-tvar <- (new-tvar False))
      (pure (TRWLock n-readers-tvar n-writers-tvar active-tvar))))

  (declare reader-acquire-tx (TRWLock -> STM IO Unit))
  (define (reader-acquire-tx lock)
    "Acquire a tlock. Blocks until the lock becomes available to readers. Returns the
token used to release the lock."
    (do
     (n-writers-waiting <- (read-tvar (.n-writers-waiting lock)))
     (writer-active? <- (read-tvar (.writer-active? lock)))
     (if (or writer-active? (> n-writers-waiting 0))
         retry
         (modify-tvar (.n-readers lock) 1+))
     (pure Unit)))

  (declare reader-release-tx (TRWLock -> STM IO Unit))
  (define (reader-release-tx lock)
    "Attempts to release a reader on LOCK. If there were no active readers, errors."
    (do
     (new-n-readers <- (modify-tvar (.n-readers lock) 1-))
     (do-when (< new-n-readers 0)
       (raise "Attempted to release a reader on a TRWLock with no active readers."))))

  ;; NOTE: This could return a transaction (STM IO Unit) instead of an IO operation. But
  ;; doing it this way makes it impossible to accidentally combine pend-writer-acquire
  ;; and writer-acquire in the same transaction, which would break the algorithm.
  (declare pend-writer-acquire (TRWLock -> IO Unit))
  (define (pend-writer-acquire lock)
    "Set LOCK to block future readers and wait for a writer to acquire the lock. Must be
run before attempting to acquire the lock as a writer. The purpose of this is to prevent
a steady stream of readers from acquiring the lock and blocking a writer from being able
to ever acquire it."
    (do-run-tx
      (modify-tvar (.n-writers-waiting lock) 1+)
      (pure Unit)))

  (declare writer-acquire (TRWLock -> IO Unit))
  (define (writer-acquire lock)
    "Acquire the writer lock on LOCK. Blocks until no readers and no writer is active."
    (do-run-tx
      (n-readers <- (read-tvar (.n-readers lock)))
      (writer-active? <- (read-tvar (.writer-active? lock)))
      (do-when (or writer-active? (> n-readers 0))
        retry)
      (write-tvar (.writer-active? lock) True)))

  (declare writer-release (TRWLock -> IO Unit))
  (define (writer-release lock)
    "Attempts to release the writer lock on LOCK. Errors if the writer lock was not acquired."
    (do-run-tx
      (writer-active? <- (read-tvar (.writer-active? lock)))
      (n-writers-waiting <- (read-tvar (.n-writers-waiting lock)))
      (do-when (or (not writer-active?) (zero? n-writers-waiting))
        (raise "Attempted to release the writer lock on a TRWLock that was not busy."))
      (write-tvar (.writer-active? lock) False)
      (write-tvar (.n-writers-waiting lock) (1- n-writers-waiting))))

  (declare with-reader-lock (TRWLock -> IO :a -> IO :a))
  (define (with-reader-lock lock op)
    "Run IO operation OP with a reader lock on LOCK held."
    (bracket-io_
     (run-tx (reader-acquire-tx lock))
     (fn (_)
       (run-tx (reader-release-tx lock)))
     (fn (_)
       op)))

  (declare with-writer-lock (TRWLock -> IO :a -> IO :a))
  (define (with-writer-lock lock op)
    "Run IO operation OP with the writer lock on LOCK held."
    ;; TODO: Convert this to use as bracket operation that doesn't mask. Then rewrite this
    ;; so it's valid in the presence of async stops. Currently, it blocks to acquire the
    ;; lock while masked.
    (bracket-io_
     (do
      (pend-writer-acquire lock)
      (writer-acquire lock))
     (fn (_)
       (writer-release lock))
     (fn (_)
       op)))
  )

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

  (declare copy-buckets (Database -> IO (List (hm:HashMap String String))))
  (define (copy-buckets db)
    "Retrieve a synchronized list of all the buckets in the database."
    (run-tx
     (do-collect (bucket-tvar (.buckets (.data db)))
       (read-tvar bucket-tvar))))

  (inline)
  (declare db-length (Database -> UFix))
  (define (db-length db)
    (v:length (.buckets (.data db))))

  (inline)
  (declare bucket-for (String -> Database -> TVar (hm:HashMap String String)))
  (define (bucket-for key db)
    (v:index-unsafe (mod-hash (hash key) (db-length db))
                    (.buckets (.data db))))

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
        (pure True)))))

  (declare save-buckets (String -> List (hm:HashMap String String) -> IO Unit))
  (define (save-buckets filename buckets)
    "Dump the contents of BUCKETS into FILENAME. Saves the data in binary format. Each key
value pair is encoded as a RespArray of two elements: the key and the value.

Note: This is NOT the file format used by Redis. The file format used by Redis also stores
key/value pairs as length encoded bytes. However, it also stores other metadata to support
other features like typed values, expiration, etc.

For more information on the Redis file format, for the curious:
https://rdb.fnordig.de/file_format.html"
    (io-f:do-with-open-file_ (f:Output (into filename) f:Overwrite) (fs)
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
           (v:set! 1 (RespBulkString val) buffer))
          (write-resp (RespArray buffer) fs)))))
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

  (declare handle-get-key (String -> Database -> IO Resp))
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

  (declare handle-set-key (String -> String -> Database -> IO Resp))
  (define (handle-set-key key val db)
    (do
     (do-with-reader-lock db
       (run-tx (write-key-tx key val db)))
     (pure OK-Response)))

  (declare handle-rename-key (String -> String -> Database -> IO Resp))
  (define (handle-rename-key key new-key db)
    (do
     (result <- (run-tx (rename-key key new-key db)))
     (if result
         (pure OK-Response)
         (pure (RespError (<> "Key not found: " key))))))

  (declare handle-save (Database -> IO Resp))
  (define (handle-save db)
    (do-with-writer-lock db
      (buckets <- (copy-buckets db))
      (save-buckets filename buckets)
      (pure OK-Response)))

  (declare handle-client (nt:ByteConnectionSocket -> Database -> IO Unit))
  (define (handle-client conn db)
    (do
     (rec % ()
       (do
        (cmd? <- (read-command conn))
        (do-match cmd?
          ((Err _)
           (%))
          ((Ok cmd)
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
                    (handle-save db))))
           (write-resp resp conn)
           (match cmd
             ((Quit)
              (pure Unit))
             (_
              (%)))))))
     (tm:write-line "client disconnected")))

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
