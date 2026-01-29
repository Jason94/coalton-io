(cl:in-package :cl-user)
(defpackage :io/gen-impl/network
  (:use
   #:coalton
   #:coalton-prelude
   #:io/classes/monad-io
   #:io/classes/exceptions
   #:io/classes/sockets
   #:io/classes/threads
   #:io/resource
   )
  (:export
   ;; Library Public
   #:implement-sockets

   #:socket-listen-with
   #:do-socket-listen-with
   #:socket-connect-with
   #:do-socket-connect-with
   #:socket-accept-with
   #:do-socket-accept-with
   #:socket-accept-fork-with
   #:do-socket-accept-fork-with
   #:byte-socket-listen-with
   #:do-byte-socket-listen-with
   #:byte-socket-connect-with
   #:do-byte-socket-connect-with
   #:byte-socket-accept-with
   #:do-byte-socket-accept-with
   #:byte-socket-accept-fork-with
   #:do-byte-socket-accept-fork-with

   ;; Library Private
   #:socket-listen%
   #:socket-accept%
   #:socket-connect%
   #:close-connection%
   #:close-server%

   #:byte-socket-listen%
   #:byte-socket-accept%
   #:byte-socket-connect%
   #:close-byte-connection%
   #:close-byte-server%
   #:write-bytes%
   #:read-exactly%
   ))
(in-package :io/gen-impl/network)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  ;;;
  ;;; Generic Implementation Functions
  ;;;

  (declare socket-listen% (MonadIo :m => String -> UFix -> :m ServerSocket))
  (define (socket-listen% hostname port)
    (wrap-io
     (lisp ServerSocket (hostname port)
       (usocket:socket-listen hostname port))))

  (declare socket-accept% (MonadIo :m => ServerSocket -> :m ConnectionSocket))
  (define (socket-accept% server-socket)
    (wrap-io
     (lisp ConnectionSocket (server-socket)
       (usocket:socket-accept server-socket :element-type 'cl:character))))

  (declare socket-connect% (MonadIo :m => String -> UFix -> :m ConnectionSocket))
  (define (socket-connect% hostname port)
    (wrap-io
     (lisp ConnectionSocket (hostname port)
       (usocket:socket-connect hostname port))))

  (declare byte-socket-listen% (MonadIo :m => String -> UFix -> :m ByteServerSocket))
  (define (byte-socket-listen% hostname port)
    (wrap-io
     (lisp ByteServerSocket (hostname port)
       (usocket:socket-listen hostname port))))

  (declare byte-socket-accept% (MonadIo :m => ByteServerSocket -> :m ByteConnectionSocket))
  (define (byte-socket-accept% server-socket)
    (wrap-io
     (lisp ByteConnectionSocket (server-socket)
       (usocket:socket-accept server-socket :element-type '(cl:unsigned-byte 8)))))

  (declare byte-socket-connect% (MonadIo :m => String -> UFix -> :m ByteConnectionSocket))
  (define (byte-socket-connect% hostname port)
    (wrap-io
     (lisp ByteConnectionSocket (hostname port)
       (usocket:socket-connect hostname port :element-type '(cl:unsigned-byte 8)))))

  (declare close-connection% (MonadIo :m => ConnectionSocket -> :m Unit))
  (define (close-connection% connection-socket)
    (wrap-io
     (lisp Unit (connection-socket)
       (usocket:socket-close connection-socket)
       Unit)))

  (declare close-server% (MonadIo :m => ServerSocket -> :m Unit))
  (define (close-server% server-socket)
    (wrap-io
     (lisp Unit (server-socket)
       (usocket:socket-close server-socket)
       Unit)))

  (declare close-byte-connection% (MonadIo :m => ByteConnectionSocket -> :m Unit))
  (define (close-byte-connection% connection-socket)
    (wrap-io
     (lisp Unit (connection-socket)
       (usocket:socket-close connection-socket)
       Unit)))

  (declare close-byte-server% (MonadIo :m => ByteServerSocket -> :m Unit))
  (define (close-byte-server% server-socket)
    (wrap-io
     (lisp Unit (server-socket)
       (usocket:socket-close server-socket)
       Unit)))

  (declare write-line% ((MonadIo :m) (Into :s String) => :s -> ConnectionSocket -> :m Unit))
  (define (write-line% msg connection-socket)
    (wrap-io
     (let str-msg = (as String msg))
     (lisp Unit (str-msg connection-socket)
       (cl:let ((stream (usocket:socket-stream connection-socket)))
         (cl:format stream "~a~%" str-msg)
         (cl:force-output stream)
         Unit))))

  (declare read-line% (MonadIo :m => ConnectionSocket -> :m String))
  (define (read-line% connection-socket)
    (wrap-io
     (lisp String (connection-socket)
       (usocket:wait-for-input connection-socket)
       (cl:read-line (usocket:socket-stream connection-socket)))))

  (declare write-bytes% (MonadIo :m => Vector U8 -> ByteConnectionSocket -> :m Unit))
  (define (write-bytes% bytes connection-socket)
    (wrap-io
     (lisp Unit (bytes connection-socket)
       (cl:let ((stream (usocket:socket-stream connection-socket)))
         (cl:write-sequence bytes stream)
         (cl:force-output stream)
         Unit))))

  (declare read-exactly% (MonadIo :m => UFix -> ByteConnectionSocket -> :m (Vector U8)))
  (define (read-exactly% n connection-socket)
    (wrap-io
     (lisp (Vector U8) (n connection-socket)
       (cl:let* ((stream (usocket:socket-stream connection-socket))
                 (buf (cl:make-array n)))
         ;; Wait for at least some input, then keep reading until we've
         ;; filled the whole buffer.
         (usocket:wait-for-input connection-socket)
         (cl:let ((pos 0))
           (cl:loop :while (cl:< pos n)
                    :do (cl:let ((new-pos (cl:read-sequence buf stream :start pos :end n)))
                         (cl:when (cl:= new-pos pos)
                           (cl:error "Unexpected EOF while reading from socket"))
                         (cl:setf pos new-pos)))
           buf)))))
  )

(defmacro implement-sockets (monad)
  `(define-instance (Sockets ,monad)
     (define socket-listen socket-listen%)
     (define socket-accept socket-accept%)
     (define socket-connect socket-connect%)
     (define close-connection close-connection%)
     (define close-server close-server%)
     (define write-line write-line%)
     (define read-line read-line%)

     (define byte-socket-listen byte-socket-listen%)
     (define byte-socket-accept byte-socket-accept%)
     (define byte-socket-connect byte-socket-connect%)
     (define close-byte-connection close-byte-connection%)
     (define close-byte-server close-byte-server%)
     (define write-bytes write-bytes%)
     (define read-exactly read-exactly%)
     ))

;;
;; Public Functions Defined on Class Functions
;;

(coalton-toplevel

  ;; TODO: Convert these to use MonadIo
  (declare socket-listen-with ((Sockets :m) (Threads :rt :t :m) (Exceptions :m)
                               => String -> UFix -> (ServerSocket -> :m :a) -> :m :a))
  (define (socket-listen-with hostname port op)
    "Run operation OP with a new server socket, listening on HOSTNAME and PORT. Guarantees
that the socket will close on cleanup."
    (bracket-io_
     (socket-listen hostname port)
     close-server
     op))

  ;; TODO: Switch these to use a non-masking bracket combinator once I write one.
  ;; It shouldn't be masked while blocking on the socket connection.
  (declare socket-connect-with ((Sockets :m) (Threads :rt :t :m) (Exceptions :m)
                                => String -> UFix -> (ConnectionSocket -> :m :a) -> :m :a))
  (define (socket-connect-with hostname port op)
    "Run operation OP with a connection to an open server socket at HOSTNAME and PORT.
Guarantees that the socket will close on cleanup."
    (bracket-io_
     (socket-connect hostname port)
     close-connection
     op))

  (declare socket-accept-with ((Sockets :m) (Threads :rt :t :m) (Exceptions :m)
                               => ServerSocket -> (ConnectionSocket -> :m :a) -> :m :a))
  (define (socket-accept-with server-socket op)
    "Accept a connection with a new client and run operation OP. Guarantees that the
socket will close on cleanup.

Note: If you fork a thread inside this, the operation on this thread will probably finish
and close the socket before you intend. For multithreaded uses, use
socket-accept-fork-with."
    (bracket-io_
     (socket-accept server-socket)
     close-connection
     op))

  ;; TODO: Bracket the top-level forking once I write a bracket combinator that uses a
  ;; non-type specific ExitCase.
  (declare socket-accept-fork-with ((Sockets :m) (Threads :rt :t :m) (Exceptions :m)
                                    (UnliftIo :m :m)
                                    => ServerSocket -> (ConnectionSocket -> :m :a) -> :m :t))
  (define (socket-accept-fork-with server-socket op)
    "Accept a connection with a new client and run operation OP on a new thread.
Guarantees that the socket will close on cleanup. Returns a handle to the forked thread."
    (do
     (conn <- (socket-accept server-socket))
     (fork-thread
       (bracket-io_
        (pure conn)
        close-connection
        op))))

  (declare byte-socket-listen-with ((Sockets :m) (Threads :rt :t :m) (Exceptions :m)
                                    => String -> UFix -> (ByteServerSocket -> :m :a) -> :m :a))
  (define (byte-socket-listen-with hostname port op)
    "Run operation OP with a new byte-stream server socket, listening on HOSTNAME and PORT.
Guarantees that the socket will close on cleanup."
    (bracket-io_
     (byte-socket-listen hostname port)
     close-byte-server
     op))

  (declare byte-socket-connect-with ((Sockets :m) (Threads :rt :t :m) (Exceptions :m)
                                     => String -> UFix -> (ByteConnectionSocket -> :m :a) -> :m :a))
  (define (byte-socket-connect-with hostname port op)
    "Run operation OP with a byte-stream connection to an open server socket at HOSTNAME and PORT.
Guarantees that the socket will close on cleanup."
    (bracket-io_
     (byte-socket-connect hostname port)
     close-byte-connection
     op))

  (declare byte-socket-accept-with ((Sockets :m) (Threads :rt :t :m) (Exceptions :m)
                                    => ByteServerSocket -> (ByteConnectionSocket -> :m :a) -> :m :a))
  (define (byte-socket-accept-with server-socket op)
    "Accept a byte-stream connection with a new client and run operation OP. Guarantees that the
socket will close on cleanup.

Note: If you fork a thread inside this, the operation on this thread will probably finish
and close the socket before you intend. For multithreaded uses, use
byte-socket-accept-fork-with."
    (bracket-io_
     (byte-socket-accept server-socket)
     close-byte-connection
     op))

  (declare byte-socket-accept-fork-with ((Sockets :m) (Threads :rt :t :m) (Exceptions :m)
                                         (UnliftIo :m :m)
                                         => ByteServerSocket -> (ByteConnectionSocket -> :m :a) -> :m :t))
  (define (byte-socket-accept-fork-with server-socket op)
    "Accept a byte-stream connection with a new client and run operation OP on a new thread.
Guarantees that the socket will close on cleanup. Returns a handle to the forked thread."
    (do
     (conn <- (byte-socket-accept server-socket))
     (fork-thread
       (bracket-io_
        (pure conn)
        close-byte-connection
        op))))
  )

(defmacro do-socket-listen-with ((socket-sym (hostname port)) cl:&body body)
  "Run operation OP with a new server socket, listening on HOSTNAME and PORT. Guarantees
that the socket will close on cleanup."
  `(socket-listen-with ,hostname ,port
     (fn (,socket-sym)
       (do
        ,@body))))

(defmacro do-socket-connect-with ((socket-sym (hostname port)) cl:&body body)
  "Run operation OP with a connection to an open server socket at HOSTNAME and PORT.
Guarantees that the socket will close on cleanup."
  `(socket-connect-with ,hostname ,port
     (fn (,socket-sym)
       (do
        ,@body))))

(defmacro do-socket-accept-with ((socket-sym (server-socket)) cl:&body body)
  "Accept a connection with a new client and run operation OP. Guarantees that the
socket will close on cleanup.

Note: If you fork a thread inside this, the operation on this thread will probably finish
and close the socket before you intend. For multithreaded uses, use
socket-accept-fork-with."
  `(socket-accept-with ,server-socket
     (fn (,socket-sym)
      ,@body)))

(defmacro do-socket-accept-fork-with ((socket-sym (server-socket)) cl:&body body)
  "Accept a connection with a new client and run operation OP on a new thread.
Guarantees that the socket will close on cleanup. Returns a handle to the forked thread."
  `(socket-accept-fork-with ,server-socket
     (fn (,socket-sym)
       ,@body)))

(defmacro do-byte-socket-listen-with ((socket-sym (hostname port)) cl:&body body)
  "Run operation OP with a new byte-stream server socket, listening on HOSTNAME and PORT. Guarantees
that the socket will close on cleanup."
  `(byte-socket-listen-with ,hostname ,port
     (fn (,socket-sym)
       (do
        ,@body))))

(defmacro do-byte-socket-connect-with ((socket-sym (hostname port)) cl:&body body)
  "Run operation OP with a byte-stream connection to an open server socket at HOSTNAME and PORT.
Guarantees that the socket will close on cleanup."
  `(byte-socket-connect-with ,hostname ,port
     (fn (,socket-sym)
       (do
        ,@body))))

(defmacro do-byte-socket-accept-with ((socket-sym (server-socket)) cl:&body body)
  "Accept a byte-stream connection with a new client and run operation OP. Guarantees that the
socket will close on cleanup.

Note: If you fork a thread inside this, the operation on this thread will probably finish
and close the socket before you intend. For multithreaded uses, use
byte-socket-accept-fork-with."
  `(byte-socket-accept-with ,server-socket
     (fn (,socket-sym)
      ,@body)))

(defmacro do-byte-socket-accept-fork-with ((socket-sym (server-socket)) cl:&body body)
  "Accept a byte-stream connection with a new client and run operation OP on a new thread.
Guarantees that the socket will close on cleanup. Returns a handle to the forked thread."
  `(byte-socket-accept-fork-with ,server-socket
     (fn (,socket-sym)
       ,@body)))
