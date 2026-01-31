(cl:in-package :cl-user)
(defpackage :io/classes/network
  (:use
   #:coalton
   #:coalton-prelude
   #:io/utils
   #:io/classes/monad-io)
  (:import-from #:coalton-library/experimental/do-control-loops-adv
   #:LoopT)
  (:local-nicknames
   (:st #:coalton-library/monad/statet)
   (:env #:coalton-library/monad/environment))
  (:export
   ;; Library Public
   #:ServerSocket
   #:ConnectionSocket
   #:ByteServerSocket
   #:ByteConnectionSocket
   #:Sockets
   #:socket-listen
   #:socket-accept
   #:socket-connect
   #:close-connection
   #:close-server
   #:write-line
   #:read-line

   #:byte-socket-listen
   #:byte-socket-accept
   #:byte-socket-connect
   #:close-byte-connection
   #:close-byte-server
   #:write-bytes
   #:read-exactly

   #:derive-sockets

   ;; Library Private
   ))
(in-package :io/classes/network)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (repr :native usocket:stream-server-usocket)
  (define-type ServerSocket
    "A server socket listening for new connections.")

  (repr :native usocket:stream-usocket)
  (define-type ConnectionSocket
    "A socket connecting a client and server.")

  (repr :native usocket:stream-server-usocket)
  (define-type ByteServerSocket
    "A server socket listening for new byte-stream connections.")

  (repr :native usocket:stream-usocket)
  (define-type ByteConnectionSocket
    "A socket connecting a client and server using a byte stream.")

  (define-class (MonadIo :m => Sockets :m)
    (socket-listen
     "Start a new server socket, listening on HOSTNAME and PORT."
     (String -> UFix -> :m ServerSocket))
    (socket-accept
     "Accept a connection with a new client."
     (ServerSocket -> :m ConnectionSocket))
    (socket-connect
     "Connect to a server at HOSTNAME and PORT, returning a new connection."
     (String -> UFix -> :m ConnectionSocket))
    (close-connection
     "Close an open connection socket."
     (ConnectionSocket -> :m Unit))
    (close-server
     "Close an open server socket."
     (ServerSocket -> :m Unit))
    (write-line
     (Into :s String => :s -> ConnectionSocket -> :m Unit))
    (read-line
     "Read the next line from the socket. Blocks until data is sent!"
     (ConnectionSocket -> :m String))

    (byte-socket-listen
     "Start a new server socket for byte-stream connections, listening on HOSTNAME and PORT."
     (String -> UFix -> :m ByteServerSocket))
    (byte-socket-accept
     "Accept a byte-stream connection with a new client."
     (ByteServerSocket -> :m ByteConnectionSocket))
    (byte-socket-connect
     "Connect to a server at HOSTNAME and PORT using a byte stream, returning a new connection."
     (String -> UFix -> :m ByteConnectionSocket))
    (close-byte-connection
     "Close an open byte-stream connection socket."
     (ByteConnectionSocket -> :m Unit))
    (close-byte-server
     "Close an open byte-stream server socket."
     (ByteServerSocket -> :m Unit))
    (write-bytes
     "Write all bytes in the vector to the socket."
     (Vector U8 -> ByteConnectionSocket -> :m Unit))
    (read-exactly
     "Read exactly N bytes from the socket. Blocks until N bytes are available."
     (UFix -> ByteConnectionSocket -> :m (Vector U8)))
    )

  )

(defmacro derive-sockets (monad-param monadT-form)
  "Automatically derive an instance of Sockets for a monad transformer.

Example:
  (derive-sockets :m (st:StateT :s :m))"
  `(define-instance (Sockets ,monad-param => Sockets ,monadT-form)
     (define socket-listen (compose2 lift socket-listen))
     (define socket-accept (compose lift socket-accept))
     (define socket-connect (compose2 lift socket-connect))
     (define close-connection (compose lift close-connection))
     (define close-server (compose lift close-server))
     (define write-line (compose2 lift write-line))
     (define read-line (compose lift read-line))

     (define byte-socket-listen (compose2 lift byte-socket-listen))
     (define byte-socket-accept (compose lift byte-socket-accept))
     (define byte-socket-connect (compose2 lift byte-socket-connect))
     (define close-byte-connection (compose lift close-byte-connection))
     (define close-byte-server (compose lift close-byte-server))
     (define write-bytes (compose2 lift write-bytes))
     (define read-exactly (compose2 lift read-exactly))
     ))

;;
;; Std. Library Transformer Instances
;;

(coalton-toplevel
  (derive-sockets :m (st:StateT :s :m))
  (derive-sockets :m (env:EnvT :e :m))
  (derive-sockets :m (LoopT :m)))
