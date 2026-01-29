(cl:in-package :cl-user)
(defpackage :io/sockets
  (:use
   #:io/classes/sockets
   #:io/gen-impl/network
   )
  (:export
   ;; Re-exports from io/classes/sockets
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

   ;; Re-exports from io/gen-impl/network
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
   ))
(in-package :io/sockets)
