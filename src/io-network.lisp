(cl:in-package :cl-user)
(defpackage :io/network
  (:use
   #:io/classes/monad-io-network
   #:io/gen-impl/network
   )
  (:export
   ;; Re-exports from io/classes/monad-io-network
   #:ServerSocket
   #:ConnectionSocket
   #:ByteServerSocket
   #:ByteConnectionSocket
   #:MonadIoNetwork
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

   #:derive-monad-io-network

   ;; Re-exports from io/gen-impl/network
   #:implement-monad-io-network

   #:with-socket-listen
   #:do-with-socket-listen
   #:with-socket-connect
   #:do-with-socket-connect
   #:with-socket-accept
   #:do-with-socket-accept
   #:with-socket-accept-fork
   #:do-with-socket-accept-fork
   ))
(in-package :io/network)
