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
   #:MonadIoNetwork
   #:socket-listen
   #:socket-accept
   #:socket-connect
   #:close-connection
   #:close-server

   #:derive-monad-io-network

   ;; Re-exports from io/gen-impl/network
   #:implement-monad-io-network
   ))
(in-package :io/network)
