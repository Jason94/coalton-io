(cl:in-package :cl-user)
(defpackage :io/classes/monad-io-network
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
   #:MonadIoNetwork
   #:socket-listen
   #:socket-accept
   #:socket-connect
   #:close-connection
   #:close-server
   #:write-line
   #:read-line

   #:derive-monad-io-network

   ;; Library Private
   ))
(in-package :io/classes/monad-io-network)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (repr :native usocket:stream-server-usocket)
  (define-type ServerSocket
    "A server socket listening for new connections.")

  (repr :native usocket:stream-usocket)
  (define-type ConnectionSocket
    "A socket connecting a client and server.")

  (define-class (MonadIo :m => MonadIoNetwork :m)
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
    )

  )

(defmacro derive-monad-io-network (monad-param monadT-form)
  "Automatically derive an instance of MonadIoNetwork for a monad transformer.

Example:
  (derive-monad-io-network :m (st:StateT :s :m))"
  `(define-instance (MonadIoNetwork ,monad-param => MonadIoNetwork ,monadT-form)
     (define socket-listen (compose2 lift socket-listen))
     (define socket-accept (compose lift socket-accept))
     (define socket-connect (compose2 lift socket-connect))
     (define close-connection (compose lift close-connection))
     (define close-server (compose lift close-server))
     (define write-line (compose2 lift write-line))
     (define read-line (compose lift read-line))
     ))

;;
;; Std. Library Transformer Instances
;;

(coalton-toplevel
  (derive-monad-io-network :m (st:StateT :s :m))
  (derive-monad-io-network :m (env:EnvT :e :m))
  (derive-monad-io-network :m (LoopT :m)))
