(cl:in-package :cl-user)

(defpackage :io/examples/network-demo
  (:use
   #:coalton
   #:coalton-prelude
   #:io/thread
   #:coalton-library/experimental/do-control-core)
  (:local-nicknames
   (:sio  #:io/simple-io)
   (:term #:io/term)
   (:net  #:io/network))
  (:export
   #:run-server
   #:run-client))

(in-package :io/examples/network-demo)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (define hostname "127.0.0.1")
  (define port (the UFix 5555))

  (declare handle-client (net:ConnectionSocket -> sio:IO Unit))
  (define (handle-client conn)
    (do
      (msg <- (net:read-line conn))
      (if (== msg "")
          (do
            (term:write-line "client disconnected")
            (net:close-connection conn))
          (do
            (term:write-line (<> "client> " msg))
            (handle-client conn)))))

  (declare accept-loop (net:ServerSocket -> sio:IO Unit))
  (define (accept-loop server)
    (do
      (conn <- (net:socket-accept server))
      (term:write-line "client connected")
      (fork-thread_ (handle-client conn))
      (accept-loop server)))

  (declare server-main (sio:IO Unit))
  (define server-main
    (do
      (server <- (net:socket-listen hostname port))
      (term:write-line (<> "listening on " (<> hostname (<> ":" (as String port)))))
      (accept-loop server)))

  (declare client-loop (net:ConnectionSocket -> sio:IO Unit))
  (define (client-loop conn)
    (do
      (term:write "> ")
      (line <- term:read-line)
      (net:write-line line conn)
      (if (== line "")
          (net:close-connection conn)
          (client-loop conn))))

  (declare client-main (sio:IO Unit))
  (define client-main
    (do
      (conn <- (net:socket-connect hostname port))
      (term:write-line (<> "connected to " (<> hostname (<> ":" (as String port)))))
      (client-loop conn)))
  )

(cl:defun run-server ()
  (coalton (sio:run-io! server-main)))

(cl:defun run-client ()
  (coalton (sio:run-io! client-main)))
