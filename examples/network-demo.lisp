(cl:in-package :cl-user)

(defpackage :io/examples/network-demo
  (:use
   #:coalton
   #:coalton-prelude
   #:io/thread
   #:io/simple-io
   #:coalton-library/experimental/do-control-core)
  (:local-nicknames
   (:tm #:io/term)
   (:nt  #:io/network))
  (:export
   #:run-server
   #:run-client))

(in-package :io/examples/network-demo)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (define hostname "127.0.0.1")
  (define port (the UFix 5555))

  (declare handle-client (nt:ConnectionSocket -> IO Unit))
  (define (handle-client conn)
    (do
      (msg <- (nt:read-line conn))
      (if (== msg "")
          (do
            (tm:write-line "client disconnected")
            (nt:close-connection conn))
          (do
            (tm:write-line (<> "client> " msg))
            (handle-client conn)))))

  (declare accept-loop (nt:ServerSocket -> IO Unit))
  (define (accept-loop server)
    (do
      (conn <- (nt:socket-accept server))
      (tm:write-line "client connected")
      (fork-thread_ (handle-client conn))
      (accept-loop server)))

  (declare server-main (IO Unit))
  (define server-main
    (do
      (server <- (nt:socket-listen hostname port))
      (tm:write-line (<> "listening on " (<> hostname (<> ":" (as String port)))))
      (accept-loop server)))

  (declare client-loop (nt:ConnectionSocket -> IO Unit))
  (define (client-loop conn)
    (do
      (tm:write "> ")
      (line <- tm:read-line)
      (nt:write-line line conn)
      (if (== line "")
          (nt:close-connection conn)
          (client-loop conn))))

  (declare client-main (IO Unit))
  (define client-main
    (do
      (conn <- (nt:socket-connect hostname port))
      (tm:write-line (<> "connected to " (<> hostname (<> ":" (as String port)))))
      (client-loop conn)))
  )

(cl:defun run-server ()
  (coalton (run-io! server-main)))

(cl:defun run-client ()
  (coalton (run-io! client-main)))
