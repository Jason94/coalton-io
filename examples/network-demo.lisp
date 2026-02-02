(cl:in-package :cl-user)

(defpackage :io/examples/network-demo
  (:use
   #:coalton
   #:coalton-prelude
   #:io/utils
   #:io/thread
   #:io/simple-io
   #:io/resource
   #:coalton-library/experimental/do-control-core
   )
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
  (define port (the UFix 5556))

  (declare handle-client (nt:ConnectionSocket -> IO Unit))
  (define (handle-client conn)
    (do
      (msg <- (nt:read-line conn))
      (nt:write-line (build-str "Read message: '" msg "'") conn)
      (if (== msg "")
          (do
            (tm:write-line "client disconnected")
            (pure Unit))
          (do
            (tm:write-line (<> "client> " msg))
            (handle-client conn)))))

  (declare accept-loop (nt:ServerSocket -> IO Unit))
  (define (accept-loop server)
    (do
     (nt:do-socket-accept-fork-with (conn (server))
       (the (IO Unit) (tm:write-line "client connected"))
       (handle-client conn))
     (accept-loop server)))

  (declare server-main (IO Unit))
  (define server-main
    (nt:do-socket-listen-with (server (hostname port))
      (tm:write-line (<> "listening on " (<> hostname (<> ":" (as String port)))))
      (accept-loop server)))

  (declare client-loop (nt:ConnectionSocket -> IO Unit))
  (define (client-loop conn)
    (do
      (tm:write "> ")
      (line <- tm:read-line)
      (nt:write-line line conn)
      (if (== line "")
          (pure Unit)
          (do
           (response <- (nt:read-line conn))
           (tm:write-line (build-str "  <response> " response))
           (client-loop conn)))))

  (declare client-main (IO Unit))
  (define client-main
    (nt:do-socket-connect-with (conn (hostname port))
      (tm:write-line (<> "connected to " (<> hostname (<> ":" (as String port)))))
      (client-loop conn)))
  )

(cl:defun run-server ()
  (coalton (run-io! server-main)))

(cl:defun run-client ()
  (coalton (run-io! client-main)))
