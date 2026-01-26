(cl:in-package :cl-user)
(defpackage :io/gen-impl/network
  (:use
   #:coalton
   #:coalton-prelude
   #:io/classes/monad-io
   #:io/classes/monad-io-network
   )
  (:export
   ;; Library Public
   #:implement-monad-io-network

   ;; Library Private
   #:socket-listen%
   #:socket-accept%
   #:socket-connect%
   #:close-connection%
   #:close-server%
   ))
(in-package :io/gen-impl/network)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

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
  )

(defmacro implement-monad-io-network (monad)
  `(define-instance (MonadIoNetwork ,monad)
     (define socket-listen socket-listen%)
     (define socket-accept socket-accept%)
     (define socket-connect socket-connect%)
     (define close-connection close-connection%)
     (define close-server close-server%)
     (define write-line write-line%)
     (define read-line read-line%)
     ))
