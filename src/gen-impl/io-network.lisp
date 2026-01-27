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
                 (buf (cl:make-array n :element-type '(cl:unsigned-byte 8))))
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

(defmacro implement-monad-io-network (monad)
  `(define-instance (MonadIoNetwork ,monad)
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
