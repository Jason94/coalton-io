(cl:in-package :cl-user)
(defpackage :io/io-impl/file
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/monad/classes
   #:io/classes/monad-io
   #:io/classes/monad-io-file
   #:io/gen-impl/file
   #:io/io-impl/simple-io
   )
  (:local-nicknames
   (:file #:coalton-library/file))
  (:export
   #:with-open-file_
   #:do-with-open-file_
   #:with-temp-file_
   #:do-with-temp-file_
   #:with-temp-directory_
   #:do-with-temp-directory_
   ))
(in-package :io/io-impl/file)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (declare with-open-file_ ((file:File :a) (UnliftIo :m IO) (LiftTo IO :m)
                            => file:StreamOptions
                            -> ((file:FileStream :a) -> IO :b)
                            -> :m :b))
  (define with-open-file_ with-open-file)

  (declare with-temp-file_ ((file:File :a) (UnliftIo :m IO) (LiftTo IO :m)
                            => String
                            -> ((file:FileStream :a) -> IO :b)
                            -> :m :b))
  (define with-temp-file_ with-temp-file)

  (declare with-temp-directory_ ((UnliftIo :m IO) (LiftTo IO :m)
                                 => (file:Pathname -> IO :a)
                                 -> :m :a))
  (define with-temp-directory_ with-temp-directory))

(defmacro do-with-open-file_ (opts (fs) cl:&body body)
  "`do` sugar for `with-open-file_`. Expands to a continuation where BODY runs in `do`.

Usage:
  (do-with-open-file_ opts (fs)
    (line <- (read-char fs))
    ...)
"
  `(with-open-file_ ,opts (fn (,fs) (do ,@body))))

(defmacro do-with-temp-file_ (type (fs) cl:&body body)
  "`do` sugar for `with-temp-file_` (TYPE is a string like \"txt\")."
  `(with-temp-file_ ,type (fn (,fs) (do ,@body))))

(defmacro do-with-temp-directory_ ((dir) cl:&body body)
  "`do` sugar for `with-temp-directory_`."
  `(with-temp-directory_ (fn (,dir) (do ,@body))))
