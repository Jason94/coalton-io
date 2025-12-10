(cl:in-package :cl-user)
(in-package :io/file)

(named-readtables:in-readtable coalton:coalton)

(cl:defmacro do-with-open-file_ (opts (fs) cl:&body body)
  "`do` sugar for `with-open-file_`. Expands to a continuation where BODY runs in `do`."

Usage:
  (do-with-open-file_ opts (fs)
    (line <- (read-char fs))
    ...)
"
  `(with-open-file_ ,opts (fn (,fs) (do ,@body))))

(cl:defmacro do-with-temp-file_ (type (fs) cl:&body body)
  "`do` sugar for `with-temp-file_` (TYPE is a string like \"txt\")."
  `(with-temp-file_ ,type (fn (,fs) (do ,@body))))

(cl:defmacro do-with-temp-directory_ ((dir) cl:&body body)
  "`do` sugar for `with-temp-directory_`."
  `(with-temp-directory_ (fn (,dir) (do ,@body))))

(coalton-toplevel

  (declare with-open-file_ ((file:File :a) (UnliftIo :m io:IO) (LiftTo io:IO :m)
                            => file:StreamOptions
                            -> ((file:FileStream :a) -> io:IO :b)
                            -> :m :b))
  (define with-open-file_ with-open-file)

  (declare with-temp-file_ ((file:File :a) (UnliftIo :m io:IO) (LiftTo io:IO :m)
                            => String
                            -> ((file:FileStream :a) -> io:IO :b)
                            -> :m :b))
  (define with-temp-file_ with-temp-file)

  (declare with-temp-directory_ ((UnliftIo :m io:IO) (LiftTo io:IO :m)
                                 => (file:Pathname -> io:IO :a)
                                 -> :m :a))
  (define with-temp-directory_ with-temp-directory))
