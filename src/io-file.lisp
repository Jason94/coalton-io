(cl:in-package :cl-user)
(defpackage :io/file
  (:use
   #:coalton
   #:coalton-prelude
   #:io/utils
   #:io/exception
   #:io/monad-io
   #:io/resource)
  (:import-from #:coalton-library/types
   #:RuntimeRepr)
  (:import-from #:coalton-library/experimental/do-control-loops-adv
   #:LoopT)
  (:import-from #:coalton-library/monad/classes
   #:LiftTo #:lift-to)
  (:local-nicknames
   (:file #:coalton-library/file)
   (:rt #:coalton-library/result)
   (:st   #:coalton-library/monad/statet)
   (:env  #:coalton-library/monad/environment)
   (:io #:io/simple-io))
  (:export
   #:MonadIoFile
   #:derive-monad-io-file

   #:exists?
   #:file-exists?
   #:directory-exists?

   #:open
   #:close
   #:abort

   #:copy
   #:create-directory
   #:delete-file
   #:remove-directory
   #:remove-directory-recursive
   #:system-relative-pathname

   #:read-file-to-string
   #:read-file-lines
   #:write-string
   #:write-line
   #:read-char
   #:read-line
   #:read-line#
   #:write-char

   #:read-file-to-vector
   #:read-vector
   #:write-vector
   #:write-to-file
   #:append-to-file
   #:set-file-position

   #:create-temp-directory
   #:create-temp-file
   #:with-open-file
   #:with-temp-file
   #:with-temp-directory
   #:with-open-file_
   #:with-temp-file_
   #:with-temp-directory_

   #:do-with-open-file_
   #:do-with-temp-file_
   #:do-with-temp-directory_
   #:do-with-open-file
   #:do-with-temp-file
   #:do-with-temp-directory
   #:implement-monad-io-file
   ))
(in-package :io/file)

(named-readtables:in-readtable coalton:coalton)

;;; ------------------------------------------------------------
;;; CL Macros
;;; ------------------------------------------------------------

(cl:defmacro derive-monad-io-file (monad-param monadT-form)
  "Derive a `MonadIoFile` instance for MONADT-FORM by lifting into the base instance.

Example:
  (derive-monad-io-file :m (st:StateT :s :m))"
  `(define-instance (MonadIoFile ,monad-param => MonadIoFile ,monadT-form)
     (define exists? (compose lift exists?))
     (define file-exists? (compose lift file-exists?))
     (define directory-exists? (compose lift directory-exists?))

     (define open (compose lift open))
     (define close (compose lift close))
     (define abort (compose lift abort))

     (define create-temp-directory create-temp-directory%)
     (define create-temp-file create-temp-file%)

     (define copy (compose2 lift copy))
     (define create-directory (compose lift create-directory))
     (define delete-file (compose lift delete-file))
     (define remove-directory (compose lift remove-directory))
     (define remove-directory-recursive (compose lift remove-directory-recursive))
     (define system-relative-pathname (compose2 lift system-relative-pathname))

     (define read-file-to-string (compose lift read-file-to-string))
     (define read-file-lines (compose lift read-file-lines))

     (define read-char (compose lift read-char))
     (define read-line (compose lift read-line))
     (define write-char (compose2 lift write-char))
     (define write-line (compose2 lift write-line))
     (define write-string (compose2 lift write-string))

     (define read-file-to-vector (compose lift read-file-to-vector))
     (define read-vector (compose2 lift read-vector))
     (define write-vector (compose2 lift write-vector))
     (define write-to-file (compose2 lift write-to-file))
     (define append-to-file (compose2 lift append-to-file))
     (define set-file-position (compose2 lift set-file-position))))

;;; ------------------------------------------------------------
;;; Coalton definitions
;;; ------------------------------------------------------------

(coalton-toplevel
  ;;
  ;; IO-backed primitives (internal, %-suffixed)
  ;;

  (declare exists?% ((Into :a file:Pathname) (MonadIo :m) => :a -> :m (Result file:FileError Boolean)))
  (define (exists?% pth)
    (wrap-io (file:exists? pth)))

  (declare file-exists?% ((Into :a file:Pathname) (MonadIo :m) => :a -> :m (Result file:FileError Boolean)))
  (define (file-exists?% pth)
    (wrap-io (file:file-exists? pth)))

  (declare directory-exists?% ((Into :a file:Pathname) (MonadIo :m) => :a -> :m (Result file:FileError Boolean)))
  (define (directory-exists?% pth)
    (wrap-io (file:directory-exists? pth)))

  (declare open% ((file:File :a) (MonadIo :m) => file:StreamOptions -> :m (Result file:FileError (file:FileStream :a))))
  (define (open% opts)
    (wrap-io (file:open opts)))

  (declare close% (MonadIo :m => (file:FileStream :a) -> :m (Result file:FileError :b)))
  (define (close% fs)
    (wrap-io (file:close fs)))

  (declare abort% (MonadIo :m => (file:FileStream :a) -> :m (Result file:FileError :b)))
  (define (abort% fs)
    (wrap-io (file:abort fs)))

  (declare create-temp-directory% (MonadIo :m => :m (Result file:FileError file:Pathname)))
  (define create-temp-directory%
    (wrap-io (file:create-temp-directory!)))

  (declare create-temp-file% (MonadIo :m => String -> :m (Result file:FileError file:Pathname)))
  (define (create-temp-file% file-ext)
    (wrap-io (file:create-temp-file! file-ext)))

  (declare copy% ((Into :a file:Pathname) (Into :b file:Pathname) (MonadIo :m) => :a -> :b -> :m (Result file:FileError Unit)))
  (define (copy% a b)
    (wrap-io (file:copy! a b)))

  (declare create-directory% ((Into :p file:Pathname) (MonadIo :m) => :p -> :m (Result file:FileError file:Pathname)))
  (define (create-directory% p)
    (wrap-io (file:create-directory! p)))

  (declare delete-file% ((Into :p file:Pathname) (MonadIo :m) => :p -> :m (Result file:FileError Unit)))
  (define (delete-file% p)
    (wrap-io (file:delete-file! p)))

  (declare remove-directory% ((Into :p file:Pathname) (MonadIo :m) => :p -> :m (Result file:FileError :p)))
  (define (remove-directory% p)
    (wrap-io (file:remove-directory! p)))

  (declare remove-directory-recursive% ((Into :p file:Pathname) (MonadIo :m) => :p -> :m (Result file:FileError Unit)))
  (define (remove-directory-recursive% p)
    (wrap-io (file:remove-directory-recursive! p)))

  (declare system-relative-pathname% ((Into :sys String) (MonadIo :m) => :sys -> String -> :m (Result file:FileError file:Pathname)))
  (define (system-relative-pathname% sys name)
    (wrap-io (file:system-relative-pathname sys name)))

  (declare read-file-to-string% ((Into :p file:Pathname) (MonadIo :m) => :p -> :m (Result file:FileError String)))
  (define (read-file-to-string% p)
    (wrap-io (file:read-file-to-string p)))

  (declare read-file-lines% ((Into :p file:Pathname) (MonadIo :m) => :p -> :m (Result file:FileError (List String))))
  (define (read-file-lines% p)
    (wrap-io (file:read-file-lines p)))

  (declare write-to-file% ((Into :p file:Pathname) (file:File :a) (RuntimeRepr :a) (MonadIo :m) => :p -> (Vector :a) -> :m (Result file:FileError Unit)))
  (define (write-to-file% p v)
    (wrap-io (file:write-to-file! p v)))

  (declare append-to-file% ((Into :p file:Pathname) (file:File :a) (RuntimeRepr :a) (MonadIo :m) => :p -> (Vector :a) -> :m (Result file:FileError Unit)))
  (define (append-to-file% p v)
    (wrap-io (file:append-to-file! p v)))

  (declare read-char% (MonadIo :m => (file:FileStream Char) -> :m (Result file:FileError Char)))
  (define (read-char% fs)
    (wrap-io (file:read-char fs)))

  (declare read-line% (MonadIo :m => (file:FileStream Char) -> :m (Result file:FileError String)))
  (define (read-line% fs)
    (wrap-io (file:read-line fs)))

  (declare write-char% (MonadIo :m => (file:FileStream Char) -> Char -> :m (Result file:FileError Unit)))
  (define (write-char% fs c)
    (wrap-io (file:write-char fs c)))

  (declare write-line% (MonadIo :m => (file:FileStream Char) -> String -> :m (Result file:FileError Unit)))
  (define (write-line% fs s)
    (wrap-io (file:write-line fs s)))

  (declare write-string% (MonadIo :m => (file:FileStream Char) -> String -> :m (Result file:FileError Unit)))
  (define (write-string% fs s)
    (wrap-io (file:write-string fs s)))

  (declare read-file-to-vector% ((file:File :a) (MonadIo :m) => (file:FileStream :a) -> :m (Result file:FileError (Vector :a))))
  (define (read-file-to-vector% fs)
    (wrap-io (file:read-file-to-vector fs)))

  (declare read-vector% ((file:File :a) (MonadIo :m) => (file:FileStream :a) -> UFix -> :m (Result file:FileError (Vector :a))))
  (define (read-vector% fs n)
    (wrap-io (file:read-vector fs n)))

  (declare write-vector% ((file:File :a) (RuntimeRepr :a) (MonadIo :m) => (file:FileStream :a) -> (Vector :a) -> :m (Result file:FileError Unit)))
  (define (write-vector% fs v)
    (wrap-io (file:write-vector fs v)))

  (declare set-file-position% (MonadIo :m => (file:FileStream :a) -> UFix -> :m (Result file:FileError Unit)))
  (define (set-file-position% fs pos)
    (wrap-io (file:set-file-position fs pos)))

  ;;
  ;; Class + instances (generalized API)
  ;;

  (define-class (MonadIo :m => MonadIoFile :m)
    (exists?
     "Returns whether a file or directory exists."
     (Into :a file:Pathname => :a -> :m (Result file:FileError Boolean)))
    (file-exists?
     "Returns True if a pathname names a file that exists."
     (Into :a file:Pathname => :a -> :m (Result file:FileError Boolean)))
    (directory-exists?
     "Returns True if a pathname names a directory that exists."
     (Into :a file:Pathname => :a -> :m (Result file:FileError Boolean)))

    (open (file:File :a => file:StreamOptions -> :m (Result file:FileError (file:FileStream :a))))
    (close
     "Closes a FileStream."
     ((file:FileStream :a) -> :m (Result file:FileError :b)))
    (abort
     "Closes a FileStream and aborts all operations.."
     ((file:FileStream :a) -> :m (Result file:FileError :b)))

    (create-temp-directory
     (:m (Result file:FileError file:Pathname)))
    (create-temp-file
     (String -> :m (Result file:FileError file:Pathname)))

    (copy
     "Copies a file to a new location."
     ((Into :a file:Pathname) (Into :b file:Pathname) => :a -> :b -> :m (Result file:FileError Unit)))
    (create-directory
     "This is equivalent to `mkdir -p`. Creates a directory and its parents. The pathname must be a valid directory pathname."
     (Into :p file:Pathname => :p -> :m (Result file:FileError file:Pathname)))
    (delete-file
     "Deletes a given file if the file exists."
     (Into :p file:Pathname => :p -> :m (Result file:FileError Unit)))
    (remove-directory
     "Deletes an empty directory."
     (Into :p file:Pathname => :p -> :m (Result file:FileError :p)))
    (remove-directory-recursive
     "Deletes a target directory recursively. Equivalent to `rm -r`. Errors if the path is not a directory."
     (Into :p file:Pathname => :p -> :m (Result file:FileError Unit)))
    (system-relative-pathname
     "Generates a system-relative-pathname for a given filename or path. This is a wrapper for `asdf:system-relative-pathname`. `Name` will likely be an empty string unless a subdirectory or filename is specified."
     (Into :sys String => :sys -> String -> :m (Result file:FileError file:Pathname)))

    (read-file-to-string
     "Reads a file into a string, given a pathname string."
     (Into :p file:Pathname => :p -> :m (Result file:FileError String)))
    (read-file-lines
     "Reads a file into lines, given a pathname or string."
     (Into :p file:Pathname => :p -> :m (Result file:FileError (List String))))
    (read-char
     "Reads a character from an FileStream."
     ((file:FileStream Char) -> :m (Result file:FileError Char)))
    (read-line ((file:FileStream Char) -> :m (Result file:FileError String)))
    (write-char
     "Writes a `Char` to the stream."
     ((file:FileStream Char) -> Char -> :m (Result file:FileError Unit)))
    (write-line
     "Writes a string with an appended newline to a filestream of type Char."
     ((file:FileStream Char) -> String -> :m (Result file:FileError Unit)))
    (write-string
     "Writes a `string` to a FileStream of type Char."
     ((file:FileStream Char) -> String -> :m (Result file:FileError Unit)))

    (read-file-to-vector
     "Reads a file into a vector of type `:a`."
     (file:File :a => (file:FileStream :a) -> :m (Result file:FileError (Vector :a))))
    (read-vector
     "Reads a chunk of a file into a vector of type `:a`."
     (file:File :a => (file:FileStream :a) -> UFix -> :m (Result file:FileError (Vector :a))))
    (write-vector
     "Writes elements of an vector of type `:a` to a stream of type `:a`."
     ((file:File :a) (RuntimeRepr :a) => (file:FileStream :a) -> (Vector :a) -> :m (Result file:FileError Unit)))
    (write-to-file
     "Opens and writes to a file with data of type :a. Supersedes existing data on the file."
     ((Into :p file:Pathname) (file:File :a) (RuntimeRepr :a) => :p -> (Vector :a) -> :m (Result file:FileError Unit)))
    (append-to-file
     "Opens and appends a file with data of type :a."
     ((Into :p file:Pathname) (file:File :a) (RuntimeRepr :a) => :p -> (Vector :a) -> :m (Result file:FileError Unit)))
    (set-file-position
     "Sets the file position of a file stream."
     ((file:FileStream :a) -> UFix -> :m (Result file:FileError Unit)))))

(cl:defmacro implement-monad-io-file (monad)
  `(define-instance (MonadIoFile ,monad)
     (define exists? exists?%)
     (define file-exists? file-exists?%)
     (define directory-exists? directory-exists?%)

     (define open open%)
     (define close close%)
     (define abort abort%)

     (define create-temp-directory create-temp-directory%)
     (define create-temp-file create-temp-file%)

     (define copy copy%)
     (define create-directory create-directory%)
     (define delete-file delete-file%)
     (define remove-directory remove-directory%)
     (define remove-directory-recursive remove-directory-recursive%)
     (define system-relative-pathname system-relative-pathname%)

     (define read-file-to-string read-file-to-string%)
     (define read-file-lines read-file-lines%)

     (define read-char read-char%)
     (define read-line read-line%)
     (define write-char write-char%)
     (define write-line write-line%)
     (define write-string write-string%)

     (define read-file-to-vector read-file-to-vector%)
     (define read-vector read-vector%)
     (define write-vector write-vector%)
     (define write-to-file write-to-file%)
     (define append-to-file append-to-file%)
     (define set-file-position set-file-position%)))

(coalton-toplevel
  (derive-monad-io-file :m (st:StateT :s :m))
  (derive-monad-io-file :m (env:EnvT :e :m))
  (derive-monad-io-file :m (LoopT :m))
  )

;;
;; Functions Using MonadIoFile
;;

(coalton-toplevel

  (declare with-open-file ((file:File :a) (MonadIoFile :r) (MonadIoFile :i) (UnliftIo :r :i)
                           (LiftTo :r :m) (MonadException :i)
                           => file:StreamOptions
                           -> ((file:FileStream :a) -> :r :b)
                           -> :m :b))
  (define (with-open-file opts k)
     "Opens a file stream, performs K on it, then closes the stream.
Can run any underlying BaseIo, which can be useful but can also cause inference issues
in some cases. Try WITH-OPEN-FILE_ if you have issues."
    (lift-to
     (with-run-in-io
         (fn (run)
           (lift-io
            (bracket-io_ (raise-result (open opts))
                         (fn (file)
                           (raise-result (close file)))
                         (fn (file)
                           (run (k file)))))))))

  (declare with-temp-file ((file:File :a) (MonadIoFile :r) (MonadIoFile :i)
                           (UnliftIo :r :i) (LiftTo :r :m) (MonadException :i)
                           => String
                           -> ((file:FileStream :a) -> :r :b)
                           -> :m :b))
  (define (with-temp-file file-type k)
     "Performs an operation `thunk` on a temporary file. File type extensions need to include `.`
Can run any underlying BaseIo, which can be useful but can also cause inference issues
in some cases. Try WITH-TEMP-FILE_ if you have issues."
    (lift-to
     (with-run-in-io
         (fn (run)
           (lift-io
            (let ((filepath (file::%make-temp-file-pathname file-type)))
              (bracket-io_ (raise-result  (open (file:Bidirectional filepath file:Overwrite)))
                           (fn (_)
                             (raise-result (delete-file filepath)))
                           (fn (file)
                             (run (k file))))))))))

  (declare with-temp-directory ((UnliftIo :r :i) (LiftTo :r :m) (MonadException :i)
                                (MonadIoFile :i)
                                => (file:Pathname -> :r :a)
                                -> :m :a))
  (define (with-temp-directory k)
      "Performs an operation `thunk` inside a temporary directory.
Can run any underlying BaseIo, which can be useful but can also cause inference issues
in some cases. Try WITH-TEMP-DIRECTORY_ if you have issues."
    (lift-to
     (with-run-in-io
         (fn (run)
           (lift-io
            (bracket-io_ (raise-result create-temp-directory)
                         remove-directory-recursive
                         (fn (pathname)
                           (run (k pathname)))))))))

  )

(cl:defmacro do-with-open-file_ (opts (fs) cl:&body body)
  "`do` sugar for `with-open-file_`. Expands to a continuation where BODY runs in `do`.

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

;;
;; Simple IO Implementation
;;

(coalton-toplevel

  (implement-monad-io-file io:IO)

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

(cl:defmacro do-with-open-file (opts (fs) cl:&body body)
  "`do` sugar for `with-open-file`. Expands to a continuation where BODY runs in `do`.

Usage:
  (do-with-open-file opts (fs)
    (line <- (read-char fs))
    ...)
"
  `(with-open-file ,opts (fn (,fs) (do ,@body))))

(cl:defmacro do-with-temp-file (type (fs) cl:&body body)
  "`do` sugar for `with-temp-file` (TYPE is a string like \"txt\")."
  `(with-temp-file ,type (fn (,fs) (do ,@body))))

(cl:defmacro do-with-temp-directory ((dir) cl:&body body)
  "`do` sugar for `with-temp-directory`."
  `(with-temp-directory (fn (,dir) (do ,@body))))
