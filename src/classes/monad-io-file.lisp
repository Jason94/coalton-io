(cl:in-package :cl-user)
(defpackage :io/classes/monad-io-file
  (:use
   #:coalton
   #:coalton-prelude
   #:io/utils
   #:io/classes/monad-io)
  (:import-from #:coalton-library/types
   #:RuntimeRepr)
  (:import-from #:coalton-library/experimental/do-control-loops-adv
   #:LoopT)
  (:local-nicknames
   (:st   #:coalton-library/monad/statet)
   (:env  #:coalton-library/monad/environment)
   (:file #:coalton-library/file))
  (:export
   ;; Library Public
   #:MonadIoFile
   #:derive-monad-io-file
   #:exists?
   #:file-exists?
   #:directory-exists?
   #:open
   #:close
   #:abort
   #:create-temp-directory
   #:create-temp-file
   #:copy
   #:create-directory
   #:delete-file
   #:remove-directory
   #:remove-directory-recursive
   #:system-relative-pathname
   #:read-file-to-string
   #:read-file-lines
   #:read-char
   #:read-line
   #:write-char
   #:write-line
   #:write-string
   #:read-file-to-vector
   #:read-vector
   #:write-vector
   #:write-to-file
   #:append-to-file
   #:set-file-position

   ;; Library Private
   #:create-temp-directory%
   #:create-temp-file%
   ))
(in-package :io/classes/monad-io-file)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
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

;;;
;;; These functions don't need to be lift-ed
;;;

(coalton-toplevel

  (declare create-temp-directory% (MonadIo :m => :m (Result file:FileError file:Pathname)))
  (define create-temp-directory%
    (wrap-io (file:create-temp-directory!)))

  (declare create-temp-file% (MonadIo :m => String -> :m (Result file:FileError file:Pathname)))
  (define (create-temp-file% file-ext)
    (wrap-io (file:create-temp-file! file-ext)))

  )

(defmacro derive-monad-io-file (monad-param monadT-form)
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

(coalton-toplevel
  (derive-monad-io-file :m (st:StateT :s :m))
  (derive-monad-io-file :m (env:EnvT :e :m))
  (derive-monad-io-file :m (LoopT :m))
  )
