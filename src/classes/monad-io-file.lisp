(cl:in-package :cl-user)
(defpackage :io/classes/monad-io-file
  (:use
   #:coalton
   #:coalton-prelude
   #:io/monad-io)
  (:import-from #:coalton-library/types
   #:RuntimeRepr)
  (:local-nicknames
   (:file #:coalton-library/file))
  (:export
   #:MonadIoFile
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
   #:set-file-position))
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

