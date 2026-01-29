(cl:in-package :cl-user)
(defpackage :io/gen-impl/file
  (:use
   #:coalton
   #:coalton-prelude
   #:io/utils
   #:io/classes/exceptions
   #:io/classes/monad-io
   #:io/classes/files
   #:io/classes/threads
   #:io/resource)
  (:import-from #:coalton-library/types
   #:RuntimeRepr)
  (:import-from #:coalton-library/monad/classes
   #:LiftTo #:lift-to)
  (:local-nicknames
   (:f_ #:coalton-library/file)
   )
  (:export
   ;; Library Public
   #:implement-files
   #:with-open-file
   #:with-temp-file
   #:with-temp-directory
   #:do-with-open-file
   #:do-with-temp-file
   #:do-with-temp-directory

   ;; Library Private
   #:exists?%
   #:file-exists?%
   #:directory-exists?%
   #:open%
   #:close%
   #:abort%
   #:copy%
   #:create-directory%
   #:delete-file%
   #:remove-directory%
   #:remove-directory-recursive%
   #:system-relative-pathname%
   #:read-file-to-string%
   #:read-file-lines%
   #:write-to-file%
   #:append-to-file%
   #:read-char%
   #:read-line%
   #:write-char%
   #:write-line%
   #:write-string%
   #:read-file-to-vector%
   #:read-vector%
   #:write-vector%
   #:set-file-position%
   #:create-temp-directory%
   #:create-temp-file%
   ))
(in-package :io/gen-impl/file)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (declare exists?% ((Into :a f_:Pathname) (MonadIo :m) => :a -> :m (Result f_:FileError Boolean)))
  (define (exists?% pth)
    (wrap-io (f_:exists? pth)))

  (declare file-exists?% ((Into :a f_:Pathname) (MonadIo :m) => :a -> :m (Result f_:FileError Boolean)))
  (define (file-exists?% pth)
    (wrap-io (f_:file-exists? pth)))

  (declare directory-exists?% ((Into :a f_:Pathname) (MonadIo :m) => :a -> :m (Result f_:FileError Boolean)))
  (define (directory-exists?% pth)
    (wrap-io (f_:directory-exists? pth)))

  (declare open% ((f_:File :a) (MonadIo :m) => f_:StreamOptions -> :m (Result f_:FileError (f_:FileStream :a))))
  (define (open% opts)
    (wrap-io (f_:open opts)))

  (declare close% (MonadIo :m => (f_:FileStream :a) -> :m (Result f_:FileError :b)))
  (define (close% fs)
    (wrap-io (f_:close fs)))

  (declare abort% (MonadIo :m => (f_:FileStream :a) -> :m (Result f_:FileError :b)))
  (define (abort% fs)
    (wrap-io (f_:abort fs)))

  (declare copy% ((Into :a f_:Pathname) (Into :b f_:Pathname) (MonadIo :m) => :a -> :b -> :m (Result f_:FileError Unit)))
  (define (copy% a b)
    (wrap-io (f_:copy! a b)))

  (declare create-directory% ((Into :p f_:Pathname) (MonadIo :m) => :p -> :m (Result f_:FileError f_:Pathname)))
  (define (create-directory% p)
    (wrap-io (f_:create-directory! p)))

  (declare delete-file% ((Into :p f_:Pathname) (MonadIo :m) => :p -> :m (Result f_:FileError Unit)))
  (define (delete-file% p)
    (wrap-io (f_:delete-file! p)))

  (declare remove-directory% ((Into :p f_:Pathname) (MonadIo :m) => :p -> :m (Result f_:FileError :p)))
  (define (remove-directory% p)
    (wrap-io (f_:remove-directory! p)))

  (declare remove-directory-recursive% ((Into :p f_:Pathname) (MonadIo :m) => :p -> :m (Result f_:FileError Unit)))
  (define (remove-directory-recursive% p)
    (wrap-io (f_:remove-directory-recursive! p)))

  (declare system-relative-pathname% ((Into :sys String) (MonadIo :m) => :sys -> String -> :m (Result f_:FileError f_:Pathname)))
  (define (system-relative-pathname% sys name)
    (wrap-io (f_:system-relative-pathname sys name)))

  (declare read-file-to-string% ((Into :p f_:Pathname) (MonadIo :m) => :p -> :m (Result f_:FileError String)))
  (define (read-file-to-string% p)
    (wrap-io (f_:read-file-to-string p)))

  (declare read-file-lines% ((Into :p f_:Pathname) (MonadIo :m) => :p -> :m (Result f_:FileError (List String))))
  (define (read-file-lines% p)
    (wrap-io (f_:read-file-lines p)))

  (declare write-to-file% ((Into :p f_:Pathname) (f_:File :a) (RuntimeRepr :a) (MonadIo :m) => :p -> (Vector :a) -> :m (Result f_:FileError Unit)))
  (define (write-to-file% p v)
    (wrap-io (f_:write-to-file! p v)))

  (declare append-to-file% ((Into :p f_:Pathname) (f_:File :a) (RuntimeRepr :a) (MonadIo :m) => :p -> (Vector :a) -> :m (Result f_:FileError Unit)))
  (define (append-to-file% p v)
    (wrap-io (f_:append-to-file! p v)))

  (declare read-char% (MonadIo :m => (f_:FileStream Char) -> :m (Result f_:FileError Char)))
  (define (read-char% fs)
    (wrap-io (f_:read-char fs)))

  (declare read-line% (MonadIo :m => (f_:FileStream Char) -> :m (Result f_:FileError String)))
  (define (read-line% fs)
    (wrap-io (f_:read-line fs)))

  (declare write-char% (MonadIo :m => (f_:FileStream Char) -> Char -> :m (Result f_:FileError Unit)))
  (define (write-char% fs c)
    (wrap-io (f_:write-char fs c)))

  (declare write-line% (MonadIo :m => (f_:FileStream Char) -> String -> :m (Result f_:FileError Unit)))
  (define (write-line% fs s)
    (wrap-io (f_:write-line fs s)))

  (declare write-string% (MonadIo :m => (f_:FileStream Char) -> String -> :m (Result f_:FileError Unit)))
  (define (write-string% fs s)
    (wrap-io (f_:write-string fs s)))

  (declare read-file-to-vector% ((f_:File :a) (MonadIo :m) => (f_:FileStream :a) -> :m (Result f_:FileError (Vector :a))))
  (define (read-file-to-vector% fs)
    (wrap-io (f_:read-file-to-vector fs)))

  (declare read-vector% ((f_:File :a) (MonadIo :m) => (f_:FileStream :a) -> UFix -> :m (Result f_:FileError (Vector :a))))
  (define (read-vector% fs n)
    (wrap-io (f_:read-vector fs n)))

  (declare write-vector% ((f_:File :a) (RuntimeRepr :a) (MonadIo :m) => (f_:FileStream :a) -> (Vector :a) -> :m (Result f_:FileError Unit)))
  (define (write-vector% fs v)
    (wrap-io (f_:write-vector fs v)))

  (declare set-file-position% (MonadIo :m => (f_:FileStream :a) -> UFix -> :m (Result f_:FileError Unit)))
  (define (set-file-position% fs pos)
    (wrap-io (f_:set-file-position fs pos)))
)

(defmacro implement-files (monad)
  `(define-instance (Files ,monad)
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

;;
;; Functions Using Files
;;

(coalton-toplevel
  (declare with-open-file ((f_:File :a) (Files :i) (UnliftIo :r :i)
                           (LiftTo :r :m) (Exceptions :i) (Threads :rt :t :i)
                           => f_:StreamOptions
                           -> ((f_:FileStream :a) -> :r :b)
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

  (declare with-temp-file ((f_:File :a) (Files :i) (Threads :rt :t :i)
                           (UnliftIo :r :i) (LiftTo :r :m) (Exceptions :i)
                           => String
                           -> ((f_:FileStream :a) -> :r :b)
                           -> :m :b))
  (define (with-temp-file file-type k)
     "Performs an operation `thunk` on a temporary file. File type extensions need to include `.`
Can run any underlying BaseIo, which can be useful but can also cause inference issues
in some cases. Try WITH-TEMP-FILE_ if you have issues."
    (lift-to
     (with-run-in-io
         (fn (run)
           (lift-io
            (let ((filepath (f_::%make-temp-file-pathname file-type)))
              (bracket-io_ (raise-result  (open (f_:Bidirectional filepath f_:Overwrite)))
                           (fn (_)
                             (raise-result (delete-file filepath)))
                           (fn (file)
                             (run (k file))))))))))

  (declare with-temp-directory ((UnliftIo :r :i) (LiftTo :r :m) (Exceptions :i)
                                (Files :i) (Threads :rt :t :i)
                                => (f_:Pathname -> :r :a)
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

(defmacro do-with-open-file (opts (fs) cl:&body body)
  "`do` sugar for `with-open-file`. Expands to a continuation where BODY runs in `do`.

Usage:
  (do-with-open-file opts (fs)
    (line <- (read-char fs))
    ...)
"
  `(with-open-file ,opts (fn (,fs) (do ,@body))))

(defmacro do-with-temp-file (type (fs) cl:&body body)
  "`do` sugar for `with-temp-file` (TYPE is a string like \"txt\")."
  `(with-temp-file ,type (fn (,fs) (do ,@body))))

(defmacro do-with-temp-directory ((dir) cl:&body body)
  "`do` sugar for `with-temp-directory`."
  `(with-temp-directory (fn (,dir) (do ,@body))))

