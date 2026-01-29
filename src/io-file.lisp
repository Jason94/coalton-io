(cl:in-package :cl-user)
(defpackage :io/files
  (:use
   #:io/classes/files
   #:io/io-impl/file
   #:io/gen-impl/file)
  (:export
   ;; Re-exports from io/classes/files
   #:Files
   #:derive-files
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

   #:read-line#
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

   ;; Re-exports from io/gen-impl/file
   #:implement-files
   ))
(in-package :io/files)
