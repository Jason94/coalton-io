(cl:in-package :cl-user)

(ql:quickload "coalton/doc")
(ql:quickload "coalton-io")
(defpackage :io.doc
  (:use :cl))
(in-package :io.doc)

(defun write-docs (&key
                     (pathname "../docs/index.html")
                     (packages (mapcar
                                (lambda (p)
                                  (coalton/doc/model::make-coalton-package (find-package p)
                                                                           :reexported-symbols t))
                                (list
                                 'io/thread-exceptions
                                 'io/exception
                                 'io/monad-io
                                 'io/simple-io
                                 'io/resource
                                 'io/mut
                                 'io/term
                                 'io/random
                                 'io/thread
                                 'io/file
                                 'io/unique
                                 'io/stm
                                 'io/conc/future
                                 'io/conc/atomic
                                 'io/conc/mvar
                                 'io/conc/group
                                 'io/io-all
                                 'io/stubs/term
                                 ))))

  (coalton/doc:write-documentation
    pathname
    packages
    :local-path (namestring (asdf:system-source-directory "coalton-io"))
    :remote-path "https://github.com/Jason94/coalton-io/tree/master"
    :backend :html))

(write-docs)
