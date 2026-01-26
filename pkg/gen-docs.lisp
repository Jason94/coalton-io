(defpackage #:coalton-io/docs
  (:use #:cl)
  (:export #:write-docs))

(in-package #:coalton-io/docs)

(defun write-docs (&key
                     (pathname (merge-pathnames #p"docs/index.html"
                                               (asdf:system-source-directory "coalton-io")))
                     (packages (mapcar
                                (lambda (p)
                                  (coalton/doc/model::make-coalton-package
                                   (find-package p)
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
                                 'io/network
                                 'io/conc/parking
                                 'io/conc/scheduler
                                 'io/conc/future
                                 'io/conc/atomic
                                 'io/conc/mvar
                                 'io/conc/group
                                 'io/conc/stm
                                 'io/conc/mchan-scheduler
                                 'io/conc/worker-pool
                                 'io/conc/ring-buffer
                                 'io/io-all
                                 'io/stubs/term
                                 )))
                     (remote-path "https://github.com/Jason94/coalton-io/tree/master"))
  (coalton/doc:write-documentation
   pathname
   packages
   :local-path (namestring (asdf:system-source-directory "coalton-io"))
   :remote-path remote-path
   :backend :html))
