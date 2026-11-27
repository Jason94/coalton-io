(defpackage #:coalton-io/docs
  (:use #:cl)
  (:export #:write-docs))

(in-package #:coalton-io/docs)

(defun write-docs (&key
                     (html-pathname (merge-pathnames #p"docs/index.html"
                                                   (asdf:system-source-directory "coalton-io")))
                     (markdown-pathname (merge-pathnames #p"docs/index.md"
                                                       (asdf:system-source-directory "coalton-io")))
                     (packages (mapcar
                                (lambda (p)
                                  (coalton/doc/model::make-coalton-package
                                   (find-package p)
                                   :reexported-symbols t))
                                (list
                                 'io/threads-exceptions
                                 'io/exceptions
                                 'io/monad-io
                                 'io/simple-io
                                 'io/simple-io/loops
                                 'io/resource
                                 'io/mut
                                 'io/term
                                 'io/random
                                 'io/thread
                                 'io/delay
                                 'io/file
                                 'io/unique
                                 'io/network
                                 'io/conc/parking
                                 'io/conc/group
                                 'io/conc/scheduler
                                 'io/conc/worker-pool
                                 'io/conc/future
                                 'io/conc/atomic
                                 'io/conc/mvar
                                 'io/conc/queues
                                 'io/conc/queues/bounded-mpmc
                                 'io/conc/queues/unbounded-mpmc
                                 'io/conc/stm
                                 'io/conc/stm/tarray
                                 'io/io-all
                                 'io/stubs/term
                                 )))
                     (remote-path "https://github.com/Jason94/coalton-io/tree/master"))
  (let ((local-path (namestring (asdf:system-source-directory "coalton-io"))))
    (coalton/doc:write-documentation
     html-pathname
     packages
     :local-path local-path
     :remote-path remote-path
     :backend :html)
    (coalton/doc:write-documentation
     markdown-pathname
     packages
     :local-path local-path
     :remote-path remote-path
     :backend :markdown)))
