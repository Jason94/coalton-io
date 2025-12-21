(cl:in-package :cl-user)
(defpackage :io/gen-impl/conc/group
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/types
   #:coalton-library/monad/classes
   #:io/utils
   #:io/thread-exceptions
   #:io/classes/monad-exception
   #:io/classes/monad-io
   #:io/classes/monad-io-thread
   )
  (:import-from #:coalton-library/experimental/do-control-loops
   #:collect
   #:foreach
   #:do-collect)
  (:export
   #:ConcurrentGroup
   #:fork-group
   #:enclose-group
   ))
(in-package :io/gen-impl/conc/group)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (define-struct (ConcurrentGroup :c :a)
    "Handles masking, stopping, and awaiting a group of Concurrents as a unit. ConcurrentGroup
does not pass messages/data. For more structured uses, see ConcurrentPool.

ConcurrentGroup provides the following guarantees:
* Masking/unmasking the group is atomic. If another thread attempts to stop the group, it will
  either stop all of the Concurrents or none of them.
* When you stop the group, it will send the stop signal to all of the enclosed Concurrents.
* When you await the group, it will block until all of the enclosed Concurrents have completed.
* When you await the group, it will error if any one of the enclosed Concurrents errored.

ConcurrentGroups guarantees are only valid if management of the enclosed Concurrents is done
through the group. For example, if one thread tries to mask the group while another thread tries
to stop one of the individual Concurrents in the group, then the second thread might stop the
individual Concurrent before the first thread has a chance to mask it."
    (pool (List :c)))

  (inline)
  (declare concurrent-prx (ConcurrentGroup :c :a -> Proxy :c))
  (define (concurrent-prx _)
    Proxy)

  (inline)
  (declare value-prx (ConcurrentGroup :c :a -> Proxy :a))
  (define (value-prx _)
    Proxy)

  (declare fork-group ((MonadIoThread :rt :t :m) (Concurrent :c :a)
                       => List (:m :c) -> :m (ConcurrentGroup :c :a)))
  (define (fork-group fork-concurrents)
    "Run a list of IO operations that each forks a Concurrent. Enclose the forked Concurrents in a
ConcurrentGroup."
    (do
     (concurrents <- (sequence fork-concurrents))
     (pure (ConcurrentGroup concurrents))))

  (declare enclose-group ((MonadIoThread :rt :t :m) (Concurrent :c :a)
                       => List :c -> :m (ConcurrentGroup :c :a)))
  (define (enclose-group concurrents)
    "Enclose already forked Concurrents in a ConcurrentGroup.

Warning: After calling, the enclosed Concurrents should only be managed through the group."
    (wrap-io (ConcurrentGroup concurrents)))

  (inline)
  (declare await% ((MonadIoThread :rt :t :m) (MonadException :m) (Concurrent :c :a)
                   => ConcurrentGroup :c :a -> :m (List :a)))
  (define (await% group)
    (do-collect (t (.pool group))
      (await t)))

  (inline)
  (declare stop% ((MonadIoThread :rt :t :m) (Concurrent :c :a)
                  => ConcurrentGroup :c :a -> :m Unit))
  (define (stop% group)
    (let _ = (the (ConcurrentGroup :c :a) group))
    (let cnc-prx = (value-concurrent-prx (value-prx group)))
    (foreach (.pool group) (as-proxy-of stop
                                        (proxy-with-arg cnc-prx))))

  (define-instance (Concurrent :c :a => Concurrent (ConcurrentGroup :c :a) (List :a))
    (inline)
    (define stop stop%)
    (inline)
    (define await await%)
    (inline)
    (define (mask _)
      (error ""))
    (inline)
    (define (unmask _)
      (error ""))
    (inline)
    (define (unmask-finally _)
      (error "")))


  )
