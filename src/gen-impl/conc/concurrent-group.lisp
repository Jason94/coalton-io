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
   #:io/resource
   )
  (:local-nicknames
   (:lk #:coalton-threads/lock))
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
does not pass messages/data. For more structured uses, see WorkerPool.

ConcurrentGroup provides the following guarantees:
* Masking/unmasking the group is atomic. If another thread attempts to stop the group, it will
  either stop all of the Concurrents or none of them.
* Stopping the group sends the stop signal to all of the enclosed Concurrents.
* Awaiting the group blocks until all of the enclosed Concurrents have completed.
* Awaiting the group will error if any one of the enclosed Concurrents errored.
* Calling unmask-finally on the group runs the callback on each Concurrent separately, not
  once on the thread calling (unmask-finally).

ConcurrentGroup's guarantees are only valid if management of the enclosed Concurrents is
done through the group. For example, if one thread tries to mask the group while another
thread tries to stop one of the individual Concurrents in the group, then the second
thread might stop the individual Concurrent before the first thread has a chance to mask
it.

Concurrent:
  - The `(Concurrent ConcurrentGroup :a)' instance masks `stop`, `mask`, `unmask`,
    and `unmask-finally` to guarantee atomicity of the enclosed Concurrents
  - `await` is not masked"
    (pool (List :c))
    (lock lk:Lock))

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
ConcurrentGroup.

Concurrent:
  - Does not mask the forking operation, so it is possible to partially fork if stopped.
    To guarantee forking completion, caller should mask the call to `fork-group`."
    (do
     (concurrents <- (sequence fork-concurrents))
     (pure (ConcurrentGroup concurrents (lk:new)))))

  (declare enclose-group ((MonadIoThread :rt :t :m) (Concurrent :c :a)
                       => List :c -> :m (ConcurrentGroup :c :a)))
  (define (enclose-group concurrents)
    "Enclose already forked Concurrents in a ConcurrentGroup.

Warning: After calling, the enclosed Concurrents should only be managed through the group."
    (wrap-io (ConcurrentGroup concurrents (lk:new))))

  (inline)
  (declare await% ((MonadIoThread :rt :t :m) (MonadException :m) (Concurrent :c :a)
                   => ConcurrentGroup :c :a -> :m (List :a)))
  (define (await% group)
    ;; CONCURRENT: Doesn't mask because (1) await blocks, and (2) await doesn't modify
    ;; the state of the enclosed threads.
    (do-collect (t (.pool group))
      (await t)))

  (declare stop% ((MonadIoThread :rt :t :m) (Concurrent :c :a) (MonadException :m)
                  => ConcurrentGroup :c :a -> :m Unit))
  (define (stop% group)
    ;; CONCURRENT: Masks entire operation to guarantee enclosed threads are left in a
    ;; consistent state.
    (let cnc-prx = (value-concurrent-prx (value-prx group)))
    (bracket-io-masked_
     (wrap-io (lk:acquire (.lock group)))
     (fn (_)
       (wrap-io (lk:release (.lock group))))
     (fn (_)
       (foreach (.pool group) (as-proxy-of stop
                                           (proxy-with-arg cnc-prx))))))

  (declare mask% ((MonadIoThread :rt :t :m) (Concurrent :c :a) (MonadException :m)
                  => ConcurrentGroup :c :a -> :m Unit))
  (define (mask% group)
    ;; CONCURRENT: Masks entire operation to guarantee enclosed threads are left in a
    ;; consistent state.
    (let cnc-prx = (value-concurrent-prx (value-prx group)))
    (bracket-io-masked_
     (wrap-io (lk:acquire (.lock group)))
     (fn (_)
       (wrap-io (lk:release (.lock group))))
     (fn (_)
       (foreach (.pool group) (as-proxy-of mask
                                           (proxy-with-arg cnc-prx))))))

  (declare unmask% ((MonadIoThread :rt :t :m) (Concurrent :c :a) (MonadException :m)
                  => ConcurrentGroup :c :a -> :m Unit))
  (define (unmask% group)
    ;; CONCURRENT: Masks entire operation to guarantee enclosed threads are left in a
    ;; consistent state.
    (let cnc-prx = (value-concurrent-prx (value-prx group)))
    (bracket-io-masked_
     (wrap-io (lk:acquire (.lock group)))
     (fn (_)
       (wrap-io (lk:release (.lock group))))
     (fn (_)
       (foreach (.pool group) (as-proxy-of unmask
                                           (proxy-with-arg cnc-prx))))))

  (declare unmask-finally% ((UnliftIo :r :io) (LiftTo :r :m) (MonadIoThread :rt :t :r) (MonadException :m)
                            (Concurrent :c :a) (MonadIoThread :rt :t :m)
                            => ConcurrentGroup :c :a -> (UnmaskFinallyMode -> :r :b) -> :m Unit))
  (define (unmask-finally% group callback)
    ;; CONCURRENT: Masks entire operation to guarantee enclosed threads are left in a
    ;; consistent state.
    (let cnc-prx = (value-concurrent-prx (value-prx group)))
    (bracket-io-masked_
     (wrap-io (lk:acquire (.lock group)))
     (fn (_)
       (wrap-io (lk:release (.lock group))))
     (fn (_)
       (foreach (.pool group)
                (as-proxy-of
                 (fn (t)
                   (unmask-finally t callback))
                 (proxy-with-arg cnc-prx))))))

  (define-instance (Concurrent :c :a => Concurrent (ConcurrentGroup :c :a) (List :a))
    (inline)
    (define stop stop%)
    (define await await%)
    (define mask mask%)
    (define unmask unmask%)
    (define unmask-finally unmask-finally%))
  )
