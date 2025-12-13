(cl:in-package :cl-user)
(defpackage :io/thread-impl/data-broadcast-pool
  (:use
   #:coalton
   #:coalton-prelude
   #:io/utils
   #:io/thread-exceptions
   #:io/thread-impl/runtime
   )
  (:local-nicknames
   (:lk  #:coalton-threads/lock)
   (:c #:coalton-library/cell)
   (:cv  #:coalton-threads/condition-variable)
   (:at #:io/thread-impl/atomics)
   )
  (:export
   #:DataBroadcastPool
   #:new-broadcast-pool
   #:publish
   #:subscribe
   ))
(in-package :io/thread-impl/data-broadcast-pool)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (define-struct (VersionEntry :a)
    (version Word)
    (remaining-subscribers at:AtomicInteger)
    (data :a))

  (inline)
  (declare stale?% (Optional (VersionEntry :a) -> Boolean))
  (define (stale?% entry?)
    (match entry?
      ((Some entry)
       (zero? (at:read-at-int (.remaining-subscribers entry))))
      ((None)
       False)))

  (inline)
  (declare checkout!% (Word -> (at:AtomicStack (VersionEntry :a)) -> :a))
  (define (checkout!% version version-entries)
    "Checkout the data for VERSION, decrementing the number of remaining subscribers.
Errors if the version can't be found."
    (for entry in version-entries
      (let _ = (the (VersionEntry :a) entry))
      (when (== (.version entry) version)
        (at:atomic-dec1 (.remaining-subscribers entry))
        (return (.data entry))))
    (error (build-str "ERROR: DataBroadcastPool missing data for version " version)))

  (inline)
  (declare new-version-entry (Word -> Word -> :a -> VersionEntry :a))
  (define (new-version-entry version n-subscribers val)
    (VersionEntry version
                  (at:new-at-int n-subscribers)
                  val))

  (define-struct (DataBroadcastPool :a)
    "The Data Broadcast Pool allows multiple threads to subscribe to the pool.
Producer threads can publish to the pool. During a publish, all subscribers
are notified and sent the data was published, and are unsubscribed. The
pool guarantees that all threads that were subscribed at publish P will see
the same published data, even if publish P+1 runs before a subscriber to P
has a chance to run.

The pool takes care of masking itself during critical periods, so it can't
be left in an inconsistent state if another thread were to attempt an
interrupt during, for example, a publish."
    (version-entries (at:AtomicStack (VersionEntry :a)))
    (notify-cv       cv:ConditionVariable)
    ;; Lock used for waking subscribers
    (notify-lock     lk:Lock)
    ;; Lock blocking publishes
    (publish-lock    lk:Lock)
    ;; TODO: This can probably just be a cell.
    (n-subscribers   at:AtomicInteger)
    (version         at:AtomicInteger))

  (inline)
  (declare new-broadcast-pool (Unit -> DataBroadcastPool :a))
  (define (new-broadcast-pool)
    (DataBroadcastPool (at:new-atomic-stack)
                       (cv:new)
                       (lk:new)
                       (lk:new)
                       (at:new-at-int 0)
                       (at:new-at-int 0)))

  (inline)
  (declare cleanup-stack% (DataBroadcastPool :a -> Unit))
  (define (cleanup-stack% pool)
    "Remove version entries of the stack that have no subscribers
left to checkout the data. ASSUMES THE PUBLISH LOCK IS HELD AND
THE THREAD IS MASKED."
    (while (stale?% (at:at-st-peek (.version-entries pool)))
      (at:at-st-pop-front! (.version-entries pool))))

  (declare publish (DataBroadcastPool :a -> :a -> Unit))
  (define (publish pool data)
    (unless (zero? (at:read-at-int (.n-subscribers pool)))
      (mask-current-thread!%)
      (lk:acquire (.publish-lock pool))
      (lk:acquire (.notify-lock pool))
      ;; First, check to make sure that the pool didn't receive another publish
      ;; in between checking the number of subscribers and acquiring the lock.
      (unless (zero? (at:read-at-int (.n-subscribers pool)))
        ;; Second, take this opportunity to clean up the stack, since we have
        ;; the lock.
        (cleanup-stack% pool)
        ;; Third, commit this version of the data.
        (let n-subscribers = (at:atomic-int-write (.n-subscribers pool) 0))
        (let version = (1- (at:atomic-inc1 (.version pool))))
        (let new-entry = (new-version-entry version n-subscribers data))
        (at:at-st-push-front! new-entry (.version-entries pool))
        ;; Fourth, notify subscribers.
        (cv:broadcast (.notify-cv pool))
        )
      (lk:release (.notify-lock pool))
      (lk:release (.publish-lock pool))
      (unmask-current-thread!%)
      ))

  (declare subscribe (DataBroadcastPool :a -> :a))
  (define (subscribe pool)
    "Subscribe to the pool, and block until a publish is made."
    (mask-current-thread!%)
    (lk:acquire (.notify-lock pool))
    (at:atomic-inc1 (.n-subscribers pool))
    (let version = (at:read-at-int (.version pool)))
    (rec % ()
      (unmask-current-thread-finally!%
       (fn (mode)
         (if (== Running mode)
             (cv:await (.notify-cv pool) (.notify-lock pool))
             (progn
               (lk:release (.notify-lock pool))
               Unit))))
      ;; Protect against spurious wake-ups
      (when (== version (at:read-at-int (.version pool)))
        (mask-current-thread!%)
        (%)))
    (lk:release (.notify-lock pool))
    (checkout!% version (.version-entries pool)))

  )
