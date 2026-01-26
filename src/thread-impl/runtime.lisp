(cl:in-package :cl-user)
(defpackage :io/thread-impl/runtime
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/monad/classes
   #:coalton-library/types
   #:io/utils
   #:io/thread-exceptions
   #:io/classes/monad-io
   #:io/classes/monad-exception
   #:io/classes/monad-io-thread
   #:io/classes/runtime-utils
   )
  (:local-nicknames
   (:opt #:coalton-library/optional)
   (:c #:coalton-library/cell)
   (:b #:coalton-library/bits)
   (:v #:coalton-library/vector)
   (:t #:coalton-threads/thread)
   (:t/l #:coalton-threads/lock)
   (:at #:coalton-threads/atomic)
   (:bt #:bordeaux-threads-2)
   (:lk #:coalton-threads/lock)
   (:cv #:coalton-threads/condition-variable)
   )
  (:export
   ;; Library Public
   #:IoThread

   ;; Library Private
   #:current-thread!%
   #:construct-toplevel-current-thread
   #:sleep!%
   #:fork!%
   #:join!%
   #:stop!%
   #:mask!%
   #:unmask!%

   #:mask-current-thread!%
   #:unmask-finally!%
   #:unmask-finally%
   #:unmask-current-thread-finally!%
   #:unmask-current-thread-finally%
   #:unmask-current-thread!%
   #:stop-and-join-children!%
   #:*current-thread*
   #:*global-thread*

   #:park-current-thread-if!%
   #:park-current-thread-if-with!%
   #:unpark-thread!%

   #:write-line-sync%
   ))
(in-package :io/thread-impl/runtime)

(cl:declaim (cl:optimize (cl:speed 3) (cl:debug 0) (cl:safety 1)))

(named-readtables:in-readtable coalton:coalton)

;;; This package provides all of the "runtime" features for the canonical MonadIoThread
;;; implementation. For example, killing threads, masking, etc.
;;;
;;; If you wanted to build your own effect type that used a fundamentally different
;;; concurrency model, such as green threads on top of an m:n scheduler, you wouldn't
;;; ues any of this, and you'd provide your own implementation of the MonadIoThread interface.

(coalton-toplevel

  (define write-term-lock% (t/l:new))

  (define (write-line-sync% msg)
    (let thread-name =
      (lisp String ()
        (bt:thread-name (bt:current-thread))))
    (t/l:acquire write-term-lock%)
    (trace (build-str (force-string msg) " <" thread-name ">"))
    (t/l:release write-term-lock%))

  (declare atomic-fetch-or (at:AtomicInteger -> Word -> Word))
  (define (atomic-fetch-or at-int mask)
    (let old = (at:read at-int))
    (let new = (b:or old mask))
    (if (at:cas! at-int old new)
        new
        (atomic-fetch-or at-int mask)))

  (inline)
  (declare matches-flag (Word -> Word -> Boolean))
  (define (matches-flag a b)
    "Check if A and B share any 1 bits."
    (lisp Boolean (a b)
      (cl:logtest a b)))

  (inline)
  (declare current-native-thread% (Unit -> t:Thread (Result Dynamic :a)))
  (define (current-native-thread%)
    (lisp (t:Thread (Result Dynamic :a)) ()
      (bt:current-thread)))

  ;;;
  ;;; Basic Thread Type
  ;;;

  (derive Eq)
  (repr :enum)
  (define-type ThreadStatus
    ThreadRunning
    ThreadStopping
    ThreadStopped)

  ;; TODO: (t:Thread :a) is just bt2:Thread. Given the design differences, this should
  ;; just use bt2 directly and coalton-threads dependency should be dropped.

  ;; TODO: Like in STM, currently using the coalton-threads AtomicInteger instead of
  ;; my Atomics package. Not sure if there is a performance benefit or not to that.
  ;; Should test and - possibly - update my atomics package to match.

  ;; Flags - Not a traditional bit flag. First bit stores a pending kill. To support
  ;; nested masks, the remaining bits (as an integer) are the number of applied masks.
  ;; As such, masking/unmasking *once* is equivalent to +/- 2, and checking if we're
  ;; masked is shifting right 1 and checking if zero.
  ;; Clean        - 0           - 0000 0000
  ;; Pending Kill - (shift 0 1) - 0000 0001
  ;;
  ;; Parking Mechanism - IoThread uses a generational scheme to park/unpark. When a
  ;; function wants to park an IoThread, it calls (park-current-thread-if! WITH-GEN THREAD)
  ;; This:
  ;; - Increments the generation of the thread
  ;; - Calls WITH-GEN with the new generation. WITH-GEN is responsible for saving the
  ;;   generation so that it can be used to later unpark the thread.
  ;;   internal CV is notified).
  ;;
  ;; To unpark a thread, the waking thread must call (signal-unpark-thread! GEN THREAD).
  ;; If the generation used to park == the generation used to signal, then the thread
  ;; will unpark.
  ;; 
  ;; The purpose of the parking mechanism is to support waiting on multiple conditions.
  ;; If a thread wants to wait on conditions A, B, and C, and it is woken by the process
  ;; concerning condition A, then it would need to unsubscribe from conditions B & C.
  ;; Sometimes unsubscribing can be less efficient than letting the B & C processes
  ;; hold on to a stale reference to the thread in a waiting queue, and then fail to
  ;; unpark the thread when the B & C processes signal their waiting queue. Failure to
  ;; accomodate this scenario can lead to a situation where the thread parks, subscribed
  ;; to conditions D & E, but then process B or C erroneously unparks the thread that
  ;; is expecting to be unparked by only D or E.
  ;;
  ;; The generation mechanism supports this use-case, because in this scenario, processes
  ;; A, B, and C will be sent the same generation to unparked the thread. When the thread
  ;; is unparked by process A, any further parks will increment the generation of the thread.
  ;; Therefore, if B or C attempt to unpark the thread after A unparks the thread, then
  ;; the unpark will fail because the thread will either: (1) be on the same generation,
  ;; but not parked; or (2) have re-parked since being unparked, but ignores the unpark
  ;; signal from B or C because they signal with a stale generation.
  (define-struct IoThread
    (handle     (c:Cell (Optional (t:Thread (Result Dynamic Unit)))))
    ;; Masking/Unmasking
    (flags      at:AtomicInteger)
    (stop-lk    lk:Lock)
    ;; Parking/Unparking
    (generation at:AtomicInteger)
    (fired-gen  at:AtomicInteger)
    (park-lock  lk:Lock)
    (park-cv    cv:ConditionVariable)
    ;; Structured Concurrency
    ;; NOTE: status is protected by the child-lk, and can only be touched with that
    ;; lock held.
    (status     (c:Cell ThreadStatus))
    (child-lk   lk:Lock)
    (children   (c:Cell (List IoThread)))
    )

  (define-instance (Eq IoThread)
    (define (== a b)
      (and (unsafe-pointer-eq? (.handle a) (.handle b))
           (unsafe-pointer-eq? (c:read (.handle a)) (c:read (.handle b))))))

  (declare CLEAN Word)
  (define CLEAN 0)

  (inline)
  (declare new-io-thread (Unit -> IoThread))
  (define (new-io-thread)
    (IoThread
     (c:new None)
     (at:new CLEAN)
     (lk:new)
     (at:new 0)
     (at:new 0)
     (lk:new)
     (cv:new)
     (c:new ThreadRunning)
     (lk:new)
     (c:new Nil)
     ))

  (declare PENDING-KILL Word)
  (define PENDING-KILL (b:shift 0 1))

  (inline)
  (declare atomic-remove-pending-kill (at:AtomicInteger -> Word))
  (define (atomic-remove-pending-kill at-int)
    "Atomically remove the pending kill bit. Return the old bitmask."
    (let old = (at:read at-int))
    (let new = (b:shift 1 (b:shift -1 old)))
    (if (at:cas! at-int old new)
        old
        (atomic-remove-pending-kill at-int)))

  (inline)
  (declare mask-once% (Word -> Word))
  (define (mask-once% word)
    (+ word 2))

  (inline)
  (declare unmask-once% (Word -> Word))
  (define (unmask-once% word)
    (- word 2))

  (inline)
  (declare masked-once? (Word -> Boolean))
  (define (masked-once? word)
    "Check that WORD is masked only once."
    (== 1 (lisp Word (word)
            (cl:ash word -1))))

  (inline)
  (declare unmasked? (Word -> Boolean))
  (define (unmasked? word)
    (zero? (lisp Word (word)
             (cl:ash word -1))))

  (declare construct-toplevel-current-thread (Unit -> IoThread))
  (define (construct-toplevel-current-thread)
    (IoThread
     (c:new (Some (current-native-thread%)))
     (at:new CLEAN)
     (lk:new)
     (at:new 0)
     (at:new 0)
     (lk:new)
     (cv:new)
     (c:new ThreadRunning)
     (lk:new)
     (c:new Nil)
     ))
  )

;; To make sure that child threads have access to their current thread, store it
;; in a dynamic variable. Here be dragons. Tread carefully, you have been warned.
;; (The alternative is doing something even worse, like passing it around in the
;; IO monad.)

(cl:defvar *global-thread*
  cl:nil
  "Hold a reference to the global thread. Used for Detached fork scope.")
(cl:defvar *current-thread*
  cl:nil)

(coalton-toplevel

  (inline)
  (declare interrupt-iothread% (IoThread -> Unit))
  (define (interrupt-iothread% thd)
    "Stop an IoThread. Does not check masked state, etc. Does check if the target
thread is alive before interrupting.

Concurrent:
  - Masks the thread before throwing the exception. Prevents another thread stopping again
    before the thread completes cleanup.
  - Locks both child-lk (to protect status) and stop-lk (to protect stop race conditions)
  - Masks around the critical region"
    ;; TODO: It might be possible to only use the child lock here, but I'd be nervous about
    ;; leaning on that for too much
    (mask-current-thread!%)
    (lk:acquire (.stop-lk thd))
    (lk:acquire (.child-lk thd))
    (let should-interrupt = (== ThreadRunning (c:read (.status thd))))
    (when should-interrupt
      (c:write! (.status thd) ThreadStopping)
      Unit)
    (atomic-remove-pending-kill (.flags thd))
    (lk:release (.child-lk thd))
    (lk:release (.stop-lk thd))
    (unmask-current-thread!%)
    (when should-interrupt
      (let native-thread? = (c:read (.handle thd)))
      (match native-thread?
        ((None)
         (error "Tried to kill misconstructed thread."))
        ((Some native-thread)
         (mask!% thd)
         (lisp Void (native-thread)
           (cl:when (bt:thread-alive-p native-thread)
             (bt:error-in-thread native-thread (InterruptCurrentThread ""))))
         Unit))))

  ;;;
  ;;; Basic Thread Operations
  ;;;

  (inline)
  (declare current-thread!% (Unit -> IoThread))
  (define (current-thread!%)
    (lisp IoThread ()
      *current-thread*))

  (inline)
  (declare global-thread!% (Unit -> IoThread))
  (define (global-thread!%)
    (lisp IoThread ()
      *global-thread*))

  (declare subscribe-child!% (IoThread -> IoThread -> Boolean))
  (define (subscribe-child!% child parent)
    "Subscribe child to parent if the parent has Running status. Returns TRUE
if the parent was running and the child should continue, FALSE if the parent
was stopping/stopped and the child should not start."
    ;; CONCURRENT:
    ;; - Masks around entire critical region
    ;; - Don't need to mask parent thread beacuse holding the child lock prevents race
    ;;   condition in case where parent tries to stop before child gets subscribed
    (mask-current-thread!%)
    (lk:acquire (.child-lk parent))
    (let should-run? = (== (c:read (.status parent)) ThreadRunning))
    (when should-run?
      (c:push! (.children parent) child)
      Unit)
    (lk:release (.child-lk parent))
    (unmask-current-thread!%)
    should-run?)

  (declare stop-and-join-children!% (IoThread -> Unit))
  (define (stop-and-join-children!% thread)
    "Concurrent: WARNING, does not mask! This MUST be run inside a masked region."
    (lk:acquire (.child-lk thread))
    (for child in (c:read (.children thread))
      (stop!% child))
    (for child in (c:read (.children thread))
      (join!% child))
    (c:write! (.status thread) ThreadStopped)
    (lk:release (.child-lk thread))
    Unit)

  (declare handle-thread-err-result!% (ForkStrategy IoThread -> Dynamic -> Result Dynamic Unit))
  (define (handle-thread-err-result!% strategy e)
    (if (can-cast-to? e (the (Proxy ThreadingException) Proxy))
        (Ok Unit)
        (match (.unhandled strategy)
          ((LogAndSwallow)
           (if (dynamic-is-threading-exception? e)
               (Ok Unit)
               (progn
                 (lisp :a (e)
                   (cl:format cl:*error-output*
                              "~%Unhandled exception occurred: ~a~%"
                              e))
                 (Err e))))
          ((Swallow)
           (Err e))
          ((ThrowException)
           (throw-dynamic e)))))

  (declare thread-runner!% (ForkStrategy IoThread -> IoThread -> (Unit -> Result Dynamic :a) -> Result Dynamic Unit))
  (define (thread-runner!% strategy thread-container thunk)
    ;; CONCURRENT:
    ;; - Once the catch machinery is set up, unmask from the initial mask. Prevents
    ;;   race conditions on stopping in an unsafe region.
    ;; - Don't need to mask here because the invariant is guaranteeing that
    ;;   children get stopped before this thread completes. If the IO returns
    ;;   with an async stop exception or this function catches one, then it will
    ;;   be masked automatically by the stop function.
    ;; - Never need to unmask from a potential stop because the thread will end
    ;;   itself anyway.
    ;; - Blocks while masked while waiting for children to stop. This is part of
    ;;   the structured concurrency contract.
    ;; - Thread starts with one mask level, and unmasks itself once it enters the
    ;;   catch block.
    (c:write! (.handle thread-container)
              (Some (current-native-thread%)))
    (catch
        (progn
          (unmask!% thread-container)
          (let res = (c:new (thunk)))
          ;; NOTE: The current thread is the thread-container thread at this point
          (mask-current-thread!%)
          (stop-and-join-children!% thread-container)
          (unmask-current-thread!%)
          (match (c:read res)
            ((Ok _)
             (Ok Unit))
            ((Err e)
             (handle-thread-err-result!% strategy e))))
      ((InterruptCurrentThread msg)
       (stop-and-join-children!% thread-container)
       (handle-thread-err-result!% strategy
                                   (to-dynamic (InterruptCurrentThread msg))))))

  (declare fork-inner!% (ForkStrategy IoThread -> (Unit -> Result Dynamic :a) -> IoThread))
  (define (fork-inner!% strategy thunk)
    ;; Both the returning thread handle and the one made available to the child
    ;; thread have to have the IoThread packaged together before they do anything
    ;; meaningful. As such, we'll construct it and then each thread will set
    ;; native thread reference separately before they do any work. This guarantees
    ;; it will be available in either thread before subsequent code could reference it,
    ;; regardless of race conditions.
    (let thread-container = (new-io-thread))
    ;; CONCURRENT:
    ;; Start the thread masked. The thread runner will unmask itself before starting the
    ;; IO thunk, but after it sets up the async catch machinery to guarantee structured
    ;; cleanup.
    (mask!% thread-container)
    (let parent =
      (match (.scope strategy)
        ((Structured)
         (current-thread!%))
        ((Detached)
         (global-thread!%))
        ((StructuredIn t)
         t)))
    (let child-should-run? = (subscribe-child!% thread-container parent))
    ;; Dynamic variables don't cross the thread boundary, so we need to capture *global-thread*
    ;; so it can be re-propogated.
    (let global-thread = (global-thread!%))
    (let native-thread =
      ;; TODO: Could we use bt:make-threads's initial-bindings param to replace this
      ;; dynamic binding nonsense?? That would involve wrapping bt directly but honestly
      ;; we're basically there.
      ;; See https://sionescu.github.io/bordeaux-threads/threads/make-thread/
      (t:spawn (fn ()
                 (if child-should-run?
                     (lisp (Result Dynamic :a) (strategy thunk thread-container global-thread)
                       (cl:let ((*current-thread* thread-container)
                                (*global-thread* global-thread))
                         (call-coalton-function thread-runner!% strategy thread-container thunk)))
                   (Ok Unit)))))
    (c:write! (.handle thread-container) (Some native-thread))
    (when (not child-should-run?)
      (c:write! (.status thread-container) ThreadStopped)
      Unit)
    thread-container)

  (inline)
  (declare fork!% (ForkStrategy IoThread -> (Unit -> Result Dynamic :a) -> IoThread))
  (define (fork!% strategy thunk)
    (fork-inner!% strategy thunk))

  (inline)
  (declare join!% (IoThread -> Result Dynamic Unit))
  (define (join!% thread)
    (let native-thread = (opt:from-some "Error: IoThread leaked without setting native thread handle"
                                        (c:read (.handle thread))))
    (let join-result =
      (lisp (Result Dynamic Unit) (native-thread)
        (bt:join-thread native-thread)))
    (match join-result
      ((Ok _)
       join-result)
      ((Err dyn)
       (if (dynamic-is-threading-exception? dyn)
           (Ok Unit)
           join-result))))

  (inline)
  (declare sleep!% (UFix -> Unit))
  (define (sleep!% msecs)
    (lisp :a (msecs)
      (cl:sleep (cl:/ msecs 1000)))
    Unit)

  ;;;
  ;;; Stopping & Masking Threads
  ;;;

  (declare mask!% (IoThread -> Unit))
  (define (mask!% thread)
    ;; CONCURRENT:
    ;;   - Stops thread if it was previously unmasked AND the pending stop was
    ;;     previously set. Prevents a race condition where a stopped thread is
    ;;     masked between checking for unmasked and throwing the exception.
    (let flags = (.flags thread))
    (rec % ()
      (let old = (at:read flags))
      (let new = (mask-once% old))
      (if (at:cas! flags old new)
          ;; The PENDING-KILL bitmask is the kill bit set with no masking bits set
          (when  (== old PENDING-KILL)
            (interrupt-iothread% thread))
          (%)))
    Unit)

  (inline)
  (declare mask-current-thread!% (Unit -> Unit))
  (define (mask-current-thread!%)
    (mask!%
     (lisp IoThread ()
       *current-thread*)))

  ;; TODO: Merge this with unmask-current-thread-finally!% when MonadIoThread
  ;; loses the unmask other thread functions
  (declare unmask-finally!% (IoThread -> (UnmaskFinallyMode -> :a) -> Unit))
  (define (unmask-finally!% thread thunk)
    "Unmask the thread. Guarantees that THUNK will be run, regardless of pending
stop, with either the RUNNING or STOPPED mode. Finally, checks if there is a pending
stop and interrupts the thread if it finds one.

This function does NOT guarantee that THUNK will be called with STOPPED in all cases
in which this function ultimately interrupts the current thread before exiting.
However, even if the runtime used locks for masking threads, that is unavoidable.

The fundamental problem is that it will always be possible for another thread to try
to stop you while you're executing (THUNK RUNNING). You can do one of three things in
that instance:

1. Wrap (THUNK RUNNING) in a try/catch on this thread. Have the stopping thread signal us,
interrupting (THUNK RUNNING). On this thread, in the catch, detect that (THUNK RUNNING)
was stopped and re-call with (THUNK STOPPED).

This sounds good. But the problem is that (THUNK RUNNING) is supposed to be guaranteed
to execute. It could leave a resource in an un-recoverably inconsistent state.
It would also require every call to this function or any of the user-facing bracket
machinery to constantly hedge their cleanup functions against checking if it already
partially or completely ran. The second problem is a deal breaker. The first is fatal.

2. Ignore any stops that occur between checking the flag state, running the thunk, and
finally unmasking the thread. This would be possibly workable, but it would require
returning to stopping threads whether or not the stop succeeded. Then they could decide
what to do with that. The stopping thread would be responsible for re-stopping.

Again, this sounds good, but in practice it has serious problems. First, we don't
currently expose a way to check if a thread is masked to users. We probably shouldn't
because that's just too much surface area for race conditions that the runtime and
concurrency primitives are supposed to manage themselves. Second, the stopping thread
has no way of knowing how long it will take for this thread to finish its masked work.
In theory this thread could run masked indefinitely, which would effectively either block
the stopping thread or force it to give up on stopping this one.

3. The solution implemented here, where the mode that THUNK sees and what this function
ultimately decides to do can be inconsistent. This doesn't create any problems for the
stopping therad, and the only onus it puts on this thread is to guarantee that both
paths in THUNK cleanup whatever resource in a reasonable way. Ultimately that's something
they should be doing anyway. Suppose a function calling this (or higher level bracket-io
machinery, which will inherent this function's semantics) did something like this:

(bracket-io (open-my-file)
            (fn (mode)
              (if (== Stopped mode)
                 (cleanup-my-file)
                 (write-line 'Actually Im good. Ill leave the file open, thank you.')))
   (do-something-expensive-with-the-file))

That function could get immediately killed as soon as that bracket-io finished, at which
point the file would remain open and whatever subsequent code that was planning on using
the file would not be run. A proper use of the Stopped/Running distinction is from
the MVar implementation:

    (unmask-current-thread-finally!%
     (fn (mode)
       (if (== Running mode)
           (cv:await cv lock)
           (progn
             (lk:release lock)
             Unit)))))

Where both codepaths release the lock, but in a slightly different way. And notice that our
choice to implement solution #3 doesn't have any negative impact, because even if the Running
path did run, the thread could still be stopped right after it started awaiting the CV,
which would have the same effect anyway.

---
Again, this is a fundamental problem with concurrency. Using locks instead of pure
atomics to implement masking would not help. Forgoing asynchronous exceptions and switching
to a purely cooperative model also wouldn't help. You'd have the same problem, but you'd
just be limited to implementing only solutions #2 or #3.
"
    (let flags = (.flags thread))
    ;; Wait to unmask until *after* we guarantee thunk has been run.
    (let flag-state = (at:read flags))
    ;; Only stop if there are no other masks applied besides the one
    ;; we're undoing now.
    (if (and (matches-flag flag-state PENDING-KILL)
             (masked-once? flag-state))
        (thunk Stopped)
        (thunk Running))
    (let new-flag-state =
      (rec % ()
        (let old = (at:read flags))
        (let new = (unmask-once% old))
        (if (at:cas! flags old new)
            new
            (%))))
    (when (and (masked-once? flag-state)
               (matches-flag new-flag-state PENDING-KILL))
      (interrupt-iothread% thread))
    Unit)

  (inline)
  (declare unmask!% (IoThread -> Unit))
  (define (unmask!% thread)
    (unmask-finally!% thread (const Unit)))

  (inline)
  (declare unmask-current-thread-finally!% ((UnmaskFinallyMode -> Unit) -> Unit))
  (define (unmask-current-thread-finally!% thunk)
    (unmask-finally!% (current-thread!%) thunk))

  (inline)
  (declare unmask-current-thread!% (Unit -> Unit))
  (define (unmask-current-thread!%)
    (unmask-current-thread-finally!%
     (const Unit)))

  (inline)
  (declare unmask-finally% ((UnliftIo :r :io) (LiftTo :r :m)
                            => IoThread -> (UnmaskFinallyMode -> :r Unit) -> :m Unit))
  (define (unmask-finally% thread thunk)
    (lift-to
     (with-run-in-io
         (fn (run)
           (wrap-io (unmask-finally!% thread (fn (m) (run! (run (thunk m))))))))))

  (inline)
  (declare unmask-current-thread-finally% ((UnliftIo :r :io) (LiftTo :r :m)
                                           => (UnmaskFinallyMode -> :r Unit) -> :m Unit))
  (define (unmask-current-thread-finally% thunk)
    (lift-to
     (with-run-in-io
         (fn (run)
           (wrap-io (unmask-current-thread-finally!% (fn (m) (run! (run (thunk m))))))))))

  (inline)
  (declare stop!% (IoThread -> Unit))
  (define (stop!% thread)
    (let flag-state = (atomic-fetch-or (.flags thread) PENDING-KILL))
    (when (unmasked? flag-state)
      (interrupt-iothread% thread)))

  ;;;
  ;;; Parking & Unparking Threads
  ;;;

  ;; For full discussion of the park algorithm, see top of the file and docs/runtime.md
  (declare park-current-thread-if-with!% (Runtime :rt IoThread
                                          => Proxy :rt
                                          -> (Generation -> Unit)
                                          -> (Unit -> Boolean)
                                          -> TimeoutStrategy
                                          -> Unit))
  (define (park-current-thread-if-with!% rt-prx with-gen should-park? strategy)
    ;; CONCURRENT:
    ;; - Masks before acquiring the lock and unmasks after releasing the lock,
    ;;   so the thread can't be stopped while the lock is held
    ;; - Acquires park-lock before running with-gen so there's no race condition window
    ;;   where the thread has been subscribed, but isn't waiting on the CV yet.
    ;; - (A) unmasking before checking if it should re-attempt parking is valid,
    ;;   because it does not create a race-condition boundary, because the thread
    ;;   can be stopped while the function is waiting on the CV anyway.
    ;; - (B) unmask-and-await-safely% guarantees that the thread can't be stopped while
    ;;   the lock and CV operations are performed such that the lock is held and the
    ;;   thread stopped
    ;; - (C) unmask the mask from unmask-and-await-safely%
    (mask-current-thread!%)
    (let thread = (current-thread!%))
    (lk:acquire (.park-lock thread))
    ;; Checkout a new generation for the thread
    (let new-gen = (at:incf! (.generation thread) 1))
    ;; Run any subscriptions with the new generation
    (with-gen (Generation new-gen))
    ;; (Re)check the "should I park?" pred now that the subscriptions have processed
    (if (should-park?)
        (progn
          ;; If another thread beat us to parking, re-attempt if SHOULD-PARK?
          (if (>= (at:read (.fired-gen thread)) new-gen)
              (progn
                (lk:release (.park-lock thread))
                (unmask-current-thread!%) ;; (A)
                (when (should-park?)
                  (park-current-thread-if!% rt-prx with-gen should-park?)))
              ;; If another thread did not beat us to parking, wait on the CV
              (rec wait-loop ()
                (unmask-and-await-safely-with% ;; (B)
                 rt-prx
                 strategy
                 (.park-cv thread)
                 (.park-lock thread))
                ;; If we've been woken up, unmask, release, and return
                (if (>= (at:read (.fired-gen thread)) new-gen)
                    (progn
                      (lk:release (.park-lock thread))
                      (unmask-current-thread!%)) ;; (C)
                    ;; Otherwise, re-loop
                    (wait-loop)))))
        (progn
          (lk:release (.park-lock thread))
          (unmask-current-thread!%))))

  (inline)
  (declare park-current-thread-if!% (Runtime :rt IoThread
                                     => Proxy :rt
                                     -> (Generation -> Unit)
                                     -> (Unit -> Boolean)
                                     -> Unit))
  (define (park-current-thread-if!% rt-prx with-gen should-park?)
    (park-current-thread-if-with!% rt-prx with-gen should-park? NoTimeout))

  (declare unpark-thread!% (Generation -> IoThread -> Unit))
  (define (unpark-thread!% gen thread)
    ;; CONCURRENT:
    ;; - Masks around the critical region
    ;; - Notifying after release is valid because all waiter/notifiers evaluate guard
    ;;   condition and interpose lock acquisition before waiting/notifying.
    ;;   See https://stackoverflow.com/questions/21439359/signal-on-condition-variable-without-holding-lock

    ;; Only unpark if the targeted gen is more recent than the last fired gen
    (when (> gen (Generation (at:read (.fired-gen thread))))
      (mask-current-thread!%)
      (lk:acquire (.park-lock thread))
      (if (> gen (Generation (at:read (.fired-gen thread))))
          (progn
            (atomic-set-generation%! gen (.fired-gen thread))
            (lk:release (.park-lock thread))
            (cv:notify (.park-cv thread))
            (unmask-current-thread!%))
          (progn
            (lk:release (.park-lock thread))
            (unmask-current-thread!%)))))
  )

;;;
;;; IoThread Concurrent Instance
;;;

;; TODO: Try to move this to a later file, where it can re-open the package
;; and base the instance on the machinery already provided by MonadIoThread.
;; I tried this and got some type inference errors that I *think* were a
;; Coalton bug on the fundeps, but need more investigation.
(coalton-toplevel

  (define-instance (Concurrent IoThread Unit)
    (inline)
    (define (stop thread)
      (wrap-io (stop!% thread)))
    (inline)
    (define (await thread)
      (raise-result-dynamic (wrap-io (join!% thread))))
    (inline)
    (define (mask thread)
      (wrap-io (mask!% thread)))
    (inline)
    (define (unmask thread)
      (wrap-io (unmask!% thread)))
    (inline)
    (define (unmask-finally thread callback)
      (lift-to
       (with-run-in-io
         (fn (run)
           (wrap-io (unmask-finally!% thread (fn (mode)
                                               (run! (run (callback mode)))))))))))

  )
