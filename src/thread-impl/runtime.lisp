(cl:in-package :cl-user)
(defpackage :io/thread-impl/runtime
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/monad/classes
   #:io/utils
   #:io/classes/monad-io
   #:io/thread-exceptions
   )
  (:local-nicknames
   (:c #:coalton-library/cell)
   (:b #:coalton-library/bits)
   (:v #:coalton-library/vector)
   (:t #:coalton-threads/thread)
   (:t/l #:coalton-threads/lock)
   (:at #:coalton-threads/atomic)
   (:bt #:bordeaux-threads-2)
   )
  (:export
   ;; Library Public
   #:IoThread
   #:ThreadingException
   #:InterruptCurrentThread

   #:UnmaskFinallyMode
   #:Stopped
   #:Running

   ;; Library Private
   #:current-thread!%
   #:sleep!%
   #:fork!%
   #:stop!%
   #:mask!%
   #:unmask!%

   #:current-thread%
   #:fork%
   #:sleep%
   #:mask%
   #:mask-current-thread%
   #:unmask%
   #:unmask-current-thread%
   #:stop%

   #:mask-current-thread!%
   #:unmask-finally!%
   #:unmask-finally%
   #:unmask-current-thread-finally!%
   #:unmask-current-thread-finally%
   #:unmask-current-thread!%

   #:write-line-sync%
   ))
(in-package :io/thread-impl/runtime)

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
  (declare current-native-thread% (Unit -> t:Thread Unit))
  (define (current-native-thread%)
    (lisp (t:Thread Unit) ()
      (bt:current-thread)))

  ;;;
  ;;; Basic Thread Type
  ;;;

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
  (define-struct IoThread
    (handle (c:Cell (Optional (t:Thread Unit))))
    (flags  at:AtomicInteger))

  (derive Eq)
  (repr :enum)
  (define-type UnmaskFinallyMode
    Running
    Stopped)

  (define-instance (Into UnmaskFinallyMode String)
    (define (into a)
      (match a
        ((Running) "Running")
        ((Stopped) "Stopped"))))

  (define-instance (Eq IoThread)
    (define (== a b)
      (and (unsafe-pointer-eq? (.handle a) (.handle b))
           (== (at:read (.flags a)) (at:read (.flags b))))))

  (declare CLEAN Word)
  (define CLEAN 0)

  (declare PENDING-KILL Word)
  (define PENDING-KILL (b:shift 0 1))

  (inline)
  (declare mask-once% (Word -> Word))
  (define (mask-once% word)
    (+ word 2))

  (inline)
  (declare unmask-once% (Word -> Word))
  (define (unmask-once% word)
    (- word 2))

  (inline)
  (declare unmasked-once? (Word -> Boolean))
  (define (unmasked-once? word)
    "Check that WORD is masked only once."
    (== 1 (lisp Word (word)
            (cl:ash word -1))))

  (inline)
  (declare unmasked? (Word -> Boolean))
  (define (unmasked? word)
    (zero? (lisp Word (word)
             (cl:ash word -1))))

  )

(coalton-toplevel

  ;; TODO: Remove this and just use interrupt-iothread% everywhere
  (inline)
  (declare interrupt-current-thread% (Unit -> Unit))
  (define (interrupt-current-thread%)
    "BT can't interrupt the current thread, so raise an exception instead."
    (throw (InterruptCurrentThread "")))

  (inline)
  (declare interrupt-iothread% (IoThread -> Unit))
  (define (interrupt-iothread% thd)
    "Stop an IoThread. Does not check masked state, etc. Does check if the target
thread is alive before interrupting."
    (let native-thread? = (c:read (.handle thd)))
    (match native-thread?
      ((None)
       (error "Tried to kill misconstructed thread."))
      ((Some native-thread)
       (lisp Void (native-thread)
         (cl:when (bt:thread-alive-p native-thread)
           (bt:error-in-thread native-thread (InterruptCurrentThread ""))))
       Unit)))
  )

;; To make sure that child threads have access to their current thread, store it
;; in a dynamic variable. Here be dragons. Tread carefully, you have been warned.
;; (The alternative is doing something even worse, like passing it around in the
;; IO monad.)
(cl:defvar *current-thread*
  (coalton (IoThread
            (c:new (Some (current-native-thread%)))
            (at:new CLEAN))))

(coalton-toplevel

  ;;;
  ;;; Basic Thread Operations
  ;;;

  (inline)
  (declare current-io-thread% (Unit -> IoThread))
  (define (current-io-thread%)
    (lisp IoThread ()
      *current-thread*))

  (inline)
  (declare current-thread!% (Unit -> IoThread))
  (define (current-thread!%)
    (lisp IoThread ()
      *current-thread*))

  (inline)
  (declare current-thread% (MonadIo :m => :m IoThread))
  (define current-thread%
    (wrap-io_ current-thread!%))

  (inline)
  (declare fork!% ((Unit -> :a) -> IoThread))
  (define (fork!% thunk)
    ;; Both the returning thread handle and the one made available to the child
    ;; thread have to have the IoThread packaged together before they do anything
    ;; meaningful. As such, we'll construct it and then each thread will set
    ;; native thread reference separately before they do any work. This guarantees
    ;; it will be available in either thread before subsequent code could reference it,
    ;; regardless of race conditions.
    (let thread-container = (IoThread (c:new None) (at:new CLEAN)))
    (let native-thread =
      (t:spawn (fn ()
                 (c:write! (.handle thread-container)
                           (Some (current-native-thread%)))
                 (let f =
                   (fn ()
                     (catch (thunk)
                       ;; If we hit this point, then we've ended the thread's meaningful work.
                       ;; It will stop after this, so we don't need to raise again.
                       ((InterruptCurrentThread msg)
                        ;; Satisfy type inference; don't force (:r :a) => (:r Unit)
                        (lisp :a () cl:nil)))))
                 ;; Set the thread-specific dynamic variables the runtime depends on. *current-thread*
                 ;; is defined here, the others are defined in the core simple-io implementation.
                 (lisp Void (f thread-container)
                   (cl:let ((*current-thread* thread-container))
                     (call-coalton-function f)))
                 Unit)))
    (c:write! (.handle thread-container) (Some native-thread))
    thread-container)

  (inline)
  (declare fork% ((MonadIo :m) (UnliftIo :r :i) (LiftTo :r :m) => :r :a -> :m IoThread))
  (define (fork% op)
    "Fork a new thread that runs OP. This function constructs the top-level thread runner,
which sets up the dynamic context and top-level error handling for asynchronous exceptions
for the new thread. In some ways, this function is the most important point in the thread
runtime."
    (lift-to
     (with-run-in-io
         (fn (run)
            (wrap-io
              (fork!% (fn (_)
                        (run! (run op)))))))))

  (inline)
  (declare sleep!% (UFix -> Unit))
  (define (sleep!% msecs)
    (lisp :a (msecs)
      (cl:sleep (cl:/ msecs 1000)))
    Unit)

  (inline)
  (declare sleep% (MonadIo :m => UFix -> :m Unit))
  (define (sleep% msecs)
    (wrap-io (sleep!% msecs)))

  ;;;
  ;;; Stopping & Masking Threads
  ;;;

  (inline)
  (declare mask!% (IoThread -> Unit))
  (define (mask!% thread)
    (let flags = (.flags thread))
    (rec % ()
      (let old = (at:read flags))
      (let new = (mask-once% old))
      (if (at:cas! flags old new)
          (Tuple old new)
          (%)))
    Unit)

  (inline)
  (declare mask% (MonadIo :m => IoThread -> :m Unit))
  (define (mask% thread)
    (wrap-io (mask!% thread)))

  (inline)
  (declare mask-current-thread!% (Unit -> Unit))
  (define (mask-current-thread!%)
    (mask!%
     (lisp IoThread ()
       *current-thread*)))

  (inline)
  (declare mask-current-thread% (MonadIo :m => :m Unit))
  (define mask-current-thread%
    (wrap-io_ mask-current-thread!%))

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
             (unmasked-once? flag-state))
        (thunk Stopped)
        (thunk Running))
    (let new-flag-state =
      (rec % ()
        (let old = (at:read flags))
        (let new = (unmask-once% old))
        (if (at:cas! flags old new)
            new
            (%))))
    (when (and (unmasked-once? flag-state)
               (matches-flag new-flag-state PENDING-KILL))
      (interrupt-iothread% thread))
    Unit)

  ;; TODO: Merge this into and replace with unmask-current-thread-finally!%
  ;; At this point, they're the same, and that function is the only call-site
  ;; of this one...
  (inline)
  (declare unmask-finally-current!% ((UnmaskFinallyMode -> Unit) -> Unit))
  (define (unmask-finally-current!% thunk)
    (unmask-finally!% (current-io-thread%) thunk))

  (inline)
  (declare unmask!% (IoThread -> Unit))
  (define (unmask!% thread)
    (unmask-finally!% thread (const Unit)))

  (inline)
  (declare unmask% (MonadIo :m => IoThread -> :m Unit))
  (define (unmask% thread)
    (wrap-io (unmask-finally!% thread (const Unit))))

  (inline)
  (declare unmask-current-thread-finally!% ((UnmaskFinallyMode -> Unit) -> Unit))
  (define (unmask-current-thread-finally!% thunk)
    (unmask-finally-current!% thunk))

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
  (declare unmask-current-thread% (MonadIo :m => :m Unit))
  (define unmask-current-thread%
    (wrap-io_ unmask-current-thread!%))

  (inline)
  (declare stop!% (IoThread -> Unit))
  (define (stop!% thread)
    (let flag-state = (atomic-fetch-or (.flags thread) PENDING-KILL))
    (when (unmasked? flag-state)
      (interrupt-iothread% thread)))

  (inline)
  (declare stop% (MonadIo :m => IoThread -> :m Unit))
  (define (stop% thread)
    (wrap-io (stop!% thread)))

  )
