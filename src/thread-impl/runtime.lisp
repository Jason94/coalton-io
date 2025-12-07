(cl:in-package :cl-user)
(defpackage :io/thread-impl/runtime
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/monad/classes
   #:io/utils
   #:io/monad-io
   )
  (:local-nicknames
   (:c #:coalton-library/cell)
   (:b #:coalton-library/bits)
   (:v #:coalton-library/vector)
   (:t #:coalton-threads/thread)
   (:at #:coalton-threads/atomic)
   (:bt #:bordeaux-threads-2)
   )
  (:export
   ;; Library Public
   #:IoThread
   #:ThreadingException
   #:InterruptCurrentThread

   ;; Library Private
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

  (declare atomic-fetch-or (at:AtomicInteger -> Word -> Word))
  (define (atomic-fetch-or at-int mask)
    (let old = (at:read at-int))
    (let new = (b:or old mask))
    (if (at:cas! at-int old new)
        new
        (atomic-fetch-or at-int mask)))

  (declare atomic-fetch-and (at:AtomicInteger -> Word -> Word))
  (define (atomic-fetch-and at-int mask)
    (let old = (at:read at-int))
    (let new = (b:and old mask))
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

  (define-exception ThreadingException
    (InterruptCurrentThread String))

  ;; (define-instance (Signalable ThreadingException)
  ;;   (define (error e)
  ;;     (match e
  ;;       ((InterruptCurrentThread)
  ;;        (error "The thread tried to interrupt itself.")))))

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
  (declare unmasked? (Word -> Boolean))
  (define (unmasked? word)
    (zero? (lisp Word (word)
             (cl:ash word -1))))
  )

(coalton-toplevel

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
       (if (unsafe-pointer-eq? native-thread (current-native-thread%))
           (interrupt-current-thread%)
           (lisp Unit (native-thread)
             (cl:when (bt:thread-alive-p native-thread)
               (bt:destroy-thread native-thread)))))))
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
  (declare current-thread% (MonadIo :m => :m IoThread))
  (define current-thread%
    (wrap-io (current-io-thread%)))

  (inline)
  (declare fork% ((MonadIo :m) (UnliftIo :r :i) (LiftTo :r :m) => :r :a -> :m IoThread))
  (define (fork% op)
    (lift-to
     (with-run-in-io
         (fn (run)
           ;; Both the returning thread handle and the one made available to the child
           ;; thread have to have the IoThread packaged together before they do anything
           ;; meaningful. As such, we'll construct it and then each thread will set
           ;; native thread reference separately before they do any work. This guarantees
           ;; it will be available in either thread before subsequent code could reference it,
           ;; regardless of race conditions.
           (wrap-io
             (let thread-container = (IoThread (c:new None) (at:new CLEAN)))
             (let native-thread = (t:spawn (fn ()
                                             (c:write! (.handle thread-container)
                                                       (Some (current-native-thread%)))
                                             (let f =
                                               (fn ()
                                                 (catch (run! (run op))
                                                   ((InterruptCurrentThread "")
                                                    (lisp :a ()
                                                      cl:nil)))))
                                             (lisp Void (f thread-container)
                                               (cl:let ((*current-thread* thread-container))
                                                 (call-coalton-function f)))
                                             Unit)))
             (c:write! (.handle thread-container) (Some native-thread))
             thread-container)))))

  (inline)
  (declare sleep% (MonadIo :m => UFix -> :m Unit))
  (define (sleep% msecs)
    (wrap-io
      (lisp :a (msecs)
        (cl:sleep (cl:/ msecs 1000)))
      Unit))

  ;;;
  ;;; Stopping & Masking Threads
  ;;;

  (declare mask-inner% (IoThread -> Unit))
  (define (mask-inner% thread)
    (let flags = (.flags thread))
    (let flag-state =
      (rec % ()
        (let old = (at:read flags))
        (let new = (mask-once% old))
        (if (at:cas! flags old new)
            new
            (%))))
    (when (matches-flag flag-state PENDING-KILL)
      (interrupt-iothread% thread)))

  (inline)
  (declare mask% (MonadIo :m => IoThread -> :m Unit))
  (define (mask% thread)
    (wrap-io (mask-inner% thread)))

  (inline)
  (declare mask-current-thread!% (Unit -> Unit))
  (define (mask-current-thread!%)
    (mask-inner% (current-io-thread%)))

  (inline)
  (declare mask-current-thread% (MonadIo :m => :m Unit))
  (define mask-current-thread%
    (wrap-io_ mask-current-thread!%))

  (declare unmask-finally!% (IoThread -> (Unit -> Unit) -> Unit))
  (define (unmask-finally!% thread thunk)
    (let flags = (.flags thread))
    (let flag-state =
      (rec % ()
        (let old = (at:read flags))
        (let new = (unmask-once% old))
        (if (at:cas! flags old new)
            new
            (%))))
    (thunk Unit)
    (when (matches-flag flag-state PENDING-KILL)
      (interrupt-iothread% thread)))

  (inline)
  (declare unmask-inner% (IoThread -> Unit))
  (define (unmask-inner% thread)
    (unmask-finally!% thread (const Unit)))

  (inline)
  (declare unmask% (MonadIo :m => IoThread -> :m Unit))
  (define (unmask% thread)
    (wrap-io (unmask-inner% thread)))

  (inline)
  (declare unmask-finally% ((UnliftIo :r :io) (LiftTo :r :m) => IoThread -> :r Unit -> :m Unit))
  (define (unmask-finally% thread thunk)
    (lift-to
     (with-run-in-io
         (fn (run)
           (wrap-io (unmask-finally!% thread (fn (_) (run! (run thunk)))))))))

  (inline)
  (declare unmask-current-thread-finally!% ((Unit -> Unit) -> Unit))
  (define (unmask-current-thread-finally!% thunk)
    (unmask-finally!% (current-io-thread%) thunk))

  (inline)
  (declare unmask-current-thread-finally% ((UnliftIo :r :io) (LiftTo :r :m) => :r Unit -> :m Unit))
  (define (unmask-current-thread-finally% thunk)
    (lift-to
     (with-run-in-io
         (fn (run)
           (wrap-io (unmask-current-thread-finally!% (fn (_) (run! (run thunk)))))))))

  (inline)
  (declare unmask-current-thread!% (Unit -> Unit))
  (define (unmask-current-thread!%)
    (unmask-inner% (current-io-thread%)))

  (inline)
  (declare unmask-current-thread% (MonadIo :m => :m Unit))
  (define unmask-current-thread%
    (wrap-io_ unmask-current-thread!%))

  (inline)
  (declare stop% (MonadIo :m => IoThread -> :m Unit))
  (define (stop% thread)
    (wrap-io
      (let flag-state = (atomic-fetch-or (.flags thread) PENDING-KILL))
      (when (unmasked? flag-state)
        (interrupt-iothread% thread))))

  )
