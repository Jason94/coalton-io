(cl:in-package :cl-user)

(defpackage :io/examples/redis/rw-lock
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/experimental/do-control-core
   #:io/exceptions
   #:io/monad-io
   #:io/simple-io
   #:io/resource
   #:io/conc/stm)
  (:export
   #:TRWLock
   #:new-trwlock
   #:with-reader-lock
   #:with-writer-lock
   #:do-with-writer-lock
   ))

(in-package :io/examples/redis/rw-lock)

(named-readtables:in-readtable coalton:coalton)

;;;
;;; Implement an STM readers/writer lock.
;;;
;;; One of the Redis commands (SAVE) specifically instructs that other commands should block
;;; while it's writing to the disk, which needs to be done outside of the transaction.
;;; To solve this, we implement a readers/writer lock *in the STM* using a (TVar Boolean). Any
;;; commands that don't need to synchronize database access check the lock out as a "reader".
;;; Any commands that need to guarantee exclusive access to the database (just SAVE in this
;;; example) check the lock out as a "writer." The RW lock guarantees that any number of
;;; "reader" commands can run simultaneously, but if any single "writer" command is running,
;;; it guarantees it has exclusive access.
;;;
;;; Note: A reader/writer lock is the standard name for this kind of lock. It's a bit of a
;;; misnomer in our case, because several of the commands that are "reader" commands actually
;;; *do* write to the database. That's because the STM handles actually synchronizing the
;;; data automatically, so the purpose of this lock is to handle a different level of access
;;; control. But we're following the standard naming conventions for the kind of data structure
;;; implemented here.
;;;
;;; This demonstrates one of the benefits of an STM: it's surprisingly good at expressing
;;; concurrency control constructs like locks, queues, etc., not just sychronizing shared memory.
;;; Although the Redis program doesn't make full use of this, the retry and or-else mechanisms
;;; in the STM make these kind of tools surprisingly powerful. For example, a transaction could
;;; try to acquire a lock before entering sub-transaction A, but if acquiring the lock fails,
;;; it could enter sub-transaction B instead.
;;;

(coalton-toplevel

  (define-struct TRWLock
    ""
    (n-readers (TVar UFix))
    (n-writers-waiting (TVar UFix))
    (writer-active? (TVar Boolean)))

  (declare new-trwlock (IO TRWLock))
  (define new-trwlock
    "Create a new TRWLock."
    (do
      (n-readers-tvar <- (new-tvar 0))
      (n-writers-tvar <- (new-tvar 0))
      (active-tvar <- (new-tvar False))
      (pure (TRWLock n-readers-tvar n-writers-tvar active-tvar))))

  (declare reader-acquire-tx (TRWLock -> STM IO Unit))
  (define (reader-acquire-tx lock)
    "Acquire a tlock. Blocks until the lock becomes available to readers. Returns the
token used to release the lock."
    (do
     (n-writers-waiting <- (read-tvar (.n-writers-waiting lock)))
     (writer-active? <- (read-tvar (.writer-active? lock)))
     (if (or writer-active? (> n-writers-waiting 0))
         retry
         (modify-tvar (.n-readers lock) 1+))
     (pure Unit)))

  (declare reader-release-tx (TRWLock -> STM IO Unit))
  (define (reader-release-tx lock)
    "Attempts to release a reader on LOCK. If there were no active readers, errors."
    (do
     (new-n-readers <- (modify-tvar (.n-readers lock) 1-))
     (do-when (< new-n-readers 0)
       (raise "Attempted to release a reader on a TRWLock with no active readers."))))

  ;; NOTE: This could return a transaction (STM IO Unit) instead of an IO operation. But
  ;; doing it this way makes it impossible to accidentally combine pend-writer-acquire
  ;; and writer-acquire in the same transaction, which would break the algorithm.
  (declare pend-writer-acquire (TRWLock -> IO Unit))
  (define (pend-writer-acquire lock)
    "Set LOCK to block future readers and wait for a writer to acquire the lock. Must be
run before attempting to acquire the lock as a writer. The purpose of this is to prevent
a steady stream of readers from acquiring the lock and blocking a writer from being able
to ever acquire it."
    (do-run-tx
      (modify-tvar (.n-writers-waiting lock) 1+)
      (pure Unit)))

  (declare writer-acquire (TRWLock -> IO Unit))
  (define (writer-acquire lock)
    "Acquire the writer lock on LOCK. Blocks until no readers and no writer is active."
    (do-run-tx
      (n-readers <- (read-tvar (.n-readers lock)))
      (writer-active? <- (read-tvar (.writer-active? lock)))
      (do-when (or writer-active? (> n-readers 0))
        retry)
      (write-tvar (.writer-active? lock) True)))

  (declare writer-release (TRWLock -> IO Unit))
  (define (writer-release lock)
    "Attempts to release the writer lock on LOCK. Errors if the writer lock was not acquired."
    (do-run-tx
      (writer-active? <- (read-tvar (.writer-active? lock)))
      (n-writers-waiting <- (read-tvar (.n-writers-waiting lock)))
      (do-when (or (not writer-active?) (zero? n-writers-waiting))
        (raise "Attempted to release the writer lock on a TRWLock that was not busy."))
      (write-tvar (.writer-active? lock) False)
      (write-tvar (.n-writers-waiting lock) (1- n-writers-waiting))))

  (declare with-reader-lock (TRWLock -> IO :a -> IO :a))
  (define (with-reader-lock lock op)
    "Run IO operation OP with a reader lock on LOCK held."
    (bracket-io_
     (run-tx (reader-acquire-tx lock))
     (fn (_)
       (run-tx (reader-release-tx lock)))
     (fn (_)
       op)))

  (declare with-writer-lock (TRWLock -> IO :a -> IO :a))
  (define (with-writer-lock lock op)
    "Run IO operation OP with the writer lock on LOCK held."
    ;; TODO: Convert this to use as bracket operation that doesn't mask. Then rewrite this
    ;; so it's valid in the presence of async stops. Currently, it blocks to acquire the
    ;; lock while masked.
    (bracket-io_
     (do
      (pend-writer-acquire lock)
      (writer-acquire lock))
     (fn (_)
       (writer-release lock))
     (fn (_)
       op)))
  )
