(cl:in-package :cl-user)
(defpackage :io/examples/fork-laws
  (:use
   #:coalton
   #:coalton-prelude
   #:io/simple-io
   #:io/threads
   #:io/terminal
   #:io/conc/future
   #:io/conc/mvar)
  (:local-nicknames
   (:at #:io/conc/atomic)
   ))
(in-package :io/examples/fork-laws)

(named-readtables:in-readtable coalton:coalton)

;;;
;;; Demonstrates that IO threads and futures are referentially transparent.
;;;
;;; See:
;;; https://www.reddit.com/r/scala/comments/3zofjl/why_is_future_totally_unusable/
;;;

(coalton-toplevel

 (declare test-a (IO Unit))
 (define test-a
   (do
    (var <- (at:new-at-var 0))
    (chan <- new-empty-chan)
     (let fork-op =
       (do-fork-thread_
         (x <- (at:modify-swap var (+ 1)))
         (push-chan chan x)))
     fork-op
     fork-op
     (a <- (pop-chan chan))
     (b <- (pop-chan chan))
     (write-line a)
     (write-line b)))

 (declare test-b (IO Unit))
 (define test-b
   (do
    (var <- (at:new-at-var 0))
    (chan <- new-empty-chan)
     (do-fork-thread_
       (x <- (at:modify-swap var (+ 1)))
       (push-chan chan x))
     (do-fork-thread_
       (x <- (at:modify-swap var (+ 1)))
       (push-chan chan x))
     (a <- (pop-chan chan))
     (b <- (pop-chan chan))
     (write-line a)
     (write-line b)))

  (declare fut-a (IO Unit))
  (define fut-a
    (do
     (var <- (at:new-at-var 0))
     (let fork-op =
       (do-fork-future_
         (at:modify-swap var (+ 1))))
     (fut-a <- fork-op)
     (fut-b <- fork-op)
     (a <- (await fut-a))
     (b <- (await fut-b))
     (write-line a)
     (write-line b)))

  (declare fut-b (IO Unit))
  (define fut-b
    (do
     (var <- (at:new-at-var 0))
     (fut-a <-
       (do-fork-future_
         (at:modify-swap var (+ 1))))
     (fut-b <-
       (do-fork-future_
         (at:modify-swap var (+ 1))))
     (a <- (await fut-a))
     (b <- (await fut-b))
     (write-line a)
     (write-line b)))
  )
