(defpackage :coalton-io/tests/conc/group
  (:use #:coalton #:coalton-prelude #:coalton-testing
   #:io/utils
   #:io/tests/utils
   #:io/monad-io
   #:io/simple-io
   #:io/exceptions
   #:io/threads
   #:io/conc/future
   #:io/conc/group
   #:io/mutable-var)
  (:local-nicknames
   (:tm #:io/terminal)
   (:at #:io/conc/atomic)))
(in-package :coalton-io/tests/conc/group)

(named-readtables:in-readtable coalton:coalton)

(fiasco:define-test-package #:coalton-io/tests/conc/group-fiasco)
(coalton-fiasco-init #:coalton-io/tests/conc/group-fiasco)

(define-test test-await-existing-threads ()
  (let result =
    (run-io!
     (do
      (f1 <- (fork-future_ (pure 1)))
      (f2 <- (fork-future_ (pure 2)))
      (group <- (enclose-group (make-list f1 f2)))
      (await group))))
  (is (== (make-list 1 2) result)))

(define-test test-await-fork-threads ()
  (let result =
    (run-io!
     (do
      (group <- (fork-group (make-list (fork-future_ (pure 1))
                                       (fork-future_ (pure 2)))))
      (await group))))
  (is (== (make-list 1 2) result)))

(define-test test-stop ()
  (let result =
    (run-io!
     (do
      (gate <- s-new)
      (store <- (new-var None))
      (group <- (fork-group (make-list
                             (do-fork-future_
                               (s-await gate)
                               (write store (Some 10))))))
      (stop group)
      (s-signal gate)
      (sleep 2)
      (read store))))
  (is (== None result)))

(define-test test-mask ()
  (let result =
    (run-io!
     (do
      (masked-gate <- s-new)
      (stored-gate <- s-new)
      (store <- (new-var None))
      (group <- (fork-group (make-list
                             (do-fork-thread_
                               (s-await masked-gate)
                               (write store (Some 10))
                               (s-signal stored-gate)))))
      (mask group)
      (stop group)
      (s-signal masked-gate)
      (s-await stored-gate)
      (read store))))
  (is (== (Some 10) result)))

(define-test test-unmask ()
  (let result =
    (run-io!
     (do
      (masked-gate <- s-new)
      (store <- (new-var None))
      (group <- (fork-group (make-list
                             (do-fork-thread_
                               (s-await masked-gate)
                               (write store (Some 10))))))
      (mask group)
      (unmask group)
      (stop group)
      (s-signal masked-gate)
      (sleep 2)
      (read store))))
  (is (== None result)))

(define-test test-unmask-finally ()
  (let result =
    (run-io!
     (do
      (masked-gate <- s-new)
      (store-1 <- (new-var 0))
      (store-2 <- (new-var 0))
      (finally-counts <- (the (:c (at:AtVar Integer)) (at:new-at-var 0)))
      (group <- (fork-group (make-list
                             (do-fork-thread_
                               (s-await masked-gate)
                               (write store-1 1))
                             (do-fork-thread_
                               (s-await masked-gate)
                               (write store-2 2)))))
      (mask group)
      (stop group)
      (unmask-finally_ group (fn (_)
                               (at:modify finally-counts 1+)))
      (sleep 2)
      (res-1 <- (read store-1))
      (res-2 <- (read store-2))
      (res-counts <- (at:read finally-counts))
      (pure (make-list res-1 res-2 res-counts)))))
  (is (== (make-list 0 0 2) result)))
