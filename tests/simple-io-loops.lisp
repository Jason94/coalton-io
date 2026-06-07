(defpackage coalton-io/tests/io/loops
  (:use #:coalton #:coalton-prelude #:coalton-testing
        #:coalton-library/types
        #:io/utils
        #:io/simple-io
        #:io/simple-io/loops
        #:io/monad-io)
  (:local-nicknames
   (:c #:coalton/cell)
   (:mt #:io/mut)
   (:l #:coalton-library/list)
   ))
(in-package :coalton-io/tests/io/loops)

(named-readtables:in-readtable coalton:coalton)

(fiasco:define-test-package #:coalton-io/tests/io-loops-fiasco)

(coalton-fiasco-init #:coalton-io/tests/io-loops-fiasco)

(define-test test-map-into-io-empty ()
  (let result =
    (run-io!
     (do-map-into-io (x (make-list))
       (pure (+ x 10)))))
  (is (== Nil result)))

(define-test test-map-into-io ()
  (let result =
    (run-io!
     (do-map-into-io (x (make-list 0 10 20 30))
       (pure (+ x 10)))))
  (is (== (make-list 10 20 30 40) result)))

(define-test test-foreach-io-empty ()
  (let run-ints = (c:new Nil))
  (run-io!
   (do-foreach-io (x (the (List Integer) (make-list)))
     (wrap-io
       (c:push! run-ints x))))
  (is (== Nil (c:read run-ints))))

(define-test test-foreach-io ()
  (let run-ints = (c:new Nil))
  (let last-var =
    (run-io!
     (do
      (last-var <- (mt:new-var -1))
      (do-foreach-io (x (make-list 0 10 20 30))
        (mt:write last-var x)
        (wrap-io
         (c:push! run-ints x)))
      (mt:read last-var))))
  (is (== (make-list 0 10 20 30)
          (l:reverse (c:read run-ints))))
  (is (== 30 last-var)))
