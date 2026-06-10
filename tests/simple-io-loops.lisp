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

(define-test test-dotimes-io-empty ()
  (let count = (c:new 0))
  (run-io!
   (do-times-io (i 0)
     (wrap-io
      (c:update! ƒx.(+ x i) count))))
  (is (== 0 (c:read count))))

(define-test test-dotimes-io ()
  (let count = (c:new 0))
  (run-io!
   (do-times-io (i 4)
     (wrap-io
      (c:update! ƒx.(+ x i) count))))
  (is (== 6 ;; 0 + 1 + 2 + 3
          (c:read count))))

(coalton-toplevel
  (declare add-to-cell (UFix * c:Cell UFix -> IO Unit))
  (define (add-to-cell n count)
    (wrap-io
     (c:update! ƒx.(+ x n) count)
     Unit)))

(define-test test-dotimes-io-inner-func ()
  (let count = (c:new 0))
  (run-io!
   (do-times-io (i 4)
     (add-to-cell i count)))
  (is (== 6 ;; 0 + 1 + 2 + 3
          (c:read count))))

(define-test test-repeat-io-empty ()
  (let count = (c:new 0))
  (run-io!
   (do-repeat-io 0
     (wrap-io
      (c:update! 1+ count))))
  (is (== 0 (c:read count))))

(define-test test-repeat-io ()
  (let count = (c:new 0))
  (run-io!
   (do-repeat-io 10
     (wrap-io
      (c:update! 1+ count))))
  (is (== 10
          (c:read count))))

(define-test test-while-io ()
  (let result =
    (run-io!
     (do
      (x-var <- (mt:new-var 1))
      (do-while-io
        (x <- (mt:modify x-var ƒx.(* x 2)))
        (pure (< x 10)))
      (mt:read x-var))))
  (is (== 16 result)))

(define-test test-while-val-io-empty ()
  (let result =
    (run-io!
     (do
      (count <- (mt:new-var 0))
      (queue <- (wrap-io (c:new Nil)))
      (do-while-val-io (x (wrap-io (c:pop! queue)))
        (mt:modify count ƒc.(+ x c) ))
      (mt:read count))))
  (is (== 0 result)))

(define-test test-while-val-io ()
  (let result =
    (run-io!
     (do
      (count <- (mt:new-var 0))
      (queue <- (wrap-io (c:new (make-list 1 2 3))))
      (do-while-val-io (x (wrap-io (c:pop! queue)))
        (mt:modify count ƒc.(+ x c) ))
      (mt:read count))))
  (is (== 6 result)))
