(defpackage :io/stress/mvar
  (:use #:coalton #:coalton-prelude #:coalton-testing
   #:coalton/experimental/do-control-core
   #:io/simple-io
   #:io/conc/mvar
   #:io/stress/stress-tests
   )
  (:local-nicknames
   (:tm #:io/term)))
(in-package :io/stress/mvar)

(named-readtables:in-readtable coalton:coalton)

(fiasco:define-test-package #:io/stress/mvar-fiasco)
(coalton-fiasco-init #:io/stress/mvar-fiasco)

(defmacro mvar-linearized-producer-consumers-stress-test (n-prod n-cons n-obs count)
  `(progn
     (let result =
       (run-io!
        (do
         (box <- new-empty-mvar)
         (linearized-producer-consumers-stress-test
          ,count
          ,n-prod
          ,n-cons
          ƒx.(try-put-mvar box x)
          (try-take-mvar box)
          (map none? (try-read-mvar box))
          :observation
          (Some
           (Tuple
            ,n-obs
            (do
             (try-read-mvar box)
             (pure Unit))))))))
     (is (== (Ok Unit)
             result))))

(defmacro mvar-blocking-linearized-producer-consumers-stress-test (n-prod n-cons n-obs count)
  `(progn
     (let result =
       (run-io!
        (do
         (box <- new-empty-mvar)
         (linearized-producer-consumers-stress-test
          ,count
          ,n-prod
          ,n-cons
          (fn (x)
            (do
             (put-mvar box x)
             (pure True)))
          (map Some (take-mvar box))
          (map none? (try-read-mvar box))
          :observation
          (Some
           (Tuple
            ,n-obs
            (do
             (read-mvar box)
             (pure Unit))))))))
     (is (== (Ok Unit)
             result))))

(coalton-toplevel
  (define +test-mvar-count+ (the UFix 300000)))

;;
;; Test non-blocking
;; 

(define-test mvar-linearized-producer-consumers-stress-test-1-prod-1-cons ()
  (mvar-linearized-producer-consumers-stress-test 1 1 0 +test-mvar-count+))

(define-test mvar-linearized-producer-consumers-stress-test-6-prod-1-cons ()
  (mvar-linearized-producer-consumers-stress-test 6 1 0 +test-mvar-count+))

(define-test mvar-linearized-producer-consumers-stress-test-1-prod-6-cons ()
  (mvar-linearized-producer-consumers-stress-test 1 6 0 +test-mvar-count+))

(define-test mvar-linearized-producer-consumers-stress-test-6-prod-6-cons ()
  (mvar-linearized-producer-consumers-stress-test 6 6 0 +test-mvar-count+))

(define-test mvar-linearized-producer-consumers-stress-test-4-prod-4-cons-4-obs ()
  (mvar-linearized-producer-consumers-stress-test 4 4 4 +test-mvar-count+))

;;
;; Test blocking
;; 

(define-test mvar-blocking-linearized-producer-consumers-stress-test-1-prod-1-cons ()
  (mvar-blocking-linearized-producer-consumers-stress-test 1 1 0 +test-mvar-count+))

(define-test mvar-blocking-linearized-producer-consumers-stress-test-6-prod-1-cons ()
  (mvar-blocking-linearized-producer-consumers-stress-test 6 1 0 +test-mvar-count+))

(define-test mvar-blocking-linearized-producer-consumers-stress-test-1-prod-6-cons ()
  (mvar-blocking-linearized-producer-consumers-stress-test 1 6 0 +test-mvar-count+))

(define-test mvar-blocking-linearized-producer-consumers-stress-test-6-prod-6-cons ()
  (mvar-blocking-linearized-producer-consumers-stress-test 6 6 0 +test-mvar-count+))

(define-test mvar-blocking-linearized-producer-consumers-stress-test-4-prod-4-cons-4-obs ()
  (mvar-blocking-linearized-producer-consumers-stress-test 4 4 4 +test-mvar-count+))
