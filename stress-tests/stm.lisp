(defpackage :io/stress/stm
  (:use #:coalton #:coalton-prelude #:coalton-testing
   #:coalton/experimental/do-control-core
   #:io/simple-io
   #:io/stress/stress-tests
   #:io/conc/stm
   #:io/conc/stm/tarray
   )
  (:local-nicknames
   (:tm #:io/term)))
(in-package :io/stress/stm)

(named-readtables:in-readtable coalton:coalton)

(fiasco:define-test-package #:io/stress/stm-fiasco)
(coalton-fiasco-init #:io/stress/stm-fiasco)

(defmacro stm-linearized-producer-consumers-stress-test (n-prod n-cons n-obs count)
  `(progn
     (let result =
       (run-io!
        (do
         (box <- (new-tvar None))
         (linearized-producer-consumers-stress-test
          ,count
          ,n-prod
          ,n-cons
          (fn (x)
            (do-run-tx
              (current-val <- (read-tvar box))
              (do-match current-val
                ((Some _)
                 (pure False))
                ((None)
                 (write-tvar box (Some x))
                 (pure True)))))
          (do-run-tx
            (current-val <- (read-tvar box))
            (do-match current-val
              ((Some _)
               (write-tvar box None)
               (pure current-val))
              ((None)
               (pure None))))
          (do-run-tx
              (map none? (read-tvar box)))
          :observation
          (Some
           (Tuple
            ,n-obs
            (do-run-tx
              (read-tvar box)
              (pure Unit))))))))
     (is (== (Ok Unit)
             result))))

(coalton-toplevel
  (define +test-stm-count+     (the UFix 2000000))
  (define +test-stm-obs-count+ (the UFix 2000000)))

(define-test stm-linearized-producer-consumers-stress-test-1-prod-1-cons ()
  (stm-linearized-producer-consumers-stress-test 1 1 0 +test-stm-count+))

(define-test stm-linearized-producer-consumers-stress-test-6-prod-1-cons ()
  (stm-linearized-producer-consumers-stress-test 6 1 0 +test-stm-count+))

(define-test stm-linearized-producer-consumers-stress-test-1-prod-6-cons ()
  (stm-linearized-producer-consumers-stress-test 1 6 0 +test-stm-count+))

(define-test stm-linearized-producer-consumers-stress-test-6-prod-6-cons ()
  (stm-linearized-producer-consumers-stress-test 6 6 0 +test-stm-count+))

(define-test stm-linearized-producer-consumers-stress-test-4-prod-4-cons-4-obs ()
  (stm-linearized-producer-consumers-stress-test 4 4 4 +test-stm-obs-count+))
