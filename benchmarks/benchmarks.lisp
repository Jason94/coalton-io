;;;
;;; Running benchmarks
;;;
;;; Code taken from Coalton:
;;; https://github.com/coalton-lang/coalton/blob/main/benchmarks/package.lisp
(cl:defpackage #:io/benchmarks
  (:use #:cl
        #:trivial-benchmark)
  (:export #:run-benchmark
           #:run-benchmarks
           #:run-benchmarks-ci))

(cl:in-package #:io/benchmarks)

;;;
;;; Define Benchmarks
;;; 

(defparameter *all-benchmarks* '())

(defmacro define-io-benchmark (name
                                    benchmark-clauses
                                    &rest native-clauses)
  (let ((benchmark-package (intern (format nil "BENCHMARK-~S" name) 'keyword))
        (native-package (intern (format nil "BENCHMARK-~S/NATIVE" name) 'keyword)))
    `(progn
       (pushnew ',benchmark-package *all-benchmarks*)
       (define-benchmark-package ,benchmark-package ,@benchmark-clauses)
       (defpackage ,native-package ,@native-clauses))))

;;;
;;; Run Benchmarks
;;; 

(defun run-benchmark (package)
  "Run a single benchmark package.  Returns a hashtable of benchmark stats."
  (run-package-benchmarks :package package :verbose t))

(defun run-benchmarks ()
  "Run all benchmark packages.  Returns a hashtable of benchmark stats."
  (labels ((merge-hash-tables (a b)
             (loop :for b-key :being :the :hash-keys :of b
                   :do (setf (gethash b-key a) (gethash b-key b)))
             a))
    (reduce #'merge-hash-tables
            (mapcar (lambda (package)
                      (run-package-benchmarks :package package :verbose t))
                    (reverse *all-benchmarks*)))))

(defun run-benchmarks-ci ()
  "Run all benchmark packages, and dump the result in bench.csv"
  (let ((result (run-benchmarks)))
    (with-open-file (out "bench.csv"
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
      (format out "benchmark,metric,samples,total,minimum,maximum,median,average,deviation~%")
      (maphash
       (lambda (bench metrics)
         ;; METRICS is a list like: ((:average ... :median ... ...) (:gc ...)) etc.
         (dolist (m metrics)
           (destructuring-bind (metric &rest plist) m
             (format out "~A,~A,~A,~A,~A,~A,~A,~A,~A~%"
                     bench metric
                     (getf plist :samples)
                     (float (getf plist :total))
                     (float (getf plist :minimum))
                     (float (getf plist :maximum))
                     (float (getf plist :median))
                     (float (getf plist :average))
                     (float (getf plist :deviation))))))
       result))
    (values)))

;; (run-benchmarks-ci)
;; (run-benchmark 'benchmark-simple-io)
