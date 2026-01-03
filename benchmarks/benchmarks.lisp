;;;
;;; Running benchmarks
;;;
;;; Code taken from Coalton:
;;; https://github.com/coalton-lang/coalton/blob/main/benchmarks/package.lisp

(cl:defpackage #:io/benchmarks
  (:use #:cl
        #:trivial-benchmark)
  (:export #:run-benchmark
           #:run-benchmark-ci
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
;;; CSV helpers
;;;

(defun %package-designator->string (package)
  (etypecase package
    (string package)
    (symbol (symbol-name package))
    (package (package-name package))))

(defun %filename-safe (s)
  "Make S safe for use as a filename component (replace / and -)."
  (let ((s (string-downcase s)))
    (with-output-to-string (out)
      (loop :for ch :across s
            :do (write-char (case ch
                              (#\/ #\_)
                              (#\- #\_)
                              (t ch))
                            out)))))

(defun %benchmark-csv-path (package)
  (format nil "~A.csv" (%filename-safe (%package-designator->string package))))

(defun %write-benchmark-csv (result pathname)
  (with-open-file (out pathname
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
     result)))

;;;
;;; Run Benchmarks
;;;

(defun run-benchmark (package)
  "Run a single benchmark package. Returns a hashtable of benchmark stats."
  (run-package-benchmarks :package package :verbose t))

(defun run-benchmark-ci (package)
  "Run a single benchmark package and dump the result to <package>.csv,
where <package> is filename-safe (slashes and dashes replaced)."
  (let* ((result (run-benchmark package))
         (path (%benchmark-csv-path package)))
    (%write-benchmark-csv result path)
    (values)))

(defun run-benchmarks ()
  "Run all benchmark packages. Returns a hashtable of benchmark stats."
  (labels ((merge-hash-tables (a b)
             (loop :for b-key :being :the :hash-keys :of b
                   :do (setf (gethash b-key a) (gethash b-key b)))
             a))
    (reduce #'merge-hash-tables
            (mapcar (lambda (package)
                      (run-package-benchmarks :package package :verbose t))
                    (reverse *all-benchmarks*)))))

(defun run-benchmarks-ci ()
  "Run all benchmark packages, writing one CSV per package (no combined bench.csv)."
  (dolist (package (reverse *all-benchmarks*))
    (run-benchmark-ci package))
  (values))

;; Examples:
;; (run-benchmark-ci 'benchmark-simple-io)
;; (run-benchmarks-ci)
;; (run-benchmark 'benchmark-simple-io)
