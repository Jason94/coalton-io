;; Minimal Common Lisp benchmark support adapted from coalton-io's
;; benchmarks/benchmarks.lisp.
;; # MIT License
;;
;; Copyright (c) 2025, Jason Walker
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(cl:defpackage #:coalton-comparison/benchmarks
  (:use #:cl #:org.shirakumo.trivial-benchmark)
  (:export #:run-benchmark #:run-benchmark-ci))

(cl:in-package #:coalton-comparison/benchmarks)

;; Preserve the qualified symbols returned by SELECTED-COALTON-BENCHMARKS.
;; These packages hold names only; no Coalton benchmarks are loaded or registered.
;; Reuse existing packages when coalton-io is already loaded.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (dolist (name '("BENCHMARK-QUEUES" "BENCHMARK-SCHEDULERS" "BENCHMARK-STM"))
    (unless (find-package name)
      (make-package name :use nil))))

(defmacro define-io-benchmark-package
    (name benchmark-clauses &rest native-clauses)
  (let ((benchmark-package (intern (format nil "BENCHMARK-~S" name) 'keyword))
        (native-package (intern (format nil "BENCHMARK-~S/NATIVE" name) 'keyword)))
    `(progn
       (define-benchmark-package ,benchmark-package ,@benchmark-clauses)
       (defpackage ,native-package ,@native-clauses))))

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
  (format t "Writing results to ~a~%" pathname)
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

(defun run-benchmark (package)
  "Run a single benchmark package. Returns a hashtable of benchmark stats."
  (run-package-benchmarks :package package :verbose t))

(defun run-benchmark-ci (package &optional (directory *default-pathname-defaults*))
  "Run a single benchmark package and write its CSV in DIRECTORY."
  (let* ((result (run-benchmark package))
         (path (merge-pathnames (%benchmark-csv-path package) directory)))
    (%write-benchmark-csv result path)
    (values)))
