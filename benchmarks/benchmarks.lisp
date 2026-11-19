;;;
;;; Running benchmarks
;;;
;;; Code taken from Coalton:
;;; https://github.com/coalton-lang/coalton/blob/main/benchmarks/package.lisp

(cl:defpackage #:io/benchmarks
  (:use #:cl
        #:trivial-benchmark)
  (:export
   #:define-io-benchmark
   #:run-benchmark
   #:run-benchmark-ci
   #:run-benchmarks
   #:run-benchmarks-ci))

(cl:in-package #:io/benchmarks)

;;;
;;; Define Benchmarks
;;;

(defparameter *all-benchmarks* '())

(defmacro define-io-benchmark-package (name
                               benchmark-clauses
                               &rest native-clauses)
  (let ((benchmark-package (intern (format nil "BENCHMARK-~S" name) 'keyword))
        (native-package (intern (format nil "BENCHMARK-~S/NATIVE" name) 'keyword)))
    `(progn
       (pushnew ',benchmark-package *all-benchmarks*)
       (define-benchmark-package ,benchmark-package ,@benchmark-clauses)
       (defpackage ,native-package ,@native-clauses))))

(defmacro define-io-benchmark (name (&key sample-count (warmup-count 0)) &body body)
  `(define-benchmark ,name ()
     (declare (optimize speed))
     (loop :repeat ,warmup-count
           :do (progn
                 ,@body))
     (loop :repeat ,sample-count
           :do (with-benchmark-sampling
                 ,@body))
  (report trivial-benchmark::*current-timer*)))

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

;;;
;;; Run Benchmarks
;;;

(defun run-benchmark (package)
  "Run a single benchmark package. Returns a hashtable of benchmark stats."
  (run-package-benchmarks :package package :verbose t))

(defun run-benchmark-ci (package &optional (directory *default-pathname-defaults*))
  "Run a single benchmark package and write its CSV in DIRECTORY."
  (let* ((result (run-benchmark package))
         (path (merge-pathnames (%benchmark-csv-path package) directory)))
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

(defun %call-with-benchmark-results-lock (directory function)
  (let ((lock-path (merge-pathnames ".benchmark-results.lock" directory)))
    (loop :repeat 100
          :for lock = (open lock-path :direction :output :if-exists nil)
          :when lock
            :do (return-from %call-with-benchmark-results-lock
                  (unwind-protect (funcall function)
                    (close lock)
                    (delete-file lock-path)))
          :do (sleep 1/10))
    (error "Timed out waiting for benchmark results lock ~A" lock-path)))

(defun %benchmark-archive-directory (directory)
  (let ((commit (uiop:run-program '("git" "rev-parse" "--verify" "HEAD")
                                  :directory directory
                                  :output '(:string :stripped t)
                                  :error-output *error-output*)))
    (loop
      (multiple-value-bind (second minute hour day month year)
          (decode-universal-time (get-universal-time) 0)
        (let* ((name (format nil "~4,'0D~2,'0D~2,'0DT~2,'0D~2,'0D~2,'0DZ_~A/"
                             year month day hour minute second commit))
               (archive (merge-pathnames name directory)))
          (when (%call-with-benchmark-results-lock
                 directory
                 (lambda () (nth-value 1 (ensure-directories-exist archive))))
            (return archive))))
      (sleep 1))))

(defun run-benchmarks-ci ()
  "Archive four full benchmark runs and publish run D as the latest results."
  (let* ((directory (asdf:system-relative-pathname "coalton-io/benchmarks" "benchmarks/"))
         (archive (%benchmark-archive-directory directory))
         (packages (reverse *all-benchmarks*)))
    (dolist (run '("A/" "B/" "C/" "D/"))
      (let ((run-directory (merge-pathnames run archive)))
        (ensure-directories-exist run-directory)
        (dolist (package packages)
          (run-benchmark-ci package run-directory))))
    (%call-with-benchmark-results-lock
     directory
     (lambda ()
       (let ((last-run (merge-pathnames "D/" archive)))
         (dolist (package packages)
           (let ((filename (%benchmark-csv-path package)))
             (uiop:with-temporary-file (:directory directory :pathname temporary :stream stream)
               (close stream)
               (uiop:copy-file (merge-pathnames filename last-run) temporary)
               (uiop:rename-file-overwriting-target temporary
                                                    (merge-pathnames filename directory)))))))))
  (values))

;; Examples:
;; (run-benchmark-ci 'benchmark-simple-io)
;; (run-benchmarks-ci)
;; (run-benchmark 'benchmark-simple-io)
