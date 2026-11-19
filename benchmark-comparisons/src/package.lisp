(in-package #:coalton-comparison/benchmarks)

(define-io-benchmark-package queue-comparison
  ((:local-nicknames
    (:io/benchmarks #:coalton-comparison/benchmarks)
    (:b #:org.shirakumo.trivial-benchmark)
    (:bt #:bordeaux-threads)
    (:ch #:coalton/threads/channel)
    (:fl #:cl-freelock)
    (:lp #:lparallel.queue)
    (:opt #:coalton/optional)))
  (:use #:cl))

(in-package #:benchmark-queue-comparison)

(export '(run run-ci selected-coalton-benchmarks))

(in-package #:coalton-comparison/benchmarks)

(define-io-benchmark-package scheduler-comparison
  ((:local-nicknames
    (:io/benchmarks #:coalton-comparison/benchmarks)
    (:at #:atomics)
    (:b #:org.shirakumo.trivial-benchmark)
    (:bt #:bordeaux-threads)
    (:lp #:lparallel)
    (:lpk #:lparallel.kernel)
    (:rt #:coalton/threads/runtime)))
  (:use #:coalton #:coalton-prelude)
  (:local-nicknames
   (:arr #:coalton/lisparray)
   (:atomic #:coalton/threads/atomic)
   (:b #:org.shirakumo.trivial-benchmark)
   (:comp #:benchmark-scheduler-comparison)
   (:iter #:coalton/iterator)
   (:math #:coalton/math)
   (:par #:coalton/threads/parallel)
   (:thread #:coalton/threads/thread)))

(in-package #:benchmark-scheduler-comparison)

(export '(run run-ci selected-coalton-benchmarks))

(in-package #:coalton-comparison/benchmarks)

(define-io-benchmark-package stm-comparison
  ((:local-nicknames
    (:io/benchmarks #:coalton-comparison/benchmarks)
    (:b #:org.shirakumo.trivial-benchmark)
    (:bt #:bordeaux-threads)
    (:stm #:stmx)))
  (:use #:cl))

(in-package #:benchmark-stm-comparison)

(export '(run run-ci selected-coalton-benchmarks))
