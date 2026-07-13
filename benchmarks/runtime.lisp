(in-package #:io/benchmarks)

(define-io-benchmark-package runtime
  ((:local-nicknames
    (:c #:coalton)))
  (:use
   #:coalton
   #:coalton-prelude
   #:io/monad-io
   #:io/simple-io
   #:io/simple-io/loops
   #:io/thread
   #:io/conc/group
   #:io/conc/mvar)
  (:import-from #:io/classes/thread
   #:wrap-io-with-runtime)
  (:import-from #:coalton-library/experimental/loops
   #:dotimes)
  (:local-nicknames
   (:b #:benchmark-utils)
   (:c #:coalton/cell)
   (:v #:coalton/vector)))

(in-package #:benchmark-runtime/native)

(cl:declaim (cl:optimize (cl:speed 3) (cl:safety 0)))

(coalton-toplevel

  (declare benchmark-mask-unmask-direct-single-thread (UFix -> Void))
  (define (benchmark-mask-unmask-direct-single-thread n-tasks)
    "This benchmark writes to a pre-allocated vector `n-tasks` times, directly masking and unmasking each iteration."
    (run-io!
     (do
      ;; Setup benchmark
      (buffer <-
        (wrap-io
          (v:with-capacity n-tasks)))
      ;; Run the benchmark
      (wrap-io (b:start (b:current-timer)))
      (do-repeat-io n-tasks
        (wrap-io-with-runtime (rt-prx)
          (mask-current! rt-prx)
          (v:push! True buffer)
          (unmask-current! rt-prx)
          Unit))
      ;; Cleanup
      (wrap-io
       (b:stop (b:current-timer))
       (b:commit (b:current-timer))
       ;; Write out the buffer to prevent SBCL from optimizing it away
       (c:write! benchmark-vector-cache (Some buffer))
       Unit)))
    (values))

  (declare benchmark-mask-unmask-direct-six-threads (UFix -> Void))
  (define (benchmark-mask-unmask-direct-six-threads n-tasks)
    "This benchmark splits `n-tasks` vector writes across six threads, directly masking and unmasking each iteration."
    (run-io!
     (do
      ;; Setup benchmark
      (let n-threads = 6)
      (let tasks-per-thread = (coalton/math:div n-tasks n-threads))
      (start-gate <- new-empty-mvar)
      (buffers <-
        (wrap-io
          (let buffers = (v:with-capacity n-threads))
          (dotimes (_ n-threads)
            (v:push! (v:with-capacity tasks-per-thread) buffers))
          buffers))
      (threads <-
        (do-fork-n-threads (i n-threads)
          (let buffer = (v:index-unsafe i buffers))
          (read-mvar start-gate)
          (do-repeat-io tasks-per-thread
            (wrap-io-with-runtime (rt-prx)
              (mask-current! rt-prx)
              (v:push! True buffer)
              (unmask-current! rt-prx)
              Unit))))
      ;; Run the benchmark
      (sleep 2)
      (wrap-io (b:start (b:current-timer)))
      (put-mvar start-gate Unit)
      (await threads)
      ;; Cleanup
      (wrap-io
       (b:stop (b:current-timer))
       (b:commit (b:current-timer))
       ;; Write out the buffers to prevent SBCL from optimizing them away
       (c:write! benchmark-vectors-cache (Some buffers))
       Unit)))
    (values))
  )

(coalton-toplevel

  (declare benchmark-vector-cache (c:Cell (Optional (Vector Boolean))))
  (define benchmark-vector-cache (c:new None))

  (declare benchmark-vectors-cache (c:Cell (Optional (Vector (Vector Boolean)))))
  (define benchmark-vectors-cache (c:new None))

  (declare benchmark-mask-unmask-single-thread (UFix -> Void))
  (define (benchmark-mask-unmask-single-thread n-tasks)
    "This benchmark writes to a pre-allocated vector `n-tasks` times and masks and unmasks each iteration."
    (run-io!
     (do
      ;; Setup benchmark
      (buffer <-
        (wrap-io
          (v:with-capacity n-tasks)))
      ;; Run the benchmark
      (wrap-io (b:start (b:current-timer)))
      (do-repeat-io n-tasks
        mask-current-thread
        (wrap-io (v:push! True buffer))
        unmask-current-thread)
      ;; Cleanup
      (wrap-io
       (b:stop (b:current-timer))
       (b:commit (b:current-timer))
       ;; Prevent optimization
       (c:write! benchmark-vector-cache (Some buffer))
       Unit)))
    (values))

  (declare benchmark-mask-unmask-six-threads (UFix -> Void))
  (define (benchmark-mask-unmask-six-threads n-tasks)
    "This benchmark splits `n-tasks` vector writes across six threads and masks and unmasks each iteration."
    (run-io!
     (do
      ;; Setup benchmark
      (let n-threads = 6)
      (let tasks-per-thread = (coalton/math:div n-tasks n-threads))
      (start-gate <- new-empty-mvar)
      (buffers <-
        (wrap-io
          (let buffers = (v:with-capacity n-threads))
          (dotimes (_ n-threads)
            (v:push! (v:with-capacity tasks-per-thread) buffers))
          buffers))
      (threads <-
        (do-fork-n-threads (i n-threads)
          (let buffer = (v:index-unsafe i buffers))
          (read-mvar start-gate)
          (do-repeat-io tasks-per-thread
            mask-current-thread
            (wrap-io (v:push! True buffer))
            unmask-current-thread)))
      ;; Run the benchmark
      (sleep 2)
      (wrap-io (b:start (b:current-timer)))
      (put-mvar start-gate Unit)
      (await threads)
      ;; Cleanup
      (wrap-io
       (b:stop (b:current-timer))
       (b:commit (b:current-timer))
       ;; Prevent optimization
       (c:write! benchmark-vectors-cache (Some buffers))
       Unit)))
    (values))
  )

(in-package #:benchmark-runtime)

(cl:declaim (cl:optimize (cl:speed 3) (cl:safety 0)))

(defparameter *count* 75)

(c:coalton-toplevel
  (c:define *tasks* (c:the c:UFix 48000)))

(define-benchmark mask-unmask-direct-vector-single-thread ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-runtime/native::benchmark-mask-unmask-direct-single-thread
             *tasks*))))

(define-benchmark mask-unmask-direct-vector-six-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-runtime/native::benchmark-mask-unmask-direct-six-threads
             *tasks*))))

(define-benchmark mask-unmask-vector-single-thread ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-runtime/native::benchmark-mask-unmask-single-thread
             *tasks*))))

(define-benchmark mask-unmask-vector-six-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-runtime/native::benchmark-mask-unmask-six-threads
             *tasks*))))
