(in-package #:io/benchmarks)

(define-io-benchmark-package mvars
  ((:local-nicknames
    (:c #:coalton)))
  (:use
   #:coalton
   #:coalton-prelude
   #:io/monad-io
   #:io/exceptions
   #:io/simple-io
   #:io/simple-io/loops
   #:io/thread
   #:io/conc/group
   #:io/conc/mvar)
  (:import-from #:coalton/experimental/do-control-core
   #:do-when)
  (:local-nicknames
   (:b #:benchmark-utils)))

(in-package #:benchmark-mvars/native)

(cl:declaim (cl:optimize (cl:speed 3) (cl:safety 0)))

(coalton-toplevel

  (declare benchmark-blocking-transfer (UFix * UFix * UFix -> Unit))
  (define (benchmark-blocking-transfer n-tasks n-put-threads n-take-threads)
    (run-io!
     (do
      (mvar <- (the (IO (MVar Unit)) new-empty-mvar))
      (let tasks-per-put-thread = (coalton/math:div n-tasks n-put-threads))
      (let tasks-per-take-thread = (coalton/math:div n-tasks n-take-threads))
      (start-gate <- new-empty-mvar)
      (putters <-
        (do-fork-n-threads (_ n-put-threads)
          (read-mvar start-gate)
          (do-repeat-io tasks-per-put-thread
            (put-mvar mvar Unit))))
      (takers <-
        (do-fork-n-threads (_ n-take-threads)
          (read-mvar start-gate)
          (do-repeat-io tasks-per-take-thread
            (take-mvar mvar))))
      (sleep 2)
      (wrap-io (b:start (b:current-timer)))
      (put-mvar start-gate Unit)
      (await putters)
      (await takers)
      (wrap-io
       (b:stop (b:current-timer))
       (b:commit (b:current-timer))
       Unit)
      (empty? <- (is-empty-mvar mvar))
      (do-when (not empty?)
        (raise "blocking transfer left the MVar full")))))

  (declare benchmark-nonblocking-transfer (UFix * UFix * UFix -> Unit))
  (define (benchmark-nonblocking-transfer n-tasks n-put-threads n-take-threads)
    (run-io!
     (do
      (mvar <- (the (IO (MVar Unit)) new-empty-mvar))
      (let tasks-per-put-thread = (coalton/math:div n-tasks n-put-threads))
      (let tasks-per-take-thread = (coalton/math:div n-tasks n-take-threads))
      (start-gate <- new-empty-mvar)
      (putters <-
        (do-fork-n-threads (_ n-put-threads)
          (read-mvar start-gate)
          (do-repeat-io tasks-per-put-thread
            (do-while-io
              (put? <- (try-put-mvar mvar Unit))
              (pure (not put?))))))
      (takers <-
        (do-fork-n-threads (_ n-take-threads)
          (read-mvar start-gate)
          (do-repeat-io tasks-per-take-thread
            (do-while-io
              (result <- (try-take-mvar mvar))
              (pure (none? result))))))
      (sleep 2)
      (wrap-io (b:start (b:current-timer)))
      (put-mvar start-gate Unit)
      (await putters)
      (await takers)
      (wrap-io
       (b:stop (b:current-timer))
       (b:commit (b:current-timer))
       Unit)
      (empty? <- (is-empty-mvar mvar))
      (do-when (not empty?)
        (raise "nonblocking transfer left the MVar full")))))

  (declare benchmark-blocking-fill-drain (UFix * UFix -> Unit))
  (define (benchmark-blocking-fill-drain n-tasks n-threads)
    (run-io!
     (do
      (mvar <- (the (IO (MVar Unit)) new-empty-mvar))
      (let tasks-per-thread = (coalton/math:div n-tasks n-threads))
      (start-gate <- new-empty-mvar)
      (threads <-
        (do-fork-n-threads (_ n-threads)
          (read-mvar start-gate)
          (do-repeat-io tasks-per-thread
            (put-mvar mvar Unit)
            (take-mvar mvar))))
      (sleep 2)
      (wrap-io (b:start (b:current-timer)))
      (put-mvar start-gate Unit)
      (await threads)
      (wrap-io
       (b:stop (b:current-timer))
       (b:commit (b:current-timer))
       Unit)
      (empty? <- (is-empty-mvar mvar))
      (do-when (not empty?)
        (raise "blocking fill/drain left the MVar full")))))

  (declare benchmark-nonblocking-fill-drain (UFix * UFix -> Unit))
  (define (benchmark-nonblocking-fill-drain n-tasks n-threads)
    (run-io!
     (do
      (mvar <- (the (IO (MVar Unit)) new-empty-mvar))
      (let tasks-per-thread = (coalton/math:div n-tasks n-threads))
      (start-gate <- new-empty-mvar)
      (threads <-
        (do-fork-n-threads (_ n-threads)
          (read-mvar start-gate)
          (do-repeat-io tasks-per-thread
            (do-while-io
              (put? <- (try-put-mvar mvar Unit))
              (pure (not put?)))
            (do-while-io
              (result <- (try-take-mvar mvar))
              (pure (none? result))))))
      (sleep 2)
      (wrap-io (b:start (b:current-timer)))
      (put-mvar start-gate Unit)
      (await threads)
      (wrap-io
       (b:stop (b:current-timer))
       (b:commit (b:current-timer))
       Unit)
      (empty? <- (is-empty-mvar mvar))
      (do-when (not empty?)
        (raise "nonblocking fill/drain left the MVar full")))))

  (declare benchmark-read-mvar-full (UFix * UFix -> Unit))
  (define (benchmark-read-mvar-full n-tasks n-threads)
    (run-io!
     (do
      (mvar <- (new-mvar Unit))
      (let tasks-per-thread = (coalton/math:div n-tasks n-threads))
      (start-gate <- new-empty-mvar)
      (threads <-
        (do-fork-n-threads (_ n-threads)
          (read-mvar start-gate)
          (do-repeat-io tasks-per-thread
            (read-mvar mvar))))
      (sleep 2)
      (wrap-io (b:start (b:current-timer)))
      (put-mvar start-gate Unit)
      (await threads)
      (wrap-io
       (b:stop (b:current-timer))
       (b:commit (b:current-timer))
       Unit)
      (empty? <- (is-empty-mvar mvar))
      (do-when empty?
        (raise "read-mvar emptied the MVar")))))

  (declare benchmark-try-read-mvar-full (UFix * UFix -> Unit))
  (define (benchmark-try-read-mvar-full n-tasks n-threads)
    (run-io!
     (do
      (mvar <- (new-mvar Unit))
      (let tasks-per-thread = (coalton/math:div n-tasks n-threads))
      (start-gate <- new-empty-mvar)
      (threads <-
        (do-fork-n-threads (_ n-threads)
          (read-mvar start-gate)
          (do-repeat-io tasks-per-thread
            (result <- (try-read-mvar mvar))
            (match result
              ((Some _) (pure Unit))
              ((None) (raise "try-read-mvar returned None for a full MVar"))))))
      (sleep 2)
      (wrap-io (b:start (b:current-timer)))
      (put-mvar start-gate Unit)
      (await threads)
      (wrap-io
       (b:stop (b:current-timer))
       (b:commit (b:current-timer))
       Unit)
      (empty? <- (is-empty-mvar mvar))
      (do-when empty?
        (raise "successful try-read-mvar emptied the MVar")))))

  (declare benchmark-try-read-mvar-empty (UFix * UFix -> Unit))
  (define (benchmark-try-read-mvar-empty n-tasks n-threads)
    (run-io!
     (do
      (mvar <- (the (IO (MVar Unit)) new-empty-mvar))
      (let tasks-per-thread = (coalton/math:div n-tasks n-threads))
      (start-gate <- new-empty-mvar)
      (threads <-
        (do-fork-n-threads (_ n-threads)
          (read-mvar start-gate)
          (do-repeat-io tasks-per-thread
            (result <- (try-read-mvar mvar))
            (match result
              ((None) (pure Unit))
              ((Some _) (raise "try-read-mvar returned Some for an empty MVar"))))))
      (sleep 2)
      (wrap-io (b:start (b:current-timer)))
      (put-mvar start-gate Unit)
      (await threads)
      (wrap-io
       (b:stop (b:current-timer))
       (b:commit (b:current-timer))
       Unit)
      (empty? <- (is-empty-mvar mvar))
      (do-when (not empty?)
        (raise "unsuccessful try-read-mvar filled the MVar")))))

  (declare benchmark-swap-mvar-full (UFix * UFix -> Unit))
  (define (benchmark-swap-mvar-full n-tasks n-threads)
    (run-io!
     (do
      (mvar <- (new-mvar Unit))
      (let tasks-per-thread = (coalton/math:div n-tasks n-threads))
      (start-gate <- new-empty-mvar)
      (threads <-
        (do-fork-n-threads (_ n-threads)
          (read-mvar start-gate)
          (do-repeat-io tasks-per-thread
            (swap-mvar mvar Unit))))
      (sleep 2)
      (wrap-io (b:start (b:current-timer)))
      (put-mvar start-gate Unit)
      (await threads)
      (wrap-io
       (b:stop (b:current-timer))
       (b:commit (b:current-timer))
       Unit)
      (empty? <- (is-empty-mvar mvar))
      (do-when empty?
        (raise "swap-mvar left the MVar empty")))))

  (declare benchmark-with-mvar (UFix * UFix -> Unit))
  (define (benchmark-with-mvar n-tasks n-threads)
    (run-io!
     (do
      (mvar <- (new-mvar Unit))
      (let tasks-per-thread = (coalton/math:div n-tasks n-threads))
      (start-gate <- new-empty-mvar)
      (threads <-
        (do-fork-n-threads (_ n-threads)
          (read-mvar start-gate)
          (do-repeat-io tasks-per-thread
            (with-mvar_ mvar (fn (_) (pure Unit))))))
      (sleep 2)
      (wrap-io (b:start (b:current-timer)))
      (put-mvar start-gate Unit)
      (await threads)
      (wrap-io
       (b:stop (b:current-timer))
       (b:commit (b:current-timer))
       Unit)
      (empty? <- (is-empty-mvar mvar))
      (do-when empty?
        (raise "with-mvar_ left the MVar empty")))))
  )

(in-package #:benchmark-mvars)

(cl:declaim (cl:optimize (cl:speed 3) (cl:safety 0)))

(defparameter *count* 30)

(c:coalton-toplevel
  (c:define *tasks* (c:the c:UFix 48000)))

(define-benchmark blocking-put-take-transfer-x-tasks-1-put-thread-1-take-thread ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-mvars/native::benchmark-blocking-transfer
             *tasks*
             1
             1))))

(define-benchmark blocking-put-take-transfer-x-tasks-1-put-thread-3-take-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-mvars/native::benchmark-blocking-transfer
             *tasks*
             1
             3))))

(define-benchmark blocking-put-take-transfer-x-tasks-1-put-thread-6-take-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-mvars/native::benchmark-blocking-transfer
             *tasks*
             1
             6))))

(define-benchmark blocking-put-take-transfer-x-tasks-3-put-threads-1-take-thread ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-mvars/native::benchmark-blocking-transfer
             *tasks*
             3
             1))))

(define-benchmark blocking-put-take-transfer-x-tasks-3-put-threads-3-take-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-mvars/native::benchmark-blocking-transfer
             *tasks*
             3
             3))))

(define-benchmark blocking-put-take-transfer-x-tasks-3-put-threads-6-take-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-mvars/native::benchmark-blocking-transfer
             *tasks*
             3
             6))))

(define-benchmark blocking-put-take-transfer-x-tasks-6-put-threads-1-take-thread ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-mvars/native::benchmark-blocking-transfer
             *tasks*
             6
             1))))

(define-benchmark blocking-put-take-transfer-x-tasks-6-put-threads-3-take-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-mvars/native::benchmark-blocking-transfer
             *tasks*
             6
             3))))

(define-benchmark blocking-put-take-transfer-x-tasks-6-put-threads-6-take-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-mvars/native::benchmark-blocking-transfer
             *tasks*
             6
             6))))

(define-benchmark nonblocking-try-put-try-take-transfer-x-tasks-1-try-put-thread-1-try-take-thread ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-mvars/native::benchmark-nonblocking-transfer
             *tasks*
             1
             1))))

(define-benchmark nonblocking-try-put-try-take-transfer-x-tasks-1-try-put-thread-3-try-take-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-mvars/native::benchmark-nonblocking-transfer
             *tasks*
             1
             3))))

(define-benchmark nonblocking-try-put-try-take-transfer-x-tasks-1-try-put-thread-6-try-take-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-mvars/native::benchmark-nonblocking-transfer
             *tasks*
             1
             6))))

(define-benchmark nonblocking-try-put-try-take-transfer-x-tasks-3-try-put-threads-1-try-take-thread ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-mvars/native::benchmark-nonblocking-transfer
             *tasks*
             3
             1))))

(define-benchmark nonblocking-try-put-try-take-transfer-x-tasks-3-try-put-threads-3-try-take-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-mvars/native::benchmark-nonblocking-transfer
             *tasks*
             3
             3))))

(define-benchmark nonblocking-try-put-try-take-transfer-x-tasks-3-try-put-threads-6-try-take-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-mvars/native::benchmark-nonblocking-transfer
             *tasks*
             3
             6))))

(define-benchmark nonblocking-try-put-try-take-transfer-x-tasks-6-try-put-threads-1-try-take-thread ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-mvars/native::benchmark-nonblocking-transfer
             *tasks*
             6
             1))))

(define-benchmark nonblocking-try-put-try-take-transfer-x-tasks-6-try-put-threads-3-try-take-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-mvars/native::benchmark-nonblocking-transfer
             *tasks*
             6
             3))))

(define-benchmark nonblocking-try-put-try-take-transfer-x-tasks-6-try-put-threads-6-try-take-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-mvars/native::benchmark-nonblocking-transfer
             *tasks*
             6
             6))))

(define-benchmark blocking-fill-drain-x-tasks-1-thread ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-mvars/native::benchmark-blocking-fill-drain
             *tasks*
             1))))

(define-benchmark blocking-fill-drain-x-tasks-2-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-mvars/native::benchmark-blocking-fill-drain
             *tasks*
             2))))

(define-benchmark blocking-fill-drain-x-tasks-4-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-mvars/native::benchmark-blocking-fill-drain
             *tasks*
             4))))

(define-benchmark blocking-fill-drain-x-tasks-6-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-mvars/native::benchmark-blocking-fill-drain
             *tasks*
             6))))

(define-benchmark blocking-fill-drain-x-tasks-12-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-mvars/native::benchmark-blocking-fill-drain
             *tasks*
             12))))

(define-benchmark nonblocking-fill-drain-x-tasks-1-thread ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-mvars/native::benchmark-nonblocking-fill-drain
             *tasks*
             1))))

(define-benchmark nonblocking-fill-drain-x-tasks-2-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-mvars/native::benchmark-nonblocking-fill-drain
             *tasks*
             2))))

(define-benchmark nonblocking-fill-drain-x-tasks-4-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-mvars/native::benchmark-nonblocking-fill-drain
             *tasks*
             4))))

(define-benchmark nonblocking-fill-drain-x-tasks-6-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-mvars/native::benchmark-nonblocking-fill-drain
             *tasks*
             6))))

(define-benchmark nonblocking-fill-drain-x-tasks-12-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-mvars/native::benchmark-nonblocking-fill-drain
             *tasks*
             12))))

(define-benchmark read-mvar-full-x-tasks-1-thread ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-mvars/native::benchmark-read-mvar-full
             *tasks*
             1))))

(define-benchmark read-mvar-full-x-tasks-2-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-mvars/native::benchmark-read-mvar-full
             *tasks*
             2))))

(define-benchmark read-mvar-full-x-tasks-4-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-mvars/native::benchmark-read-mvar-full
             *tasks*
             4))))

(define-benchmark read-mvar-full-x-tasks-6-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-mvars/native::benchmark-read-mvar-full
             *tasks*
             6))))

(define-benchmark read-mvar-full-x-tasks-12-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-mvars/native::benchmark-read-mvar-full
             *tasks*
             12))))

(define-benchmark try-read-mvar-full-x-tasks-1-thread ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-mvars/native::benchmark-try-read-mvar-full
             *tasks*
             1))))

(define-benchmark try-read-mvar-full-x-tasks-2-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-mvars/native::benchmark-try-read-mvar-full
             *tasks*
             2))))

(define-benchmark try-read-mvar-full-x-tasks-4-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-mvars/native::benchmark-try-read-mvar-full
             *tasks*
             4))))

(define-benchmark try-read-mvar-full-x-tasks-6-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-mvars/native::benchmark-try-read-mvar-full
             *tasks*
             6))))

(define-benchmark try-read-mvar-full-x-tasks-12-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-mvars/native::benchmark-try-read-mvar-full
             *tasks*
             12))))

(define-benchmark try-read-mvar-empty-x-tasks-1-thread ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-mvars/native::benchmark-try-read-mvar-empty
             *tasks*
             1))))

(define-benchmark try-read-mvar-empty-x-tasks-2-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-mvars/native::benchmark-try-read-mvar-empty
             *tasks*
             2))))

(define-benchmark try-read-mvar-empty-x-tasks-4-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-mvars/native::benchmark-try-read-mvar-empty
             *tasks*
             4))))

(define-benchmark try-read-mvar-empty-x-tasks-6-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-mvars/native::benchmark-try-read-mvar-empty
             *tasks*
             6))))

(define-benchmark try-read-mvar-empty-x-tasks-12-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-mvars/native::benchmark-try-read-mvar-empty
             *tasks*
             12))))

(define-benchmark swap-mvar-full-x-tasks-1-thread ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-mvars/native::benchmark-swap-mvar-full
             *tasks*
             1))))

(define-benchmark swap-mvar-full-x-tasks-2-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-mvars/native::benchmark-swap-mvar-full
             *tasks*
             2))))

(define-benchmark swap-mvar-full-x-tasks-4-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-mvars/native::benchmark-swap-mvar-full
             *tasks*
             4))))

(define-benchmark swap-mvar-full-x-tasks-6-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-mvars/native::benchmark-swap-mvar-full
             *tasks*
             6))))

(define-benchmark swap-mvar-full-x-tasks-12-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-mvars/native::benchmark-swap-mvar-full
             *tasks*
             12))))

(define-benchmark with-mvar-x-tasks-1-thread ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-mvars/native::benchmark-with-mvar
             *tasks*
             1))))

(define-benchmark with-mvar-x-tasks-2-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-mvars/native::benchmark-with-mvar
             *tasks*
             2))))

(define-benchmark with-mvar-x-tasks-4-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-mvars/native::benchmark-with-mvar
             *tasks*
             4))))

(define-benchmark with-mvar-x-tasks-6-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-mvars/native::benchmark-with-mvar
             *tasks*
             6))))

(define-benchmark with-mvar-x-tasks-12-threads ()
  (declare (optimize speed))
  (loop :repeat *count*
        :do
           (c:coalton
            (benchmark-mvars/native::benchmark-with-mvar
             *tasks*
             12))))
