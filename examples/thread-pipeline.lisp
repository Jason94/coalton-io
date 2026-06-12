(cl:in-package :cl-user)
(defpackage :io/examples/thread-pipeline
  (:use
   #:coalton
   #:coalton-prelude
   #:io/simple-io
   #:io/simple-io/loops
   #:io/thread
   #:io/term
   #:io/exceptions
   #:io/random
   #:io/conc/future
   #:io/conc/scheduler
   #:io/conc/worker-pool
   #:coalton-library/experimental/do-control-core)
  (:local-nicknames
   (:s #:coalton-library/string)
   (:f_ #:coalton-library/file)
   (:f #:io/file)
   (:r #:coalton-library/result)
   (:mv #:io/conc/mvar)
   (:m #:io/mut))
  (:export
   #:run-example))
(in-package :io/examples/thread-pipeline)

(named-readtables:in-readtable coalton:coalton)

;;; This example implements a three-step, multithreaded pipeline to parallelize reading
;;; the lines from a large file, parsing them as integers, and summing them.
;;;
;;; It demonstrates how to use several of coalton-io's concurrency tools together to
;;; write an optimized multithreaded pipeline, such as: worker pools, queues, and futures.
;;;
;;; The first step: Read the file.
;;; The second step: Parse the lines into integers.
;;; The third step: Sum the integers together.
;;;
;;; The first step, reading the file, runs in a single thread. All that it does is
;;; read a line from the file, pass it on to the next step in the pipeline, and
;;; repeat until the entire file has been read. This is a good task to single-thread
;;; because it's complicated to read from a resource like a file in parallel on different
;;; threads.
;;;
;;; The second step, parsing the integers, runs in a worker pool with 4 threads. Workers
;;; in the pool receive a string to parse, parse them as an integer, and pass the parsed
;;; integer on to the next step in the pipeline. This is a good task to parallelize
;;; because (1) parsing an integer is a relatively expensive task and (2) it's a pure
;;; calculation, so it doesn't involve any synchronized memory access which could slow
;;; down at high-thread counts.
;;;
;;; The third step, summing the parsed integers, also runs in a single thread.
;;; It has a thread-local running total. When it receives a parsed integer from the second
;;; step, it adds it to the running total. This is a good task to run on a single thread
;;; because (1) adding is a relatively cheap operation, and (2) by keeping the running
;;; total local to one thread, it avoids the overhead of synchronized access. (If you wanted
;;; to parallelize summing, it would be easy to have multiple threads with individual
;;; thread-local sums, and add them together at the end. That is, essentially, the
;;; map-reduce algorithm. But that isn't always easy to do.)
;;;
;;; The main thread sets up the threads for each step in the pipeline, starts them, and
;;; waits for the processing to complete.

(coalton-toplevel
  (define data-filename "rands.txt")

  (declare data-pathname f_:Pathname)
  (define data-pathname (into data-filename))

  (define data-rows (the UFix 1000))
  (define data-max (the UFix 500000000))
  (define n-workers (the UFix 4))

  ;; Note: Not part of the multithreaded example. Runs on a single thread before the main
  ;; multithreaded processing kicks off.
  (declare write-data-file (IO Unit))
  (define write-data-file
    "Check if the data file exists. If it does not, write a new file with `data-rows`
number of lines, where each line is a random integer between `0` and `data-max`."
    (do
     (exists? <- (map (fn (file?) (r:ok-or-def False file?))
                      (f:exists? data-filename)))
     (do-when (not exists?)
       (write-line "Writing data file...")
       (f:do-with-open-file_ (data-filename fs :direction f_:Output
                                               :if-exists f_:Overwrite)
         (do-repeat-io data-rows
           (x <- (random_ data-max))
           (f:write-line fs (into x)))
         (pure Unit))
       (write-line "Done writing data file..."))))

  ;; Pipeline Step 1: Read lines from the data file and send them as raw strings to Step 2.
  (declare reader-thread ((String -> IO Unit) -> IO Unit))
  (define (reader-thread submit-line)
    (f:do-with-open-file_ (data-filename fs)
      (do-while-val-io (line (f:read-line fs))
        (submit-line line))))

  ;; Pipeline Step 2: Parse raw strings from Step 1 and send them to Step 3.
  (declare parser-job (mv:MChan (Optional Integer) * String -> IO Unit))
  (define (parser-job mchan-sum str)
    (match (s:parse-int str)
      ((None)
       (raise (<> "Data file contained invalid string: " str)))
      ((Some x)
       (mv:push-chan mchan-sum (Some x)))))

  ;; Pipeline Step 3: Sum integers from step 2 and return the sum at the end.
  (declare summer-thread (mv:MChan (Optional Integer) -> IO Integer))
  (define (summer-thread mchan-int)
    (do
     (sum <- (m:new-var 0))
     (do-while-val-io (x (mv:pop-chan mchan-int))
       (m:modify sum (fn (val) (+ x val))))
     (m:read sum)))

  (declare sum-file (IO Integer))
  (define sum-file
    (do
     ;;;; Set up the data file
     write-data-file

     ;;;; Setup the worker pool, but don't submit jobs yet

     ;; Because one line in a file will get sent as one job into the pool, we
     ;; create a scheduler with a larger capacity.
     (scheduler <- (new-bounded-scheduler (* 100 n-workers)))
     (pool <- (new-worker-pool_ n-workers scheduler))
     
     ;; Worker threads in the pool will pass the parsed lines to the summer thread
     ;; via this channel. We use an unbounded queue for this, instead of the
     ;; buffered queue used for the worker pool, to prevent the worker threads from
     ;; blocking on submitting to the summer thread.
     (ints-chan <- mv:new-empty-chan)

     ;; The main thread won't submit work to the pool directly. The reader thread
     ;; will submit work to the pool, but we don't want it to have all of the implementation
     ;; details of the downstream pipeline. This closure takes a line from the data file
     ;; and submits a job to the pool to process it. All that the reader thread needs to
     ;; do is call this closure.
     (let submit-line-job =
       (fn (line)
         (do-submit-job pool
           (parser-job ints-chan line))))

     ;;;; Start the threads
     
     (write-line "Forking threads...")
     (reader-thread <- (fork-thread_ (reader-thread submit-line-job)))
     (sum-fut <- (fork-future_ (summer-thread ints-chan)))

     ;;;; Wait for finish, then cleanup
     (write-line "Waiting for reading to finish...")
     (await reader-thread)
      
     (write-line "Reading finished. Waiting for parsing to finish...")
     (request-shutdown pool)
     (await pool) 

     (write-line "Parsing finished. Letting sum thread know parsing is done and waiting...")
     (mv:push-chan ints-chan None)
     (sum <- (await sum-fut))
    
     (write-line (<> "Calculated sum: " (into sum)))
     (pure sum)))
  )

(cl:defun run-example ()
  (coalton (run-io! sum-file)))
