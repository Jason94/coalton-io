(cl:in-package :cl-user)
(defpackage :io/examples/thread-pool
  (:use
   #:coalton
   #:coalton-prelude
   #:io/monad-io
   #:io/simple-io
   #:io/thread
   #:io/conc/ring-buffer
   #:io/conc/worker-pool)
  (:local-nicknames
   (:v #:coalton/vector))
  (:import-from #:coalton/experimental/loops
   #:doiter
   #:dotimes)
  (:export
   #:run-single
   #:run-multi
  ))
(in-package :io/examples/thread-pool)

(named-readtables:in-readtable coalton:coalton)

;;; This is a good starting point for a common use case: taking a computation-heavy
;;; Coalton program and multithreading the work using coalton-io. If the computation
;;; work doesn't need to share state or coordinate with other threads, these are
;;; usually very easy to multithread with a thin coalton-io layer on top of the plain
;;; computation code.
;;;
;;; In this simple example program, we take in some data (the integers 1-1000) and
;;; do some processing with them (square them), storing them in an output buffer.
;;;
;;; A single-threaded runner that does not use coalton-io and a multi-threaded runner
;;; that does use coalton-io are both implemented, to show how similar it is to convert
;;; the single-threaded version.

(coalton-toplevel
  (define count (the UFix 1000))
  
  (define-struct ProgramData
    (input (v:Vector UFix))
    (output (v:Vector UFix)))

  (declare new-data (Void -> ProgramData))
  (define (new-data)
    (let input = (v:with-initial-element count 0))
    (dotimes (i (v:length input))
      (v:set! i (1+ i) input))
    (ProgramData
     input
     (v:with-initial-element (v:length input) 0)))

  (declare calculate (UFix -> UFix))
  (define (calculate x)
    "Calculate the output value for input value `x`."
    (* x x))

  (declare write-out (ProgramData * UFix -> Void))
  (define (write-out data i)
    "Calculate and write the output value into `data` at `i`."
    (let input = (v:index-unsafe i (.input data)))
    (let result = (calculate input))
    (v:set! i result (.output data))))
      
;;;
;;; Single-Threaded version of the program
;;; 

(coalton-toplevel
  (declare run-single_ (Void -> Void))
  (define (run-single_)
    (let data = (new-data))
    (dotimes (i count)
      (write-out data i))
    (traceobject "output" (.output data))))

(cl:defun run-single ()
  (coalton (run-single_)))

;;;
;;; Multi-Threaded version of the program
;;;
;;; Uses a `WorkerPool` to automatically start the worker threads. Submits
;;; jobs to the pool, which automatically runs them on the worker threads.
;;; This is more convenient (and likely more efficient) than manually splitting
;;; the work between threads.
;;;
;;; The submit jobs loop uses a strategy called "chunking" to get better performance.
;;; If the input data has 1,000 elements, it's a bad idea to submit 1,000 separate
;;; jobs to the pool. The workers will spend more time retrieving work from the queue,
;;; and less time running the calculations.
;;;

(coalton-toplevel
  ;; A good practice is to start with the same number of threads as CPU cores,
  ;; and experiment from there.
  (define n-threads (the UFix 4))
  ;; A good chunk size is large enough to not waste cycles with threads taking new work,
  ;; but small enough that threads which complete tasks more quickly have more work left
  ;; to take. A good practice is to have the number of chunks between 4-10x the number of
  ;; worker threads. The more consistent the amount of time to finish working on a chunk,
  ;; the smaller number of chunks are necessary.
  (define chunk-size (the UFix 50))

  (declare main (IO Unit))
  (define main
    (do
     ;;;; Setup the program data

     ;; For this pure-calculation program, it's critical that no two threads will ever
     ;; write to the same data at a time. Each thread owns a range of indices in the output
     ;; buffer to calculate and write to. This means that we don't need to use complex
     ;; synchronization data structures like atomics or the STM.
     (let data = (new-data))
     
     ;;;; Setup the worker pool
     (scheduler <- (new-ring-buffer-scheduler n-threads))
     (pool <- (new-worker-pool_ n-threads scheduler))

     ;;;; Send off the work
     (do-submit-indexed-chunks_ pool (chunk (0 count chunk-size))
       (wrap-io
        (doiter (i chunk)
          (write-out data i))
        Unit))

     ;;;; Wait for finish, then cleanup
     
     ;; This is a common pattern when using a `WorkerPool`. `request-shutdown` sends a
     ;; shutdown request to the end of the work queue. When all of the work that has
     ;; been submitted is processed, the worker threads will read the shutdown request
     ;; and terminate. 
     (request-shutdown pool)
     ;; Calling `await` on the pool will block the main thread until all of the worker
     ;; threads in the pool have finished the work and have stopped.
     (await pool)

     (wrap-io
      (traceobject "output" (.output data))
      Unit))))

(cl:defun run-multi ()
  (coalton (run-io! main)))
