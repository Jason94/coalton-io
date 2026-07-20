(defpackage #:io/stress/stress-tests
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton/experimental/do-control-core
   #:io/monad-io
   #:io/simple-io
   #:io/simple-io/loops
   #:io/thread
   #:io/conc/group
   #:io/tests/utils)
  (:import-from #:coalton/math/integral
   #:toInteger)
  (:import-from #:coalton/experimental/loops
   #:dotimes
   #:doiter)
  (:local-nicknames
   (:tm #:io/term)
   (:f #:coalton/format)
   (:c #:coalton/cell)
   (:la #:coalton/lisparray)
   (:v #:coalton/vector)
   )
  (:export
   #:Transfer
   #:linearized-producer-consumers-stress-test))

(in-package #:io/stress/stress-tests)

(named-readtables:in-readtable coalton:coalton)
(cl:declaim (cl:optimize (cl:speed 3) (cl:safety 0)))

;;;
;;; This file contains generic versions of stress-tests, which are reused by package files.
;;; 

(coalton-toplevel
  (derive Eq)
  (repr :transparent)
  (define-type ProducerId
    (ProducerId% UFix))

  (derive Eq Show)
  (repr :transparent)
  (define-type Count
    (Count% Integer))

  (derive Hash Eq)
  (define-type Transfer
    (Packet ProducerId Count)
    Finished))

(coalton-toplevel
  (declare verify-consumer-buffers (UFix * UFix * UFix * Vector (Vector Transfer) -> Result String Unit))
  (define (verify-consumer-buffers elts-per-producer n-producer-threads n-consumer-threads consumer-buffers)
    ;; Track, for each producer, which IDs were globally observed. Used to test completeness.
    (let observed-count-arrays = (v:new))
    (dotimes (_ n-producer-threads)
      (let count-array = (la:make elts-per-producer (the Bit 0)))
      (v:push! count-array observed-count-arrays))
    ;; Track, for each consumer, the max ProducerId observed so far per producer.
    ;; Used to test linearized observations.
    (let consumers-max-observed-counts = (v:new))
    (dotimes (_ n-consumer-threads)
      (let max-count-array = (v:new))
      (dotimes (_ n-producer-threads)
        (v:push! (c:new (Count% -1)) max-count-array))
      (v:push! max-count-array consumers-max-observed-counts))
    ;; Loop through the consumer buffers
    (dotimes (i-consumer n-consumer-threads)
      (let consumer-buffer = (v:index-unsafe i-consumer consumer-buffers))
      (let consumer-max-counts = (v:index-unsafe i-consumer consumers-max-observed-counts))
      (doiter (itm consumer-buffer)
        (match itm
          ((Finished)
           (return (Err "Observed `Finished` sentinel out of place. (This is probably a bug in the stress test.)")))
          ((Packet (ProducerId% prod-id) (Count% x))
           (let obs-count-array = (v:index-unsafe prod-id observed-count-arrays))
           (let max-count-cell = (v:index-unsafe prod-id consumer-max-counts))
           ;; Check and update the max count
           (let (Count% max-count-so-far) = (c:read max-count-cell))
           (when (<= x max-count-so-far)
             (return (Err (f:format f:Str "Observed packet (~a ~a) after packet (~a ~a)" prod-id x prod-id (c:read max-count-cell)))))
           (c:write! max-count-cell (Count% x))
           ;; Check and update globally observed count
           (when (== 1 (la:aref obs-count-array (unwrap-into x)))
             (return (Err (f:format f:Str "Globally observed packet (~a ~a) multiple times" prod-id x))))
           (la:set! obs-count-array (unwrap-into x) 1)))))
    (Ok Unit))
  
  (declare linearized-producer-consumers-stress-test
           (UFix * UFix * UFix * (Transfer -> IO Boolean) * IO (Optional Transfer) * IO Boolean -> IO (Result String Unit)))
  (define (linearized-producer-consumers-stress-test elts-per-producer n-producer-threads n-consumer-threads
                                                     producer-op consumer-op
                                                     check-empty-op)
    "Stress test that runs `n-producer-threads` and `n-consumer-threads`.

Each producer runs `producer-op` to push a `Packet` into some synchronized resource. Producers
should not block. If production fails, it should return `False` and will be retried.

Each consumer runs `consumer-op` to take a packet from the synchronized resource. If consumption
fails, it will be retried.

`check-empty-op` should return `True` if the shared resource is empty, or `False` if it is not.

At the end, the test returns `Ok Unit` if the following invariants are observed:
  - `elts-per-producer` * `n-producer-threads` total packets were transferred
  - No packet was transferred more than once
  - Each consumer observed a linearizable ordering of packets
  - All producer and consumer threads completed (or the test will hang indefinitely)
  - None of the producer or consumer threads raised an exception"
    (do
      (tm:write-line "starting test")
      ;; Set up the test
      (start-gate <- s-new)
      (consumer-buffers <-
        (wrap-io
         (let buffers = (v:new))
         (dotimes (_ n-consumer-threads)
           (let buffer = (v:with-capacity elts-per-producer))
           (v:push! buffer buffers))
         buffers))
     (producers <-
      (do-fork-n-threads (i-producer n-producer-threads)
        (s-await start-gate)
        (do-times-io (x elts-per-producer)
          (let packet = (Packet (ProducerId% i-producer) (Count% (toInteger x))))
          (do-while-io
           (map not (producer-op packet))))))
      (consumers <-
       (do-fork-n-threads (i-consumer n-consumer-threads)
         (s-await start-gate)
         (buffer <-
           (wrap-io (v:index-unsafe i-consumer consumer-buffers)))
         (do-while-io
           (transf <- (do-until-val-io consumer-op))
           (do-match transf
             ((Finished)
              (pure False))
             (pkt
              (wrap-io (v:push! pkt buffer))
              (pure True))
             ))))
      ;; Wait for threads to start then kick off the test
      (sleep 10)
      (s-signal start-gate :count (+ n-producer-threads n-consumer-threads))
      ;; Wait for the producers to finish, then submit n-consumer-threads finish sentinels
      ;; and wait for the consumers to finish
      (await producers)
      (do-repeat-io n-consumer-threads
        (producer-op Finished))
      (await consumers)
      ;; Now verify
      (empty? <- check-empty-op)
      (pure
       (if empty?
           (verify-consumer-buffers elts-per-producer n-producer-threads n-consumer-threads consumer-buffers)
           (Err "Synchronized resource not empty at end of test"))))))
      
