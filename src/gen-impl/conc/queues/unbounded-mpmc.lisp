(cl:in-package :cl-user)
(defpackage :io/gen-impl/conc/queues/unbounded-mpmc
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton/types
   #:coalton/experimental/do-control-core
   #:io/utils
   #:io/classes/monad-io
   #:io/classes/thread
   #:io/gen-impl/conc/mvar
   #:io/gen-impl/conc/queues
   #:io/gen-impl/conc/parking
   )
  (:import-from #:coalton/experimental/loops
   #:dotimes)
  (:local-nicknames
   (:b #:coalton-library/bits)
   (:i #:coalton/math/integral)
   (:c #:coalton/cell)
   (:la #:coalton/lisparray)
   (:at #:atomics)
   (:atc #:io/utilities/atom-compat)
   )
  (:export
   ;; Library Public
   #:UnboundedMpmcQueue
   #:new-unbounded-mpmc-queue
   ))
(in-package :io/gen-impl/conc/queues/unbounded-mpmc)

(cl:declaim (cl:optimize (cl:speed 3) (cl:debug 0) (cl:safety 0)))

(named-readtables:in-readtable coalton:coalton)

(cl:deftype ufixnum ()
  `(cl:integer 0 ,cl:most-positive-fixnum))

;; This is an implementation of the LPRQ Queue algorithm, described:
;; https://nikitakoval.org/publications/ppopp23-lprq.pdf
;; Research implementation:
;; https://zenodo.org/records/7337237

;; An LPRQ is a Linked list of Portable Ring Queues. Each PRQ is a fully-functional,
;; bounded MPMC circular buffer in its own right. At start, an LPRQ is a linked list 
;; with one node: a single PRQ. Enqueues push elements onto the tail of the PRQ, and
;; dequeues pop elements from the head of the PRQ. If an enqueue attempts to push onto
;; a full PRQ, then the LPRQ closes that PRQ to further additions, allocates a new PRQ,
;; pushes that onto the end of the linked list of PRQs, and enqueues the element onto
;; the new PRQ. Similarly, if a dequeue fully drains a PRQ that has already been closed,
;; it pops it from the linked list.
;;
;; If items are being enqueued and dequeued in a perfectly even rate, then the LPRQ will
;; never need to allocate a second PRQ. This property allows the LPRQ to retain the
;; performance of a bounded circular ring buffer in the optimal case. Because the PRQ
;; buffers are relatively large (the paper recommends 1024 items) and can often reuse
;; buffer cells without allocating new buffers, the LPRQ is much more memory efficient
;; than a naive unbounded MPMC queue backed by a synchronized linked-list where each
;; element is its own node (synchronized by atomics, mvars, or similar).
;; 
;; Warning: Because pointers to the current thread are used as metadata in the algorithm,
;; the queue CANNOT be used to store the :t in (Threads :rt :t :m) directly. They must
;; be boxed first, then they can be used safely.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;        PRQ Implementation         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:defconstant +prq-length+ 1024
    "The paper recommends a PRQ length of 1024 based on empirical results. Regardless,
must be a power of 2 so the compiler can optimize the integer div operations.")

(cl:defconstant +prq-index-mask+ (cl:1- +prq-length+))

;;; --------------------------------- Slot Metadata --------------------------------- ;;;
;;; 
;;; The slot metadata contains two fields
;;;   - Safe:  a boolean flag whether the slot is safe (1) or is not safe (0)
;;;   - Epoch: an integer value for the current epoch
;;;
;;; In the Word, the first bit is the safe flag. The remaining bits are the epoch.
;;;
;;; 0 0000000    00000000 ... 00000000
;;; ^ ^                              ^
;;; | |                              |---End of epoch bits
;;; | |--- Start of epoch bits
;;; |
;;; |--- Safe flag bit
;;; 
;;; --------------------------------------------------------------------------------- ;;;

(cl:deftype slot-metadata () 'cl:fixnum)

(cl:defconstant +cl-word-mask+
  #+64-bit #xffffffffffffffff
  #+32-bit #xffffffff
  #-(or 32-bit 64-bit) -1)

(cl:declaim (cl:inline pack))
(cl:defun pack (safe? epoch)
  "Pack `safe?` and `epoch` into metadata bits."
  (cl:declare (cl:type cl:boolean safe?)
              (cl:type cl-word epoch)
              (cl:values slot-metadata))
  (cl:logior
   (cl:logand (cl:ash epoch 1) +cl-word-mask+)
   (cl:if safe? 1 0)))

(cl:declaim (cl:inline safe?))
(cl:defun safe? (metadata)
  "Is `metadata` safe? set to `t`?"
  (cl:declare (cl:type slot-metadata metadata)
              (cl:values cl:boolean))
  (cl:logbitp 0 metadata))

(cl:declaim (cl:inline epoch))
(cl:defun epoch (metadata)
  "Get the epoch value for `metadata`."
  (cl:declare (cl:type slot-metadata metadata)
              (cl:values slot-metadata))
  (cl:ash metadata -1))
            
(cl:defconstant +new-metadata+
  (cl:logior (cl:logand (cl:ash 0 1) +cl-word-mask+)
             (cl:if cl:t 1 0)))

;;;
;;; Empty Sentinel
;;; 

(cl:defconstant +empty-token+
  '%empty-token%)

(cl:declaim (cl:inline empty-p))
(cl:defun empty-p (obj)
  (cl:declare (cl:values cl:boolean))
  (cl:eq obj +empty-token+))
      
;;;
;;; PRQ Structure
;;; 

;; Slot data is stored as adjacent pairs:
;; [metadata_0 value_0 metadata_1 value_1 ... metadata_1024 value_1024]
;; Types are
;;   - value:    anything
;;   - metadata: slot-metadata (fixnum)
;; (Yes, this is just cl:t, but it serves a useful documentation purpose)
(cl:deftype slot-data () '(cl:or slot-metadata cl:t))

(cl:defstruct (prq% (:constructor make-prq% (head tail closed slot-data next)))
  (head      (cl:error "must provide head")   :type cl-word)
  (%pad2     0                                :type cl:fixnum)
  (%pad3     0                                :type cl:fixnum)
  (%pad4     0                                :type cl:fixnum)
  (%pad5     0                                :type cl:fixnum)
  (%pad6     0                                :type cl:fixnum)
  (%pad7     0                                :type cl:fixnum)
  (%pad8     0                                :type cl:fixnum)

  (tail      (cl:error "must provide tail")   :type cl-word)
  (%pad1-2   0                                :type cl:fixnum)
  (%pad1-3   0                                :type cl:fixnum)
  (%pad1-4   0                                :type cl:fixnum)
  (%pad1-5   0                                :type cl:fixnum)
  (%pad1-6   0                                :type cl:fixnum)
  (%pad1-7   0                                :type cl:fixnum)
  (%pad1-8   0                                :type cl:fixnum)

  (slot-data (cl:error "must provide slots")  :type (cl:simple-array slot-data (2048)))
  (%pad2-2   0                                :type cl:fixnum)
  (%pad2-3   0                                :type cl:fixnum)
  (%pad2-4   0                                :type cl:fixnum)
  (%pad2-5   0                                :type cl:fixnum)
  (%pad2-6   0                                :type cl:fixnum)
  (%pad2-7   0                                :type cl:fixnum)
  (%pad2-8   0                                :type cl:fixnum)

  (next      (cl:error "must provide next")   :type (cl:or cl:null prq%))
  (closed    (cl:error "must provide closed") :type cl:boolean))
  
(cl:declaim
 (cl:inline make-prq% prq%-head prq%-tail prq%-closed prq%-slot-data prq%-next)
 (cl:ftype (cl:function (cl-word cl-word cl:boolean (cl:simple-array slot-data (2048)) (cl:or cl:null prq%))
                        prq%)
           make-prq%))

(cl:declaim (cl:type cl:fixnum +cache-line-size+ +slot-size+ +slots-per-cache-line+ +num-cache-lines+))
(cl:defconstant +cache-line-size+ 128)
(cl:defconstant +slot-size+ (cl:+ 8 8))
(cl:defconstant +slots-per-cache-line+ (cl:/ +cache-line-size+ +slot-size+))
(cl:defconstant +num-cache-lines+ (cl:/ (cl:* +prq-length+ +slot-size+) +cache-line-size+))

(cl:declaim (cl:inline to-slot-indices))
(cl:defun to-slot-indices (i)
  "Convert a head/tail ticket to an index on the array. Returns (metadata-index, value-index)."
  (cl:declare (cl:type cl-word i)
              (cl:values cl:fixnum cl:fixnum))
  (cl:let ((j (cl:mod i +prq-length+)))
    (cl:multiple-value-bind (quotient rem)
        (cl:floor j +num-cache-lines+)
      (cl:let* ((metadata-index (cl:* 2
                                      (cl:+ (cl:* rem +slots-per-cache-line+)
                                            quotient)))
                (value-index (cl:1+ metadata-index)))
        (cl:values metadata-index value-index)))))

;; NOTE: The idiomatic solution would be to define separate accessor and setf variants for
;; metadata and value. The problem is that, on SBCL, this *also* requires defining a
;; separate sb-ext:cas variant. Even worse, CCL does not support custom CAS targets at all.
;;
;; Just having place macros makes it difficult to annotate the type and use it as a CAS
;; target at the same time (the type annotation form confuses the CAS syntax).
(cl:defmacro place--prq%-slot-data (prq i)
  "Place for the metadata for the `i`th slot in `prq` (out of `+prq-length+`)."
  `(cl:svref (prq%-slot-data ,prq) ,i))

(cl:declaim (cl:inline prq%-slot-metadata))
(cl:defun prq%-slot-metadata (prq i)
  "Requires `i` to be a physical index on the array. Assumes `i` is a valid metadata index."
  (cl:declare (cl:type prq% prq)
              (cl:type cl:fixnum i)
              (cl:values slot-metadata))
  (place--prq%-slot-data prq i))

(cl:declaim (cl:inline prq%-slot-value))
(cl:defun prq%-slot-value (prq i)
  "Requires `i` to be a physical index on the array. Assumes `i` is a valid value index."
  (cl:declare (cl:type prq% prq)
              (cl:type cl:fixnum i)
              (cl:values cl:t))
  (place--prq%-slot-data prq i))

(coalton-toplevel
  (repr :native prq%)
  (define-type (PRQ :a)))

(cl:declaim (cl:inline new-prq))
(cl:defun new-prq ()
  (cl:declare (cl:values prq%))
  ;; Start all slot-data values as the initial value, +empty-token+. Then loop back and fill in the metadata.
  (cl:let ((slots (cl:make-array (cl:* 2 +prq-length+) :element-type 'slot-data :initial-element +empty-token+)))
    (cl:declare (cl:type (cl:simple-array slot-data (2048)) slots))

    (cl:dotimes (i +prq-length+)
      (cl:setf (cl:aref slots (cl:* i 2)) +new-metadata+)) 

    (make-prq% +prq-length+
               +prq-length+
               cl:nil
               slots
               cl:nil)))

(cl:declaim (cl:inline slot-cas-metadata))
(cl:defun slot-cas-metadata (prq i old new)
  "Requires `i` to be a physical index on the array."
  (cl:declare (cl:type prq% prq)
              (cl:type cl:fixnum i)
              (cl:type slot-metadata old new)
              (cl:values cl:boolean))
  (at:cas (place--prq%-slot-data prq i) old new))

(cl:declaim (cl:inline slot-cas-value))
(cl:defun slot-cas-value (prq i old new)
  "Requires `i` to be a physical index on the array."
  (cl:declare (cl:type prq% prq)
              (cl:type cl:fixnum i)
              (cl:values cl:boolean))
  (at:cas (place--prq%-slot-data prq i) old new))

(cl:declaim (cl:inline slot-set-value))
(cl:defun slot-set-value (prq i val)
  "Requires `i` to be a physical index on the array."
  (cl:declare (cl:type prq% prq)
              (cl:type cl:fixnum i)
              (cl:values))
  (cl:setf (place--prq%-slot-data prq i) val)
  (cl:values))

(cl:declaim (cl:inline prq-cas-close))
(cl:defun prq-cas-close (prq)
  "Atomically close a PRQ."
  (cl:declare (cl:type prq% prq)
              (cl:values cl:boolean))
  (at:cas (prq%-closed prq) cl:nil cl:t))

(cl:declaim (cl:inline prq-cas-next))
(cl:defun prq-cas-next (prq old new)
  (cl:declare (cl:type prq% prq)
              (cl:values cl:boolean))
  (at:cas (prq%-next prq) old new))

(cl:declaim (cl:inline prq-cas-next-empty))
(cl:defun prq-cas-next-empty (prq new)
  "Atomically set an empty `next` to `new`."
  (cl:declare (cl:type prq% prq)
              (cl:values cl:boolean))
  (at:cas (prq%-next prq) cl:nil new))

(cl:declaim (cl:inline atm-inc-tail))
(cl:defun atm-inc-tail (prq)
  "Atomically increment tail for `prq`. Return the old tail value."
  (cl:declare (cl:type prq% prq)
              (cl:values cl-word))
  (atc:atomic-incf-old (prq%-tail prq)))

(cl:declaim (cl:inline atm-inc-head))
(cl:defun atm-inc-head (prq)
  "Atomically increment head for `prq`. Return the old head value."
  (cl:declare (cl:type prq% prq)
              (cl:values cl-word))
  (atc:atomic-incf-old (prq%-head prq)))

(cl:declaim (cl:inline atm-catch-tail))
(cl:defun atm-catch-tail (prq)
  "Atomically catch `tail` up to `head` for `prq`."
  (cl:declare (cl:type prq% prq))
  (atc:atomic-max (prq%-tail prq) (prq%-head prq))
  (cl:values))

;;;
;;; Enqueue and Dequeue algorithms
;;; 

;; Prevent a livelock situation where consumers outrace producers.
;; See "Theorem 4.3. PRQ is obstruction-free." from the paper.
(cl:defconstant +prq-max-enqueue-attempts+ 512)

;; (cl:declaim (cl:inline try-close-segment))
;; (cl:defun try-close-segment (tail_ force?)
;;   (cl:declare (cl:type cl-word tail_)
;;               (cl:type cl:boolean force?)
;;               (cl:values cl:boolean))
;;   ;; atomic FAA at start of enqueue-prq-inner% increments by one but returns old
;;   (cl:let ((tmp-tail (1+ tail_)))
;;     (cl:if force?
           

(cl:declaim (cl:inline enqueue-prq-inner%))
(cl:defun enqueue-prq-inner% (rt-prx prq val ownership-token is-thread-fn)
  (cl:declare (cl:type prq% prq)
              (cl:type cl:function is-thread-fn)
              (cl:values cl:boolean))
  ;; CONCURRENT: Masking handled by top-level call on LPRQ
  (cl:let ((try-close 0)
           ;; bind slot-data pointer to prevent repeated trips to contested PRQ header
           (slot-data (prq%-slot-data prq)))
    (cl:declare (cl:type cl:fixnum try-close))
    
    (cl:loop
      (cl:let ((tail_ (atm-inc-tail prq)))
        (cl:multiple-value-bind (i-tail-metadata i-tail-value)
            (to-slot-indices tail_)

          (cl:when (prq%-closed prq)
            (cl:return cl:nil))

          (cl:let* ((cycle (cl:truncate tail_ +prq-length+))
                    (metadata (cl:svref slot-data i-tail-metadata))
                    (is-safe? (safe? metadata))
                    (epoch (epoch metadata))
                    (value (cl:svref slot-data i-tail-value)))

            (cl:when (cl:and (cl:< epoch cycle)                              ;; enqueue is not overtaken
                             (cl:or is-safe? (cl:<= (prq%-head prq) tail_))
                             (cl:or (cl:eq +empty-token+ value)              ;; and the slot is empty
                                    (cl:funcall is-thread-fn rt-prx value)))

              (cl:when ;; Lock the slot with the ownership token
                  (at:cas (cl:svref slot-data i-tail-value) value ownership-token)
                ;; Advance the epoch
                (cl:if (cl:not (at:cas  (cl:svref slot-data i-tail-metadata) metadata (pack cl:t cycle)))
                       ;; Another thread enqueued, so cleanup and try again
                       (at:cas (cl:svref slot-data i-tail-value) ownership-token +empty-token+)
                       ;; Publish item
                       (cl:when (at:cas (cl:svref slot-data i-tail-value) ownership-token val)
                         (cl:return cl:t)))))

            ;; Check overflow
            (cl:let* ((head_ (prq%-head prq))
                      (head+len #+sbcl
                                (sb-ext:truly-the cl-word
                                                  (cl:+ head_ +prq-length+))
                                #-sbcl
                                (cl:+ head_ +prq-length+)))
              (cl:when (cl:or (cl:>= tail_ head+len)                          ;; Is the queue full?
                              (cl:eql try-close +prq-max-enqueue-attempts+))  ;; defensive closure against livelock
                ;; TODO: Does this have to CAS? It's not in the paper
                (prq-cas-close prq)
                (cl:return cl:nil)))

            (cl:incf try-close)
            ))))))

(cl:declaim (cl:inline try-dequeue-prq-inner%))
(cl:defun try-dequeue-prq-inner% (rt-prx prq is-thread-fn)
  (cl:declare (cl:type prq% prq)
              (cl:type cl:function is-thread-fn)
              (cl:values (cl:or cl:null cl:t) cl:boolean))
  ;; bind slot-data pointer to prevent repeated trips to contested PRQ header
  (cl:let ((slot-data (prq%-slot-data prq)))
    (cl:loop
      ;; CONCURRENT: Masking handled by top-level call on LPRQ
      (cl:let* ((head_ (atm-inc-head prq))
                (i-head-metadata 0)
                (i-head-value 0)
                (cycle (cl:truncate head_ +prq-length+))
                ;; Don't need to read initially because will be re-read with r & 255 == 0
                ;; on the first iteration, if necessary
                (closed? cl:nil)
                (tail_ 0)

                (r 0))
        (cl:declare (cl:type cl-word tail_)
                    (cl:type cl:fixnum r))

        (cl:multiple-value-bind (i-head-metadata_ i-head-value_)
            (to-slot-indices head_)
          (cl:setf i-head-metadata i-head-metadata_
                   i-head-value i-head-value_))

        ;; Try to update the slot state
        (cl:loop
          (cl:tagbody
           start-iteration
             (cl:let* ((metadata (cl:svref slot-data i-head-metadata))
                       (safe? (safe? metadata))
                       (epoch (epoch metadata))
                       (value (cl:svref slot-data i-head-value)))

               (cl:when (cl:not (cl:eq metadata (cl:svref slot-data i-head-metadata)))
                 ;; Inconsistent view of slot
                 (cl:go next-iteration))

               (cl:let* ((empty? (cl:eq value +empty-token+))
                         (is-token? (cl:and (cl:not empty?)
                                            (cl:funcall is-thread-fn rt-prx value))))

                 (cl:cond
                   ((cl:and (cl:eq epoch cycle)
                            (cl:not empty?)
                            (cl:not is-token?))
                    ;; slot has not been overwritten and value is legitimate - dequeue transition
                    (cl:setf (cl:svref slot-data i-head-value) +empty-token+)
                    (cl:return-from try-dequeue-prq-inner%
                      (cl:values value cl:t)))
                   ((cl:and (cl:<= epoch cycle)
                            (cl:or empty?
                                   is-token?))
                    ;; wait optimization - see reference implementation
                    (cl:when (cl:zerop (cl:logand r 255))
                      (cl:setf tail_ (prq%-tail prq)
                               closed? (prq%-closed prq)))

                    (cl:when (cl:or (cl:not safe?)
                                    (cl:<= tail_ head_)
                                    closed?
                                    (cl:> r 4096))
                      ;; empty transition - unlock the cell
                      (cl:when (cl:and is-token?
                                       (cl:not (at:cas (cl:svref slot-data i-head-value) value +empty-token+)))
                        (cl:go next-iteration))
                      ;; advance the epoch
                      (cl:when (at:cas (cl:svref slot-data i-head-metadata) metadata (pack safe? cycle))
                        (cl:return)))
                    (cl:incf r)
                    )
                   ((cl:and (cl:< epoch cycle)
                            (cl:not empty?)
                            (cl:not is-token?))
                    ;; unsafe transition
                    (cl:when (at:cas (cl:svref slot-data i-head-metadata) metadata (pack cl:nil epoch))
                      (cl:return)))
                   (cl:t
                    ;; epoch > cycle
                    (cl:return-from try-dequeue-prq-inner%
                      (cl:values cl:nil cl:nil))))))
             ;; deq is overtaken
           next-iteration)) 

        ;; Is the queue empty?
        (cl:let ((head+1 #+sbcl (sb-ext:truly-the cl-word (cl:1+ head_))
                         #-sbcl (cl:1+ head_)))
          (cl:when (cl:<= (prq%-tail prq) head+1)
            ;; monotonically advance tail to the current head before returning
            ;; See footnote #2 of the paper. Prevents pathological behavior in
            ;; consumer >>> producer livelock case.
            (atm-catch-tail prq)
            (cl:return cl:nil))))
      )))

(coalton-toplevel
  (inline)
  (declare try-dequeue-prq% (Runtime :rt :t => Proxy :rt * PRQ :a -> Optional :a))
  (define (try-dequeue-prq% rt-prx prq)
    (let (values val found?) =
      (lisp (-> Anything * Boolean) (rt-prx prq is-thread?)
        (try-dequeue-prq-inner% rt-prx prq is-thread?)))
    (if found?
        (Some (from-anything val))
        None)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;        LPRQ Implementation        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:defstruct (lprq% (:constructor make-lprq% (head tail parkers)))
  (head    (cl:error "must provide head"))
  (%pad1-2   0                                :type cl:fixnum)
  (%pad1-3   0                                :type cl:fixnum)
  (%pad1-4   0                                :type cl:fixnum)
  (%pad1-5   0                                :type cl:fixnum)
  (%pad1-6   0                                :type cl:fixnum)
  (%pad1-7   0                                :type cl:fixnum)
  (%pad1-8   0                                :type cl:fixnum)

  (tail    (cl:error "must provide tail"))
  (%pad2-2   0                                :type cl:fixnum)
  (%pad2-3   0                                :type cl:fixnum)
  (%pad2-4   0                                :type cl:fixnum)
  (%pad2-5   0                                :type cl:fixnum)
  (%pad2-6   0                                :type cl:fixnum)
  (%pad2-7   0                                :type cl:fixnum)
  (%pad2-8   0                                :type cl:fixnum)

  (parkers (cl:error "must provide parkers")))

(cl:declaim (cl:inline lprq%-head lprq%-tail lprq%-parkers))

(coalton-toplevel
  (repr :native lprq%)
  (define-type (LPRQ :a))

  (inline)
  (declare new-lprq (Void -> LPRQ :a))
  (define (new-lprq)
    (let initial-prq =
      (lisp (-> PRQ :a) ()
        (new-prq)))
    (let parkers = (new-parking-set%))
    (lisp (-> LPRQ :a) (initial-prq parkers)
      (make-lprq% initial-prq initial-prq parkers))))

(cl:declaim (cl:inline lprq-cas-head))
(cl:defun lprq-cas-head (lprq old new)
  (cl:declare (cl:type lprq% lprq)
              (cl:values cl:boolean))
  (at:cas (lprq%-head lprq) old new))

(cl:declaim (cl:inline lprq-cas-tail))
(cl:defun lprq-cas-tail (lprq old new)
  (cl:declare (cl:type lprq% lprq)
              (cl:values cl:boolean))
  (at:cas (lprq%-tail lprq) old new))

(coalton-toplevel
  (inline)
  (declare lprq-parkers (LPRQ :a -> ParkingSet))
  (define (lprq-parkers lprq)
    (lisp (-> ParkingSet) (lprq)
      (lprq%-parkers lprq))))

(coalton-toplevel
  (inline)
  (declare notify-parker (LPRQ :a -> Void))
  (define (notify-parker lprq)
    "Notify parkers if necessary."
    (unpark-one-masked% (lprq-parkers lprq))))

(cl:declaim (cl:inline enqueue-inner%))
(cl:defun enqueue-inner% (rt-prx lprq elt notify-parker-fn ownership-token is-thread-fn)
  (cl:declare (cl:type lprq% lprq)
              (cl:type cl:t elt)
              (cl:type cl:function notify-parker-fn is-thread-fn))
  ;; CONCURRENT: masking handled by Coalton wrapper
  (cl:loop
    (cl:let ((prq (lprq%-tail lprq)))

      ;; fast-path: add to the current PRQ
      (cl:when (enqueue-prq-inner% rt-prx prq elt ownership-token is-thread-fn)
        (cl:funcall notify-parker-fn lprq)
        (cl:return-from enqueue-inner%))

      ;; slow-path: Tail is full, add new PRQ
      (cl:let ((new-tail (new-prq)))
        (enqueue-prq-inner% rt-prx new-tail elt ownership-token is-thread-fn)

        (cl:if (prq-cas-next-empty prq new-tail)
          (cl:progn
            (lprq-cas-tail lprq prq new-tail)
            (cl:funcall notify-parker-fn lprq)
            (cl:return-from enqueue-inner%))
          (cl:let ((next (prq%-next prq)))
            (lprq-cas-tail lprq prq next)))))))

(coalton-toplevel
  (declare enqueue% (Runtime :rt :t => Proxy :rt * LPRQ :a * :a -> Void))
  (define (enqueue% rt-prx lprq elt)
    ;; CONCURRENT:
    ;; For simplicity, conservatively masks the entire run of the function.
    
    ;; Take advantage of the fact that the ownership-token is the thread handle
    ;; to avoid looking it up two more times in mask and unmask
    (let ownership-token = (current-thread! rt-prx))
    (mask! rt-prx ownership-token)
    (lisp (-> :a) (rt-prx lprq elt notify-parker ownership-token is-thread?)
      (enqueue-inner% rt-prx lprq elt notify-parker ownership-token is-thread?))
    (unmask! rt-prx ownership-token)
    ))

(cl:declaim (cl:inline try-dequeue-inner%))
(cl:defun try-dequeue-inner% (rt-prx lprq is-thread-fn)
  ;; CONCURRENT: masking handled by Coalton wrapper
  (cl:declare (cl:type lprq% lprq)
              (cl:type cl:function is-thread-fn)
              (cl:values (cl:or cl:t cl:null) cl:boolean))
  (cl:loop
     (cl:let ((prq (lprq%-head lprq)))
       (cl:multiple-value-bind (res found?)
           (try-dequeue-prq-inner% rt-prx prq is-thread-fn)
         (cl:if found?
           (cl:return-from try-dequeue-inner% (cl:values res cl:t))
           ;; failed, is this queue empty?     
           (cl:let ((next (prq%-next prq)))
             (cl:if next
               ;; prq is closed but next may store elements
               (cl:multiple-value-bind (res found?)
                   (try-dequeue-prq-inner% rt-prx prq is-thread-fn)
                 (cl:if found?
                   (cl:return-from try-dequeue-inner% (cl:values res cl:t))
                   ;; prq is empty. Update head and restart.
                   (lprq-cas-head lprq prq next)))
               ;; prq is empty and no next
               (cl:return-from try-dequeue-inner% (cl:values cl:nil cl:nil)))))))))

(coalton-toplevel
  (inline)
  (declare try-dequeue-masked% (Runtime :rt :t => Proxy :rt * LPRQ :a -> Optional :a))
  (define (try-dequeue-masked% rt-prx lprq)
    ;; CONCURRENT:
    ;; Assumes masked
    (let (values val found?) =
      (lisp (-> Anything * Boolean) (rt-prx lprq is-thread?)
        (try-dequeue-inner% rt-prx lprq is-thread?)))
    (if found?
        (Some (from-anything val))
        None)))

(coalton-toplevel
  (inline)
  (declare try-dequeue% (Runtime :rt :t => Proxy :rt * LPRQ :a -> Optional :a))
  (define (try-dequeue% rt-prx lprq)
    ;; CONCURRENT:
    ;; For simplicity, conservatively masks the entire run of the function.
    (mask-current! rt-prx)
    (let (values val found?) =
      (lisp (-> Anything * Boolean) (rt-prx lprq is-thread?)
        (try-dequeue-inner% rt-prx lprq is-thread?)))
    (unmask-current! rt-prx)
    (if found?
        (Some (from-anything val))
        None)))

(cl:defconstant +spin-base+ 3)
(cl:defconstant +spin-attempts+ 5)

(cl:declaim (cl:inline lprq-tail-changed))
(cl:defun lprq-tail-changed (lprq tail-init prq-tail-init)
  (cl:declare (cl:type lprq% lprq)
              (cl:type prq% tail-init)
              (cl:type cl-word prq-tail-init)
              (cl:values cl:boolean))
  (cl:or (cl:not (cl:eq (lprq%-tail lprq) tail-init))
         (cl:not (cl:eq (prq%-tail tail-init) prq-tail-init))))

(coalton-toplevel
  (inline)
  (declare spin-until-tail-changed (LPRQ :a * PRQ :a * Word -> Boolean))
  (define (spin-until-tail-changed lprq tail-init prq-tail-init)
    "Returns True if the tail changed, false otherwise."
    (lisp (-> Boolean) (lprq tail-init prq-tail-init)
      (cl:let (#-sbcl (sink 0))
        (cl:loop :repeat +spin-attempts+
           :for x := +spin-base+ :then (cl:* x +spin-base+)
           :do
              (cl:loop :repeat x
                 :do
                   #+sbcl (sb-ext:spin-loop-hint)
                   #-sbcl (cl:setf sink x))
           :when (lprq-tail-changed lprq tail-init prq-tail-init)
             :return cl:t)))))
         
(coalton-toplevel
  (inline)
  (declare dequeue% (Runtime :rt :t => Proxy :rt * LPRQ :a -> :a))
  (define (dequeue% rt-prx lprq)
    ;; CONCURRENT:
    ;; For simplicity, masks around whole operation
    (mask-current! rt-prx)
    
    (let result =
      (match (try-dequeue-masked% rt-prx lprq)
        ((Some val)
         val)
        (_
         (rec % ((tail-init (lisp (-> PRQ :a) (lprq)
                              (lprq%-tail lprq)))
                 (prq-tail-init (lisp (-> Word) (tail-init)
                                  (prq%-tail tail-init))))
           (match (try-dequeue-masked% rt-prx lprq)
             ((Some val)
              val)
             ((None)
              (when (not (spin-until-tail-changed lprq tail-init prq-tail-init))
                (park-in-set-if-masked%
                 rt-prx
                 (fn ()
                   (not
                    (lisp (-> Boolean) (lprq tail-init prq-tail-init)
                      (lprq-tail-changed lprq tail-init prq-tail-init))))
                 (lprq-parkers lprq)))
              (let tail-init =
                (lisp (-> PRQ :a) (lprq)
                  (lprq%-tail lprq)))
              (%
               tail-init
               (lisp (-> Word) (tail-init)
                 (prq%-tail tail-init)))))))))

    (unmask-current! rt-prx)
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;            Public API             ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(coalton-toplevel
  (repr :transparent)
  (define-type (UnboundedMpmcQueue :a)
    "A synchronized FIFO queue to pass data between threads."
    (UnboundedMpmcQueue% (LPRQ :a)))

  (inline)
  (declare lprq% (UnboundedMpmcQueue :a -> LPRQ :a))
  (define (lprq% (UnboundedMpmcQueue% lprq))
    lprq)

  (inline)
  (declare new-unbounded-mpmc-queue (Threads :rt :t :m => :m (UnboundedMpmcQueue :a)))
  (define new-unbounded-mpmc-queue
    (wrap-io
     (UnboundedMpmcQueue%
      (new-lprq))))

  (define-instance (Queue UnboundedMpmcQueue)
    (inline)
    (define (enqueue elt queue &key (timeout NoTimeout))
      (wrap-io-with-runtime (rt-prx)
        (enqueue% rt-prx (lprq% queue) elt)
        Unit))
    (inline)
    (define (try-enqueue elt queue)
      (wrap-io-with-runtime (rt-prx)
        (enqueue% rt-prx (lprq% queue) elt)
        True))
    (inline)
    (define (dequeue queue &key (timeout NoTimeout))
      (wrap-io-with-runtime (rt-prx)
        (dequeue% rt-prx (lprq% queue))))
    (inline)
    (define (try-dequeue queue)
      (wrap-io-with-runtime (rt-prx)
        (try-dequeue% rt-prx (lprq% queue)))))
  )
