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

;; This is an implementation of the LPRQ Queue algorithm, described:
;; https://nikitakoval.org/publications/ppopp23-lprq.pdf

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

(coalton-toplevel
  (inline)
  (declare prq-len (Void -> UFix))
  (define (prq-len)
    (lisp (-> UFix) ()
      +prq-length+))

  (inline)
  (declare prq-len-word (Void -> Word))
  (define (prq-len-word)
    (lisp (-> Word) ()
      +prq-length+))

  (inline)
  (declare prq-index-mask (Void -> UFix))
  (define (prq-index-mask)
    (lisp (-> UFix) ()
      +prq-index-mask+)))

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

(coalton-toplevel
  (repr :transparent)
  (define-type SlotMetadata
    (SlotMetadata% Word))

  (inline)
  (declare bits (SlotMetadata -> Word))
  (define (bits (SlotMetadata% bits))
    bits))

(coalton-toplevel
  (inline)
  (declare pack (Boolean * Word -> SlotMetadata))
  (define (pack safe? epoch)
    "Pack `safe?` and `epoch` into metadata bits."
    (SlotMetadata%
     (b:or (b:shift 1 epoch)
           (if safe? 1 0)))))

(coalton-toplevel
  (inline)
  (declare safe? (SlotMetadata -> Boolean))
  (define (safe? metadata)
    "Is `metadata` safe? set to True?"
    (let bits = (bits metadata))
    (lisp (-> Boolean) (bits)
      (cl:logbitp 0 bits))))

(coalton-toplevel  
  (inline)
  (declare epoch (SlotMetadata -> Word))
  (define (epoch metadata)
    "Get the epoch value for `metadata`."
    (let bits = (bits metadata))
    (b:shift -1 bits)))

(cl:defconstant +new-metadata+
  (coalton 
   (pack True 0)))

;;   (inline)
;;   (declare increment-epoch! (SlotMetadata -> Word))
;;   (define (increment-epoch! metadata)
;;     "Increment the epoch in `metadata` and return the new epoch value."
;;     (let new-metadata = (at:atomic-inc (atm% metadata) 2))
;;     (b:shift -1 new-metadata))

;;   (inline)
;;   (declare read-metadata (SlotMetadata -> Word))
;;   (define (read-metadata metadata)
;;     "Atomically read the bits for `metadata`."
;;     (at:read-at-int (atm% metadata)))

;;;
;;; Empty Sentinel
;;; 

(cl:defstruct empty-sentinel)

(coalton-toplevel
  (inline)
  (define empty-token
    (lisp (-> Anything) ()
      (make-empty-sentinel)))

  (inline)
  (declare is-empty? (Anything -> Boolean))
  (define (is-empty? obj)
    (unsafe-pointer-eq? obj empty-token)))

(cl:defvar +empty-token+
  (coalton empty-token))
  
;;;
;;; Slots
;;; 

(cl:defstruct (slot% (:constructor make-slot% (metadata value)))
  (metadata 0      :type cl-word)
  (value    cl:nil :type cl:t))

(cl:declaim
 (cl:inline make-slot% slot%-metadata slot%-value)
 (cl:ftype (cl:function (cl-word cl:t) slot%)
           make-slot%))

(coalton-toplevel
  (repr :native slot%)
  (define-type (Slot :a))
  
  (inline)
  (declare new-slot (Void -> Slot :a))
  (define (new-slot)
    (lisp (-> (Slot :a)) ()
      (make-slot% +new-metadata+ +empty-token+)))
  )

(coalton-toplevel
  (inline)
  (declare slot-metadata (Slot :a -> SlotMetadata))
  (define (slot-metadata slot)
    (lisp (-> SlotMetadata) (slot)
      (slot%-metadata slot))))

(coalton-toplevel
  (inline)
  (declare slot-value (Slot :a -> Anything))
  (define (slot-value slot)
    (lisp (-> Anything) (slot)
      (slot%-value slot))))

(coalton-toplevel
  (inline)
  (declare slot-cas-value (Slot :a * Anything * Anything -> Boolean))
  (define (slot-cas-value slot old new)
    (lisp (-> Boolean) (slot old new)
      (at:cas (slot%-value slot) old new))))      

(coalton-toplevel
  (inline)
  (declare slot-cas-metadata (Slot :a * SlotMetadata * SlotMetadata -> Boolean))
  (define (slot-cas-metadata slot old new)
    (lisp (-> Boolean) (slot old new)
      (at:cas (slot%-metadata slot) old new))))      
      
;;;
;;; PRQ Structure
;;; 

(cl:defstruct (prq% (:constructor make-prq% (head tail closed slots next)))
  (head   (cl:error "must provide head")   :type cl-word)
  (tail   (cl:error "must provide tail")   :type cl-word)
  (closed (cl:error "must provide closed") :type cl:boolean)
  (slots  (cl:error "must provide slots")  :type (cl:simple-array slot% (1024)))
  (next   (cl:error "must provide next")   :type (cl:or cl:null prq%)))
  
(cl:declaim
 (cl:inline make-prq% prq%-head prq%-tail prq%-closed prq%-slots prq%-next)
 (cl:ftype (cl:function (cl-word cl-word cl:boolean (cl:simple-array slot% (1024)) (cl:or cl:null prq%))
                        prq%)
           make-prq%))

(coalton-toplevel
  (repr :native prq%)
  (define-type (PRQ :a))

  (inline)
  (declare new-prq (Void -> PRQ :a))
  (define (new-prq)
    (let slots = (la:make-uninitialized (prq-len)))
    (dotimes (i (prq-len))
      (la:set! slots i (new-slot)))
    (lisp (-> PRQ :a) (slots)
      (make-prq% +PRQ-LENGTH+
                 +PRQ-LENGTH+
                 cl:nil
                 slots
                 cl:nil))))

(coalton-toplevel
  (inline)
  (declare prq-head (PRQ :a -> Word))
  (define (prq-head prq)
    (lisp (-> Word) (prq)
      (prq%-head prq))))

(coalton-toplevel
  (inline)
  (declare prq-closed? (PRQ :a -> Boolean))
  (define (prq-closed? prq)
    (lisp (-> Boolean) (prq)
      (prq%-closed prq))))

(coalton-toplevel
  (inline)
  (declare prq-cas-close (PRQ :a -> Boolean))
  (define (prq-cas-close prq)
    "Atomically close a PRQ."
    (lisp (-> Boolean) (prq)
      (at:cas (prq%-closed prq) cl:nil cl:t))))

(coalton-toplevel
  (inline)
  (declare wd->index (Word -> UFix))
  (define (wd->index wd)
    "Calculate the slot-index for a machine word. Automatically does % +prq-length+."
    (lisp (-> UFix) (wd)
      (cl:logand wd +prq-index-mask+)))

  (inline)
  (declare prq-slot (PRQ :a * Word -> Slot :a))
  (define (prq-slot prq i)
    (let i = (wd->index i))
    (lisp (-> Slot :a) (prq i)
      (cl:aref (prq%-slots prq) i))))

(coalton-toplevel
  (inline)
  (declare atm-inc-tail (PRQ :a -> Word))
  (define (atm-inc-tail prq)
    "Atomically increment tail for `prq`. Return the old tail value."
    (lisp (-> Word) (prq)
      (atc:atomic-incf-old (prq%-tail prq)))))

(coalton-toplevel
  (inline)
  (declare atm-inc-head (PRQ :a -> Word))
  (define (atm-inc-head prq)
    "Atomically increment head for `prq`. Return the old head value."
    (lisp (-> Word) (prq)
      (atc:atomic-incf-old (prq%-head prq)))))

;;;
;;; Enqueue and Dequeue algorithms
;;; 

(coalton-toplevel
  ;; Prevent a livelock situation where consumers outrace producers.
  ;; See "Theorem 4.3. PRQ is obstruction-free." from the paper.
  (inline)
  (define (+PRQ-MAX-ENQUEUE-ATTEMPTS+) (the UFix 32))
  )

(coalton-toplevel
  (declare enqueue-prq% (Runtime :rt :t => Proxy :rt * PRQ :a * :a &key (:attempts UFix) -> Boolean))
  (define (enqueue-prq% rt-prx prq val &key (attempts 1))
    (let t = (atm-inc-tail prq))

    (when (prq-closed? prq)
      (return False))
    
    (let cycle = (i:div t (prq-len-word)))
    (let slot = (prq-slot prq t))
    (let old-metadata = (slot-metadata slot))
    (let is-safe? = (safe? old-metadata))
    (let old-epoch = (epoch old-metadata))
    (let old-value = (slot-value slot))
    (let h = (prq-head prq))

    (when (and (< old-epoch cycle)                ;; enqueue is not overtaken
               (or is-safe? (<= h t))
               (or (is-empty? old-value)          ;; and the slot is empty
                   (is-thread? rt-prx old-value)))
      (let ownership-token = (to-anything (current-thread! rt-prx)))
      (when ;; Lock the slot with the ownership token
            (slot-cas-value slot old-value ownership-token)
        ;; Advance the epoch
        (if (not (slot-cas-metadata slot old-metadata (pack True cycle)))
            ;; Another thread enqueued, so clean up and try again
            (progn
              (slot-cas-value slot ownership-token empty-token)
              (values))
            ;; Publish item
            (when (slot-cas-value slot ownership-token (to-anything val))
              (return True)))))
    
    ;; Check overflow
    (let h = (prq-head prq))
    (if (or (and (>= t h)
                 (>= (- t h) (prq-len-word)))           ;; Is the queue full?
            (== attempts (+PRQ-MAX-ENQUEUE-ATTEMPTS+))) ;; defensive closure against livelock
        (progn
          (prq-cas-close prq)
          False)
        (enqueue-prq% rt-prx prq val)))
  )

;; (cl:disassemble enqueue-prq%)
;;     ;; CONCURRENT: Masking handled by top-level call on LPRQ
;;     (let t = (at:atomic-inc1-old (.tail prq)))

;;     (when (at:read (.closed prq))
;;       (return False))

;;     (let (values cycle i) = (i:divmod t +PRQ-LENGTH+))
;;     (let slot = (la:aref (.slots prq) (w->uf i)))
;;     (let old-metadata = (read-metadata (.metadata slot)))
;;     (let (values safe? epoch) = (unpack old-metadata))
;;     (let old-value = (at:read (.value slot)))
;;     (let h = (at:read-at-int (.head prq)))

;;     (when (and (or (is-empty? old-value)          ;; the slot is empty
;;                    (is-thread? rt-prx old-value))
;;                (< epoch cycle)                    ;; and enqueue is not overtaken
;;                (or safe? (<= h t)))
;;       (let ownership-token = (to-anything (current-thread! rt-prx)))
;;       (when ;; Lock the cell with the ownership token
;;             (at:compare-and-swap (.value slot) old-value ownership-token)
;;         ;; Advance the epoch
;;         (if (not (at:int-cas (atm% (.metadata slot))
;;                              old-metadata 
;;                              (pack True cycle)))
;;             ;; Another thread enqueued, so clean up and try again
;;             (progn
;;               (at:compare-and-swap (.value slot) ownership-token empty-token)
;;               (values))
;;             ;; Publish item
;;             (when (at:compare-and-swap (.value slot) ownership-token (to-anything val))
;;               (return True)))))

;;     ;; Check overflow
;;     (let h = (at:read-at-int (.head prq)))
;;     (if (or (and (>= t h)
;;                  (>= (- t h) +PRQ-LENGTH+))           ;; is the queue full?
;;             (== attempts +PRQ-MAX-ENQUEUE-ATTEMPTS+)) ;; defensive closure against livelock
;;         (progn
;;           (at:atomic-write (.closed prq) True)
;;           False)
;;         (enqueue-prq% rt-prx prq val :attempts (1+ attempts))))
;;   )

;; (coalton-toplevel
;;   (declare try-dequeue-prq% (Runtime :rt :t => Proxy :rt * PRQ :a -> Optional :a))
;;   (define (try-dequeue-prq% rt-prx prq)
;;     ;; CONCURRENT: Masking handled by top-level call on LPRQ
;;     (let h = (at:atomic-inc1-old (.head prq)))
;;     (let (values cycle i) = (i:divmod h +PRQ-LENGTH+))

;;     ;; Try to update the slot state
;;     (for ()
;;       (let slot = (la:aref (.slots prq) (w->uf i)))
;;       (let metadata = (read-metadata (.metadata slot)))
;;       (let (values safe? epoch) = (unpack metadata))
;;       (let value = (at:read (.value slot)))

;;       (when (/= metadata (read-metadata (.metadata slot)))
;;         ;; Inconsistent view of the slot
;;         (continue))

;;       (let val-empty? = (is-empty? value))
;;       (let val-is-token? = (is-thread? rt-prx value))

;;       (cond
;;         ((and (== epoch cycle)
;;               (not val-empty?)
;;               (not val-is-token?))
;;          ;; slot has not been overwritten and value is legitimate - dequeue transition
;;          (at:atomic-write (.value slot) empty-token) 
;;          (return (Some (from-anything value))))
;;         ((and (<= epoch cycle)
;;               (or val-empty?
;;                   val-is-token?))
;;          ;; empty transition
;;          ;; unlock the cell
;;          (when (and val-is-token?
;;                     (not (at:compare-and-swap (.value slot) value empty-token)))
;;            (continue))
;;          ;; advance the epoch
;;          (when (at:int-cas (atm% (.metadata slot)) metadata (pack safe? cycle))
;;            (break)))
;;         ((and (< epoch cycle)
;;               (not val-empty?)
;;               (not val-is-token?))
;;          ;; unsafe transition
;;          (when (at:int-cas (atm% (.metadata slot)) metadata (pack False epoch))
;;            (break)))
;;         (True
;;          ;; epoch > cycle
;;          (break)))) ;; deq is qvertaken

;;     ;; Is the queue empty?
;;     (let t = (at:read-at-int (.tail prq)))
;;     (if (<= t (1+ h))
;;         ;; monotonically advance tail to the current head before returning
;;         ;; See footnote #2 on page 18 of the paper. Prevents pathological
;;         ;; behavior in the consumer >>> producer livelock case.
;;         (progn
;;           (at:atomic-max (.tail prq) (at:read-at-int (.head prq)))
;;           None)
;;         (try-dequeue-prq% rt-prx prq))
;;     )
;;   )

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;        LPRQ Implementation        ;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (coalton-toplevel

;;   (define-struct (LPRQ :a)
;;     (head (at:Atomic (PRQ :a)))
;;     (tail (at:Atomic (PRQ :a)))
;;     (parkers ParkingSet))

;;   (inline)
;;   (declare new-lprq (Void -> LPRQ :a))
;;   (define (new-lprq)
;;     (let initial-prq = (new-prq))
;;     (LPRQ
;;      (at:new initial-prq)
;;      (at:new initial-prq)
;;      (new-parking-set%)))

;;   (inline)
;;   (declare notify-parker (Runtime :rt :t => Proxy :rt * LPRQ :a -> Void))
;;   (define (notify-parker rt-prx lprq)
;;     "Notify parkers if necessary."
;;     (when (not (parking-set-empty?% (.parkers lprq)))
;;       (unpark-one% (.parkers lprq) rt-prx))) 

;;   (inline)
;;   (declare enqueue% (Runtime :rt :t => Proxy :rt * LPRQ :a * :a -> Void))
;;   (define (enqueue% rt-prx lprq elt)
;;     ;; CONCURRENT:
;;     ;; For simplicity, conservatively masks the entire run of the function.
;;     (mask-current! rt-prx)
;;     (rec % ()
;;       (let prq = (at:read (.tail lprq)))

;;       ;; fast-path: add to the current PRQ
;;       (when (enqueue-prq% rt-prx prq elt)
;;         (notify-parker rt-prx lprq)
;;         (unmask-current! rt-prx)
;;         (return))

;;       ;; slow-path: Tail is full, add new PRQ
;;       (let new-tail = (new-prq))
;;       (enqueue-prq% rt-prx new-tail elt)
;;       (if (at:compare-and-swap (.next prq) None (Some new-tail))
;;           (progn
;;             (at:compare-and-swap (.tail lprq) prq new-tail)
;;             (notify-parker rt-prx lprq)
;;             (unmask-current! rt-prx)
;;             (values))
;;           (progn
;;             (let next = (at:read (.next prq)))
;;             (match next
;;               ((Some next)
;;                (at:compare-and-swap (.tail lprq) prq next))
;;               (_
;;                False))
;;             (%)))))

;;   (inline)
;;   (declare try-dequeue% (Runtime :rt :t => Proxy :rt * LPRQ :a -> Optional :a))
;;   (define (try-dequeue% rt-prx lprq)
;;     ;; CONCURRENT:
;;     ;; For simplicity, conservatively masks the entire run of the function.
;;     (mask-current! rt-prx)

;;     (let result =
;;       (rec % ()
;;         (let prq = (at:read (.head lprq)))
;;         (let res = (try-dequeue-prq% rt-prx prq))

;;         (match res
;;           ((Some _)
;;            res)
;;           ((None)
;;            ;; failed, is this queue empty?
;;            (match (at:read (.next prq))
;;              ((None)
;;               None)
;;              ;; prq is closed but may store elements
;;              ((Some next)
;;               (let res = (try-dequeue-prq% rt-prx prq))
;;               (match res
;;                 ((Some _)
;;                  res)
;;                 ((None)
;;                  ;; prq is empty. Update head and restart.
;;                  (at:compare-and-swap (.head lprq) prq next)
;;                  (%)))))))))

;;     (unmask-current! rt-prx)

;;     result)

;;   (declare dequeue% (Runtime :rt :t => Proxy :rt * LPRQ :a -> :a))
;;   (define (dequeue% rt-prx lprq)
;;     ;; CONCURRENT:
;;     ;; Doesn't need to mask because try-dequeue% properly masks around LPRQ operations.
;;     (match (try-dequeue% rt-prx lprq)
;;       ((Some val)
;;        (return val))
;;       ((None)
;;        Unit))

;;     (let result = (c:new None))

;;     (park-in-set-if%
;;      rt-prx
;;      (fn ()
;;        (match (try-dequeue% rt-prx lprq)
;;          ((Some val)
;;           (c:write! result (Some val))
;;           False)
;;          ((None)
;;           True)))
;;      (.parkers lprq))

;;     (match (c:read result)
;;       ((Some val)
;;        val)
;;       ((None)
;;        (dequeue% rt-prx lprq))))
;;   )


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;            Public API             ;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (coalton-toplevel
;;   (repr :transparent)
;;   (define-type (UnboundedMpmcQueue :a)
;;     "A synchronized FIFO queue to pass data between threads."
;;     (UnboundedMpmcQueue% (LPRQ :a)))

;;   (inline)
;;   (declare lprq% (UnboundedMpmcQueue :a -> LPRQ :a))
;;   (define (lprq% (UnboundedMpmcQueue% lprq))
;;     lprq)

;;   (inline)
;;   (declare new-unbounded-mpmc-queue (Threads :rt :t :m => :m (UnboundedMpmcQueue :a)))
;;   (define new-unbounded-mpmc-queue
;;     (wrap-io
;;      (UnboundedMpmcQueue%
;;       (new-lprq))))

;;   (define-instance (Queue UnboundedMpmcQueue)
;;     (inline)
;;     (define (enqueue elt queue &key (timeout NoTimeout))
;;       (wrap-io-with-runtime (rt-prx)
;;         (enqueue% rt-prx (lprq% queue) elt)
;;         Unit))
;;     (inline)
;;     (define (try-enqueue elt queue)
;;       (wrap-io-with-runtime (rt-prx)
;;         (enqueue% rt-prx (lprq% queue) elt)
;;         True))
;;     (inline)
;;     (define (dequeue queue &key (timeout NoTimeout))
;;       (wrap-io-with-runtime (rt-prx)
;;         (dequeue% rt-prx (lprq% queue))))
;;     (inline)
;;     (define (try-dequeue queue)
;;       (wrap-io-with-runtime (rt-prx)
;;         (try-dequeue% rt-prx (lprq% queue)))))
;;   )
