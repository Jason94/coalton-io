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
   )
  (:import-from #:coalton/experimental/loops
   #:dotimes)
  (:local-nicknames
   (:b #:coalton-library/bits)
   (:i #:coalton/math/integral)
   (:c #:coalton/cell)
   (:la #:coalton/lisparray)
   (:at #:io/threads-impl/atomics)
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

(coalton-toplevel
  (inline)
  (declare w->uf (Word -> UFix))
  (define (w->uf x)
    (lisp (-> UFix) (x)
      x))
  
  (define +PRQ-LENGTH+
    "The paper recommends a PRQ length of 1024 based on empirical results. Regardless,
must be a power of 2 so the compiler can optimize the integer div operations."
    (the Word 1024))
  (define +PRQ-LENGTH-UFIX+
    (the UFix 1024)))

(coalton-toplevel

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

  (repr :transparent)
  (define-type SlotMetadata
    (SlotMetadata% at:AtomicInteger))

  (inline)
  (declare new-metadata (Void -> SlotMetadata))
  (define (new-metadata)
    (SlotMetadata% (at:new-at-int 0)))

  (inline)
  (declare atm% (SlotMetadata -> at:AtomicInteger))
  (define (atm% (SlotMetadata% word))
    word)

  (inline)
  (declare increment-epoch! (SlotMetadata -> Word))
  (define (increment-epoch! metadata)
    "Increment the epoch in `metadata` and return the new epoch value."
    (let new-metadata = (at:atomic-inc (atm% metadata) 2))
    (b:shift -1 new-metadata))

  (inline)
  (declare read-metadata (SlotMetadata -> Word))
  (define (read-metadata metadata)
    "Atomically read the bits for `metadata`."
    (at:read-at-int (atm% metadata)))
  
  (inline)
  (declare unpack (Word -> Boolean * Word))
  (define (unpack bits)
    "Unpack values in `bits`."
    (let safe? =
      (lisp (-> Boolean) (bits)
        (cl:logbitp 0 bits)))
    (let epoch = (b:shift -1 bits))
    (values safe? epoch))

  (inline)
  (declare pack (Boolean * Word -> Word))
  (define (pack safe? epoch)
    "Pack `safe?` and `epoch` into metadata bits."
    (b:or (b:shift 1 epoch)
          (if safe? 1 0)))

  ;;;
  ;;; PRQ Structure
  ;;; 

  (define-struct (Slot :a)
    (metadata SlotMetadata)
    ;; Can be cl:nil, the current thread token, or a value of type :a
    (value (at:Atomic Anything)))

  (inline)
  (declare new-slot (Void -> Slot :a))
  (define (new-slot)
    (Slot
     (new-metadata)
     (at:new (to-anything None))))

  (define-struct (PRQ :a)
    (head at:AtomicInteger)
    (tail at:AtomicInteger)
    (closed (c:Cell Boolean))
    (slots (la:LispArray (Slot :a)))
    (next (at:Atomic (Optional (PRQ :a)))))

  (inline)
  (declare new-prq (Void -> PRQ :a))
  (define (new-prq)
    (let slots = (la:make-uninitialized (unwrap-as UFix +PRQ-LENGTH+)))
    (dotimes (i (unwrap-as UFix +PRQ-LENGTH+))
      (la:set! slots i (new-slot)))
    (PRQ
     (at:new-at-int +PRQ-LENGTH+)
     (at:new-at-int +PRQ-LENGTH+)
     (c:new False)
     slots
     (at:new None))))

(coalton-toplevel
  (declare enqueue-prq% (Runtime :rt :t => Proxy :rt * PRQ :a * :a -> Boolean))
  (define (enqueue-prq% rt-prx prq val)
    ;; CONCURRENT: Only needs to mask while the thread has ownership over the slot.
    ;; This happens when it CASes the thread token into the (.value) for the slot,
    ;; so masking needs to occur immediately prior. Then, the thread can unmask
    ;; either (A) right before returning or (B) it realizes it's been beaten to
    ;; the slot.
    (let t = (at:atomic-inc1-old (.tail prq)))

    (when (c:read (.closed prq))
      (return False))

    (let (values cycle i) = (i:divmod t +PRQ-LENGTH+))
    (let slot = (la:aref (.slots prq) (w->uf i)))
    (let old-metadata = (read-metadata (.metadata slot)))
    (let (values safe? epoch) = (unpack old-metadata))
    (let old-value = (at:read (.value slot)))
    (let h = (at:read-at-int (.head prq)))

    (when (and (anything-nil? old-value)  ;; the slot is empty
               (< epoch cycle)            ;; and enqueue is not overtaken
               (or safe? (< h t)))
      (let thread-token = (to-anything (current-thread! rt-prx)))
      (mask-current! rt-prx)
      (when ;; Lock the cell with the current thread token
            (at:compare-and-swap (.value slot) old-value thread-token)
        ;; Advance the epoch
        (if (not (at:int-cas (atm% (.metadata slot))
                             old-metadata 
                             (pack True cycle)))
            ;; Another thread enqueued, so clean up and try again
            (progn
              (at:compare-and-swap (.value slot) thread-token cl-nil)
              (values))
            ;; Publish item
            (when (at:compare-and-swap (.value slot) thread-token (to-anything val))
              (unmask-current! rt-prx) ;; (A) about to return, unmask
              (return True)))))
    (unmask-current! rt-prx) ;; (B) thread has been beaten to the slot, unmask
       
    ;; Check overflow
    (if (>= (- t h) +PRQ-LENGTH+) ;; is the queue full?
        (progn
          (c:write! (.closed prq) True)
          False)
        (enqueue-prq% rt-prx prq val)))
  )

(coalton-toplevel
  (inline)
  (declare thread-token? (Anything -> Boolean))
  (define (thread-token? val)
    "Check in a slot value is a thread token."
    (lisp (-> Boolean) (val)
      (bt2:threadp val)))
  
  (declare try-dequeue-prq% (PRQ :a -> Optional :a))
  (define (try-dequeue-prq% prq)
    (let h = (at:atomic-inc1-old (.head prq)))
    (let (values cycle i) = (i:divmod h +PRQ-LENGTH+))

    ;; Try to update the slot state
    (for ()
      (let slot = (la:aref (.slots prq) (w->uf i)))
      (let metadata = (read-metadata (.metadata slot)))
      (let (values safe? epoch) = (unpack metadata))
      (let value = (at:read (.value slot)))

      (when (/= metadata (read-metadata (.metadata slot)))
        ;; Inconsistent view of the slot
        (continue))

      (let val-nil? = (anything-nil? value))
      (let val-thread-token? = (thread-token? value))

      (cond
        ((and (== epoch cycle)
              (not val-nil?)
              (not val-thread-token?))
         ;; slot has not been overwritten and value is legitimate - dequeue transition
         (at:atomic-write (.value slot) cl-nil) 
         (return (Some (from-anything value))))
        ((and (<= epoch cycle)
              (or val-nil?
                  val-thread-token?))
         ;; empty transition
         ;; unlock the cell
         (when (and val-thread-token?
                    (at:compare-and-swap (.value slot) value cl-nil))
           (continue))
         ;; advance the epoch
         (when (at:int-cas (atm% (.metadata slot)) metadata (pack safe? cycle))
           (break)))
        ((and (< epoch cycle)
              (not val-nil?)
              (not val-thread-token?))
         ;; unsafe transition
         (when (at:int-cas (atm% (.metadata slot)) metadata (pack False epoch))
           (break)))
        (True
         ;; epoch > cycle
         (break)))) ;; deq is qvertaken

    ;; Is the queue empty?
    (let t = (at:read-at-int (.tail prq)))
    (if (<= t (1+ h))
        None
        (try-dequeue-prq% prq))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;        LPRQ Implementation        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(coalton-toplevel

  (define-struct (LPRQ :a)
    (head (at:Atomic (PRQ :a)))
    (tail (at:Atomic (PRQ :a))))

  (inline)
  (declare new-lprq (Void -> LPRQ :a))
  (define (new-lprq)
    (let initial-prq = (new-prq))
    (LPRQ
     (at:new initial-prq)
     (at:new initial-prq)))

  (inline)
  (declare enqueue% (Runtime :rt :t => Proxy :rt * LPRQ :a * :a -> Void))
  (define (enqueue% rt-prx lprq elt)
    (let prq = (at:read (.tail lprq)))

    ;; fast-path: add to the current PRQ
    (when (enqueue-prq% rt-prx prq elt)
      (return))

    ;; slow-path: Tail is full, add new PRQ
    (let new-tail = (new-prq))
    (enqueue-prq% rt-prx prq elt)
    (if (at:compare-and-swap (.next prq) None (Some prq))
        (progn
          (at:compare-and-swap (.tail lprq) prq new-tail)
          (values))
        (progn
          (let next = (at:read (.next prq)))
          (match next
            ((Some next)
             (at:compare-and-swap (.tail lprq) prq next))
            (_
             False))
          (enqueue% rt-prx lprq elt))))

  (inline)
  (declare try-dequeue% (Runtime :rt :t => Proxy :rt * LPRQ :a -> Optional :a))
  (define (try-dequeue% rt-prx lprq)
    (let prq = (at:read (.head lprq)))
    (let res = (try-dequeue-prq% prq))

    (match res
      ((Some _)
       res)
      ((None)
       ;; failed, is this queue empty?
       (match (at:read (.next prq))
         ((None)
          None)
         ;; prq is closed but may store elements
         ((Some next)
          (let res = (try-dequeue-prq% prq))
          (match res
            ((Some _)
             res)
            ((None)
             ;; prq is empty. Update head and restart.
             (at:compare-and-swap (.head lprq) prq next)
             (try-dequeue% rt-prx lprq))))))))
    )


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
    (define (dequeue _ &key (timeout NoTimeout))
      (error "dequeue not implemented"))
    (inline)
    (define (try-dequeue queue)
      (wrap-io-with-runtime (rt-prx)
        (try-dequeue% rt-prx (lprq% queue)))))
  )
