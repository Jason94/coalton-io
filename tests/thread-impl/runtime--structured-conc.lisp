(defpackage :coalton-io/tests/thread-impl/runtime/structured-conc
  (:use #:coalton #:coalton-prelude #:coalton-testing
   #:io/monad-io
   #:io/utils
   #:io/simple-io
   #:io/threads
   #:io/mutable-var
   #:io/conc/mvar
   #:io/tests/utils
   )
  (:local-nicknames
   (:c #:coalton-library/cell)
   (:opt #:coalton-library/optional)
   (:tm #:io/terminal))
  )
(in-package :coalton-io/tests/thread-impl/runtime/structured-conc)

(named-readtables:in-readtable coalton:coalton)

(fiasco:define-test-package #:coalton-io/tests/thread-impl/runtime/structured-conc-fiasco)
(coalton-fiasco-init #:coalton-io/tests/thread-impl/runtime/structured-conc-fiasco)

(define-test test-threads-dont-outlive-run-io ()
  ;; NOTE: Normally you wouldn't interleave run-io! calls like this, but that's what
  ;; we need to test here, so it must be done.
  (let thread-finished? = (c:new False))
  (let (Tuple thread gate) =
    (run-io!
     (do
      (gate <- s-new)
      (thread <-
        (do-fork-thread-with_ (ForkStrategy LogAndSwallow Detached)
          (s-await gate)
          (wrap-io (c:write! thread-finished? True))))
      (pure (Tuple thread gate)))))
  (run-io! (s-signal gate))
  (run-io! (await thread))
  (is (== False (c:read thread-finished?))))

(define-test test-detached-thread-outlives-parent ()
  (let result =
    (run-io!
     (do
      (s-child-ending <- s-new)
      (result <- (new-var False))
      (do-fork-thread_
        (parent <- current-thread)
        (do-fork-thread-with_ (ForkStrategy LogAndSwallow Detached)
          ;; Wait for the parent to end
          (await parent)
          ;; Signal the child completed successfully
          (write result True)
          (s-signal s-child-ending)))
      (s-await s-child-ending)
      (read result))))
  (is (== True result)))

(define-test test-nested-run-io-doesnt-rebind-global-thread-or-stop-threads ()
  (let result =
    (run-io!
     (do
      (s-child-ending <- s-new)
      (result <- (new-var False))
      (wrap-io
       (run-io!
        (do
         (do-fork-thread_
           (parent <- current-thread)
           (do-fork-thread-with_ (ForkStrategy LogAndSwallow Detached)
             ;; Wait for the parent to end
             (await parent)
             ;; Signal the child completed successfully
             (write result True)
             (s-signal s-child-ending))))))
      (sleep 5)
      (s-await s-child-ending)
      (read result))))
  (is (== True result)))

(define-test test-structured-thread-stops-with-parent ()
  (let result =
    (run-io!
     (do
      (result <- (new-var False))
      (child-thread-var <- (new-var None))
      (parent-thread <-
        (do-fork-thread_
          (parent <- current-thread)
          (child-thread <-
            (do-fork-thread-with_ (ForkStrategy LogAndSwallow Structured)
              ;; Wait for the parent to end - this should never complete!
              (await parent)
              ;; Signal the child completed successfully
              (write result True)))
          (write child-thread-var (Some child-thread))))
      (await parent-thread)
      (child-thread <- (map (opt:from-some "Error: parent thread should have written to child-thread-var")
                            (read child-thread-var)))
      (await child-thread)
      (read result))))
  (is (== False result)))
