(defpackage :coalton-io/tests/thread-async-boundary
  (:use #:coalton #:coalton-prelude #:coalton-testing
        #:io/monad-io
        #:io/utils
        #:io/simple-io
        #:io/thread
        #:io/mut
        #:io/conc/mvar)
  (:import-from #:io/io-impl/simple-io
    #:run-io-no-cleanup!)
  (:import-from #:io/thread-impl/runtime
    #:write-line-sync%)
  (:local-nicknames
   (:c #:coalton-library/cell)
   (:s #:coalton-threads/semaphore)
   (:lk #:coalton-threads/lock))
  )
(in-package :coalton-io/tests/thread-async-boundary)

(named-readtables:in-readtable coalton:coalton)

(fiasco:define-test-package #:coalton-io/tests/thread-async-boundary-fiasco)
(coalton-fiasco-init #:coalton-io/tests/thread-async-boundary-fiasco)

(coalton-toplevel
  (define (run-test)
    ;; Because simple-io::>>= takes 5 extra MS now, we're going to do everything
    ;; outside of the monad.

    (let gate = (s:new))
    (let result = (c:new 0))

    (let thread =
      (run-io-no-cleanup!
       ;; This should wait 5 MS between the signal and write!
       (fork-thread_ ((noinline >>=)
                      (wrap-io
                       (s:signal gate 1)
                       Unit)
                      (fn (_)
                        (wrap-io
                         (c:write! result 100)))))))

    ;; Wait until the thread is running, wait 2 MS, kill it, wait 8 MS, then read.
    (s:await gate)
    (lisp Void ()
      (cl:sleep (cl:/ 2.0 1000)))
    (run-io! (stop-thread thread))
    (lisp Void ()
      (cl:sleep (cl:/ 80.0 1000)))

    (c:read result))
  )

;; If this test is failing, make sure you're exceuting the tests from the
;; repository's root directory. If you're in Slime/Sly, use ,cd at the REPL.
(define-test test-stop-outside-wrap-io ()
  ;; See simple-io.lisp
  (lisp Void ()
    (cl:setf (uiop:getenv "SIMPLE_IO_DEBUG_SLEEP") "y")
    (cl:compile-file "src/io-impl/simple-io.lisp"
                     :output-file "tests/simple-io---sleep.fasl"
                     :verbose cl:nil
                     :print cl:nil)
    (cl:load "tests/simple-io---sleep.fasl")
    (cl:compile-file "tests/thread-async-boundary.lisp"
                     :output-file "tests/thread-async-boundary---sleep.fasl"
                     :verbose cl:nil
                     :print cl:nil)
    (cl:load "tests/thread-async-boundary---sleep.fasl")
    )

  (let result = (run-test))

  (lisp Void ()
    (cl:setf (uiop:getenv "SIMPLE_IO_DEBUG_SLEEP") "")
    (cl:compile-file "src/io-impl/simple-io.lisp"
                     :output-file "tests/simple-io---sleep.fasl"
                     :verbose cl:nil
                     :print cl:nil)
    (cl:load "tests/simple-io---sleep.fasl"))

  (is (== result 0)))
