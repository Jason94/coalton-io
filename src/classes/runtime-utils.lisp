;; TODO: Find a better folder to put this package in...
(cl:in-package :cl-user)
(defpackage :io/classes/runtime-utils
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/types
   ;; #:io/utils
   #:io/thread-exceptions
   #:io/classes/monad-io-thread
   )
  (:local-nicknames
   (:lk #:coalton-threads/lock)
   (:cv #:coalton-threads/condition-variable)
   )
  (:export
   #:unmask-and-await-safely%
   #:unmask-and-await-safely-finally%
   ))
(in-package :io/classes/runtime-utils)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  ;; TODO: Standardize usage of 'finally' throughout the library.
  ;; There's two separate concepts: (1) Call a cleanup function only on failure,
  ;; and (2) guarantee a cleanup function is called regardless of failure or success.
  ;; "Finally" should cleanly refer to one, and another word should refer to the other.
  ;; TODO: Remove lambda when this is fixed:
  ;; https://github.com/coalton-lang/coalton/issues/1719
  (inline)
  (declare unmask-and-await-safely-finally% (Runtime :rt :t
                                             => Proxy :rt
                                             -> cv:ConditionVariable
                                             -> lk:Lock
                                             -> (Unit -> Unit)
                                             -> Unit))
  (define (unmask-and-await-safely-finally% rt-prx cv lock finally)
    "Unmask the thread. Finally, either await (still running) the CV
or just release the LOCK. Masks after resuming post-await. FINALLY
is run AFTER the lock is released, and only if the thread is stopped!!"
    (let f =
      (fn ()
        (unmask! rt-prx (current-thread! rt-prx))
        (cv:await cv lock)
        (mask-current! rt-prx)))
    (catch (inline (f))
      ((InterruptCurrentThread msg)
       (lk:release lock)
       (finally)
       (throw (InterruptCurrentThread msg)))))

  ;; TODO: Remove lambda when this is fixed:
  ;; https://github.com/coalton-lang/coalton/issues/1719
  (inline)
  (declare unmask-and-await-safely% (Runtime :rt :t => Proxy :rt -> cv:ConditionVariable -> lk:Lock -> Unit))
  (define (unmask-and-await-safely% rt-prx cv lock)
    "Unmask the thread. Finally, either await (still running) the CV
or just release the LOCK. Masks after resuming post-await."
    (let f =
      (fn ()
        (unmask! rt-prx (current-thread! rt-prx))
        (cv:await cv lock)
        (mask-current! rt-prx)))
    (catch (inline (f))
      ((InterruptCurrentThread msg)
       (lk:release lock)
       (throw (InterruptCurrentThread msg)))))
  )
