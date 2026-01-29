;; TODO: Find a better folder to put this package in...
(cl:in-package :cl-user)
(defpackage :io/classes/runtime-utils
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/types
   #:io/utils
   #:io/threads-exceptions
   #:io/classes/threads
   )
  (:local-nicknames
   (:lk #:coalton-threads/lock)
   (:cv #:coalton-threads/condition-variable)
   (:bt2 #:bordeaux-threads-2)
   )
  (:export
   #:cv-await-with
   #:lk-acquire-with
   #:raise-timeout-exception
   #:unmask-and-await-safely%
   #:unmask-and-await-safely-finally%
   #:unmask-and-await-safely-with%
   #:unmask-and-await-safely-finally-with%
   ))
(in-package :io/classes/runtime-utils)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (declare raise-timeout-exception (String -> Unit))
  (define (raise-timeout-exception msg)
    (let ((exc (TimeoutException msg)))
      (throw
          (HandledError
           (to-dynamic exc)
           (fn ()
             (throw exc))))))

  (declare lk-acquire-with (lk:Lock -> TimeoutStrategy -> Unit))
  (define (lk-acquire-with lock strategy)
    "Acquire LOCK, optionally using a timeout."
    (match strategy
      ((NoTimeout)
       (lk:acquire lock)
       Unit)
      ((Timeout timeout-time)
       (lisp Unit (lock timeout-time)
         (cl:if (bt2:acquire-lock lock :timeout (cl:/ timeout-time 1000.0d0))
                Unit
                (coalton
                 (raise-timeout-exception
                  (build-str "Timed out acquiring lock after "
                             (lisp Double-Float () timeout-time)
                             " milliseconds."))))))))

  (declare cv-await-with (cv:ConditionVariable -> lk:Lock -> TimeoutStrategy -> Unit))
  (define (cv-await-with cv lock strategy)
    "Await CV while holding LOCK, optionally using a timeout."
    (match strategy
      ((NoTimeout)
       (cv:await cv lock)
       Unit)
      ((Timeout timeout-time)
       (lisp Unit (cv lock timeout-time)
         (cl:if (bt2:condition-wait cv lock :timeout (cl:/ timeout-time 1000.0d0))
                Unit
                (coalton
                 (raise-timeout-exception
                  (build-str "Timed out waiting on condition variable after "
                             (lisp Double-Float () timeout-time)
                             " milliseconds."))))))))

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
    (unmask-and-await-safely-finally-with% rt-prx NoTimeout cv lock finally))

  (inline)
  (declare unmask-and-await-safely-finally-with% (Runtime :rt :t
                                                  => Proxy :rt
                                                  -> TimeoutStrategy
                                                  -> cv:ConditionVariable
                                                  -> lk:Lock
                                                  -> (Unit -> Unit)
                                                  -> Unit))
  (define (unmask-and-await-safely-finally-with% rt-prx strategy cv lock finally)
    "Unmask the thread. Finally, either await (still running) the CV
or just release the LOCK. Masks after resuming post-await. FINALLY
is run AFTER the lock is released, and only if the thread is stopped!!"
    (let f =
      (fn ()
        (unmask! rt-prx (current-thread! rt-prx))
        (cv-await-with cv lock strategy)
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
    (unmask-and-await-safely-with% rt-prx NoTimeout cv lock))

  ;; TODO: Remove lambda when this is fixed:
  ;; https://github.com/coalton-lang/coalton/issues/1719
  (declare unmask-and-await-safely-with% (Runtime :rt :t
                                          => Proxy :rt
                                          -> TimeoutStrategy
                                          -> cv:ConditionVariable
                                          -> lk:Lock
                                          -> Unit))
  (define (unmask-and-await-safely-with% rt-prx strategy cv lock)
    "Unmask the thread. Finally, either await (still running) the CV
or just release the LOCK. Masks after resuming post-await."
    (let f =
      (fn ()
        (unmask! rt-prx (current-thread! rt-prx))
        (cv-await-with cv lock strategy)
        (mask-current! rt-prx)))
    (catch (inline (f))
      ((InterruptCurrentThread msg)
       (lk:release lock)
       (throw (InterruptCurrentThread msg)))))
  )
