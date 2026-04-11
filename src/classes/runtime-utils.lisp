;; TODO: Find a better folder to put this package in...
(cl:in-package :cl-user)
(defpackage :io/classes/runtime-utils
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/types
   #:io/utils
   #:io/threads-exceptions
   #:io/classes/thread
   )
  (:local-nicknames
   (:bt #:io/utilities/bt-compat)

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
  (declare raise-timeout-exception (String -> Void))
  (define (raise-timeout-exception msg)
    (let ((exc (TimeoutException msg)))
      (throw
          (HandledError
           (to-dynamic exc)
           (fn ()
             (throw exc))))))

  (declare lk-acquire-with (bt:Lock * TimeoutStrategy -> Void))
  (define (lk-acquire-with lock strategy)
    "Acquire LOCK, optionally using a timeout."
    (match strategy
      ((NoTimeout)
       (bt:acquire lock)
       (values))
      ((Timeout timeout-time)
       (lisp (-> Unit) (lock timeout-time)
         (cl:if (bt2:acquire-lock lock :timeout (cl:/ timeout-time 1000.0d0))
                Unit
                (coalton
                 (raise-timeout-exception
                  (build-str "Timed out acquiring lock after "
                             (lisp (-> Double-Float) () timeout-time)
                             " milliseconds.")))))
       (values))))

  (declare cv-await-with (bt:ConditionVariable * bt:Lock * TimeoutStrategy -> Void))
  (define (cv-await-with cv lock strategy)
    "Await CV while holding LOCK, optionally using a timeout."
    (match strategy
      ((NoTimeout)
       (bt:await cv lock)
       (values))
      ((Timeout timeout-time)
       (lisp (-> Unit) (cv lock timeout-time)
         (cl:if (bt2:condition-wait cv lock :timeout (cl:/ timeout-time 1000.0d0))
                Unit
                (coalton
                 (raise-timeout-exception
                  (build-str "Timed out waiting on condition variable after "
                             (lisp (-> Double-Float) () timeout-time)
                             " milliseconds.")))))
       (values))))

  ;; TODO: Standardize usage of 'finally' throughout the library.
  ;; There's two separate concepts: (1) Call a cleanup function only on failure,
  ;; and (2) guarantee a cleanup function is called regardless of failure or success.
  ;; "Finally" should cleanly refer to one, and another word should refer to the other.
  ;; TODO: Remove lambda when this is fixed:
  ;; https://github.com/coalton-lang/coalton/issues/1719
  (inline)
  (declare unmask-and-await-safely-finally% (Runtime :rt :t
                                             => Proxy :rt
                                             * bt:ConditionVariable
                                             * bt:Lock
                                             * (Void -> Void)
                                             -> Void))
  (define (unmask-and-await-safely-finally% rt-prx cv lock finally)
    "Unmask the thread. Finally, either await (still running) the CV
or just release the LOCK. Masks after resuming post-await. FINALLY
is run AFTER the lock is released, and only if the thread is stopped!!"
    (unmask-and-await-safely-finally-with% rt-prx NoTimeout cv lock finally))

  (inline)
  (declare unmask-and-await-safely-finally-with% (Runtime :rt :t
                                                  => Proxy :rt
                                                  * TimeoutStrategy
                                                  * bt:ConditionVariable
                                                  * bt:Lock
                                                  * (Void -> Void)
                                                  -> Void))
  (define (unmask-and-await-safely-finally-with% rt-prx strategy cv lock finally)
    "Unmask the thread. Finally, either await (still running) the CV
or just release the LOCK. Masks after resuming post-await. FINALLY
is run AFTER the lock is released, and only if the thread is stopped!!"
    (catch (progn
             (unmask! rt-prx (current-thread! rt-prx))
             (cv-await-with cv lock strategy)
             (mask-current! rt-prx))
      ((InterruptCurrentThread msg)
       (bt:release lock)
       (finally)
       (throw (InterruptCurrentThread msg)))))

  ;; TODO: Remove lambda when this is fixed:
  ;; https://github.com/coalton-lang/coalton/issues/1719
  (inline)
  (declare unmask-and-await-safely% (Runtime :rt :t => Proxy :rt * bt:ConditionVariable * bt:Lock -> Void))
  (define (unmask-and-await-safely% rt-prx cv lock)
    "Unmask the thread. Finally, either await (still running) the CV
or just release the LOCK. Masks after resuming post-await."
    (unmask-and-await-safely-with% rt-prx NoTimeout cv lock))

  ;; TODO: Remove lambda when this is fixed:
  ;; https://github.com/coalton-lang/coalton/issues/1719
  (declare unmask-and-await-safely-with% (Runtime :rt :t
                                          => Proxy :rt
                                          * TimeoutStrategy
                                          * bt:ConditionVariable
                                          * bt:Lock
                                          -> Void))
  (define (unmask-and-await-safely-with% rt-prx strategy cv lock)
    "Unmask the thread. Finally, either await (still running) the CV
or just release the LOCK. Masks after resuming post-await."
    (catch (progn
             (unmask! rt-prx (current-thread! rt-prx))
             (cv-await-with cv lock strategy)
             (mask-current! rt-prx))
      ((InterruptCurrentThread msg)
       (bt:release lock)
       (throw (InterruptCurrentThread msg)))))
  )
