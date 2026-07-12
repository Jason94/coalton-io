(cl:in-package :cl-user)
(defpackage :io/utilities/atom-compat
  (:use
   #:coalton
   #:coalton-prelude)
  (:local-nicknames
   (:at :atomics))
  (:export
   #:atomic-incf-old
   #:atomic-max
  ))

(in-package :io/utilities/atom-compat)

(cl:declaim (cl:optimize (cl:speed 3) (cl:debug 0) (cl:safety 0)))

(named-readtables:in-readtable coalton:coalton)

(cl:defmacro atomic-incf-old (place cl:&optional (delta 1))
  "Atomically increment `place` by `delta`. Returns the old value."
  #+sbcl
  `(sb-ext:atomic-incf ,place ,delta)
  #+ccl
  `(ccl::atomic-incf-decf ,place ,delta))
  

(cl:defmacro atomic-max (place new)
  "Atomically advance `atm` to its current value or `n`, whichever is greater. Returns
the new value."
  (cl:let ((val-sym (cl:gensym)))
    `(cl:loop
       :for ,val-sym := ,place
       :if (cl:>= ,val-sym ,new)
           :return ,val-sym
           :when (at:cas ,place ,val-sym ,new)
               :return ,new)))
               
