(cl:in-package :cl-user)
(defpackage :io/gen-impl/thread
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/types
   #:coalton-library/monad/classes
   #:coalton-library/types
   #:io/utils
   #:io/classes/exceptions
   #:io/classes/monad-io
   #:io/classes/thread
   #:io/classes/term
   #:io/threads-impl/runtime
   )
  (:export
   #:write-line-sync
   #:with-mask
   #:do-with-mask
   ))
(in-package :io/gen-impl/thread)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (declare write-line-sync ((Into :s String) (Terminal :m) => :s -> :m Unit))
  (define (write-line-sync msg)
    "Perform a synchrozied write-line to the terminal. Not performant - mainly useful
for debugging."
    (wrap-io (write-line-sync% msg) Unit))

  (declare with-mask ((Threads :rt :t :m) (Exceptions :m)
                      => :m :a -> :m :a))
  (define (with-mask op)
    "Mask the current thread while running OP, unmasking afterward."
    (do
     mask-current-thread
     (reraise
      (do
       (result <- op)
       unmask-current-thread
       (pure result))
      (fn ()
        unmask-current-thread))))
  )

(defmacro do-with-mask (cl:&body body)
  "Evaluate BODY with the current thread masked, automatically unmasking
afterward."
  `(with-mask
     (do
      ,@body)))
