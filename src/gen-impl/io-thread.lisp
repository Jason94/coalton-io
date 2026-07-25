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
   )
  (:local-nicknames
   (:bt #:io/utilities/bt-compat)
   )
  (:export
   #:write-line-sync
   #:with-mask
   #:with-unmask
   #:do-with-mask
   #:do-with-unmask
   ))
(in-package :io/gen-impl/thread)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (define write-term-lock% (bt:new-lk))

  (declare write-line-sync ((Into :s String) (Terminal :m) => :s -> :m Unit))
  (define (write-line-sync msg)
    "Perform a synchrozied write-line to the terminal. Not performant - mainly useful
for debugging."
    (wrap-io
     (let thread-name =
       (lisp (-> String) ()
         (bt2:thread-name (bt2:current-thread))))
     (bt:acquire write-term-lock%)
     (trace (build-str (force-string msg) " <" thread-name ">"))
     (bt:release write-term-lock%)
     Unit))

  (inline)
  (declare with-mask ((Threads :rt :t :m) (Exceptions :m)
                      => :m :a -> :m :a))
  (define (with-mask op)
    "Mask the current thread while running OP, unmasking afterward."
    (do
     mask-current-thread
     ;; TODO: Standard exception handling functions like reraise should NOT
     ;; catch/handle thread stops. Change reraise to ignore thread stops,
     ;; and add a specific function to the Threads class to implement this
     ;; behavior - on-stop, or something.
     (finally
      op
      unmask-current-thread)))

  (inline)
  (declare with-unmask ((Threads :rt :t :m) (Exceptions :m)
                        => :m :a -> :m :a))
  (define (with-unmask op)
    "Unmask the current thread once while running OP, masking afterward."
    (do
     unmask-current-thread
     (finally
      op
      mask-current-thread)))
  )

(defmacro do-with-mask (cl:&body body)
  "Evaluate BODY with the current thread masked, automatically unmasking
afterward."
  `(with-mask
     (do
      ,@body)))

(defmacro do-with-unmask (cl:&body body)
  "Evaluate BODY with the current thread unmasked once, automatically masking
afterward."
  `(with-unmask
     (do
      ,@body)))
