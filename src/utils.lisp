(cl:in-package :cl-user)
(defpackage :io/utils
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/system
   #:coalton-library/types
   )
  (:local-nicknames
   (:b #:coalton-library/bits))
  (:export
   #:cl-maptree
   #:Word
   #:build-str
   #:IoError
   #:UnhandledError
   #:HandledError
   #:throw-handled-error
   #:flatten-err
   #:catch-thunk
   #:force-string
   #:compose2
   #:bit-odd?
   #:Anything
   #:to-anything
   #:from-anything
   #:from-anything-opt
   #:anything-eq
   #:Dynamic
   #:Dynamic%
   #:to-dynamic
   #:force-dynamic
   #:cast
   #:can-cast-to?
   #:MockException
   #:throw-dynamic
   #:proxy-swap-inner
   #:proxy-outer
   #:proxies-eql
   #:proxy-with-arg
   #:proxy-result-of
   #:proxy-returning
   #:equate-proxies
   ))
(in-package :io/utils)

(cl:declaim (cl:optimize (cl:speed 3) (cl:debug 0) (cl:safety 0)))

(named-readtables:in-readtable coalton:coalton)

(cl:defun cl-maptree (fn tree)
  "Recursively applies FN to all non-cons elements (atoms) in a tree structure."
  (cl:if (cl:atom tree)
         (cl:funcall fn tree)
         (cl:cons (cl-maptree fn (cl:car tree))
                  (cl-maptree fn (cl:cdr tree)))))

(defmacro build-str (cl:&rest str-parts)
  "Concatenate all STR-PARTS."
  `(fold <> "" (make-list ,@(cl:mapcar (cl:lambda (clause)
                                         `(as String ,clause))
                                       str-parts))))

(coalton-toplevel

  ;; https://github.com/garlic0x1/coalton-threads/blob/master/src/atomic.lisp
  (define-type-alias Word #+32-bit U32 #+64-bit U64
    "An integer that fits in a CPU word.")

  (define-exception IoError 
    "An unhandled error that was thrown inside a wrap-io call."
    (UnhandledError Anything (Unit -> Unit)) ;; re-throw thunk
    (HandledError Dynamic (Unit -> Unit))) ;; error val, error thunk
  )

(defmacro throw-handled-error (exception-form)
  `(throw (HandledError
           (to-dynamic ,exception-form)
           (fn ()
             (throw ,exception-form)))))

(coalton-toplevel
  ;; (define-instance (Signalable (IoError :e))
  ;;   (define (error err)
  ;;     (match err
  ;;       ((UnhandledError _ throw-thunk)
  ;;        (throw-thunk)
  ;;        (error "Malformed UnhandledError throw-thunk"))
  ;;       ((HandledError dyn-e)
  ;;        (throw-dynamic dyn-e)))))

  (inline)
  (declare flatten-err (Result :e (Result :e :a) -> Result :e :a))
  (define (flatten-err res)
    (match res
      ((Ok a)
       a)
      ((Err e)
       (Err e))))

  (declare catch-thunk ((Unit -> :a) -> Result IoError :a))
  (define (catch-thunk thunk)
    "Wraps `thunk` in a Lisp `handler-case`, and captures the output
as Err or Ok. Useful if you want to capture any thrown error, which is
currently not possible natively in Coalton. Works even with custom
Coalton exceptions via `define-exception`."
    (lisp (Result IoError :a) (thunk)
      (cl:handler-case (Ok (call-coalton-function thunk))
        (IoError/UnhandledError (e)
          (Err e))
        (IoError/HandledError (e)
          (Err e))
        (cl:error (e)
          (cl:let ((throw-thunk (coalton
                                 (fn ()
                                   (lisp :a ()
                                     (cl:error e))
                                   Unit))))
            (Err (UnhandledError e throw-thunk)))))))

  (inline)
  (declare force-string (:a -> String))
  (define (force-string x)
    (lisp String (x)
      (cl:format cl:nil "~a" x)))

  (inline)
  (declare compose2 ((:c -> :d) -> (:a -> :b -> :c) -> :a -> :b -> :d))
  (define (compose2 fcd fabc a b)
    (fcd (fabc a b)))

  (inline)
  (declare proxy-outer (Proxy :a -> Proxy (:m :a)))
  (define (proxy-outer _)
    Proxy)

  (inline)
  (declare proxies-eql (Proxy :a -> Proxy :a -> Unit))
  (define (proxies-eql _ _)
    "Force two proxies to represent the same type."
    Unit)

  (inline)
  (declare proxy-with-arg (Proxy :a -> Proxy (:a -> :b)))
  (define (proxy-with-arg _)
    Proxy)

  (inline)
  (declare proxy-result-of ((:a -> :b) -> Proxy :b))
  (define (proxy-result-of _)
    Proxy)

  (inline)
  (declare proxy-returning (Proxy :b -> Proxy (:a -> :b)))
  (define (proxy-returning _)
    Proxy)

  (inline)
  (declare bit-odd? (b:Bits :a => :a -> Boolean))
  (define (bit-odd? x)
    "Efficiently determine if x is odd."
    (/= (b:and x 1) 0))

  ;;;
  ;;; Dynamic
  ;;;

  (repr :native cl:t)
  (define-type Anything)

  (inline)
  (declare anything-eq (Anything -> Anything -> Boolean))
  (define (anything-eq a b)
    (lisp Boolean (a b)
      (cl:eq a b)))

  (define-type Dynamic
    (Dynamic% Anything LispType))

  (inline)
  (declare to-anything (:a -> Anything))
  (define (to-anything a)
    (lisp Anything (a)
      a))

  (inline)
  (declare from-anything (Anything -> :a))
  (define (from-anything a)
    (lisp :a (a)
      a))

  (inline)
  (declare from-anything-opt (Anything -> Optional :a))
  (define (from-anything-opt a)
    (lisp (Optional :a) (a)
      (cl:if a
             (Some a)
             None)))

  (inline)
  (declare to-dynamic (RuntimeRepr :a => :a -> Dynamic))
  (define (to-dynamic a)
    (Dynamic% (to-anything a) (runtime-repr-of a)))

  (inline)
  (declare force-dynamic (RuntimeRepr :a => Proxy :a -> :b -> Dynamic))
  (define (force-dynamic ty-prx val)
    "Force anything into a Dynamic with runtime representation for the type
represented by TY-PRX. Mainly useful after a type-test in CL."
    (Dynamic% (to-anything val) (runtime-repr ty-prx)))

  (declare cast (RuntimeRepr :b => Dynamic -> Optional :b))
  (define (cast (Dynamic% dyn-val dyn-repr))
    "Attempt to cast :a into a :b. WARNING: This will falsely cast :a into :b
if they are different Coalton types, but nonetheless have the same runtime
representation. To be safe, only use on types that have `(repr :lisp)`."
    (let prx-b = Proxy)
    (as-proxy-of
     (if (== dyn-repr
             (runtime-repr prx-b))
         (Some (lisp :b (dyn-val) dyn-val))
         None)
     (proxy-outer prx-b)))

  (inline)
  (declare can-cast-to? (RuntimeRepr :b => Dynamic -> Proxy :b -> Boolean))
  (define (can-cast-to? (Dynamic% _ dyn-repr) repr-prx)
    "Check whether dyn-val can cast to a type."
    (== dyn-repr (runtime-repr repr-prx)))

  (define-exception MockException
    MockException)

  (inline)
  (declare throw-dynamic (Dynamic -> :a))
  (define (throw-dynamic dyn-e)
    "Throw the dynamic value. Will fail if it isn't a Signalable/LispCondition."
    (let (Dynamic% val _) = dyn-e)
    (throw (the MockException (from-anything val))))

  (inline)
  (declare proxy-swap-inner (Proxy (:m :a) -> Proxy (:m :b)))
  (define (proxy-swap-inner _)
    Proxy)

  (inline)
  (declare equate-proxies (Proxy :a -> Proxy :a -> Unit))
  (define (equate-proxies _ _)
    Unit)
  )
