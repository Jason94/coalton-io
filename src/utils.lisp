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
   #:Word
   #:build-str
   #:UnhandledError
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
   #:to-dynamic
   #:force-dynamic
   #:cast
   #:MockException
   #:throw-dynamic
   #:proxy-swap-inner
   #:proxy-outer
   #:proxies-eql
   #:proxy-result-of
   ))
(in-package :io/utils)

(named-readtables:in-readtable coalton:coalton)

(cl:defmacro build-str (cl:&rest str-parts)
  "Concatenate all STR-PARTS."
  `(fold <> "" (make-list ,@(cl:mapcar (cl:lambda (clause)
                                         `(as String ,clause))
                                       str-parts))))

(coalton-toplevel

  ;; https://github.com/garlic0x1/coalton-threads/blob/master/src/atomic.lisp
  (define-type-alias Word #+32-bit U32 #+64-bit U64
    "An integer that fits in a CPU word.")

  (derive Eq)
  (repr :lisp)
  (define-type (UnhandledError :e)
    "An unhandled error that was thrown inside a wrap-io call."
    (UnhandledError :e))

  (define-instance (Signalable :e => Signalable (UnhandledError :e))
    (define (error (UnhandledError e))
      (error e)))

  (inline)
  (declare flatten-err (Result :e (Result :e :a) -> Result :e :a))
  (define (flatten-err res)
    (match res
      ((Ok a)
       a)
      ((Err e)
       (Err e))))

  (declare catch-thunk ((Unit -> :a) -> Result (UnhandledError :e) :a))
  (define (catch-thunk thunk)
    "Wraps `thunk` in a Lisp `handler-case`, and captures the output
as Err or Ok. Useful if you want to capture any thrown error, which is
currently not possible natively in Coalton. Works even with custom
Coalton exceptions via `define-exception`."
    (lisp (Result (UnhandledError :e) :a) (thunk)
      (cl:handler-case (Ok (call-coalton-function thunk))
        (cl:error (e)
          (Err (UnhandledError e))))))

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
  (declare proxy-result-of ((:a -> :b) -> Proxy :b))
  (define (proxy-result-of _)
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

  (define-exception MockException
    MockException)

  (declare throw-dynamic (Dynamic -> :a))
  (define (throw-dynamic dyn-e)
    "Throw the dynamic value. Will fail if it isn't a Signalable/LispCondition."
    (let (Dynamic% val _) = dyn-e)
    (throw (the MockException (from-anything val))))

  (declare proxy-swap-inner (Proxy (:m :a) -> Proxy (:m :b)))
  (define (proxy-swap-inner _)
    Proxy)
  )
