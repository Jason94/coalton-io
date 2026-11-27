# Package `IO/THREADS-EXCEPTIONS`<a name="io-threads-exceptions-package"></a>

### Types

#### <a href="#io-threads-exceptions-synchronousthreadexception-type"><code>SynchronousThreadException</code></a> <sup><sub>[TYPE] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/thread-exceptions.lisp#L65-L70">src</a></sub></sup><a name="io-threads-exceptions-synchronousthreadexception-type"></a>
- <code>(JoinedFailedThread <a href="#io-utils-dynamic-type">Dynamic</a>)</code>
- <code>(TimeoutException <a href="#coalton-string-type">String</a>)</code>
- <code>UnexpectedStop</code> 

Exceptions that a thread raises whenever it encounters a threading
related problem. Unlike ThreadingException, these are actual exceptions.

<details>
<summary>Instances</summary>

- <code><a href="#coalton-classes-exception-class">Exception</a> <a href="#io-threads-exceptions-synchronousthreadexception-type">SynchronousThreadException</a></code>
- <code><a href="#coalton-types-runtimerepr-class">RuntimeRepr</a> <a href="#io-threads-exceptions-synchronousthreadexception-type">SynchronousThreadException</a></code>

</details>



***

#### <a href="#io-threads-exceptions-threadingexception-type"><code>ThreadingException</code></a> <sup><sub>[TYPE] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/thread-exceptions.lisp#L37-L40">src</a></sub></sup><a name="io-threads-exceptions-threadingexception-type"></a>
- <code>(InterruptCurrentThread <a href="#coalton-string-type">String</a>)</code>

Thread exception sent asynchronously between threads.
This type isn't really an exception, it's more of a message.

<details>
<summary>Instances</summary>

- <code><a href="#coalton-classes-exception-class">Exception</a> <a href="#io-threads-exceptions-threadingexception-type">ThreadingException</a></code>
- <code><a href="#coalton-types-runtimerepr-class">RuntimeRepr</a> <a href="#io-threads-exceptions-threadingexception-type">ThreadingException</a></code>

</details>



***

#### <a href="#io-threads-exceptions-unmaskfinallymode-type"><code>UnmaskFinallyMode</code></a> <sup><sub>[TYPE] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/thread-exceptions.lisp#L81-L89">src</a></sub></sup><a name="io-threads-exceptions-unmaskfinallymode-type"></a>
- <code>Running</code> 
- <code>Stopped</code> 

When a thread unmasks and calls a cleanup operation, the
thread is either:

* Still running, and will cleanup and continue
* Received a stop while it was masked, and will cleanup and then
  terminate itself.

<details>
<summary>Instances</summary>

- <code><a href="#coalton-classes-eq-class">Eq</a> <a href="#io-threads-exceptions-unmaskfinallymode-type">UnmaskFinallyMode</a></code>
- <code><a href="#coalton-classes-into-class">Into</a> <a href="#io-threads-exceptions-unmaskfinallymode-type">UnmaskFinallyMode</a> <a href="#coalton-string-type">String</a></code>
- <code><a href="#coalton-types-runtimerepr-class">RuntimeRepr</a> <a href="#io-threads-exceptions-unmaskfinallymode-type">UnmaskFinallyMode</a></code>

</details>



***

### Values

#### <a href="#io-threads-exceptions-dynamic-is-threading-exception?-value"><code>(DYNAMIC-IS-THREADING-EXCEPTION? DYN)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/thread-exceptions.lisp#L43-L53">src</a></sub></sup><a name="io-threads-exceptions-dynamic-is-threading-exception?-value"></a>
<code><a href="#io-utils-dynamic-type">Dynamic</a> &rarr; <a href="#coalton-boolean-type">Boolean</a></code>

Returns true if the dynamic val is a threading exception or an
IoError containing a ThreadingException.



***

#### <a href="#io-threads-exceptions-dynamic-is-unexpected-stop-exc?-value"><code>(DYNAMIC-IS-UNEXPECTED-STOP-EXC? DYN)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/thread-exceptions.lisp#L74-L77">src</a></sub></sup><a name="io-threads-exceptions-dynamic-is-unexpected-stop-exc?-value"></a>
<code><a href="#io-utils-dynamic-type">Dynamic</a> &rarr; <a href="#coalton-boolean-type">Boolean</a></code>


***

#### <a href="#io-threads-exceptions-is-threading-exception-value"><code>(IS-THREADING-EXCEPTION IO-ERR)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/thread-exceptions.lisp#L56-L63">src</a></sub></sup><a name="io-threads-exceptions-is-threading-exception-value"></a>
<code><a href="#io-utils-ioerror-type">IoError</a> &rarr; <a href="#coalton-boolean-type">Boolean</a></code>

Return True if IO-ERR contains a threading exception.



***

# Package `IO/EXCEPTIONS`<a name="io-exceptions-package"></a>

### Classes

#### <a href="#io-classes-exceptions-exceptions-class"><code>Exceptions</code></a> <sup><sub>[CLASS] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/classes/exceptions.lisp#L74-L97">src</a></sub></sup><a name="io-classes-exceptions-exceptions-class"></a>
<code><a href="#coalton-classes-monad-class">Monad</a> :M &rArr; <a href="#io-classes-exceptions-exceptions-class">Exceptions</a> :M</code>


A Monad that can raise and handle exceptions. IMPORTANT: Any Exceptions
must catch and wrap all unhandled errors inside a wrap-io call.

Methods:
- <code>RAISE :: (<a href="#coalton-types-runtimerepr-class">RuntimeRepr</a> :E) (<a href="#coalton-classes-exception-class">Exception</a> :E) &rArr; :E &rarr; :M :A</code><br/>Raise an exception.
- <code>RAISE-DYNAMIC :: <a href="#io-utils-dynamic-type">Dynamic</a> &rarr; :M :A</code><br/>Raise an exception wrapped in a Dynamic. Mainly useful to hand-off eexceptions&#xA;between IO instances.
- <code>RERAISE :: :M :A * (Void &rarr; :M :B) &rarr; :M :A</code><br/>Run an operation, run `on-exception` if the first operation raised, then re-raise&#xA;the exception. If `on-exception` raises, that exception will be emitted instead of the&#xA;original exception.
- <code>HANDLE-EXC :: <a href="#coalton-types-runtimerepr-class">RuntimeRepr</a> :E &rArr; :M :A * (:E &rarr; :M :A) &rarr; :M :A</code><br/>Run an operation, immediately handling if it raised an exception that matches :e.
- <code>HANDLE-ALL :: :M :A * (Void &rarr; :M :A) &rarr; :M :A</code><br/>Run an operation, immediately handling any exceptions raised.
- <code>TRY-DYNAMIC :: :M :A &rarr; :M (<a href="#coalton-classes-result-type">Result</a> <a href="#io-utils-dynamic-type">Dynamic</a> :A)</code><br/>Bring any unhandled exceptions into a Result wrapped in Dynamic.
<details>
<summary>Instances</summary>

- <code><a href="#io-classes-exceptions-exceptions-class">Exceptions</a> <a href="#io-io-impl-simple-io-io-type">IO</a></code>
- <code><a href="#io-classes-exceptions-exceptions-class">Exceptions</a> :M &rArr; <a href="#io-classes-exceptions-exceptions-class">Exceptions</a> (<a href="#coalton-monad-environment-envt-type">EnvT</a> :E :M)</code>
- <code><a href="#io-classes-exceptions-exceptions-class">Exceptions</a> :M &rArr; <a href="#io-classes-exceptions-exceptions-class">Exceptions</a> (<a href="#coalton-monad-statet-statet-type">StateT</a> :S :M)</code>

</details>



***

### Values

#### <a href="#io-classes-exceptions-finally-value"><code>(FINALLY OP FINALLY-OP)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/classes/exceptions.lisp#L160-L169">src</a></sub></sup><a name="io-classes-exceptions-finally-value"></a>
<code>&forall; (:M (Type &rarr; Type)) :A :B. <a href="#io-classes-exceptions-exceptions-class">Exceptions</a> :M &rArr; :M :A * :M :B &rarr; :M :A</code>

Run `op` followed by `finally-op`, returning the result of `op`. If `op` raises,
`finally-op` is run and `op`'s exception is re-raised. If `finally-op` raises, that
exception is propogated instead of `op`'s exception.



***

#### <a href="#io-classes-exceptions-on-success-or-exception-value"><code>(ON-SUCCESS-OR-EXCEPTION OP ON-SUCCESS ON-EXCEPTION)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/classes/exceptions.lisp#L173-L185">src</a></sub></sup><a name="io-classes-exceptions-on-success-or-exception-value"></a>
<code>&forall; (:M (Type &rarr; Type)) :A :B :C. <a href="#io-classes-exceptions-exceptions-class">Exceptions</a> :M &rArr; :M :A * :M :B * :M :C &rarr; :M :A</code>

Run `op`. If `op` raises an exception, run `on-exception` and re-raise `op`'s
exception. If `op` succeeds, run `on-success` and return the result from `op`.

If either `on-success` or `on-exception` raise an exception, that exception is raised
instead.



***

#### <a href="#io-classes-exceptions-raise-result-value"><code>(RAISE-RESULT IO-RES)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/classes/exceptions.lisp#L120-L127">src</a></sub></sup><a name="io-classes-exceptions-raise-result-value"></a>
<code>&forall; (:M (Type &rarr; Type)) :E :A. (<a href="#io-classes-exceptions-exceptions-class">Exceptions</a> :M) (<a href="#coalton-classes-exception-class">Exception</a> :E) &rArr; :M (<a href="#coalton-classes-result-type">Result</a> :E :A) &rarr; :M :A</code>

Raise any (Err :e) into :m. Useful if (Err :e) represents any unhandleable, fatal
exception to the program.



***

#### <a href="#io-classes-exceptions-raise-result-dynamic-value"><code>(RAISE-RESULT-DYNAMIC OP)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/classes/exceptions.lisp#L131-L136">src</a></sub></sup><a name="io-classes-exceptions-raise-result-dynamic-value"></a>
<code>&forall; (:M (Type &rarr; Type)) :A. <a href="#io-classes-exceptions-exceptions-class">Exceptions</a> :M &rArr; :M (<a href="#coalton-classes-result-type">Result</a> <a href="#io-utils-dynamic-type">Dynamic</a> :A) &rarr; :M :A</code>


***

#### <a href="#io-classes-exceptions-try-value"><code>(TRY OP)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/classes/exceptions.lisp#L102-L107">src</a></sub></sup><a name="io-classes-exceptions-try-value"></a>
<code>&forall; (:M (Type &rarr; Type)) :A :E. (<a href="#io-classes-exceptions-exceptions-class">Exceptions</a> :M) (<a href="#coalton-types-runtimerepr-class">RuntimeRepr</a> :E) &rArr; :M :A &rarr; :M (<a href="#coalton-classes-result-type">Result</a> :E :A)</code>

Bring any unhandled exceptions of type :e up into a Result.
Continues to carry any unhandeld exceptions not of type :e.



***

#### <a href="#io-classes-exceptions-try-all-value"><code>(TRY-ALL OP)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/classes/exceptions.lisp#L111-L116">src</a></sub></sup><a name="io-classes-exceptions-try-all-value"></a>
<code>&forall; (:M (Type &rarr; Type)) :A. <a href="#io-classes-exceptions-exceptions-class">Exceptions</a> :M &rArr; :M :A &rarr; :M (<a href="#coalton-optional-type">Optional</a> :A)</code>

Bring the result of OP up into an Optional. Returns None if OP
raised any exceptions.



***

#### <a href="#io-classes-exceptions-wrap-error_-value"><code>(WRAP-ERROR_ OP)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/classes/exceptions.lisp#L140-L145">src</a></sub></sup><a name="io-classes-exceptions-wrap-error_-value"></a>
<code>&forall; :A (:M (Type &rarr; Type)). <a href="#io-classes-exceptions-exceptions-class">Exceptions</a> :M &rArr; (Void &rarr; :A) &rarr; :M :A</code>

Run `op`, catching any unhandled Lisp/Coalton errors and raising
them as exceptions.



***

### Macros

#### <a href="#io-classes-exceptions-do-finally-macro"><code>DO-FINALLY (FINALLY-OP &amp;BODY BODY)</code></a> <sup><sub>[MACRO]</sub></sup><a name="io-classes-exceptions-do-finally-macro"></a>

Type: (Exceptions :m => :m :b &body :m :a -> :m :a)

Run `body` followed by `finally-op`, returning the result of `body`. If `body` raises,
`finally-op` is run and `body`'s exception is re-raised. If `finally-op` raises, that
exception is propogated instead of `body`'s exception.

`body` is wrapped in `do` notation.



***

#### <a href="#io-classes-exceptions-do-handle-all-macro"><code>DO-HANDLE-ALL (OP &amp;BODY BODY)</code></a> <sup><sub>[MACRO]</sub></sup><a name="io-classes-exceptions-do-handle-all-macro"></a>

Convenience macro for handle-all.

Example:

(do-handle-all add-three-ints
  (modify (Cons 2))
  (pure 10))
===>
(handle-all add-three-ints
  (fn ()
    (do
     (modify (cons 2))
     (pure 10))))




***

#### <a href="#io-classes-exceptions-do-handle-exc-macro"><code>DO-HANDLE-EXC (OP (ERR-SYM) &amp;BODY BODY)</code></a> <sup><sub>[MACRO]</sub></sup><a name="io-classes-exceptions-do-handle-exc-macro"></a>

Convenience macro for handle.



***

#### <a href="#io-classes-exceptions-do-reraise-macro"><code>DO-RERAISE (OP &amp;BODY BODY)</code></a> <sup><sub>[MACRO]</sub></sup><a name="io-classes-exceptions-do-reraise-macro"></a>

Convenience macro for reraise.



***

#### <a href="#io-classes-exceptions-wrap-error-macro"><code>WRAP-ERROR (&amp;BODY BODY)</code></a> <sup><sub>[MACRO]</sub></sup><a name="io-classes-exceptions-wrap-error-macro"></a>

Run BODY, catching any unhandled Lisp/Coalton errors and raising
them as exceptions.



***

# Package `IO/MONAD-IO`<a name="io-monad-io-package"></a>

### Classes

#### <a href="#io-classes-monad-io-baseio-class"><code>BaseIo</code></a> <sup><sub>[CLASS] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/classes/monad-io.lisp#L61-L76">src</a></sub></sup><a name="io-classes-monad-io-baseio-class"></a>
<code>(<a href="#io-classes-monad-io-monadio-class">MonadIo</a> :M) (<a href="#io-classes-monad-io-liftio-class">LiftIo</a> :M :M) &rArr; <a href="#io-classes-monad-io-baseio-class">BaseIo</a> :M</code>


A 'base' IO implementation, which can be run to execute some
(potentially side-effectful) operation.

Methods:
- <code>RUN! :: :M :A &rarr; :A</code><br/>Run a (potentially) side-effectful operation. Throws any unhandled exceptions.
- <code>RUN-UNSAFE! :: :M :A &rarr; :A</code><br/>Run a (potentially) side-effectful operation. Throws any unhandled exceptions.&#xA;&#xA;WARNING: Not meant to be used top-level! Use `run!` to run an operation. `run-unsafe!` is&#xA;intended to be used to run an IO inside of an already-running IO for optimizations.
- <code>RUN-HANDLED! :: :M :A &rarr; <a href="#coalton-classes-result-type">Result</a> <a href="#io-utils-dynamic-type">Dynamic</a> :A</code><br/>Run a (potentially) side-effectful operation. Returns any unhandled&#xA;exceptions as an (Err e).
<details>
<summary>Instances</summary>

- <code><a href="#io-classes-monad-io-baseio-class">BaseIo</a> <a href="#io-io-impl-simple-io-io-type">IO</a></code>

</details>



***

#### <a href="#io-classes-monad-io-liftio-class"><code>LiftIo</code></a> <sup><sub>[CLASS] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/classes/monad-io.lisp#L58-L59">src</a></sub></sup><a name="io-classes-monad-io-liftio-class"></a>
<code><a href="#coalton-classes-monad-class">Monad</a> :M &rArr; <a href="#io-classes-monad-io-liftio-class">LiftIo</a> :I :M</code>

Methods:
- <code>LIFT-IO :: :I :A &rarr; :M :A</code>
<details>
<summary>Instances</summary>

- <code><a href="#io-classes-monad-io-liftio-class">LiftIo</a> <a href="#io-io-impl-simple-io-io-type">IO</a> <a href="#io-io-impl-simple-io-io-type">IO</a></code>
- <code><a href="#io-classes-monad-io-liftio-class">LiftIo</a> :I :M &rArr; <a href="#io-classes-monad-io-liftio-class">LiftIo</a> :I (<a href="#coalton-experimental-do-control-loops-adv-loopt-type">LoopT</a> :M)</code>
- <code><a href="#io-classes-monad-io-liftio-class">LiftIo</a> :I :M &rArr; <a href="#io-classes-monad-io-liftio-class">LiftIo</a> :I (<a href="#coalton-monad-environment-envt-type">EnvT</a> :ENV :M)</code>
- <code><a href="#io-classes-monad-io-liftio-class">LiftIo</a> :I :M &rArr; <a href="#io-classes-monad-io-liftio-class">LiftIo</a> :I (<a href="#coalton-monad-statet-statet-type">StateT</a> :S :M)</code>

</details>



***

#### <a href="#io-classes-monad-io-monadio-class"><code>MonadIo</code></a> <sup><sub>[CLASS] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/classes/monad-io.lisp#L53-L56">src</a></sub></sup><a name="io-classes-monad-io-monadio-class"></a>
<code><a href="#coalton-classes-monad-class">Monad</a> :M &rArr; <a href="#io-classes-monad-io-monadio-class">MonadIo</a> :M</code>

Methods:
- <code>WRAP-IO_ :: (Void &rarr; :A) &rarr; :M :A</code><br/>Wrap a (potentially) side-effectful function in the monad.
<details>
<summary>Instances</summary>

- <code><a href="#io-classes-monad-io-monadio-class">MonadIo</a> <a href="#io-io-impl-simple-io-io-type">IO</a></code>
- <code><a href="#io-classes-monad-io-monadio-class">MonadIo</a> :M &rArr; <a href="#io-classes-monad-io-monadio-class">MonadIo</a> (<a href="#coalton-experimental-do-control-loops-adv-loopt-type">LoopT</a> :M)</code>
- <code><a href="#io-classes-monad-io-monadio-class">MonadIo</a> :M &rArr; <a href="#io-classes-monad-io-monadio-class">MonadIo</a> (<a href="#coalton-monad-environment-envt-type">EnvT</a> :ENV :M)</code>
- <code><a href="#io-classes-monad-io-monadio-class">MonadIo</a> :M &rArr; <a href="#io-classes-monad-io-monadio-class">MonadIo</a> (<a href="#coalton-monad-statet-statet-type">StateT</a> :S :M)</code>

</details>



***

#### <a href="#io-classes-monad-io-unliftio-class"><code>UnliftIo</code></a> <sup><sub>[CLASS] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/classes/monad-io.lisp#L78-L79">src</a></sub></sup><a name="io-classes-monad-io-unliftio-class"></a>
<code>(<a href="#io-classes-monad-io-monadio-class">MonadIo</a> :M) (<a href="#io-classes-monad-io-liftio-class">LiftIo</a> :I :M) (<a href="#io-classes-monad-io-baseio-class">BaseIo</a> :I) &rArr; <a href="#io-classes-monad-io-unliftio-class">UnliftIo</a> :M :I</code>

Methods:
- <code>WITH-RUN-IN-IO :: ((:M :A &rarr; :I :A) &rarr; :I :B) &rarr; :M :B</code>
<details>
<summary>Instances</summary>

- <code><a href="#io-classes-monad-io-unliftio-class">UnliftIo</a> <a href="#io-io-impl-simple-io-io-type">IO</a> <a href="#io-io-impl-simple-io-io-type">IO</a></code>
- <code>(<a href="#io-classes-monad-io-baseio-class">BaseIo</a> :R) (<a href="#io-classes-monad-io-unliftio-class">UnliftIo</a> :M :R) &rArr; <a href="#io-classes-monad-io-unliftio-class">UnliftIo</a> (<a href="#coalton-monad-environment-envt-type">EnvT</a> :ENV :M) :R</code>

</details>



***

### Macros

#### <a href="#io-classes-monad-io-derive-lift-io-macro"><code>DERIVE-LIFT-IO (MONAD-PARAM MONADT-FORM)</code></a> <sup><sub>[MACRO]</sub></sup><a name="io-classes-monad-io-derive-lift-io-macro"></a>

Automatically derive an instance of LiftIo for a monad transformer.

Example:
  (derive-lift-io :m (e:EnvT :e :m))



***

#### <a href="#io-classes-monad-io-derive-monad-io-macro"><code>DERIVE-MONAD-IO (MONAD-PARAM MONADT-FORM)</code></a> <sup><sub>[MACRO]</sub></sup><a name="io-classes-monad-io-derive-monad-io-macro"></a>

Automatically derive an instance of MonadIo for a monad transformer.

Example:
  (derive-monad-io :m (st:StateT :s :m))



***

#### <a href="#io-classes-monad-io-run-as!-macro"><code>RUN-AS! (M-TYPE M-OP)</code></a> <sup><sub>[MACRO]</sub></sup><a name="io-classes-monad-io-run-as!-macro"></a>

Run M-OP using the concrete RunIo M-TYPE. Useful for situations where
you want to create a generic MonadIo operation and immediately run it,
so the compiler can't infer the type of the actual monad you want to use.

Example:
  (run-as! (IO Unit) (pure Unit))

NOTE: Unfortunately, there seems to be a type inference bug that requires
putting in the full type of M-OP, not just (IO :a).




***

#### <a href="#io-classes-monad-io-wrap-io-macro"><code>WRAP-IO (&amp;BODY BODY)</code></a> <sup><sub>[MACRO]</sub></sup><a name="io-classes-monad-io-wrap-io-macro"></a>

Wrap the execution of BODY in the IO monad.
Supports any MonadIo instance.

Example:
  (wrap-io
    (lisp (-> :a) (str)
      (cl:print str))



***

# Package `IO/SIMPLE-IO`<a name="io-simple-io-package"></a>

### Types

#### <a href="#io-io-impl-simple-io-io-type"><code>IO</code></a> <sup><sub>[TYPE] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/io-impl/simple-io.lisp#L74-L75">src</a></sub></sup><a name="io-io-impl-simple-io-io-type"></a>
<details>
<summary>Instances</summary>

- <code><a href="#coalton-classes-applicative-class">Applicative</a> <a href="#io-io-impl-simple-io-io-type">IO</a></code>
- <code><a href="#io-classes-monad-io-baseio-class">BaseIo</a> <a href="#io-io-impl-simple-io-io-type">IO</a></code>
- <code><a href="#io-classes-delay-delays-class">Delays</a> <a href="#io-io-impl-runtime-ioruntime-type">IoRuntime</a> <a href="#io-io-impl-simple-io-io-type">IO</a></code>
- <code><a href="#io-classes-exceptions-exceptions-class">Exceptions</a> <a href="#io-io-impl-simple-io-io-type">IO</a></code>
- <code><a href="#io-classes-file-files-class">Files</a> <a href="#io-io-impl-simple-io-io-type">IO</a></code>
- <code><a href="#coalton-classes-functor-class">Functor</a> <a href="#io-io-impl-simple-io-io-type">IO</a></code>
- <code><a href="#io-classes-monad-io-liftio-class">LiftIo</a> <a href="#io-io-impl-simple-io-io-type">IO</a> <a href="#io-io-impl-simple-io-io-type">IO</a></code>
- <code><a href="#coalton-classes-monad-class">Monad</a> <a href="#io-io-impl-simple-io-io-type">IO</a></code>
- <code><a href="#io-classes-monad-io-monadio-class">MonadIo</a> <a href="#io-io-impl-simple-io-io-type">IO</a></code>
- <code><a href="#io-classes-mut-mutablevar-class">MutableVar</a> <a href="#io-io-impl-simple-io-io-type">IO</a></code>
- <code><a href="#io-classes-random-random-class">Random</a> <a href="#io-io-impl-simple-io-io-type">IO</a></code>
- <code><a href="#coalton-types-runtimerepr-class">RuntimeRepr</a> (<a href="#io-io-impl-simple-io-io-type">IO</a> :|0|)</code>
- <code><a href="#io-classes-network-sockets-class">Sockets</a> <a href="#io-io-impl-simple-io-io-type">IO</a></code>
- <code><a href="#io-classes-term-terminal-class">Terminal</a> <a href="#io-io-impl-simple-io-io-type">IO</a></code>
- <code><a href="#io-classes-thread-threads-class">Threads</a> <a href="#io-io-impl-runtime-ioruntime-type">IoRuntime</a> <a href="#io-io-impl-io-runtime-runtime-iothread-type">IoThread</a> <a href="#io-io-impl-simple-io-io-type">IO</a></code>
- <code><a href="#io-classes-monad-io-unliftio-class">UnliftIo</a> <a href="#io-io-impl-simple-io-io-type">IO</a> <a href="#io-io-impl-simple-io-io-type">IO</a></code>

</details>



***

### Values

#### <a href="#io-io-impl-simple-io-handle-all-io-value"><code>(HANDLE-ALL-IO IO-OP ON-EXCEPTION-OP)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/io-impl/simple-io.lisp#L283-L295">src</a></sub></sup><a name="io-io-impl-simple-io-handle-all-io-value"></a>
<code>&forall; :A. <a href="#io-io-impl-simple-io-io-type">IO</a> :A * (Void &rarr; <a href="#io-io-impl-simple-io-io-type">IO</a> :A) &rarr; <a href="#io-io-impl-simple-io-io-type">IO</a> :A</code>

Run IO-OP, and run `on-exception-op` to handle exceptions of any type thrown by IO-OP.



***

#### <a href="#io-io-impl-simple-io-handle-io-value"><code>(HANDLE-IO IO-OP ON-EXCEPTION-OP)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/io-impl/simple-io.lisp#L262-L279">src</a></sub></sup><a name="io-io-impl-simple-io-handle-io-value"></a>
<code>&forall; :A :E. <a href="#coalton-types-runtimerepr-class">RuntimeRepr</a> :E &rArr; <a href="#io-io-impl-simple-io-io-type">IO</a> :A * (:E &rarr; <a href="#io-io-impl-simple-io-io-type">IO</a> :A) &rarr; <a href="#io-io-impl-simple-io-io-type">IO</a> :A</code>


***

#### <a href="#io-io-impl-simple-io-raise-dynamic-io-value"><code>(RAISE-DYNAMIC-IO DYN)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/io-impl/simple-io.lisp#L239-L241">src</a></sub></sup><a name="io-io-impl-simple-io-raise-dynamic-io-value"></a>
<code>&forall; :A. <a href="#io-utils-dynamic-type">Dynamic</a> &rarr; <a href="#io-io-impl-simple-io-io-type">IO</a> :A</code>


***

#### <a href="#io-io-impl-simple-io-raise-io-value"><code>(RAISE-IO E)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/io-impl/simple-io.lisp#L233-L235">src</a></sub></sup><a name="io-io-impl-simple-io-raise-io-value"></a>
<code>&forall; :E :A. <a href="#coalton-classes-exception-class">Exception</a> :E &rArr; :E &rarr; <a href="#io-io-impl-simple-io-io-type">IO</a> :A</code>


***

#### <a href="#io-io-impl-simple-io-raise-io_-value"><code>(RAISE-IO_)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/io-impl/simple-io.lisp#L245-L245">src</a></sub></sup><a name="io-io-impl-simple-io-raise-io_-value"></a>
<code>&forall; :E. <a href="#coalton-classes-exception-class">Exception</a> :E &rArr; :E &rarr; <a href="#io-io-impl-simple-io-io-type">IO</a> <a href="#coalton-unit-type">Unit</a></code>


***

#### <a href="#io-io-impl-simple-io-reraise-io-value"><code>(RERAISE-IO OP ON-EXCEPTION-OP)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/io-impl/simple-io.lisp#L249-L258">src</a></sub></sup><a name="io-io-impl-simple-io-reraise-io-value"></a>
<code>&forall; :A :B. <a href="#io-io-impl-simple-io-io-type">IO</a> :A * (Void &rarr; <a href="#io-io-impl-simple-io-io-type">IO</a> :B) &rarr; <a href="#io-io-impl-simple-io-io-type">IO</a> :A</code>


***

#### <a href="#io-io-impl-simple-io-run-io!-value"><code>(RUN-IO! IO-OP)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/io-impl/simple-io.lisp#L139-L193">src</a></sub></sup><a name="io-io-impl-simple-io-run-io!-value"></a>
<code>&forall; :A. <a href="#io-io-impl-simple-io-io-type">IO</a> :A &key (:disable-masking <a href="#coalton-boolean-type">Boolean</a>) &rarr; :A</code>

Top-level run-io! that raises any unhandled exceptions. Also sets the current thread
as the global thread for structured concurrency, and exits any child threads on exit.

Can disable masking with `:disable-masking` set to `True.` For programs that need
high-performance hot loops around a cheap synchronized IO operation, such as enqueuing to
an MPMC queue, this can give up to a ~33% speedup. However, if the loop is also doing any
meaningful CPU work or any kind of slow IO call (like writing to a file or reading from a
network socket), then disabling masking will likely not result in a significant speedup.

If masking is disabled, then asynchronous thread stopping is not safe. Because of this,
all structured concurrency thread cleanups except the top level will join child threads
but not stop them. So if masking is disabled, threads must stop cooperatively. Regardless,
all threads will be stopped by the main thread when `run-io!` completes.



***

#### <a href="#io-io-impl-simple-io-try-dynamic-io-value"><code>(TRY-DYNAMIC-IO IO-OP)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/io-impl/simple-io.lisp#L299-L314">src</a></sub></sup><a name="io-io-impl-simple-io-try-dynamic-io-value"></a>
<code>&forall; :A. <a href="#io-io-impl-simple-io-io-type">IO</a> :A &rarr; <a href="#io-io-impl-simple-io-io-type">IO</a> (<a href="#coalton-classes-result-type">Result</a> <a href="#io-utils-dynamic-type">Dynamic</a> :A)</code>


***

#### <a href="#io-io-impl-simple-io-with-run-in-io_-value"><code>(WITH-RUN-IN-IO_)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/io-impl/simple-io.lisp#L346-L349">src</a></sub></sup><a name="io-io-impl-simple-io-with-run-in-io_-value"></a>
<code>&forall; (:M (Type &rarr; Type)) :A :B. <a href="#io-classes-monad-io-unliftio-class">UnliftIo</a> :M <a href="#io-io-impl-simple-io-io-type">IO</a> &rArr; ((:M :A &rarr; <a href="#io-io-impl-simple-io-io-type">IO</a> :A) &rarr; <a href="#io-io-impl-simple-io-io-type">IO</a> :B) &rarr; :M :B</code>

`with-run-in-io`, but pegged to the simple-io implementation. Useful when you
need to unlift, run, then immediately re-run a function. See, e.g., io-file:with-open-file%.



***

### Macros

#### <a href="#io-io-impl-simple-io-do-run-io!-macro"><code>DO-RUN-IO! (&amp;BODY BODY)</code></a> <sup><sub>[MACRO]</sub></sup><a name="io-io-impl-simple-io-do-run-io!-macro"></a>


***

# Package `IO/SIMPLE-IO/LOOPS`<a name="io-simple-io-loops-package"></a>

### Macros

#### <a href="#io-io-impl-simple-io-loops-do-foreach-io-macro"><code>DO-FOREACH-IO ((VAR-SYM INTO-ITR) &amp;BODY BODY)</code></a> <sup><sub>[MACRO]</sub></sup><a name="io-io-impl-simple-io-loops-do-foreach-io-macro"></a>

Efficiently perform an IO operation for each element of an iterator.



***

#### <a href="#io-io-impl-simple-io-loops-do-map-into-io-macro"><code>DO-MAP-INTO-IO ((VAR LST) &amp;BODY BODY)</code></a> <sup><sub>[MACRO]</sub></sup><a name="io-io-impl-simple-io-loops-do-map-into-io-macro"></a>

Efficiently perform an IO operation for each element of an iterator and return the
results.



***

#### <a href="#io-io-impl-simple-io-loops-do-repeat-io-macro"><code>DO-REPEAT-IO (N &amp;BODY BODY)</code></a> <sup><sub>[MACRO]</sub></sup><a name="io-io-impl-simple-io-loops-do-repeat-io-macro"></a>

Efficiently perform an IO operation N times.



***

#### <a href="#io-io-impl-simple-io-loops-do-times-io-macro"><code>DO-TIMES-IO ((COUNT-SYM N) &amp;BODY BODY)</code></a> <sup><sub>[MACRO]</sub></sup><a name="io-io-impl-simple-io-loops-do-times-io-macro"></a>

Efficiently perform an IO operation N times with a counter.



***

#### <a href="#io-io-impl-simple-io-loops-do-until-val-io-macro"><code>DO-UNTIL-VAL-IO (&amp;BODY BODY)</code></a> <sup><sub>[MACRO]</sub></sup><a name="io-io-impl-simple-io-loops-do-until-val-io-macro"></a>

Efficiently run `body` until it produces a value, and return that value.



***

#### <a href="#io-io-impl-simple-io-loops-do-while-io-macro"><code>DO-WHILE-IO (&amp;BODY BODY)</code></a> <sup><sub>[MACRO]</sub></sup><a name="io-io-impl-simple-io-loops-do-while-io-macro"></a>

Efficiently loop an IO operation until it returns `False`.



***

#### <a href="#io-io-impl-simple-io-loops-do-while-val-io-macro"><code>DO-WHILE-VAL-IO ((VAR VAR-OP) &amp;BODY BODY)</code></a> <sup><sub>[MACRO]</sub></sup><a name="io-io-impl-simple-io-loops-do-while-val-io-macro"></a>

Efficiently run `body` with `var` so long as `var-op` produces a value.



***

# Package `IO/RESOURCE`<a name="io-resource-package"></a>

### Types

#### <a href="#io-resource-exitcase-type"><code>ExitCase</code></a> <sup><sub>[TYPE] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/io-resource.lisp#L34-L37">src</a></sub></sup><a name="io-resource-exitcase-type"></a>
- <code>Completed</code> 
- <code>Errored</code> 

Signals the exit condition for an effectful computation using some resource.

<details>
<summary>Instances</summary>

- <code><a href="#coalton-classes-eq-class">Eq</a> <a href="#io-resource-exitcase-type">ExitCase</a></code>
- <code><a href="#coalton-types-runtimerepr-class">RuntimeRepr</a> <a href="#io-resource-exitcase-type">ExitCase</a></code>

</details>



***

### Values

#### <a href="#io-resource-bracket-lifecycle-masked-value"><code>(BRACKET-LIFECYCLE-MASKED ACQUIRE-OP RELEASE-OP COMPUTATION-OP)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/io-resource.lisp#L151-L163">src</a></sub></sup><a name="io-resource-bracket-lifecycle-masked-value"></a>
<code>&forall; :RT :T (:M (Type &rarr; Type)) :R :A :B. (<a href="#io-classes-exceptions-exceptions-class">Exceptions</a> :M) (<a href="#io-classes-thread-threads-class">Threads</a> :RT :T :M) &rArr; :M :R * (:R &rarr; :M :A) * (:R &rarr; :M :B) &rarr; :M :B</code>

Acquire a resource, run a computation with it, and release it. Guarantees that
RELEASE-OP will run if ACQUIRE-OP completes. If COMPUTATION-OP raises an exception,
it will be re-raised after the resource cleans up. If ACQUIRE-OP or RELEASE-OP raise
an exception, then release is not guaranteed.

RELEASE-OP receives only the acquired resource.

Concurrent:
  - Masks the thread during resource acquisition and release.
  - The computation is not masked, but if another thread stops this one during the
    computation then the resource the resource will still be released.



***

#### <a href="#io-resource-bracket-lifecycle-masked-case-value"><code>(BRACKET-LIFECYCLE-MASKED-CASE ACQUIRE-OP RELEASE-OP COMPUTATION-OP)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/io-resource.lisp#L171-L184">src</a></sub></sup><a name="io-resource-bracket-lifecycle-masked-case-value"></a>
<code>&forall; :RT :T (:M (Type &rarr; Type)) :R :A :B. (<a href="#io-classes-exceptions-exceptions-class">Exceptions</a> :M) (<a href="#io-classes-thread-threads-class">Threads</a> :RT :T :M) &rArr; :M :R * (:R * <a href="#io-resource-exitcase-type">ExitCase</a> &rarr; :M :A) * (:R &rarr; :M :B) &rarr; :M :B</code>

Acquire a resource, run a computation with it, and release it. Guarantees that
RELEASE-OP will run if ACQUIRE-OP completes. If COMPUTATION-OP raises an exception,
it will be re-raised after the resource cleans up. If ACQUIRE-OP or RELEASE-OP raise
an exception, then release is not guaranteed.

RELEASE-OP receives both the acquired resource and an ExitCase indicating whether
the computation completed successfully (Completed) or errored (Errored).

Concurrent:
  - Masks the thread during resource acquisition and release.
  - The computation is not masked, but if another thread stops this one during the
    computation then the resource the resource will still be released.



***

#### <a href="#io-resource-bracket-masked-value"><code>(BRACKET-MASKED ACQUIRE-OP RELEASE-OP COMPUTATION-OP)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/io-resource.lisp#L111-L122">src</a></sub></sup><a name="io-resource-bracket-masked-value"></a>
<code>&forall; :RT :T (:M (Type &rarr; Type)) :R :A :B. (<a href="#io-classes-exceptions-exceptions-class">Exceptions</a> :M) (<a href="#io-classes-thread-threads-class">Threads</a> :RT :T :M) &rArr; :M :R * (:R &rarr; :M :A) * (:R &rarr; :M :B) &rarr; :M :B</code>

Acquire a resource, run a computation with it, and release it. Guarantees that RELEASE-OP will run
if ACQUIRE-OP completes. If COMPUTATION-OP raises an exception, it will be re-raised after the
resource cleans up. If ACQUIRE-OP or RELEASE-OP raise an exception, then release is not guaranteed.

The entire sequence is masked, including `computation-op`.

Concurrent:
  - Masks the thread before ACQUIRE-OP starts.
  - Unmasks the thread after RELEASE-OP finishes.



***

#### <a href="#io-resource-bracket-masked-case-value"><code>(BRACKET-MASKED-CASE ACQUIRE-OP RELEASE-OP COMPUTATION-OP)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/io-resource.lisp#L130-L143">src</a></sub></sup><a name="io-resource-bracket-masked-case-value"></a>
<code>&forall; :RT :T (:M (Type &rarr; Type)) :R :A :B. (<a href="#io-classes-exceptions-exceptions-class">Exceptions</a> :M) (<a href="#io-classes-thread-threads-class">Threads</a> :RT :T :M) &rArr; :M :R * (:R * <a href="#io-resource-exitcase-type">ExitCase</a> &rarr; :M :A) * (:R &rarr; :M :B) &rarr; :M :B</code>

Acquire a resource, run a computation with it, and release it. Guarantees that RELEASE-OP will run
if ACQUIRE-OP completes. If COMPUTATION-OP raises an exception, it will be re-raised after the
resource cleans up. If ACQUIRE-OP or RELEASE-OP raise an exception, then release is not guaranteed.
RELEASE-OP receives both the acquired resource and an ExitCase indicating whether
the computation completed successfully (Completed) or errored (Errored).

The entire sequence is masked, including `computation-op`.

Concurrent:
  - Masks the thread before ACQUIRE-OP starts.
  - Unmasks the thread after RELEASE-OP finishes.



***

#### <a href="#io-resource-bracket-unmasked-value"><code>(BRACKET-UNMASKED ACQUIRE-OP RELEASE-OP COMPUTATION-OP)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/io-resource.lisp#L44-L76">src</a></sub></sup><a name="io-resource-bracket-unmasked-value"></a>
<code>&forall; :RT :T (:M (Type &rarr; Type)) :R :A :B. (<a href="#io-classes-exceptions-exceptions-class">Exceptions</a> :M) (<a href="#io-classes-thread-threads-class">Threads</a> :RT :T :M) &rArr; :M :R * (:R &rarr; :M :A) * (:R &rarr; :M :B) &rarr; :M :B</code>

Acquire a resource, run a computation with it, and release it. Guarantees that
`release-op` will run if `computation-op` raises. If `computation-op` raises an exception,
it will be re-raised after the resource cleans up. If `acquire-op` or `release-op` raise
an exception, then release is not guaranteed.

Concurrent:
  - Does not mask.
  - Cannot guarantee resource will be released on an asynchronous stop. If you need to
    guarantee release during an asynchronous stop, use `bracket-lifecycle-masked` or
    `bracket-masked`.



***

#### <a href="#io-resource-bracket-unmasked-case-value"><code>(BRACKET-UNMASKED-CASE ACQUIRE-OP RELEASE-OP COMPUTATION-OP)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/io-resource.lisp#L83-L103">src</a></sub></sup><a name="io-resource-bracket-unmasked-case-value"></a>
<code>&forall; :RT :T (:M (Type &rarr; Type)) :R :A :B. (<a href="#io-classes-exceptions-exceptions-class">Exceptions</a> :M) (<a href="#io-classes-thread-threads-class">Threads</a> :RT :T :M) &rArr; :M :R * (:R * <a href="#io-resource-exitcase-type">ExitCase</a> &rarr; :M :A) * (:R &rarr; :M :B) &rarr; :M :B</code>

Acquire a resource, run a computation with it, and release it. Guarantees that
`release-op` will run if `computation-op` raises. If COMPUTATION-OP raises an exception,
it will be re-raised after the resource cleans up. If `acquire-op` or `release-op` raise
an exception, then release is not guaranteed.

`release-op` receives both the acquired resource and an `ExitCase` indicating whether
the computation completed successfully (Completed) or errored (Errored).

Concurrent:
  - Does not mask.
  - Cannot guarantee resource will be released on an asynchronous stop. If you need to
    guarantee release during an asynchronous stop, use `bracket-lifecycle-masked-case`
    or `bracket-masked-case`.



***

# Package `IO/MUT`<a name="io-mut-package"></a>

### Types

#### <a href="#io-classes-mut-var-type"><code>Var</code></a> <sup><sub>[TYPE] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/classes/mut.lisp#L33-L34">src</a></sub></sup><a name="io-classes-mut-var-type"></a>
<details>
<summary>Instances</summary>

- <code><a href="#coalton-classes-eq-class">Eq</a> (<a href="#coalton-cell-cell-type">Cell</a> :A) &rArr; <a href="#coalton-classes-eq-class">Eq</a> (<a href="#io-classes-mut-var-type">Var</a> :A)</code>
- <code><a href="#coalton-types-runtimerepr-class">RuntimeRepr</a> (<a href="#io-classes-mut-var-type">Var</a> :|0|)</code>

</details>



***

### Classes

#### <a href="#io-classes-mut-mutablevar-class"><code>MutableVar</code></a> <sup><sub>[CLASS] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/classes/mut.lisp#L36-L48">src</a></sub></sup><a name="io-classes-mut-mutablevar-class"></a>
<code><a href="#coalton-classes-monad-class">Monad</a> :M &rArr; <a href="#io-classes-mut-mutablevar-class">MutableVar</a> :M</code>

Methods:
- <code>NEW-VAR :: :A &rarr; :M (<a href="#io-classes-mut-var-type">Var</a> :A)</code><br/>Create a new variable with an initial value.
- <code>READ :: <a href="#io-classes-mut-var-type">Var</a> :A &rarr; :M :A</code><br/>Read the current value stored in a variable.
- <code>WRITE :: <a href="#io-classes-mut-var-type">Var</a> :A * :A &rarr; :M :A</code><br/>Set the value in a variable and return the old value.
- <code>MODIFY :: <a href="#io-classes-mut-var-type">Var</a> :A * (:A &rarr; :A) &rarr; :M :A</code><br/>Modify the value in a variable by applying F, and return the new value.
<details>
<summary>Instances</summary>

- <code><a href="#io-classes-mut-mutablevar-class">MutableVar</a> <a href="#io-io-impl-simple-io-io-type">IO</a></code>
- <code><a href="#io-classes-mut-mutablevar-class">MutableVar</a> :M &rArr; <a href="#io-classes-mut-mutablevar-class">MutableVar</a> (<a href="#coalton-experimental-do-control-loops-adv-loopt-type">LoopT</a> :M)</code>
- <code><a href="#io-classes-mut-mutablevar-class">MutableVar</a> :M &rArr; <a href="#io-classes-mut-mutablevar-class">MutableVar</a> (<a href="#coalton-monad-environment-envt-type">EnvT</a> :E :M)</code>
- <code><a href="#io-classes-mut-mutablevar-class">MutableVar</a> :M &rArr; <a href="#io-classes-mut-mutablevar-class">MutableVar</a> (<a href="#coalton-monad-statet-statet-type">StateT</a> :S :M)</code>

</details>



***

### Macros

#### <a href="#io-classes-mut-derive-mutable-var-macro"><code>DERIVE-MUTABLE-VAR (MONAD-PARAM MONADT-FORM)</code></a> <sup><sub>[MACRO]</sub></sup><a name="io-classes-mut-derive-mutable-var-macro"></a>

Automatically derive an instance of MutableVar for a monad transformer.

Example:
  (derive-mutable-var :m (st:StateT :s :m))



***

#### <a href="#io-gen-impl-mut-implement-mutable-var-macro"><code>IMPLEMENT-MUTABLE-VAR (MONAD)</code></a> <sup><sub>[MACRO]</sub></sup><a name="io-gen-impl-mut-implement-mutable-var-macro"></a>


***

# Package `IO/TERM`<a name="io-term-package"></a>

### Classes

#### <a href="#io-classes-term-terminal-class"><code>Terminal</code></a> <sup><sub>[CLASS] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/classes/terminal.lisp#L24-L33">src</a></sub></sup><a name="io-classes-term-terminal-class"></a>
<code><a href="#io-classes-monad-io-monadio-class">MonadIo</a> :M &rArr; <a href="#io-classes-term-terminal-class">Terminal</a> :M</code>

Methods:
- <code>WRITE :: <a href="#coalton-classes-into-class">Into</a> :A <a href="#coalton-string-type">String</a> &rArr; :A &rarr; :M <a href="#coalton-unit-type">Unit</a></code><br/>Write a string to standard output.
- <code>WRITE-LINE :: <a href="#coalton-classes-into-class">Into</a> :A <a href="#coalton-string-type">String</a> &rArr; :A &rarr; :M <a href="#coalton-unit-type">Unit</a></code><br/>Write a string to standard output followed by a newline.
- <code>READ-LINE :: :M <a href="#coalton-string-type">String</a></code><br/>Read a line from standard input.
<details>
<summary>Instances</summary>

- <code><a href="#io-classes-term-terminal-class">Terminal</a> <a href="#io-io-impl-simple-io-io-type">IO</a></code>
- <code><a href="#io-classes-term-terminal-class">Terminal</a> :M &rArr; <a href="#io-classes-term-terminal-class">Terminal</a> (<a href="#coalton-experimental-do-control-loops-adv-loopt-type">LoopT</a> :M)</code>
- <code><a href="#io-classes-term-terminal-class">Terminal</a> :M &rArr; <a href="#io-classes-term-terminal-class">Terminal</a> (<a href="#coalton-monad-environment-envt-type">EnvT</a> :E :M)</code>
- <code><a href="#io-classes-term-terminal-class">Terminal</a> :M &rArr; <a href="#io-classes-term-terminal-class">Terminal</a> (<a href="#coalton-monad-statet-statet-type">StateT</a> :S :M)</code>

</details>



***

### Macros

#### <a href="#io-classes-term-derive-terminal-macro"><code>DERIVE-TERMINAL (MONAD-PARAM MONADT-FORM)</code></a> <sup><sub>[MACRO]</sub></sup><a name="io-classes-term-derive-terminal-macro"></a>

Automatically derive an instance of Terminal for a monad transformer.

Example:
  (derive-terminal :m (st:StateT :s :m))



***

#### <a href="#io-gen-impl-term-implement-terminal-macro"><code>IMPLEMENT-TERMINAL (MONAD)</code></a> <sup><sub>[MACRO]</sub></sup><a name="io-gen-impl-term-implement-terminal-macro"></a>


***

#### <a href="#io-gen-impl-term-write-format-macro"><code>WRITE-FORMAT (CONTROL-STRING &amp;REST REST)</code></a> <sup><sub>[MACRO]</sub></sup><a name="io-gen-impl-term-write-format-macro"></a>

Write formatted string to the terminal.



***

# Package `IO/RANDOM`<a name="io-random-package"></a>

### Types

#### <a href="#io-classes-random-randomstate-type"><code>RandomState</code></a> <sup><sub>[TYPE] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/classes/random.lisp#L45-L45">src</a></sub></sup><a name="io-classes-random-randomstate-type"></a>
<details>
<summary>Instances</summary>

- <code><a href="#coalton-types-runtimerepr-class">RuntimeRepr</a> <a href="#io-classes-random-randomstate-type">RandomState</a></code>

</details>



***

### Classes

#### <a href="#io-classes-random-random-class"><code>Random</code></a> <sup><sub>[CLASS] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/classes/random.lisp#L47-L65">src</a></sub></sup><a name="io-classes-random-random-class"></a>
<code><a href="#coalton-classes-monad-class">Monad</a> :M &rArr; <a href="#io-classes-random-random-class">Random</a> :M</code>

Methods:
- <code>MAKE-RANDOM-STATE :: :M <a href="#io-classes-random-randomstate-type">RandomState</a></code><br/>Create a fresh random state.
- <code>COPY-RANDOM-STATE :: <a href="#io-classes-random-randomstate-type">RandomState</a> &rarr; :M <a href="#io-classes-random-randomstate-type">RandomState</a></code><br/>Create a copy of another random state, starting at the same seed.
- <code>GET-CURRENT-RANDOM-STATE :: :M <a href="#io-classes-random-randomstate-type">RandomState</a></code><br/>Get the current thread&apos;s random state.
- <code>SET-CURRENT-RANDOM-STATE :: <a href="#io-classes-random-randomstate-type">RandomState</a> &rarr; :M <a href="#coalton-unit-type">Unit</a></code><br/>Set the current thread&apos;s random state.
- <code>RANDOM :: <a href="#io-classes-random-randomlimit-class">RandomLimit</a> :A &rArr; <a href="#io-classes-random-randomstate-type">RandomState</a> * :A &rarr; :M :A</code><br/>Generate a random value less than LIMIT using the given random state.
- <code>RANDOM_ :: <a href="#io-classes-random-randomlimit-class">RandomLimit</a> :A &rArr; :A &rarr; :M :A</code><br/>Generate a random value less than LIMIT using the current random state.
<details>
<summary>Instances</summary>

- <code><a href="#io-classes-random-random-class">Random</a> <a href="#io-io-impl-simple-io-io-type">IO</a></code>
- <code><a href="#io-classes-random-random-class">Random</a> :M &rArr; <a href="#io-classes-random-random-class">Random</a> (<a href="#coalton-experimental-do-control-loops-adv-loopt-type">LoopT</a> :M)</code>
- <code><a href="#io-classes-random-random-class">Random</a> :M &rArr; <a href="#io-classes-random-random-class">Random</a> (<a href="#coalton-monad-environment-envt-type">EnvT</a> :E :M)</code>
- <code><a href="#io-classes-random-random-class">Random</a> :M &rArr; <a href="#io-classes-random-random-class">Random</a> (<a href="#coalton-monad-statet-statet-type">StateT</a> :S :M)</code>

</details>



***

#### <a href="#io-classes-random-randomlimit-class"><code>RandomLimit</code></a> <sup><sub>[CLASS] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/classes/random.lisp#L36-L37">src</a></sub></sup><a name="io-classes-random-randomlimit-class"></a>
<code><a href="#coalton-classes-num-class">Num</a> :A &rArr; <a href="#io-classes-random-randomlimit-class">RandomLimit</a> :A</code>


A number that can be used to bound a random number value.

Methods:
<details>
<summary>Instances</summary>

- <code><a href="#io-classes-random-randomlimit-class">RandomLimit</a> <a href="#coalton-f64-type">F64</a></code>
- <code><a href="#io-classes-random-randomlimit-class">RandomLimit</a> <a href="#coalton-f32-type">F32</a></code>
- <code><a href="#io-classes-random-randomlimit-class">RandomLimit</a> <a href="#coalton-integer-type">Integer</a></code>
- <code><a href="#io-classes-random-randomlimit-class">RandomLimit</a> <a href="#coalton-ufix-type">UFix</a></code>

</details>



***

### Macros

#### <a href="#io-classes-random-derive-random-macro"><code>DERIVE-RANDOM (MONAD-PARAM MONADT-FORM)</code></a> <sup><sub>[MACRO]</sub></sup><a name="io-classes-random-derive-random-macro"></a>

Automatically derive an instance of Random for a monad transformer.

Example:
  (derive-random :m (st:StateT :s :m))



***

#### <a href="#io-gen-impl-random-implement-random-macro"><code>IMPLEMENT-RANDOM (MONAD)</code></a> <sup><sub>[MACRO]</sub></sup><a name="io-gen-impl-random-implement-random-macro"></a>


***

# Package `IO/THREAD`<a name="io-thread-package"></a>

### Types

#### <a href="#io-classes-thread-forkscope-type"><code>ForkScope</code></a> <sup><sub>[TYPE] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/classes/thread.lisp#L131-L139">src</a></sub></sup><a name="io-classes-thread-forkscope-type"></a>

Controls whether a forked thread is attached to a scope, and if so which one.

  - Structured:   Attach to the current thread's scope.
  - StructuredIn: Attach to the provided thread handle's scope.
  - Detached:     Attach to the global scope; the thread will end with the toplevel run!.

<details>
<summary>Instances</summary>

- <code><a href="#coalton-types-runtimerepr-class">RuntimeRepr</a> (<a href="#io-classes-thread-forkscope-type">ForkScope</a> :|0|)</code>

</details>



***

#### <a href="#io-classes-thread-generation-type"><code>Generation</code></a> <sup><sub>[TYPE] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/classes/thread.lisp#L109-L110">src</a></sub></sup><a name="io-classes-thread-generation-type"></a>
<details>
<summary>Instances</summary>

- <code><a href="#coalton-classes-eq-class">Eq</a> <a href="#coalton-u64-type">U64</a> &rArr; <a href="#coalton-classes-eq-class">Eq</a> <a href="#io-classes-thread-generation-type">Generation</a></code>
- <code><a href="#coalton-classes-ord-class">Ord</a> <a href="#io-classes-thread-generation-type">Generation</a></code>
- <code><a href="#coalton-types-runtimerepr-class">RuntimeRepr</a> <a href="#io-classes-thread-generation-type">Generation</a></code>

</details>



***

#### <a href="#io-classes-thread-timeoutstrategy-type"><code>TimeoutStrategy</code></a> <sup><sub>[TYPE] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/classes/thread.lisp#L113-L116">src</a></sub></sup><a name="io-classes-thread-timeoutstrategy-type"></a>

Controls whether blocking IO operations use a timeout in milliseconds.

<details>
<summary>Instances</summary>

- <code><a href="#coalton-classes-eq-class">Eq</a> <a href="#coalton-f64-type">F64</a> &rArr; <a href="#coalton-classes-eq-class">Eq</a> <a href="#io-classes-thread-timeoutstrategy-type">TimeoutStrategy</a></code>
- <code><a href="#coalton-types-runtimerepr-class">RuntimeRepr</a> <a href="#io-classes-thread-timeoutstrategy-type">TimeoutStrategy</a></code>

</details>



***

#### <a href="#io-classes-thread-unhandledexceptionstrategy-type"><code>UnhandledExceptionStrategy</code></a> <sup><sub>[TYPE] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/classes/thread.lisp#L120-L129">src</a></sub></sup><a name="io-classes-thread-unhandledexceptionstrategy-type"></a>

Controls what happens when a forked thread raises an exception that is not a
ThreadingException.

  - ThrowException: Immediately throws the Dynamic value in the child thread.
  - LogAndSwallow:  Logs the exception to *ERROR-OUTPUT* and ignore it until/if joined.
  - Swallow:        Ignore the error until/if joined.

<details>
<summary>Instances</summary>

- <code><a href="#coalton-classes-eq-class">Eq</a> <a href="#io-classes-thread-unhandledexceptionstrategy-type">UnhandledExceptionStrategy</a></code>
- <code><a href="#coalton-types-runtimerepr-class">RuntimeRepr</a> <a href="#io-classes-thread-unhandledexceptionstrategy-type">UnhandledExceptionStrategy</a></code>

</details>



***

#### <a href="#io-threads-exceptions-unmaskfinallymode-type"><code>UnmaskFinallyMode</code></a> <sup><sub>[TYPE] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/thread-exceptions.lisp#L81-L89">src</a></sub></sup><a name="io-threads-exceptions-unmaskfinallymode-type"></a>

When a thread unmasks and calls a cleanup operation, the
thread is either:

* Still running, and will cleanup and continue
* Received a stop while it was masked, and will cleanup and then
  terminate itself.

<details>
<summary>Instances</summary>

- <code><a href="#coalton-classes-eq-class">Eq</a> <a href="#io-threads-exceptions-unmaskfinallymode-type">UnmaskFinallyMode</a></code>
- <code><a href="#coalton-classes-into-class">Into</a> <a href="#io-threads-exceptions-unmaskfinallymode-type">UnmaskFinallyMode</a> <a href="#coalton-string-type">String</a></code>
- <code><a href="#coalton-types-runtimerepr-class">RuntimeRepr</a> <a href="#io-threads-exceptions-unmaskfinallymode-type">UnmaskFinallyMode</a></code>

</details>



***

### Structs

#### <a href="#io-classes-thread-forkstrategy-type"><code>ForkStrategy :T</code></a> <sup><sub>[STRUCT] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/classes/thread.lisp#L141-L144">src</a></sub></sup><a name="io-classes-thread-forkstrategy-type"></a>

Strategy object controlling fork behavior (exception semantics + structured concurrency).

<details>
<summary>Instances</summary>

- <code><a href="#coalton-types-runtimerepr-class">RuntimeRepr</a> (<a href="#io-classes-thread-forkstrategy-type">ForkStrategy</a> :|0|)</code>

</details>



***

#### <a href="#io-io-impl-io-runtime-runtime-iothread-type"><code>IoThread</code></a> <sup><sub>[STRUCT] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/io-impl/io-runtime/runtime.lisp#L155-L171">src</a></sub></sup><a name="io-io-impl-io-runtime-runtime-iothread-type"></a>
<details>
<summary>Instances</summary>

- <code><a href="#io-classes-thread-concurrent-class">Concurrent</a> <a href="#io-io-impl-io-runtime-runtime-iothread-type">IoThread</a> <a href="#coalton-unit-type">Unit</a></code>
- <code><a href="#coalton-classes-eq-class">Eq</a> <a href="#io-io-impl-io-runtime-runtime-iothread-type">IoThread</a></code>
- <code><a href="#io-classes-thread-runtime-class">Runtime</a> <a href="#io-io-impl-runtime-ioruntime-type">IoRuntime</a> <a href="#io-io-impl-io-runtime-runtime-iothread-type">IoThread</a></code>
- <code><a href="#coalton-types-runtimerepr-class">RuntimeRepr</a> <a href="#io-io-impl-io-runtime-runtime-iothread-type">IoThread</a></code>
- <code><a href="#io-classes-thread-threads-class">Threads</a> <a href="#io-io-impl-runtime-ioruntime-type">IoRuntime</a> <a href="#io-io-impl-io-runtime-runtime-iothread-type">IoThread</a> <a href="#io-io-impl-simple-io-io-type">IO</a></code>

</details>



***

### Classes

#### <a href="#io-classes-thread-concurrent-class"><code>Concurrent</code></a> <sup><sub>[CLASS] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/classes/thread.lisp#L274-L307">src</a></sub></sup><a name="io-classes-thread-concurrent-class"></a>
<code><a href="#io-classes-thread-concurrent-class">Concurrent</a> :C :A</code>


A Concurrent has thread-like semantics. It can be stopped, masked, unmasked, and await-ed.
Concurrents don't have a uniform fork function, becasue they require different initialization
input.

Methods:
- <code>STOP :: (<a href="#io-classes-exceptions-exceptions-class">Exceptions</a> :M) (<a href="#io-classes-thread-threads-class">Threads</a> :RT :T :M) &rArr; :C &rarr; :M <a href="#coalton-unit-type">Unit</a></code><br/>Stop a Concurrent. If the Concurrent has already stopped, does nothing. If the Concurrent is&#xA;masked, this will pend a stop on the Concurrent. When/if the Concurrent becomes completely unmaksed,&#xA;it will stop iself. Regardless of whether the target Concurrent is masked, STOP does not block or&#xA;wait for the target to complete.
- <code>AWAIT :: (<a href="#io-classes-exceptions-exceptions-class">Exceptions</a> :M) (<a href="#io-classes-thread-threads-class">Threads</a> :RT :T :M) &rArr; :C &rarr; :M :A</code><br/>Block the current thread until the target Concurrent is completed, and retrieve its value.&#xA;Re-raises if the target Concurrent raised an unhandled exception
- <code>MASK :: (<a href="#io-classes-exceptions-exceptions-class">Exceptions</a> :M) (<a href="#io-classes-thread-threads-class">Threads</a> :RT :T :M) &rArr; :C &rarr; :M <a href="#coalton-unit-type">Unit</a></code><br/>Mask the Concurrent so it can&apos;t be stopped.
- <code>UNMASK :: (<a href="#io-classes-exceptions-exceptions-class">Exceptions</a> :M) (<a href="#io-classes-thread-threads-class">Threads</a> :RT :T :M) &rArr; :C &rarr; :M <a href="#coalton-unit-type">Unit</a></code><br/>Unmask the Concurrent so it can be stopped. Unmask respects nested masks - if the&#xA;Concurrent has been masked N times, it can only be stopped after being unmasked N times. When the&#xA;Concurrent unmasks, if there are any pending stops, it will immediately stop itself.
- <code>UNMASK-FINALLY :: (<a href="#io-classes-monad-io-unliftio-class">UnliftIo</a> :R :IO) (<a href="#coalton-monad-classes-liftto-class">LiftTo</a> :R :M) (<a href="#io-classes-thread-threads-class">Threads</a> :RT :T :R) (<a href="#io-classes-exceptions-exceptions-class">Exceptions</a> :M) (<a href="#io-classes-thread-threads-class">Threads</a> :RT :T :M) &rArr; :C * (<a href="#io-threads-exceptions-unmaskfinallymode-type">UnmaskFinallyMode</a> &rarr; :R <a href="#coalton-unit-type">Unit</a>) &rarr; :M <a href="#coalton-unit-type">Unit</a></code><br/>Unmask the thread, run the provided action, and then honor any pending stop for that&#xA;thread after the action finishes.&#xA;&#xA;Warning: There is a very small chance that the UnmaskFinallyMode passed to the callback could be&#xA;inconsistent with whether the Concurrent is ultimately stopped. Regardless of the input, the&#xA;callback should leave any resources in a valid state. An example of a valid callback: closing a log&#xA;file if the thread is stopped, or closing the log file with a final message if the thread is&#xA;continuing.
<details>
<summary>Instances</summary>

- <code><a href="#io-classes-thread-concurrent-class">Concurrent</a> (<a href="#io-io-impl-io-runtime-delay-iodelay-type">IoDelay</a> :A) :A</code>
- <code><a href="#io-classes-thread-concurrent-class">Concurrent</a> <a href="#io-io-impl-io-runtime-runtime-iothread-type">IoThread</a> <a href="#coalton-unit-type">Unit</a></code>
- <code>(<a href="#io-classes-thread-concurrent-class">Concurrent</a> (<a href="#io-gen-impl-conc-group-concurrentgroup-type">ConcurrentGroup</a> :T <a href="#coalton-unit-type">Unit</a>) (<a href="#coalton-list-type">List</a> <a href="#coalton-unit-type">Unit</a>)) (<a href="#io-classes-conc-scheduler-scheduler-class">Scheduler</a> :S) &rArr; <a href="#io-classes-thread-concurrent-class">Concurrent</a> (<a href="#io-gen-impl-conc-worker-pool-workerpool-type">WorkerPool</a> :S :I :T) <a href="#coalton-unit-type">Unit</a></code>
- <code><a href="#io-classes-thread-concurrent-class">Concurrent</a> :C :A &rArr; <a href="#io-classes-thread-concurrent-class">Concurrent</a> (<a href="#io-gen-impl-conc-group-concurrentgroup-type">ConcurrentGroup</a> :C :A) (<a href="#coalton-list-type">List</a> :A)</code>

</details>



***

#### <a href="#io-classes-thread-runtime-class"><code>Runtime</code></a> <sup><sub>[CLASS] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/classes/thread.lisp#L151-L272">src</a></sub></sup><a name="io-classes-thread-runtime-class"></a>
<code><a href="#io-classes-thread-runtime-class">Runtime</a> :R :T</code>


This class doesn't represent data, but the type tells a Concurrent and
a Threads how to hook into the native threading implementations that
a runtime provides.  A runtime has a 'base' concurrent, which is the underlying
thread/fiber/etc. that the runtime produces to run concurrently. All other
Concurrents are built by composing on the base concurrent somehow.

Runtime is a low-level type that operates inside the normal MonadIo layer.
It should not be used by normal application code. Its two main purposes are:
(1) to make Threads generic over the type of thread it forks, and
(2) to build low-level, efficient concurrency tools that are generic
over the underlying thread type.

Methods:
- <code>CURRENT-THREAD! :: <a href="#coalton-types-proxy-type">Proxy</a> :R &rarr; :T</code><br/>Get a handle for the current thread.
- <code>IS-THREAD? :: <a href="#coalton-types-proxy-type">Proxy</a> :R * <a href="#io-utils-anything-type">Anything</a> &rarr; <a href="#coalton-boolean-type">Boolean</a></code><br/>Check if a Common Lisp object is a thread.
- <code>SLEEP! :: <a href="#coalton-types-proxy-type">Proxy</a> :R * <a href="#coalton-ufix-type">UFix</a> &rarr; Void</code><br/>Sleep the current thread for MSECS milliseconds.
- <code>FORK! :: <a href="#coalton-types-proxy-type">Proxy</a> :R * <a href="#io-classes-thread-forkstrategy-type">ForkStrategy</a> :T * (Void &rarr; :A) &rarr; :T</code><br/>Spawn a new thread, which starts running immediately. Returns the handle to the&#xA;thread. The runtime is responsible for catching any errors `thunk` raises.&#xA;&#xA;The ForkStrategy controls both:&#xA;  - how unhandled exceptions behave, and&#xA;  - whether the fork is structured (and which thread&apos;s scope owns it).
- <code>JOIN! :: <a href="#coalton-types-proxy-type">Proxy</a> :R * :T &rarr; <a href="#coalton-classes-result-type">Result</a> <a href="#io-utils-dynamic-type">Dynamic</a> <a href="#coalton-unit-type">Unit</a></code><br/>Block the current thread until the target thread is completed.&#xA;Does not a retrieve value. Raises an exception if the target thread&#xA;raised an unhandled exception, wrapping the target thread&apos;s raised&#xA;exception. JOIN! is the lowest level operation to block on another&#xA;thread&apos;s termination, and most code should use AWAIT instead.
- <code>STOP! :: <a href="#coalton-types-proxy-type">Proxy</a> :R * :T &rarr; Void</code><br/>Stop a :t. If the thread has already stopped, does nothing.&#xA;If the :t is masked, this will pend a stop on the :t. When/if&#xA;the :t becomes completely unmaksed, it will stop iself. Regardless&#xA;of whether the target :t is masked, STOP does not block or wait for&#xA;the target thread to complete.
- <code>MASK! :: <a href="#coalton-types-proxy-type">Proxy</a> :R * :T &rarr; Void</code><br/>Mask the thread so it can&apos;t be stopped.
- <code>MASK-CURRENT! :: <a href="#coalton-types-proxy-type">Proxy</a> :R &rarr; Void</code><br/>Mask the current thread.
- <code>UNMASK! :: <a href="#coalton-types-proxy-type">Proxy</a> :R * :T &rarr; Void</code><br/>Unmask the thread so it can be stopped. Unmask respects&#xA;nested masks - if the thread has been masked N times, it can only be&#xA;stopped after being unmasked N times. When the thread unmasks, if&#xA;there are any pending stops, it will immediately be stopped.
- <code>UNMASK-CURRENT! :: <a href="#coalton-types-proxy-type">Proxy</a> :R &rarr; Void</code><br/>Unmask the current thread.
- <code>UNMASK-FINALLY! :: <a href="#coalton-types-proxy-type">Proxy</a> :R * :T * (<a href="#io-threads-exceptions-unmaskfinallymode-type">UnmaskFinallyMode</a> &rarr; Void) &rarr; Void</code><br/>Unmask the thread, run the provided action, and then honor any pending stop for that&#xA;thread after the action finishes.&#xA;&#xA;Warning: There is a very small chance that the UnmaskFinallyMode passed to the callback could be&#xA;inconsistent with whether the Concurrent is ultimately stopped. Regardless of the input, the&#xA;callback should leave any resources in a valid state. An example of a valid callback: closing a log&#xA;file if the thread is stopped, or closing the log file with a final message if the thread is&#xA;continuing.
- <code>PARK-CURRENT-THREAD-IF! :: <a href="#coalton-types-proxy-type">Proxy</a> :R * (<a href="#io-classes-thread-generation-type">Generation</a> &rarr; Void) * (Void &rarr; <a href="#coalton-boolean-type">Boolean</a>) &key (:timeout <a href="#io-classes-thread-timeoutstrategy-type">TimeoutStrategy</a>) &rarr; Void</code><br/>Parks the current thread if SHOULD-PARK? returns True. Will park the thread until&#xA;woken by an unpark from another thread. Upon an unpark, the thread will resume even if&#xA;SHOULD-PARK? is False! SHOULD-PARK? is only checked to determine if the thread should&#xA;park, *not* if it should resume. Can specify a timeout.&#xA;&#xA;Concurrent:&#xA;  - WARNING: SHOULD-PARK? must not block, or the thread could be left blocked in a masked&#xA;    state.&#xA;  - Can briefly block while trying to park the thread, if contended.
- <code>PARK-CURRENT-THREAD-IF-MASKED! :: <a href="#coalton-types-proxy-type">Proxy</a> :R * (<a href="#io-classes-thread-generation-type">Generation</a> &rarr; Void) * (Void &rarr; <a href="#coalton-boolean-type">Boolean</a>) &key (:timeout <a href="#io-classes-thread-timeoutstrategy-type">TimeoutStrategy</a>) &rarr; Void</code><br/>Parks the current thread if SHOULD-PARK? returns True. Will park the thread until&#xA;woken by an unpark from another thread. Upon an unpark, the thread will resume even if&#xA;SHOULD-PARK? is False! SHOULD-PARK? is only checked to determine if the thread should&#xA;park, *not* if it should resume. Can specify a timeout.&#xA;&#xA;Must be masked when entering! This is a low-level function meant for optimizing specific&#xA;low-level algorithms.&#xA;&#xA;Concurrent:&#xA;  - WARNING: Assumes there is a mask already set on the function. Will leave the mask state&#xA;    the same when it exits as when it started. Will pop one mask level when/if the thread&#xA;    waits for a park.&#xA;  - WARNING: SHOULD-PARK? must not block, or the thread could be left blocked in a masked&#xA;    state.&#xA;  - Can briefly block while trying to park the thread, if contended.
- <code>UNPARK-THREAD-MASKED! :: <a href="#coalton-types-proxy-type">Proxy</a> :R * <a href="#io-classes-thread-generation-type">Generation</a> * :T &rarr; <a href="#coalton-boolean-type">Boolean</a></code><br/>Unparks the thread if it is still waiting on the generation. Attempting to unpark&#xA;the thread with a stale generation has no effect. A generation will be stale if the thread&#xA;has unparked and re-parked since the initial park. Returns `True` if unparked, `False` if&#xA;parking was stale.&#xA;&#xA;Must be masked when entering! This is a low-level function meant for optimizing specific&#xA;low-level algorithms.&#xA;&#xA;Concurrent:&#xA;  - Can briefly block while trying to unpark the thread, if contended.
- <code>UNPARK-THREAD! :: <a href="#coalton-types-proxy-type">Proxy</a> :R * <a href="#io-classes-thread-generation-type">Generation</a> * :T &rarr; <a href="#coalton-boolean-type">Boolean</a></code><br/>Unparks the thread if it is still waiting on the generation. Attempting to unpark&#xA;the thread with a stale generation has no effect. A generation will be stale if the thread&#xA;has unparked and re-parked since the initial park. Returns `True` if unparked, `False` if&#xA;parking was stale.&#xA;&#xA;Concurrent:&#xA;  - Can briefly block while trying to unpark the thread, if contended.
<details>
<summary>Instances</summary>

- <code><a href="#io-classes-thread-runtime-class">Runtime</a> <a href="#io-io-impl-runtime-ioruntime-type">IoRuntime</a> <a href="#io-io-impl-io-runtime-runtime-iothread-type">IoThread</a></code>

</details>



***

#### <a href="#io-classes-thread-threads-class"><code>Threads</code></a> <sup><sub>[CLASS] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/classes/thread.lisp#L319-L322">src</a></sub></sup><a name="io-classes-thread-threads-class"></a>
<code>(<a href="#io-classes-monad-io-monadio-class">MonadIo</a> :M) (<a href="#io-classes-thread-runtime-class">Runtime</a> :RT :T) &rArr; <a href="#io-classes-thread-threads-class">Threads</a> :RT :T :M</code>


A MonadIo which can spawn :t's. Other :t's error separately. A spawned :t erroring
will not cause the parent :t to fail. :t can be any 'thread-like' object, depending on
the underlying implementation - system threads, software-managed green threads, etc.

Methods:
<details>
<summary>Instances</summary>

- <code><a href="#io-classes-thread-threads-class">Threads</a> <a href="#io-io-impl-runtime-ioruntime-type">IoRuntime</a> <a href="#io-io-impl-io-runtime-runtime-iothread-type">IoThread</a> <a href="#io-io-impl-simple-io-io-type">IO</a></code>
- <code><a href="#io-classes-thread-threads-class">Threads</a> :RUNTIME :THREAD :M &rArr; <a href="#io-classes-thread-threads-class">Threads</a> :RUNTIME :THREAD (<a href="#coalton-experimental-do-control-loops-adv-loopt-type">LoopT</a> :M)</code>
- <code><a href="#io-classes-thread-threads-class">Threads</a> :RUNTIME :THREAD :M &rArr; <a href="#io-classes-thread-threads-class">Threads</a> :RUNTIME :THREAD (<a href="#coalton-monad-environment-envt-type">EnvT</a> :E :M)</code>
- <code><a href="#io-classes-thread-threads-class">Threads</a> :RUNTIME :THREAD :M &rArr; <a href="#io-classes-thread-threads-class">Threads</a> :RUNTIME :THREAD (<a href="#coalton-monad-statet-statet-type">StateT</a> :S :M)</code>

</details>



***

### Values

#### <a href="#io-classes-thread-fork-thread-value"><code>(FORK-THREAD OP)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/classes/thread.lisp#L400-L414">src</a></sub></sup><a name="io-classes-thread-fork-thread-value"></a>
<code>&forall; (:I (Type &rarr; Type)) :RT (:R (Type &rarr; Type)) :A :T (:M (Type &rarr; Type)). (<a href="#io-classes-monad-io-unliftio-class">UnliftIo</a> :R :I) (<a href="#coalton-monad-classes-liftto-class">LiftTo</a> :R :M) (<a href="#io-classes-thread-threads-class">Threads</a> :RT :T :R) &rArr; :R :A &key (:scope <a href="#io-classes-thread-forkscope-type">ForkScope</a> :T) (:unhandled <a href="#io-classes-thread-unhandledexceptionstrategy-type">UnhandledExceptionStrategy</a>) &rarr; :M :T</code>

Spawn a new thread, which starts running immediately. Returns
the handle to the thread. Can specify an unhandled exception strategy and fork scope.

If the thread raises an unhandled exception, the default behavior is to log it to
*ERROR-OUTPUT* and swallow it until/if the thread is joined. The default fork scope is
structured.



***

#### <a href="#io-io-impl-thread-fork-thread_-value"><code>(FORK-THREAD_ OP)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/io-impl/io-thread.lisp#L35-L37">src</a></sub></sup><a name="io-io-impl-thread-fork-thread_-value"></a>
<code>&forall; :A (:M (Type &rarr; Type)). (<a href="#io-classes-thread-threads-class">Threads</a> <a href="#io-io-impl-runtime-ioruntime-type">IoRuntime</a> <a href="#io-io-impl-io-runtime-runtime-iothread-type">IoThread</a> :M) (<a href="#coalton-monad-classes-liftto-class">LiftTo</a> <a href="#io-io-impl-simple-io-io-type">IO</a> :M) &rArr; <a href="#io-io-impl-simple-io-io-type">IO</a> :A &key (:scope <a href="#io-classes-thread-forkscope-type">ForkScope</a> <a href="#io-io-impl-io-runtime-runtime-iothread-type">IoThread</a>) (:unhandled <a href="#io-classes-thread-unhandledexceptionstrategy-type">UnhandledExceptionStrategy</a>) &rarr; :M <a href="#io-io-impl-io-runtime-runtime-iothread-type">IoThread</a></code>

Spawn a new IoThread. Can specify an unhandled exception strategy and fork scope.



***

#### <a href="#io-classes-thread-join-thread-value"><code>(JOIN-THREAD THREAD)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/classes/thread.lisp#L418-L432">src</a></sub></sup><a name="io-classes-thread-join-thread-value"></a>
<code>&forall; :RT :T (:M (Type &rarr; Type)). (<a href="#io-classes-thread-threads-class">Threads</a> :RT :T :M) (<a href="#io-classes-exceptions-exceptions-class">Exceptions</a> :M) &rArr; :T &rarr; :M <a href="#coalton-unit-type">Unit</a></code>

Block the current thread until the target thread is completed.
Does not a retrieve value. Raises an exception if the target thread
raised an unhandled exception, wrapping the target thread's raised
exception. JOIN-THREAD is the lowest level operation to block on another
thread's termination.



***

#### <a href="#io-classes-thread-mask-thread-value"><code>(MASK-THREAD THREAD)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/classes/thread.lisp#L454-L456">src</a></sub></sup><a name="io-classes-thread-mask-thread-value"></a>
<code>&forall; :RT :T (:M (Type &rarr; Type)). <a href="#io-classes-thread-threads-class">Threads</a> :RT :T :M &rArr; :T &rarr; :M <a href="#coalton-unit-type">Unit</a></code>

Mask the thread so it can't be stopped.



***

#### <a href="#io-classes-thread-park-current-thread-if-value"><code>(PARK-CURRENT-THREAD-IF WITH-GEN SHOULD-PARK?)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/classes/thread.lisp#L548-L567">src</a></sub></sup><a name="io-classes-thread-park-current-thread-if-value"></a>
<code>&forall; :RT :T (:IO (Type &rarr; Type)) (:M (Type &rarr; Type)). (<a href="#io-classes-monad-io-baseio-class">BaseIo</a> :IO) (<a href="#io-classes-thread-threads-class">Threads</a> :RT :T :IO) (<a href="#io-classes-monad-io-monadio-class">MonadIo</a> :M) &rArr; (<a href="#io-classes-thread-generation-type">Generation</a> &rarr; :IO <a href="#coalton-unit-type">Unit</a>) * :IO <a href="#coalton-boolean-type">Boolean</a> &key (:timeout <a href="#io-classes-thread-timeoutstrategy-type">TimeoutStrategy</a>) &rarr; :M <a href="#coalton-unit-type">Unit</a></code>

Parks the current thread if SHOULD-PARK? returns True. Will park the thread until
woken by an unpark from another thread. Upon an unpark, the thread will resume even if
SHOULD-PARK? is False! SHOULD-PARK? is only checked to determine if the thread should
park, *not* if it should resume. Can specify a timeout.

Concurrent:
  - WARNING: SHOULD-PARK? must not block, or the thread could be left blocked in a masked
    state.
  - Can briefly block while trying to park the thread, if contended.



***

#### <a href="#io-io-impl-thread-park-current-thread-if_-value"><code>(PARK-CURRENT-THREAD-IF_)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/io-impl/io-thread.lisp#L56-L66">src</a></sub></sup><a name="io-io-impl-thread-park-current-thread-if_-value"></a>
<code>&forall; (:M (Type &rarr; Type)). <a href="#io-classes-monad-io-monadio-class">MonadIo</a> :M &rArr; (<a href="#io-classes-thread-generation-type">Generation</a> &rarr; <a href="#io-io-impl-simple-io-io-type">IO</a> <a href="#coalton-unit-type">Unit</a>) * <a href="#io-io-impl-simple-io-io-type">IO</a> <a href="#coalton-boolean-type">Boolean</a> &key (:timeout <a href="#io-classes-thread-timeoutstrategy-type">TimeoutStrategy</a>) &rarr; :M <a href="#coalton-unit-type">Unit</a></code>

Parks the current thread if SHOULD-PARK? returns True. Will park the thread until
woken by an unpark from another thread. Upon an unpark, the thread will resume even if
SHOULD-PARK? is False! SHOULD-PARK? is only checked to determine if the thread should
park, *not* if it should resume. Can specify a timeout.

Concurrent:
  - WARNING: SHOULD-PARK? must not block, or the thread could be left blocked in a masked
    state.
  - Can briefly block while trying to park the thread, if contended.



***

#### <a href="#io-classes-thread-sleep-value"><code>(SLEEP MSEC)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/classes/thread.lisp#L442-L444">src</a></sub></sup><a name="io-classes-thread-sleep-value"></a>
<code>&forall; :RT :T (:M (Type &rarr; Type)). <a href="#io-classes-thread-threads-class">Threads</a> :RT :T :M &rArr; <a href="#coalton-ufix-type">UFix</a> &rarr; :M <a href="#coalton-unit-type">Unit</a></code>

Sleep the current thread for MSECS milliseconds.



***

#### <a href="#io-classes-thread-stop-thread-value"><code>(STOP-THREAD THREAD)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/classes/thread.lisp#L436-L438">src</a></sub></sup><a name="io-classes-thread-stop-thread-value"></a>
<code>&forall; :RT :T (:M (Type &rarr; Type)). <a href="#io-classes-thread-threads-class">Threads</a> :RT :T :M &rArr; :T &rarr; :M <a href="#coalton-unit-type">Unit</a></code>

Stop a thread. If the thread has already stopped, does nothing.



***

#### <a href="#io-classes-thread-unmask-current-thread-finally-value"><code>(UNMASK-CURRENT-THREAD-FINALLY FINALLY-OP)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/classes/thread.lisp#L518-L538">src</a></sub></sup><a name="io-classes-thread-unmask-current-thread-finally-value"></a>
<code>&forall; (:IO (Type &rarr; Type)) :RT :T (:R (Type &rarr; Type)) (:M (Type &rarr; Type)). (<a href="#io-classes-monad-io-unliftio-class">UnliftIo</a> :R :IO) (<a href="#coalton-monad-classes-liftto-class">LiftTo</a> :R :M) (<a href="#io-classes-thread-threads-class">Threads</a> :RT :T :R) &rArr; (<a href="#io-threads-exceptions-unmaskfinallymode-type">UnmaskFinallyMode</a> &rarr; :R <a href="#coalton-unit-type">Unit</a>) &rarr; :M <a href="#coalton-unit-type">Unit</a></code>

Unmask the current thread, run the provided action, and then honor any pending stop
for that thread after the action finishes.

Warning: There is a very small chance that the UnmaskFinallyMode passed to the callback
could be inconsistent with whether the Concurrent is ultimately stopped. Regardless of the
input, the callback should leave any resources in a valid state.

An example of a valid callback: closing a log file if the thread is stopped, or closing
the log file with a final message if the thread is continuing.



***

#### <a href="#io-io-impl-thread-unmask-finally_-value"><code>(UNMASK-FINALLY_)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/io-impl/io-thread.lisp#L49-L49">src</a></sub></sup><a name="io-io-impl-thread-unmask-finally_-value"></a>
<code>&forall; :A :C (:M (Type &rarr; Type)). (<a href="#coalton-monad-classes-liftto-class">LiftTo</a> <a href="#io-io-impl-simple-io-io-type">IO</a> :M) (<a href="#io-classes-exceptions-exceptions-class">Exceptions</a> :M) (<a href="#io-classes-thread-concurrent-class">Concurrent</a> :C :A) (<a href="#io-classes-thread-threads-class">Threads</a> <a href="#io-io-impl-runtime-ioruntime-type">IoRuntime</a> <a href="#io-io-impl-io-runtime-runtime-iothread-type">IoThread</a> :M) &rArr; :C * (<a href="#io-threads-exceptions-unmaskfinallymode-type">UnmaskFinallyMode</a> &rarr; <a href="#io-io-impl-simple-io-io-type">IO</a> <a href="#coalton-unit-type">Unit</a>) &rarr; :M <a href="#coalton-unit-type">Unit</a></code>


***

#### <a href="#io-classes-thread-unmask-thread-value"><code>(UNMASK-THREAD THREAD)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/classes/thread.lisp#L472-L476">src</a></sub></sup><a name="io-classes-thread-unmask-thread-value"></a>
<code>&forall; :RT :T (:M (Type &rarr; Type)). <a href="#io-classes-thread-threads-class">Threads</a> :RT :T :M &rArr; :T &rarr; :M <a href="#coalton-unit-type">Unit</a></code>

Unmask the thread so it can be stopped. Unmask respects
nested masks - if the thread has been masked N times, it can only be
stopped after being unmasked N times.



***

#### <a href="#io-classes-thread-unmask-thread-finally-value"><code>(UNMASK-THREAD-FINALLY THREAD FINALLY-OP)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/classes/thread.lisp#L494-L511">src</a></sub></sup><a name="io-classes-thread-unmask-thread-finally-value"></a>
<code>&forall; (:IO (Type &rarr; Type)) :RT :T (:R (Type &rarr; Type)) (:M (Type &rarr; Type)). (<a href="#io-classes-monad-io-unliftio-class">UnliftIo</a> :R :IO) (<a href="#coalton-monad-classes-liftto-class">LiftTo</a> :R :M) (<a href="#io-classes-thread-threads-class">Threads</a> :RT :T :R) &rArr; :T * (<a href="#io-threads-exceptions-unmaskfinallymode-type">UnmaskFinallyMode</a> &rarr; :R <a href="#coalton-unit-type">Unit</a>) &rarr; :M <a href="#coalton-unit-type">Unit</a></code>

Unmask the thread, run the provided action, and then honor any
 pending stop for that thread after the action finishes.

Warning: There is a very small chance that the UnmaskFinallyMode passed to the callback could be
inconsistent with whether the Concurrent is ultimately stopped. Regardless of the input, the
callback should leave any resources in a valid state. An example of a valid callback: closing a log
file if the thread is stopped, or closing the log file with a final message if the thread is
continuing.



***

#### <a href="#io-io-impl-thread-unmask-thread-finally_-value"><code>(UNMASK-THREAD-FINALLY_)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/io-impl/io-thread.lisp#L43-L43">src</a></sub></sup><a name="io-io-impl-thread-unmask-thread-finally_-value"></a>
<code>&forall; (:M (Type &rarr; Type)). (<a href="#coalton-monad-classes-liftto-class">LiftTo</a> <a href="#io-io-impl-simple-io-io-type">IO</a> :M) (<a href="#io-classes-exceptions-exceptions-class">Exceptions</a> :M) (<a href="#io-classes-thread-threads-class">Threads</a> <a href="#io-io-impl-runtime-ioruntime-type">IoRuntime</a> <a href="#io-io-impl-io-runtime-runtime-iothread-type">IoThread</a> :M) &rArr; <a href="#io-io-impl-io-runtime-runtime-iothread-type">IoThread</a> * (<a href="#io-threads-exceptions-unmaskfinallymode-type">UnmaskFinallyMode</a> &rarr; <a href="#io-io-impl-simple-io-io-type">IO</a> <a href="#coalton-unit-type">Unit</a>) &rarr; :M <a href="#coalton-unit-type">Unit</a></code>


***

#### <a href="#io-classes-thread-unpark-thread-value"><code>(UNPARK-THREAD GEN THREAD)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/classes/thread.lisp#L571-L580">src</a></sub></sup><a name="io-classes-thread-unpark-thread-value"></a>
<code>&forall; :RT :T (:M (Type &rarr; Type)). <a href="#io-classes-thread-threads-class">Threads</a> :RT :T :M &rArr; <a href="#io-classes-thread-generation-type">Generation</a> * :T &rarr; :M <a href="#coalton-boolean-type">Boolean</a></code>

Unparks the thread if it is still waiting on the generation. Attempting to unpark
the thread with a stale generation has no effect. A generation will be stale if the thread
has unparked and re-parked since the initial park. Returns `True` if unparked, `False` if
the parking was stale.

Concurrent:
  - Can briefly block while trying to unpark the thread, if contended.



***

#### <a href="#io-gen-impl-thread-with-mask-value"><code>(WITH-MASK OP)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/io-thread.lisp#L48-L58">src</a></sub></sup><a name="io-gen-impl-thread-with-mask-value"></a>
<code>&forall; :RT :T (:M (Type &rarr; Type)) :A. (<a href="#io-classes-thread-threads-class">Threads</a> :RT :T :M) (<a href="#io-classes-exceptions-exceptions-class">Exceptions</a> :M) &rArr; :M :A &rarr; :M :A</code>

Mask the current thread while running OP, unmasking afterward.



***

#### <a href="#io-gen-impl-thread-with-unmask-value"><code>(WITH-UNMASK OP)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/io-thread.lisp#L63-L69">src</a></sub></sup><a name="io-gen-impl-thread-with-unmask-value"></a>
<code>&forall; :RT :T (:M (Type &rarr; Type)) :A. (<a href="#io-classes-thread-threads-class">Threads</a> :RT :T :M) (<a href="#io-classes-exceptions-exceptions-class">Exceptions</a> :M) &rArr; :M :A &rarr; :M :A</code>

Unmask the current thread once while running OP, masking afterward.



***

#### <a href="#io-gen-impl-thread-write-line-sync-value"><code>(WRITE-LINE-SYNC MSG)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/io-thread.lisp#L33-L43">src</a></sub></sup><a name="io-gen-impl-thread-write-line-sync-value"></a>
<code>&forall; :S (:M (Type &rarr; Type)). (<a href="#coalton-classes-into-class">Into</a> :S <a href="#coalton-string-type">String</a>) (<a href="#io-classes-term-terminal-class">Terminal</a> :M) &rArr; :S &rarr; :M <a href="#coalton-unit-type">Unit</a></code>

Perform a synchrozied write-line to the terminal. Not performant - mainly useful
for debugging.



***

#### <a href="#io-classes-thread-current-thread-value"><code>CURRENT-THREAD</code></a> <sup><sub>[VALUE] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/classes/thread.lisp#L448-L450">src</a></sub></sup><a name="io-classes-thread-current-thread-value"></a>
<code>&forall; :RT (:M (Type &rarr; Type)) :T. <a href="#io-classes-thread-threads-class">Threads</a> :RT :T :M &rArr; :M :T</code>

Get the current thread.



***

#### <a href="#io-classes-thread-mask-current-thread-value"><code>MASK-CURRENT-THREAD</code></a> <sup><sub>[VALUE] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/classes/thread.lisp#L460-L468">src</a></sub></sup><a name="io-classes-thread-mask-current-thread-value"></a>
<code>&forall; :RT :T (:M (Type &rarr; Type)). <a href="#io-classes-thread-threads-class">Threads</a> :RT :T :M &rArr; :M <a href="#coalton-unit-type">Unit</a></code>

Mask the current thread so it can't be stopped.



***

#### <a href="#io-classes-thread-unmask-current-thread-value"><code>UNMASK-CURRENT-THREAD</code></a> <sup><sub>[VALUE] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/classes/thread.lisp#L480-L489">src</a></sub></sup><a name="io-classes-thread-unmask-current-thread-value"></a>
<code>&forall; :RT :T (:M (Type &rarr; Type)). <a href="#io-classes-thread-threads-class">Threads</a> :RT :T :M &rArr; :M <a href="#coalton-unit-type">Unit</a></code>

Unmask the current thread so it can be stopped. Unmask respects
nested masks - if the thread has been masked N times, it can only be
stopped after being unmasked N times.



***

### Macros

#### <a href="#io-classes-thread-derive-threads-macro"><code>DERIVE-THREADS (MONAD-PARAM MONADT-FORM)</code></a> <sup><sub>[MACRO]</sub></sup><a name="io-classes-thread-derive-threads-macro"></a>

Automatically derive an instance of Threads for a monad transformer.

Example:
  (derive-threads :m (st:StateT :s :m))



***

#### <a href="#io-classes-thread-do-fork-thread-macro"><code>DO-FORK-THREAD (&amp;BODY FORMS)</code></a> <sup><sub>[MACRO]</sub></sup><a name="io-classes-thread-do-fork-thread-macro"></a>


***

#### <a href="#io-io-impl-thread-do-fork-thread_-macro"><code>DO-FORK-THREAD_ (&amp;BODY FORMS)</code></a> <sup><sub>[MACRO]</sub></sup><a name="io-io-impl-thread-do-fork-thread_-macro"></a>


***

#### <a href="#io-gen-impl-thread-do-with-mask-macro"><code>DO-WITH-MASK (&amp;BODY BODY)</code></a> <sup><sub>[MACRO]</sub></sup><a name="io-gen-impl-thread-do-with-mask-macro"></a>

Evaluate BODY with the current thread masked, automatically unmasking
afterward.



***

#### <a href="#io-gen-impl-thread-do-with-unmask-macro"><code>DO-WITH-UNMASK (&amp;BODY BODY)</code></a> <sup><sub>[MACRO]</sub></sup><a name="io-gen-impl-thread-do-with-unmask-macro"></a>

Evaluate BODY with the current thread unmasked once, automatically masking
afterward.



***

# Package `IO/DELAY`<a name="io-delay-package"></a>

### Types

#### <a href="#io-io-impl-io-runtime-delay-iodelay-type"><code>IoDelay</code></a> <sup><sub>[TYPE] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/io-impl/io-runtime/delay.lisp#L30-L31">src</a></sub></sup><a name="io-io-impl-io-runtime-delay-iodelay-type"></a>
<details>
<summary>Instances</summary>

- <code><a href="#io-classes-thread-concurrent-class">Concurrent</a> (<a href="#io-io-impl-io-runtime-delay-iodelay-type">IoDelay</a> :A) :A</code>
- <code><a href="#io-classes-delay-delayruntime-class">DelayRuntime</a> <a href="#io-io-impl-runtime-ioruntime-type">IoRuntime</a> <a href="#io-io-impl-io-runtime-delay-iodelay-type">IoDelay</a></code>
- <code><a href="#coalton-types-runtimerepr-class">RuntimeRepr</a> (<a href="#io-io-impl-io-runtime-delay-iodelay-type">IoDelay</a> :|0|)</code>

</details>



***

### Classes

#### <a href="#io-classes-delay-delayruntime-class"><code>DelayRuntime</code></a> <sup><sub>[CLASS] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/classes/delay.lisp#L31-L47">src</a></sub></sup><a name="io-classes-delay-delayruntime-class"></a>
<code><a href="#io-classes-thread-runtime-class">Runtime</a> :R :T &rArr; <a href="#io-classes-delay-delayruntime-class">DelayRuntime</a> :R :D</code>


A runtime that can fork a delayed computation onto another thread and await the
result of that computation later.

Runtime is a low-level type that operates inside the normal MonadIo layer.
It should not be used by normal application code. Its two main purposes are:
(1) to make Delays generic over the type of thread it forks, and
(2) to build low-level, efficient concurrency tools that are generic
over the underlying thread type.

Methods:
- <code>FORK-DELAY! :: <a href="#coalton-types-proxy-type">Proxy</a> :R * <a href="#io-classes-thread-forkstrategy-type">ForkStrategy</a> :T * (Void &rarr; :A) &rarr; :D :A</code><br/>Spawn a new thread to calculate a value, which starts running immediately.&#xA;Returns the handle to the delay, which can manage the lifecycle of the thread.&#xA;&#xA;The ForkStrategy controls both:&#xA;  - how unhandled exceptions behave, and&#xA;  - whether the fork is structured (and which thread&apos;s scope owns it).
<details>
<summary>Instances</summary>

- <code><a href="#io-classes-delay-delayruntime-class">DelayRuntime</a> <a href="#io-io-impl-runtime-ioruntime-type">IoRuntime</a> <a href="#io-io-impl-io-runtime-delay-iodelay-type">IoDelay</a></code>

</details>



***

#### <a href="#io-classes-delay-delays-class"><code>Delays</code></a> <sup><sub>[CLASS] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/classes/delay.lisp#L51-L52">src</a></sub></sup><a name="io-classes-delay-delays-class"></a>
<code>(<a href="#io-classes-thread-threads-class">Threads</a> :RT :T :M) (<a href="#io-classes-delay-delayruntime-class">DelayRuntime</a> :RT :D) &rArr; <a href="#io-classes-delay-delays-class">Delays</a> :RT :M</code>


A MonadIo which can fork a delayed computation.

Methods:
<details>
<summary>Instances</summary>

- <code><a href="#io-classes-delay-delays-class">Delays</a> <a href="#io-io-impl-runtime-ioruntime-type">IoRuntime</a> <a href="#io-io-impl-simple-io-io-type">IO</a></code>

</details>



***

### Values

#### <a href="#io-classes-delay-fork-delay-value"><code>(FORK-DELAY OP)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/classes/delay.lisp#L73-L87">src</a></sub></sup><a name="io-classes-delay-fork-delay-value"></a>
<code>&forall; (:I (Type &rarr; Type)) :RT (:R (Type &rarr; Type)) :A :T (:M (Type &rarr; Type)) (:D (Type &rarr; Type)). (<a href="#io-classes-monad-io-unliftio-class">UnliftIo</a> :R :I) (<a href="#coalton-monad-classes-liftto-class">LiftTo</a> :R :M) (<a href="#io-classes-delay-delays-class">Delays</a> :RT :R) (<a href="#io-classes-delay-delayruntime-class">DelayRuntime</a> :RT :D) &rArr; :R :A &key (:scope <a href="#io-classes-thread-forkscope-type">ForkScope</a> :T) (:unhandled <a href="#io-classes-thread-unhandledexceptionstrategy-type">UnhandledExceptionStrategy</a>) &rarr; :M (:D :A)</code>

Start calculation in a new thread, which starts running immediately. Returns
the handle to the thread. Can specify an unhandled exception strategy and fork scope.

If the thread raises an unhandled exception, the default behavior is to log it to
*ERROR-OUTPUT* and swallow it until/if the thread is joined. The default fork scope is
structured.



***

#### <a href="#io-io-impl-delay-fork-delay_-value"><code>(FORK-DELAY_ OP)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/io-impl/io-delay.lisp#L34-L37">src</a></sub></sup><a name="io-io-impl-delay-fork-delay_-value"></a>
<code>&forall; :A (:M (Type &rarr; Type)). <a href="#coalton-monad-classes-liftto-class">LiftTo</a> <a href="#io-io-impl-simple-io-io-type">IO</a> :M &rArr; <a href="#io-io-impl-simple-io-io-type">IO</a> :A &key (:scope <a href="#io-classes-thread-forkscope-type">ForkScope</a> <a href="#io-io-impl-io-runtime-runtime-iothread-type">IoThread</a>) (:unhandled <a href="#io-classes-thread-unhandledexceptionstrategy-type">UnhandledExceptionStrategy</a>) &rarr; :M (<a href="#io-io-impl-io-runtime-delay-iodelay-type">IoDelay</a> :A)</code>

Delay a calculation in an IoThread. Can specify an unhandled exception strategy and
fork scope.



***

### Macros

#### <a href="#io-classes-delay-do-fork-delay-macro"><code>DO-FORK-DELAY (&amp;BODY FORMS)</code></a> <sup><sub>[MACRO]</sub></sup><a name="io-classes-delay-do-fork-delay-macro"></a>


***

#### <a href="#io-io-impl-delay-do-fork-delay_-macro"><code>DO-FORK-DELAY_ (&amp;BODY FORMS)</code></a> <sup><sub>[MACRO]</sub></sup><a name="io-io-impl-delay-do-fork-delay_-macro"></a>


***

# Package `IO/FILE`<a name="io-file-package"></a>

### Classes

#### <a href="#io-classes-file-files-class"><code>Files</code></a> <sup><sub>[CLASS] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/classes/file.lisp#L57-L142">src</a></sub></sup><a name="io-classes-file-files-class"></a>
<code><a href="#io-classes-monad-io-monadio-class">MonadIo</a> :M &rArr; <a href="#io-classes-file-files-class">Files</a> :M</code>

Methods:
- <code>EXISTS? :: <a href="#coalton-classes-into-class">Into</a> :A <a href="#coalton-file-pathname-type">Pathname</a> &rArr; :A &rarr; :M (<a href="#coalton-classes-result-type">Result</a> <a href="#coalton-file-fileerror-type">FileError</a> <a href="#coalton-boolean-type">Boolean</a>)</code><br/>Returns whether a file or directory exists.
- <code>FILE-EXISTS? :: <a href="#coalton-classes-into-class">Into</a> :A <a href="#coalton-file-pathname-type">Pathname</a> &rArr; :A &rarr; :M (<a href="#coalton-classes-result-type">Result</a> <a href="#coalton-file-fileerror-type">FileError</a> <a href="#coalton-boolean-type">Boolean</a>)</code><br/>Returns True if a pathname names a file that exists.
- <code>DIRECTORY-EXISTS? :: <a href="#coalton-classes-into-class">Into</a> :A <a href="#coalton-file-pathname-type">Pathname</a> &rArr; :A &rarr; :M (<a href="#coalton-classes-result-type">Result</a> <a href="#coalton-file-fileerror-type">FileError</a> <a href="#coalton-boolean-type">Boolean</a>)</code><br/>Returns True if a pathname names a directory that exists.
- <code>OPEN :: <a href="#coalton-file-file-class">File</a> :A &rArr; <a href="#coalton-file-pathname-type">Pathname</a> &key (:direction <a href="#coalton-file-opendirection-type">OpenDirection</a>) (:if-exists <a href="#coalton-file-ifexists-type">IfExists</a>) &rarr; :M (<a href="#coalton-classes-result-type">Result</a> <a href="#coalton-file-fileerror-type">FileError</a> (<a href="#coalton-file-filestream-type">FileStream</a> :A))</code>
- <code>CLOSE :: <a href="#coalton-file-filestream-type">FileStream</a> :A &rarr; :M (<a href="#coalton-classes-result-type">Result</a> <a href="#coalton-file-fileerror-type">FileError</a> <a href="#coalton-unit-type">Unit</a>)</code><br/>Closes a FileStream.
- <code>ABORT :: <a href="#coalton-file-filestream-type">FileStream</a> :A &rarr; :M (<a href="#coalton-classes-result-type">Result</a> <a href="#coalton-file-fileerror-type">FileError</a> <a href="#coalton-unit-type">Unit</a>)</code><br/>Closes a FileStream and aborts all operations..
- <code>CREATE-TEMP-DIRECTORY :: :M (<a href="#coalton-classes-result-type">Result</a> <a href="#coalton-file-fileerror-type">FileError</a> <a href="#coalton-file-pathname-type">Pathname</a>)</code>
- <code>CREATE-TEMP-FILE :: <a href="#coalton-string-type">String</a> &rarr; :M (<a href="#coalton-classes-result-type">Result</a> <a href="#coalton-file-fileerror-type">FileError</a> <a href="#coalton-file-pathname-type">Pathname</a>)</code>
- <code>COPY :: (<a href="#coalton-classes-into-class">Into</a> :A <a href="#coalton-file-pathname-type">Pathname</a>) (<a href="#coalton-classes-into-class">Into</a> :B <a href="#coalton-file-pathname-type">Pathname</a>) &rArr; :A * :B &rarr; :M (<a href="#coalton-classes-result-type">Result</a> <a href="#coalton-file-fileerror-type">FileError</a> <a href="#coalton-unit-type">Unit</a>)</code><br/>Copies a file to a new location.
- <code>CREATE-DIRECTORY :: <a href="#coalton-classes-into-class">Into</a> :P <a href="#coalton-file-pathname-type">Pathname</a> &rArr; :P &rarr; :M (<a href="#coalton-classes-result-type">Result</a> <a href="#coalton-file-fileerror-type">FileError</a> <a href="#coalton-file-pathname-type">Pathname</a>)</code><br/>This is equivalent to `mkdir -p`. Creates a directory and its parents. The pathname must be a valid directory pathname.
- <code>DELETE-FILE :: <a href="#coalton-classes-into-class">Into</a> :P <a href="#coalton-file-pathname-type">Pathname</a> &rArr; :P &rarr; :M (<a href="#coalton-classes-result-type">Result</a> <a href="#coalton-file-fileerror-type">FileError</a> <a href="#coalton-unit-type">Unit</a>)</code><br/>Deletes a given file if the file exists.
- <code>REMOVE-DIRECTORY :: <a href="#coalton-classes-into-class">Into</a> :P <a href="#coalton-file-pathname-type">Pathname</a> &rArr; :P &rarr; :M (<a href="#coalton-classes-result-type">Result</a> <a href="#coalton-file-fileerror-type">FileError</a> <a href="#coalton-unit-type">Unit</a>)</code><br/>Deletes an empty directory.
- <code>REMOVE-DIRECTORY-RECURSIVE :: <a href="#coalton-classes-into-class">Into</a> :P <a href="#coalton-file-pathname-type">Pathname</a> &rArr; :P &rarr; :M (<a href="#coalton-classes-result-type">Result</a> <a href="#coalton-file-fileerror-type">FileError</a> <a href="#coalton-unit-type">Unit</a>)</code><br/>Deletes a target directory recursively. Equivalent to `rm -r`. Errors if the path is not a directory.
- <code>SYSTEM-RELATIVE-PATHNAME :: <a href="#coalton-classes-into-class">Into</a> :SYS <a href="#coalton-string-type">String</a> &rArr; :SYS * <a href="#coalton-string-type">String</a> &rarr; :M (<a href="#coalton-classes-result-type">Result</a> <a href="#coalton-file-fileerror-type">FileError</a> <a href="#coalton-file-pathname-type">Pathname</a>)</code><br/>Generates a system-relative-pathname for a given filename or path. This is a wrapper for `asdf:system-relative-pathname`. `Name` will likely be an empty string unless a subdirectory or filename is specified.
- <code>READ-FILE-TO-STRING :: <a href="#coalton-classes-into-class">Into</a> :P <a href="#coalton-file-pathname-type">Pathname</a> &rArr; :P &rarr; :M (<a href="#coalton-classes-result-type">Result</a> <a href="#coalton-file-fileerror-type">FileError</a> <a href="#coalton-string-type">String</a>)</code><br/>Reads a file into a string, given a pathname string.
- <code>READ-FILE-LINES :: <a href="#coalton-classes-into-class">Into</a> :P <a href="#coalton-file-pathname-type">Pathname</a> &rArr; :P &rarr; :M (<a href="#coalton-classes-result-type">Result</a> <a href="#coalton-file-fileerror-type">FileError</a> (<a href="#coalton-list-type">List</a> <a href="#coalton-string-type">String</a>))</code><br/>Reads a file into lines, given a pathname or string.
- <code>READ-CHAR :: <a href="#coalton-file-filestream-type">FileStream</a> <a href="#coalton-char-type">Char</a> &rarr; :M (<a href="#coalton-classes-result-type">Result</a> <a href="#coalton-file-fileerror-type">FileError</a> <a href="#coalton-char-type">Char</a>)</code><br/>Reads a character from an FileStream.
- <code>READ-LINE :: <a href="#coalton-file-filestream-type">FileStream</a> <a href="#coalton-char-type">Char</a> &rarr; :M (<a href="#coalton-classes-result-type">Result</a> <a href="#coalton-file-fileerror-type">FileError</a> <a href="#coalton-string-type">String</a>)</code>
- <code>WRITE-CHAR :: <a href="#coalton-file-filestream-type">FileStream</a> <a href="#coalton-char-type">Char</a> * <a href="#coalton-char-type">Char</a> &rarr; :M (<a href="#coalton-classes-result-type">Result</a> <a href="#coalton-file-fileerror-type">FileError</a> <a href="#coalton-unit-type">Unit</a>)</code><br/>Writes a `Char` to the stream.
- <code>WRITE-LINE :: <a href="#coalton-file-filestream-type">FileStream</a> <a href="#coalton-char-type">Char</a> * <a href="#coalton-string-type">String</a> &rarr; :M (<a href="#coalton-classes-result-type">Result</a> <a href="#coalton-file-fileerror-type">FileError</a> <a href="#coalton-unit-type">Unit</a>)</code><br/>Writes a string with an appended newline to a filestream of type Char.
- <code>WRITE-STRING :: <a href="#coalton-file-filestream-type">FileStream</a> <a href="#coalton-char-type">Char</a> * <a href="#coalton-string-type">String</a> &rarr; :M (<a href="#coalton-classes-result-type">Result</a> <a href="#coalton-file-fileerror-type">FileError</a> <a href="#coalton-unit-type">Unit</a>)</code><br/>Writes a `string` to a FileStream of type Char.
- <code>READ-FILE-TO-VECTOR :: <a href="#coalton-file-file-class">File</a> :A &rArr; <a href="#coalton-file-filestream-type">FileStream</a> :A &rarr; :M (<a href="#coalton-classes-result-type">Result</a> <a href="#coalton-file-fileerror-type">FileError</a> (<a href="#coalton-vector-vector-type">Vector</a> :A))</code><br/>Reads a file into a vector of type `:a`.
- <code>READ-VECTOR :: <a href="#coalton-file-file-class">File</a> :A &rArr; <a href="#coalton-file-filestream-type">FileStream</a> :A * <a href="#coalton-ufix-type">UFix</a> &rarr; :M (<a href="#coalton-classes-result-type">Result</a> <a href="#coalton-file-fileerror-type">FileError</a> (<a href="#coalton-vector-vector-type">Vector</a> :A))</code><br/>Reads a chunk of a file into a vector of type `:a`.
- <code>WRITE-VECTOR :: (<a href="#coalton-file-file-class">File</a> :A) (<a href="#coalton-types-runtimerepr-class">RuntimeRepr</a> :A) &rArr; <a href="#coalton-file-filestream-type">FileStream</a> :A * <a href="#coalton-vector-vector-type">Vector</a> :A &rarr; :M (<a href="#coalton-classes-result-type">Result</a> <a href="#coalton-file-fileerror-type">FileError</a> <a href="#coalton-unit-type">Unit</a>)</code><br/>Writes elements of an vector of type `:a` to a stream of type `:a`.
- <code>WRITE-TO-FILE :: (<a href="#coalton-classes-into-class">Into</a> :P <a href="#coalton-file-pathname-type">Pathname</a>) (<a href="#coalton-file-file-class">File</a> :A) (<a href="#coalton-types-runtimerepr-class">RuntimeRepr</a> :A) &rArr; :P * <a href="#coalton-vector-vector-type">Vector</a> :A &rarr; :M (<a href="#coalton-classes-result-type">Result</a> <a href="#coalton-file-fileerror-type">FileError</a> <a href="#coalton-unit-type">Unit</a>)</code><br/>Opens and writes to a file with data of type :a. Supersedes existing data on the file.
- <code>APPEND-TO-FILE :: (<a href="#coalton-classes-into-class">Into</a> :P <a href="#coalton-file-pathname-type">Pathname</a>) (<a href="#coalton-file-file-class">File</a> :A) (<a href="#coalton-types-runtimerepr-class">RuntimeRepr</a> :A) &rArr; :P * <a href="#coalton-vector-vector-type">Vector</a> :A &rarr; :M (<a href="#coalton-classes-result-type">Result</a> <a href="#coalton-file-fileerror-type">FileError</a> <a href="#coalton-unit-type">Unit</a>)</code><br/>Opens and appends a file with data of type :a.
- <code>SET-FILE-POSITION :: <a href="#coalton-file-filestream-type">FileStream</a> :A * <a href="#coalton-ufix-type">UFix</a> &rarr; :M (<a href="#coalton-classes-result-type">Result</a> <a href="#coalton-file-fileerror-type">FileError</a> <a href="#coalton-unit-type">Unit</a>)</code><br/>Sets the file position of a file stream.
<details>
<summary>Instances</summary>

- <code><a href="#io-classes-file-files-class">Files</a> <a href="#io-io-impl-simple-io-io-type">IO</a></code>
- <code><a href="#io-classes-file-files-class">Files</a> :M &rArr; <a href="#io-classes-file-files-class">Files</a> (<a href="#coalton-experimental-do-control-loops-adv-loopt-type">LoopT</a> :M)</code>
- <code><a href="#io-classes-file-files-class">Files</a> :M &rArr; <a href="#io-classes-file-files-class">Files</a> (<a href="#coalton-monad-environment-envt-type">EnvT</a> :E :M)</code>
- <code><a href="#io-classes-file-files-class">Files</a> :M &rArr; <a href="#io-classes-file-files-class">Files</a> (<a href="#coalton-monad-statet-statet-type">StateT</a> :S :M)</code>

</details>



***

### Values

#### <a href="#io-gen-impl-file-with-open-file-value"><code>(WITH-OPEN-FILE PTH OP)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/io-file.lisp#L257-L271">src</a></sub></sup><a name="io-gen-impl-file-with-open-file-value"></a>
<code>&forall; (:I (Type &rarr; Type)) :RT :T :P :A (:R (Type &rarr; Type)) :B (:M (Type &rarr; Type)). (<a href="#coalton-file-file-class">File</a> :A) (<a href="#io-classes-file-files-class">Files</a> :I) (<a href="#io-classes-monad-io-unliftio-class">UnliftIo</a> :R :I) (<a href="#coalton-classes-into-class">Into</a> :P <a href="#coalton-file-pathname-type">Pathname</a>) (<a href="#coalton-monad-classes-liftto-class">LiftTo</a> :R :M) (<a href="#io-classes-exceptions-exceptions-class">Exceptions</a> :I) (<a href="#io-classes-thread-threads-class">Threads</a> :RT :T :I) &rArr; :P * (<a href="#coalton-file-filestream-type">FileStream</a> :A &rarr; :R :B) &key (:direction <a href="#coalton-file-opendirection-type">OpenDirection</a>) (:if-exists <a href="#coalton-file-ifexists-type">IfExists</a>) &rarr; :M :B</code>

Opens a file stream, performs `op` on it, then closes the stream.

Can run any underlying BaseIo, which can be useful but can also cause inference issues
in some cases. Try WITH-OPEN-FILE_ if you have issues.



***

#### <a href="#io-io-impl-file-with-open-file_-value"><code>(WITH-OPEN-FILE_)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/io-impl/io-file.lisp#L36-L36">src</a></sub></sup><a name="io-io-impl-file-with-open-file_-value"></a>
<code>&forall; :P :A :B (:M (Type &rarr; Type)). (<a href="#coalton-file-file-class">File</a> :A) (<a href="#io-classes-monad-io-unliftio-class">UnliftIo</a> :M <a href="#io-io-impl-simple-io-io-type">IO</a>) (<a href="#coalton-monad-classes-liftto-class">LiftTo</a> <a href="#io-io-impl-simple-io-io-type">IO</a> :M) (<a href="#coalton-classes-into-class">Into</a> :P <a href="#coalton-file-pathname-type">Pathname</a>) &rArr; :P * (<a href="#coalton-file-filestream-type">FileStream</a> :A &rarr; <a href="#io-io-impl-simple-io-io-type">IO</a> :B) &key (:direction <a href="#coalton-file-opendirection-type">OpenDirection</a>) (:if-exists <a href="#coalton-file-ifexists-type">IfExists</a>) &rarr; :M :B</code>


***

#### <a href="#io-gen-impl-file-with-temp-directory-value"><code>(WITH-TEMP-DIRECTORY OP)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/io-file.lisp#L300-L314">src</a></sub></sup><a name="io-gen-impl-file-with-temp-directory-value"></a>
<code>&forall; (:I (Type &rarr; Type)) :RT :T (:R (Type &rarr; Type)) :A (:M (Type &rarr; Type)). (<a href="#io-classes-monad-io-unliftio-class">UnliftIo</a> :R :I) (<a href="#coalton-monad-classes-liftto-class">LiftTo</a> :R :M) (<a href="#io-classes-exceptions-exceptions-class">Exceptions</a> :I) (<a href="#io-classes-file-files-class">Files</a> :I) (<a href="#io-classes-thread-threads-class">Threads</a> :RT :T :I) &rArr; (<a href="#coalton-file-pathname-type">Pathname</a> &rarr; :R :A) &rarr; :M :A</code>

Performs `op` inside a temporary directory.

Can run any underlying BaseIo, which can be useful but can also cause inference issues
in some cases. Try WITH-TEMP-DIRECTORY_ if you have issues.



***

#### <a href="#io-io-impl-file-with-temp-directory_-value"><code>(WITH-TEMP-DIRECTORY_)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/io-impl/io-file.lisp#L48-L48">src</a></sub></sup><a name="io-io-impl-file-with-temp-directory_-value"></a>
<code>&forall; :A (:M (Type &rarr; Type)). (<a href="#io-classes-monad-io-unliftio-class">UnliftIo</a> :M <a href="#io-io-impl-simple-io-io-type">IO</a>) (<a href="#coalton-monad-classes-liftto-class">LiftTo</a> <a href="#io-io-impl-simple-io-io-type">IO</a> :M) &rArr; (<a href="#coalton-file-pathname-type">Pathname</a> &rarr; <a href="#io-io-impl-simple-io-io-type">IO</a> :A) &rarr; :M :A</code>


***

#### <a href="#io-gen-impl-file-with-temp-file-value"><code>(WITH-TEMP-FILE OP)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/io-file.lisp#L279-L294">src</a></sub></sup><a name="io-gen-impl-file-with-temp-file-value"></a>
<code>&forall; (:I (Type &rarr; Type)) :RT :T :A (:R (Type &rarr; Type)) :B (:M (Type &rarr; Type)). (<a href="#coalton-file-file-class">File</a> :A) (<a href="#io-classes-file-files-class">Files</a> :I) (<a href="#io-classes-thread-threads-class">Threads</a> :RT :T :I) (<a href="#io-classes-monad-io-unliftio-class">UnliftIo</a> :R :I) (<a href="#coalton-monad-classes-liftto-class">LiftTo</a> :R :M) (<a href="#io-classes-exceptions-exceptions-class">Exceptions</a> :I) &rArr; (<a href="#coalton-file-filestream-type">FileStream</a> :A &rarr; :R :B) &key (:extension <a href="#coalton-string-type">String</a>) &rarr; :M :B</code>

Performs `op` on a temporary file. File type extensions need to include `.`

Can run any underlying BaseIo, which can be useful but can also cause inference issues
in some cases. Try WITH-TEMP-FILE_ if you have issues.



***

#### <a href="#io-io-impl-file-with-temp-file_-value"><code>(WITH-TEMP-FILE_)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/io-impl/io-file.lisp#L43-L43">src</a></sub></sup><a name="io-io-impl-file-with-temp-file_-value"></a>
<code>&forall; :A :B (:M (Type &rarr; Type)). (<a href="#coalton-file-file-class">File</a> :A) (<a href="#io-classes-monad-io-unliftio-class">UnliftIo</a> :M <a href="#io-io-impl-simple-io-io-type">IO</a>) (<a href="#coalton-monad-classes-liftto-class">LiftTo</a> <a href="#io-io-impl-simple-io-io-type">IO</a> :M) &rArr; (<a href="#coalton-file-filestream-type">FileStream</a> :A &rarr; <a href="#io-io-impl-simple-io-io-type">IO</a> :B) &key (:extension <a href="#coalton-string-type">String</a>) &rarr; :M :B</code>


***

### Macros

#### <a href="#io-classes-file-derive-files-macro"><code>DERIVE-FILES (MONAD-PARAM MONADT-FORM)</code></a> <sup><sub>[MACRO]</sub></sup><a name="io-classes-file-derive-files-macro"></a>

Derive a `Files` instance for MONADT-FORM by lifting into the base instance.

Example:
  (derive-files :m (st:StateT :s :m))



***

#### <a href="#io-gen-impl-file-do-with-open-file-macro"><code>DO-WITH-OPEN-FILE ((PATH FS &amp;KEY (DIRECTION (QUOTE INPUT)) (IF-EXISTS (QUOTE EERROR))) &amp;BODY BODY)</code></a> <sup><sub>[MACRO]</sub></sup><a name="io-gen-impl-file-do-with-open-file-macro"></a>

`do` sugar for `with-open-file`. Expands to a continuation where BODY runs in `do`.

Usage:
  (do-with-open-file path (fs)
    (line <- (read-char fs))
    ...)
  (do-with-open-file path (fs)
    :direction f_:Output :if-exists f_:Supersede
    (write-line fs "hello")
    ...)




***

#### <a href="#io-io-impl-file-do-with-open-file_-macro"><code>DO-WITH-OPEN-FILE_ ((PATH FS &amp;KEY (DIRECTION (QUOTE INPUT)) (IF-EXISTS (QUOTE EERROR))) &amp;BODY BODY)</code></a> <sup><sub>[MACRO]</sub></sup><a name="io-io-impl-file-do-with-open-file_-macro"></a>

`do` sugar for `with-open-file_`. Expands to a continuation where BODY runs in `do`.

Usage:
  (do-with-open-file_ path (fs)
    (line <- (read-char fs))
    ...)
  (do-with-open-file_ path (fs)
    :direction file:Output :if-exists file:Supersede
    (write-line fs "hello")
    ...)




***

#### <a href="#io-gen-impl-file-do-with-temp-directory-macro"><code>DO-WITH-TEMP-DIRECTORY ((DIR) &amp;BODY BODY)</code></a> <sup><sub>[MACRO]</sub></sup><a name="io-gen-impl-file-do-with-temp-directory-macro"></a>

`do` sugar for `with-temp-directory`.



***

#### <a href="#io-io-impl-file-do-with-temp-directory_-macro"><code>DO-WITH-TEMP-DIRECTORY_ ((DIR) &amp;BODY BODY)</code></a> <sup><sub>[MACRO]</sub></sup><a name="io-io-impl-file-do-with-temp-directory_-macro"></a>

`do` sugar for `with-temp-directory_`.



***

#### <a href="#io-gen-impl-file-do-with-temp-file-macro"><code>DO-WITH-TEMP-FILE (TYPE (FS) &amp;BODY BODY)</code></a> <sup><sub>[MACRO]</sub></sup><a name="io-gen-impl-file-do-with-temp-file-macro"></a>

`do` sugar for `with-temp-file` (TYPE is a string like "txt").



***

#### <a href="#io-io-impl-file-do-with-temp-file_-macro"><code>DO-WITH-TEMP-FILE_ (TYPE (FS) &amp;BODY BODY)</code></a> <sup><sub>[MACRO]</sub></sup><a name="io-io-impl-file-do-with-temp-file_-macro"></a>

`do` sugar for `with-temp-file_` (TYPE is a string like "txt").



***

#### <a href="#io-gen-impl-file-implement-files-macro"><code>IMPLEMENT-FILES (MONAD)</code></a> <sup><sub>[MACRO]</sub></sup><a name="io-gen-impl-file-implement-files-macro"></a>


***

# Package `IO/UNIQUE`<a name="io-unique-package"></a>

### Types

#### <a href="#io-classes-unique-unique-type"><code>Unique</code></a> <sup><sub>[TYPE] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/classes/unique.lisp#L31-L32">src</a></sub></sup><a name="io-classes-unique-unique-type"></a>
<details>
<summary>Instances</summary>

- <code><a href="#coalton-classes-eq-class">Eq</a> <a href="#coalton-integer-type">Integer</a> &rArr; <a href="#coalton-classes-eq-class">Eq</a> <a href="#io-classes-unique-unique-type">Unique</a></code>
- <code><a href="#coalton-classes-ord-class">Ord</a> <a href="#io-classes-unique-unique-type">Unique</a></code>
- <code><a href="#coalton-types-runtimerepr-class">RuntimeRepr</a> <a href="#io-classes-unique-unique-type">Unique</a></code>

</details>



***

### Classes

#### <a href="#io-classes-unique-uniquegen-class"><code>UniqueGen</code></a> <sup><sub>[CLASS] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/classes/unique.lisp#L47-L52">src</a></sub></sup><a name="io-classes-unique-uniquegen-class"></a>
<code><a href="#coalton-classes-monad-class">Monad</a> :M &rArr; <a href="#io-classes-unique-uniquegen-class">UniqueGen</a> :M</code>

Methods:
- <code>NEW-UNIQUE :: :M <a href="#io-classes-unique-unique-type">Unique</a></code><br/>Generate a value that will be unique within this run of the program.&#xA;Threadsafe - calling from different threads will still result in unique&#xA;values across all threads.
<details>
<summary>Instances</summary>

- <code><a href="#io-classes-unique-uniquegen-class">UniqueGen</a> :M &rArr; <a href="#io-classes-unique-uniquegen-class">UniqueGen</a> (<a href="#coalton-experimental-do-control-loops-adv-loopt-type">LoopT</a> :M)</code>
- <code><a href="#io-classes-unique-uniquegen-class">UniqueGen</a> :M &rArr; <a href="#io-classes-unique-uniquegen-class">UniqueGen</a> (<a href="#coalton-monad-environment-envt-type">EnvT</a> :E :M)</code>
- <code><a href="#io-classes-unique-uniquegen-class">UniqueGen</a> :M &rArr; <a href="#io-classes-unique-uniquegen-class">UniqueGen</a> (<a href="#coalton-monad-statet-statet-type">StateT</a> :S :M)</code>

</details>



***

### Values

#### <a href="#io-classes-unique-to-int-value"><code>(TO-INT (UNIQUE% I))</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/classes/unique.lisp#L40-L45">src</a></sub></sup><a name="io-classes-unique-to-int-value"></a>
<code><a href="#io-classes-unique-unique-type">Unique</a> &rarr; <a href="#coalton-integer-type">Integer</a></code>

Convert a unique value to an integer.

It is guaranteed that: (/= (to-int a) (to-int b))
for any two different Unique instances.



***

### Macros

#### <a href="#io-classes-unique-derive-unique-gen-macro"><code>DERIVE-UNIQUE-GEN (MONAD-PARAM MONADT-FORM)</code></a> <sup><sub>[MACRO]</sub></sup><a name="io-classes-unique-derive-unique-gen-macro"></a>

Automatically derive an instance of UniqueGen for a monad transformer.

Example:
  (derive-unique-gen :m (st:StateT :s :m))



***

#### <a href="#io-gen-impl-unique-implement-unique-gen-macro"><code>IMPLEMENT-UNIQUE-GEN (MONAD)</code></a> <sup><sub>[MACRO]</sub></sup><a name="io-gen-impl-unique-implement-unique-gen-macro"></a>


***

# Package `IO/NETWORK`<a name="io-network-package"></a>

### Types

#### <a href="#io-classes-network-byteconnectionsocket-type"><code>ByteConnectionSocket</code></a> <sup><sub>[TYPE] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/classes/network.lisp#L69-L70">src</a></sub></sup><a name="io-classes-network-byteconnectionsocket-type"></a>

A socket connecting a client and server using a byte stream.

<details>
<summary>Instances</summary>

- <code><a href="#coalton-types-runtimerepr-class">RuntimeRepr</a> <a href="#io-classes-network-byteconnectionsocket-type">ByteConnectionSocket</a></code>

</details>



***

#### <a href="#io-classes-network-byteserversocket-type"><code>ByteServerSocket</code></a> <sup><sub>[TYPE] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/classes/network.lisp#L65-L66">src</a></sub></sup><a name="io-classes-network-byteserversocket-type"></a>

A server socket listening for new byte-stream connections.

<details>
<summary>Instances</summary>

- <code><a href="#coalton-types-runtimerepr-class">RuntimeRepr</a> <a href="#io-classes-network-byteserversocket-type">ByteServerSocket</a></code>

</details>



***

#### <a href="#io-classes-network-connectionsocket-type"><code>ConnectionSocket</code></a> <sup><sub>[TYPE] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/classes/network.lisp#L61-L62">src</a></sub></sup><a name="io-classes-network-connectionsocket-type"></a>

A socket connecting a client and server.

<details>
<summary>Instances</summary>

- <code><a href="#coalton-types-runtimerepr-class">RuntimeRepr</a> <a href="#io-classes-network-connectionsocket-type">ConnectionSocket</a></code>

</details>



***

#### <a href="#io-classes-network-networkexception-type"><code>NetworkException</code></a> <sup><sub>[TYPE] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/classes/network.lisp#L53-L54">src</a></sub></sup><a name="io-classes-network-networkexception-type"></a>
<details>
<summary>Instances</summary>

- <code><a href="#coalton-classes-exception-class">Exception</a> <a href="#io-classes-network-networkexception-type">NetworkException</a></code>
- <code><a href="#coalton-types-runtimerepr-class">RuntimeRepr</a> <a href="#io-classes-network-networkexception-type">NetworkException</a></code>

</details>



***

#### <a href="#io-classes-network-serversocket-type"><code>ServerSocket</code></a> <sup><sub>[TYPE] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/classes/network.lisp#L57-L58">src</a></sub></sup><a name="io-classes-network-serversocket-type"></a>

A server socket listening for new connections.

<details>
<summary>Instances</summary>

- <code><a href="#coalton-types-runtimerepr-class">RuntimeRepr</a> <a href="#io-classes-network-serversocket-type">ServerSocket</a></code>

</details>



***

### Classes

#### <a href="#io-classes-network-sockets-class"><code>Sockets</code></a> <sup><sub>[CLASS] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/classes/network.lisp#L72-L115">src</a></sub></sup><a name="io-classes-network-sockets-class"></a>
<code><a href="#io-classes-monad-io-monadio-class">MonadIo</a> :M &rArr; <a href="#io-classes-network-sockets-class">Sockets</a> :M</code>

Methods:
- <code>SOCKET-LISTEN :: <a href="#coalton-string-type">String</a> * <a href="#coalton-ufix-type">UFix</a> &rarr; :M <a href="#io-classes-network-serversocket-type">ServerSocket</a></code><br/>Start a new server socket, listening on HOSTNAME and PORT.
- <code>SOCKET-ACCEPT :: <a href="#io-classes-network-serversocket-type">ServerSocket</a> &rarr; :M <a href="#io-classes-network-connectionsocket-type">ConnectionSocket</a></code><br/>Accept a connection with a new client.
- <code>SOCKET-CONNECT :: <a href="#coalton-string-type">String</a> * <a href="#coalton-ufix-type">UFix</a> &rarr; :M <a href="#io-classes-network-connectionsocket-type">ConnectionSocket</a></code><br/>Connect to a server at HOSTNAME and PORT, returning a new connection.
- <code>CLOSE-CONNECTION :: <a href="#io-classes-network-connectionsocket-type">ConnectionSocket</a> &rarr; :M <a href="#coalton-unit-type">Unit</a></code><br/>Close an open connection socket.
- <code>CLOSE-SERVER :: <a href="#io-classes-network-serversocket-type">ServerSocket</a> &rarr; :M <a href="#coalton-unit-type">Unit</a></code><br/>Close an open server socket.
- <code>WRITE-LINE :: <a href="#coalton-classes-into-class">Into</a> :S <a href="#coalton-string-type">String</a> &rArr; :S * <a href="#io-classes-network-connectionsocket-type">ConnectionSocket</a> &rarr; :M <a href="#coalton-unit-type">Unit</a></code>
- <code>READ-LINE :: <a href="#io-classes-network-connectionsocket-type">ConnectionSocket</a> &rarr; :M <a href="#coalton-string-type">String</a></code><br/>Read the next line from the socket. Blocks until data is sent!
- <code>BYTE-SOCKET-LISTEN :: <a href="#coalton-string-type">String</a> * <a href="#coalton-ufix-type">UFix</a> &rarr; :M <a href="#io-classes-network-byteserversocket-type">ByteServerSocket</a></code><br/>Start a new server socket for byte-stream connections, listening on HOSTNAME and PORT.
- <code>BYTE-SOCKET-ACCEPT :: <a href="#io-classes-network-byteserversocket-type">ByteServerSocket</a> &rarr; :M <a href="#io-classes-network-byteconnectionsocket-type">ByteConnectionSocket</a></code><br/>Accept a byte-stream connection with a new client.
- <code>BYTE-SOCKET-CONNECT :: <a href="#coalton-string-type">String</a> * <a href="#coalton-ufix-type">UFix</a> &rarr; :M <a href="#io-classes-network-byteconnectionsocket-type">ByteConnectionSocket</a></code><br/>Connect to a server at HOSTNAME and PORT using a byte stream, returning a new connection.
- <code>CLOSE-BYTE-CONNECTION :: <a href="#io-classes-network-byteconnectionsocket-type">ByteConnectionSocket</a> &rarr; :M <a href="#coalton-unit-type">Unit</a></code><br/>Close an open byte-stream connection socket.
- <code>CLOSE-BYTE-SERVER :: <a href="#io-classes-network-byteserversocket-type">ByteServerSocket</a> &rarr; :M <a href="#coalton-unit-type">Unit</a></code><br/>Close an open byte-stream server socket.
- <code>WRITE-BYTES :: <a href="#coalton-vector-vector-type">Vector</a> <a href="#coalton-u8-type">U8</a> * <a href="#io-classes-network-byteconnectionsocket-type">ByteConnectionSocket</a> &rarr; :M <a href="#coalton-unit-type">Unit</a></code><br/>Write all bytes in the vector to the socket.
- <code>READ-EXACTLY :: <a href="#coalton-ufix-type">UFix</a> * <a href="#io-classes-network-byteconnectionsocket-type">ByteConnectionSocket</a> &rarr; :M (<a href="#coalton-vector-vector-type">Vector</a> <a href="#coalton-u8-type">U8</a>)</code><br/>Read exactly N bytes from the socket. Blocks until N bytes are available.
<details>
<summary>Instances</summary>

- <code><a href="#io-classes-network-sockets-class">Sockets</a> <a href="#io-io-impl-simple-io-io-type">IO</a></code>
- <code><a href="#io-classes-network-sockets-class">Sockets</a> :M &rArr; <a href="#io-classes-network-sockets-class">Sockets</a> (<a href="#coalton-experimental-do-control-loops-adv-loopt-type">LoopT</a> :M)</code>
- <code><a href="#io-classes-network-sockets-class">Sockets</a> :M &rArr; <a href="#io-classes-network-sockets-class">Sockets</a> (<a href="#coalton-monad-environment-envt-type">EnvT</a> :E :M)</code>
- <code><a href="#io-classes-network-sockets-class">Sockets</a> :M &rArr; <a href="#io-classes-network-sockets-class">Sockets</a> (<a href="#coalton-monad-statet-statet-type">StateT</a> :S :M)</code>

</details>



***

### Values

#### <a href="#io-gen-impl-network-byte-socket-accept-fork-with-value"><code>(BYTE-SOCKET-ACCEPT-FORK-WITH SERVER-SOCKET OP)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/io-network.lisp#L309-L319">src</a></sub></sup><a name="io-gen-impl-network-byte-socket-accept-fork-with-value"></a>
<code>&forall; :RT (:M (Type &rarr; Type)) :A :T. (<a href="#io-classes-network-sockets-class">Sockets</a> :M) (<a href="#io-classes-thread-threads-class">Threads</a> :RT :T :M) (<a href="#io-classes-exceptions-exceptions-class">Exceptions</a> :M) (<a href="#io-classes-monad-io-unliftio-class">UnliftIo</a> :M :M) &rArr; <a href="#io-classes-network-byteserversocket-type">ByteServerSocket</a> * (<a href="#io-classes-network-byteconnectionsocket-type">ByteConnectionSocket</a> &rarr; :M :A) &rarr; :M :T</code>

Accept a byte-stream connection with a new client and run operation OP on a new thread.
Guarantees that the socket will close on cleanup. Returns a handle to the forked thread.



***

#### <a href="#io-gen-impl-network-byte-socket-accept-with-value"><code>(BYTE-SOCKET-ACCEPT-WITH SERVER-SOCKET OP)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/io-network.lisp#L293-L304">src</a></sub></sup><a name="io-gen-impl-network-byte-socket-accept-with-value"></a>
<code>&forall; :RT :T (:M (Type &rarr; Type)) :A. (<a href="#io-classes-network-sockets-class">Sockets</a> :M) (<a href="#io-classes-thread-threads-class">Threads</a> :RT :T :M) (<a href="#io-classes-exceptions-exceptions-class">Exceptions</a> :M) &rArr; <a href="#io-classes-network-byteserversocket-type">ByteServerSocket</a> * (<a href="#io-classes-network-byteconnectionsocket-type">ByteConnectionSocket</a> &rarr; :M :A) &rarr; :M :A</code>

Accept a byte-stream connection with a new client and run operation OP. Guarantees that the
socket will close on cleanup.

Note: If you fork a thread inside this, the operation on this thread will probably finish
and close the socket before you intend. For multithreaded uses, use
byte-socket-accept-fork-with.



***

#### <a href="#io-gen-impl-network-byte-socket-connect-with-value"><code>(BYTE-SOCKET-CONNECT-WITH HOSTNAME PORT OP)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/io-network.lisp#L282-L289">src</a></sub></sup><a name="io-gen-impl-network-byte-socket-connect-with-value"></a>
<code>&forall; :RT :T (:M (Type &rarr; Type)) :A. (<a href="#io-classes-network-sockets-class">Sockets</a> :M) (<a href="#io-classes-thread-threads-class">Threads</a> :RT :T :M) (<a href="#io-classes-exceptions-exceptions-class">Exceptions</a> :M) &rArr; <a href="#coalton-string-type">String</a> * <a href="#coalton-ufix-type">UFix</a> * (<a href="#io-classes-network-byteconnectionsocket-type">ByteConnectionSocket</a> &rarr; :M :A) &rarr; :M :A</code>

Run operation OP with a byte-stream connection to an open server socket at HOSTNAME and PORT.
Guarantees that the socket will close on cleanup.



***

#### <a href="#io-gen-impl-network-byte-socket-listen-with-value"><code>(BYTE-SOCKET-LISTEN-WITH HOSTNAME PORT OP)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/io-network.lisp#L271-L278">src</a></sub></sup><a name="io-gen-impl-network-byte-socket-listen-with-value"></a>
<code>&forall; :RT :T (:M (Type &rarr; Type)) :A. (<a href="#io-classes-network-sockets-class">Sockets</a> :M) (<a href="#io-classes-thread-threads-class">Threads</a> :RT :T :M) (<a href="#io-classes-exceptions-exceptions-class">Exceptions</a> :M) &rArr; <a href="#coalton-string-type">String</a> * <a href="#coalton-ufix-type">UFix</a> * (<a href="#io-classes-network-byteserversocket-type">ByteServerSocket</a> &rarr; :M :A) &rarr; :M :A</code>

Run operation OP with a new byte-stream server socket, listening on HOSTNAME and PORT.
Guarantees that the socket will close on cleanup.



***

#### <a href="#io-gen-impl-network-socket-accept-fork-with-value"><code>(SOCKET-ACCEPT-FORK-WITH SERVER-SOCKET OP)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/io-network.lisp#L257-L267">src</a></sub></sup><a name="io-gen-impl-network-socket-accept-fork-with-value"></a>
<code>&forall; :RT (:M (Type &rarr; Type)) :A :T. (<a href="#io-classes-network-sockets-class">Sockets</a> :M) (<a href="#io-classes-thread-threads-class">Threads</a> :RT :T :M) (<a href="#io-classes-exceptions-exceptions-class">Exceptions</a> :M) (<a href="#io-classes-monad-io-unliftio-class">UnliftIo</a> :M :M) &rArr; <a href="#io-classes-network-serversocket-type">ServerSocket</a> * (<a href="#io-classes-network-connectionsocket-type">ConnectionSocket</a> &rarr; :M :A) &rarr; :M :T</code>

Accept a connection with a new client and run operation OP on a new thread.
Guarantees that the socket will close on cleanup. Returns a handle to the forked thread.



***

#### <a href="#io-gen-impl-network-socket-accept-with-value"><code>(SOCKET-ACCEPT-WITH SERVER-SOCKET OP)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/io-network.lisp#L239-L250">src</a></sub></sup><a name="io-gen-impl-network-socket-accept-with-value"></a>
<code>&forall; :RT :T (:M (Type &rarr; Type)) :A. (<a href="#io-classes-network-sockets-class">Sockets</a> :M) (<a href="#io-classes-thread-threads-class">Threads</a> :RT :T :M) (<a href="#io-classes-exceptions-exceptions-class">Exceptions</a> :M) &rArr; <a href="#io-classes-network-serversocket-type">ServerSocket</a> * (<a href="#io-classes-network-connectionsocket-type">ConnectionSocket</a> &rarr; :M :A) &rarr; :M :A</code>

Accept a connection with a new client and run operation OP. Guarantees that the
socket will close on cleanup.

Note: If you fork a thread inside this, the operation on this thread will probably finish
and close the socket before you intend. For multithreaded uses, use
socket-accept-fork-with.



***

#### <a href="#io-gen-impl-network-socket-connect-with-value"><code>(SOCKET-CONNECT-WITH HOSTNAME PORT OP)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/io-network.lisp#L228-L235">src</a></sub></sup><a name="io-gen-impl-network-socket-connect-with-value"></a>
<code>&forall; :RT :T (:M (Type &rarr; Type)) :A. (<a href="#io-classes-network-sockets-class">Sockets</a> :M) (<a href="#io-classes-thread-threads-class">Threads</a> :RT :T :M) (<a href="#io-classes-exceptions-exceptions-class">Exceptions</a> :M) &rArr; <a href="#coalton-string-type">String</a> * <a href="#coalton-ufix-type">UFix</a> * (<a href="#io-classes-network-connectionsocket-type">ConnectionSocket</a> &rarr; :M :A) &rarr; :M :A</code>

Run operation OP with a connection to an open server socket at HOSTNAME and PORT.
Guarantees that the socket will close on cleanup.



***

#### <a href="#io-gen-impl-network-socket-listen-with-value"><code>(SOCKET-LISTEN-WITH HOSTNAME PORT OP)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/io-network.lisp#L215-L222">src</a></sub></sup><a name="io-gen-impl-network-socket-listen-with-value"></a>
<code>&forall; :RT :T (:M (Type &rarr; Type)) :A. (<a href="#io-classes-network-sockets-class">Sockets</a> :M) (<a href="#io-classes-thread-threads-class">Threads</a> :RT :T :M) (<a href="#io-classes-exceptions-exceptions-class">Exceptions</a> :M) &rArr; <a href="#coalton-string-type">String</a> * <a href="#coalton-ufix-type">UFix</a> * (<a href="#io-classes-network-serversocket-type">ServerSocket</a> &rarr; :M :A) &rarr; :M :A</code>

Run operation OP with a new server socket, listening on HOSTNAME and PORT. Guarantees
that the socket will close on cleanup.



***

### Macros

#### <a href="#io-classes-network-derive-sockets-macro"><code>DERIVE-SOCKETS (MONAD-PARAM MONADT-FORM)</code></a> <sup><sub>[MACRO]</sub></sup><a name="io-classes-network-derive-sockets-macro"></a>

Automatically derive an instance of Sockets for a monad transformer.

Example:
  (derive-sockets :m (st:StateT :s :m))



***

#### <a href="#io-gen-impl-network-do-byte-socket-accept-fork-with-macro"><code>DO-BYTE-SOCKET-ACCEPT-FORK-WITH ((SOCKET-SYM (SERVER-SOCKET)) &amp;BODY BODY)</code></a> <sup><sub>[MACRO]</sub></sup><a name="io-gen-impl-network-do-byte-socket-accept-fork-with-macro"></a>

Accept a byte-stream connection with a new client and run operation OP on a new thread.
Guarantees that the socket will close on cleanup. Returns a handle to the forked thread.



***

#### <a href="#io-gen-impl-network-do-byte-socket-accept-with-macro"><code>DO-BYTE-SOCKET-ACCEPT-WITH ((SOCKET-SYM (SERVER-SOCKET)) &amp;BODY BODY)</code></a> <sup><sub>[MACRO]</sub></sup><a name="io-gen-impl-network-do-byte-socket-accept-with-macro"></a>

Accept a byte-stream connection with a new client and run operation OP. Guarantees that the
socket will close on cleanup.

Note: If you fork a thread inside this, the operation on this thread will probably finish
and close the socket before you intend. For multithreaded uses, use
byte-socket-accept-fork-with.



***

#### <a href="#io-gen-impl-network-do-byte-socket-connect-with-macro"><code>DO-BYTE-SOCKET-CONNECT-WITH ((SOCKET-SYM (HOSTNAME PORT)) &amp;BODY BODY)</code></a> <sup><sub>[MACRO]</sub></sup><a name="io-gen-impl-network-do-byte-socket-connect-with-macro"></a>

Run operation OP with a byte-stream connection to an open server socket at HOSTNAME and PORT.
Guarantees that the socket will close on cleanup.



***

#### <a href="#io-gen-impl-network-do-byte-socket-listen-with-macro"><code>DO-BYTE-SOCKET-LISTEN-WITH ((SOCKET-SYM (HOSTNAME PORT)) &amp;BODY BODY)</code></a> <sup><sub>[MACRO]</sub></sup><a name="io-gen-impl-network-do-byte-socket-listen-with-macro"></a>

Run operation OP with a new byte-stream server socket, listening on HOSTNAME and PORT. Guarantees
that the socket will close on cleanup.



***

#### <a href="#io-gen-impl-network-do-socket-accept-fork-with-macro"><code>DO-SOCKET-ACCEPT-FORK-WITH ((SOCKET-SYM (SERVER-SOCKET)) &amp;BODY BODY)</code></a> <sup><sub>[MACRO]</sub></sup><a name="io-gen-impl-network-do-socket-accept-fork-with-macro"></a>

Accept a connection with a new client and run operation OP on a new thread.
Guarantees that the socket will close on cleanup. Returns a handle to the forked thread.



***

#### <a href="#io-gen-impl-network-do-socket-accept-with-macro"><code>DO-SOCKET-ACCEPT-WITH ((SOCKET-SYM (SERVER-SOCKET)) &amp;BODY BODY)</code></a> <sup><sub>[MACRO]</sub></sup><a name="io-gen-impl-network-do-socket-accept-with-macro"></a>

Accept a connection with a new client and run operation OP. Guarantees that the
socket will close on cleanup.

Note: If you fork a thread inside this, the operation on this thread will probably finish
and close the socket before you intend. For multithreaded uses, use
socket-accept-fork-with.



***

#### <a href="#io-gen-impl-network-do-socket-connect-with-macro"><code>DO-SOCKET-CONNECT-WITH ((SOCKET-SYM (HOSTNAME PORT)) &amp;BODY BODY)</code></a> <sup><sub>[MACRO]</sub></sup><a name="io-gen-impl-network-do-socket-connect-with-macro"></a>

Run operation OP with a connection to an open server socket at HOSTNAME and PORT.
Guarantees that the socket will close on cleanup.



***

#### <a href="#io-gen-impl-network-do-socket-listen-with-macro"><code>DO-SOCKET-LISTEN-WITH ((SOCKET-SYM (HOSTNAME PORT)) &amp;BODY BODY)</code></a> <sup><sub>[MACRO]</sub></sup><a name="io-gen-impl-network-do-socket-listen-with-macro"></a>

Run operation OP with a new server socket, listening on HOSTNAME and PORT. Guarantees
that the socket will close on cleanup.



***

#### <a href="#io-gen-impl-network-implement-sockets-macro"><code>IMPLEMENT-SOCKETS (MONAD)</code></a> <sup><sub>[MACRO]</sub></sup><a name="io-gen-impl-network-implement-sockets-macro"></a>


***

# Package `IO/CONC/PARKING`<a name="io-conc-parking-package"></a>

### Types

#### <a href="#io-gen-impl-conc-parking-parkingset-type"><code>ParkingSet</code></a> <sup><sub>[TYPE] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/parking.lisp#L46-L58">src</a></sub></sup><a name="io-gen-impl-conc-parking-parkingset-type"></a>

ParkingSet is a thread-safe list of parked threads. A parking thread can atomically
subscibe and park on a Parking Set, and a signalling thread can atomically unpark all
parkers on a ParkingSet.

In general, ParkingSet is the preferred way to park and unpark threads. The lower-level
parking functions exposed by the Threads and Runtime classes should only be used
if ParkingSet doesn't provide enough functionality for the algorithm.

Concurrent:
  - ParkingSet's algorithms are lock free, but individual threads can block for a very
    short window if contention on the parking set is very high.

<details>
<summary>Instances</summary>

- <code><a href="#coalton-types-runtimerepr-class">RuntimeRepr</a> <a href="#io-gen-impl-conc-parking-parkingset-type">ParkingSet</a></code>

</details>



***

### Values

#### <a href="#io-gen-impl-conc-parking-park-in-set-value"><code>(PARK-IN-SET PSET)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/parking.lisp#L217-L231">src</a></sub></sup><a name="io-gen-impl-conc-parking-park-in-set-value"></a>
<code>&forall; :RT :T (:M (Type &rarr; Type)). <a href="#io-classes-thread-threads-class">Threads</a> :RT :T :M &rArr; <a href="#io-gen-impl-conc-parking-parkingset-type">ParkingSet</a> &key (:timeout <a href="#io-classes-thread-timeoutstrategy-type">TimeoutStrategy</a>) &rarr; :M <a href="#coalton-unit-type">Unit</a></code>

Parks the current thread in `pset`. Will park the thread until woken by an unpark
from another thread. Can specify a timeout.



***

#### <a href="#io-gen-impl-conc-parking-park-in-set-if-value"><code>(PARK-IN-SET-IF SHOULD-PARK? PSET)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/parking.lisp#L193-L212">src</a></sub></sup><a name="io-gen-impl-conc-parking-park-in-set-if-value"></a>
<code>&forall; :RT :T (:IO (Type &rarr; Type)) (:M (Type &rarr; Type)). (<a href="#io-classes-monad-io-baseio-class">BaseIo</a> :IO) (<a href="#io-classes-thread-threads-class">Threads</a> :RT :T :IO) (<a href="#io-classes-monad-io-monadio-class">MonadIo</a> :M) &rArr; :IO <a href="#coalton-boolean-type">Boolean</a> * <a href="#io-gen-impl-conc-parking-parkingset-type">ParkingSet</a> &key (:timeout <a href="#io-classes-thread-timeoutstrategy-type">TimeoutStrategy</a>) &rarr; :M <a href="#coalton-unit-type">Unit</a></code>

Parks the current thread in PSET if SHOULD-PARK? returns True. Will park the thread
until woken by an unpark from another thread. Upon an unpark, the thread will resume even
if SHOULD-PARK? is False! SHOULD-PARK? is only checked to determine if the thread should
park, *not* if it should resume. Can specify a timeout.

Concurrent:
  - WARNING: SHOULD-PARK? must not block, or the thread could be left blocked in a masked
    state.



***

#### <a href="#io-io-impl-conc-parking-park-in-set-if_-value"><code>(PARK-IN-SET-IF_)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/io-impl/conc/parking.lisp#L41-L51">src</a></sub></sup><a name="io-io-impl-conc-parking-park-in-set-if_-value"></a>
<code>&forall; (:M (Type &rarr; Type)). <a href="#io-classes-monad-io-monadio-class">MonadIo</a> :M &rArr; <a href="#io-io-impl-simple-io-io-type">IO</a> <a href="#coalton-boolean-type">Boolean</a> * <a href="#io-gen-impl-conc-parking-parkingset-type">ParkingSet</a> &key (:timeout <a href="#io-classes-thread-timeoutstrategy-type">TimeoutStrategy</a>) &rarr; :M <a href="#coalton-unit-type">Unit</a></code>

Parks the current thread in PSET if SHOULD-PARK? returns True. Will park the thread
until woken by an unpark from another thread. Upon an unpark, the thread will resume even
if SHOULD-PARK? is False! SHOULD-PARK? is only checked to determine if the thread should
park, *not* if it should resume. Can specify a timeout.

Concurrent:
  - WARNING: SHOULD-PARK? must not block, or the thread could be left blocked in a masked
    state.
  - Can briefly block while trying to park the thread, if contended.



***

#### <a href="#io-gen-impl-conc-parking-park-in-sets-value"><code>(PARK-IN-SETS PSETS)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/parking.lisp#L172-L186">src</a></sub></sup><a name="io-gen-impl-conc-parking-park-in-sets-value"></a>
<code>&forall; :RT :T (:M (Type &rarr; Type)). <a href="#io-classes-thread-threads-class">Threads</a> :RT :T :M &rArr; <a href="#coalton-list-type">List</a> <a href="#io-gen-impl-conc-parking-parkingset-type">ParkingSet</a> &key (:timeout <a href="#io-classes-thread-timeoutstrategy-type">TimeoutStrategy</a>) &rarr; :M <a href="#coalton-unit-type">Unit</a></code>

Parks the current thread in `psets`. Will park the thread until woken by an unpark
from another thread. Can specify a timeout.



***

#### <a href="#io-gen-impl-conc-parking-park-in-sets-if-value"><code>(PARK-IN-SETS-IF SHOULD-PARK? PSETS)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/parking.lisp#L147-L167">src</a></sub></sup><a name="io-gen-impl-conc-parking-park-in-sets-if-value"></a>
<code>&forall; :RT :T (:IO (Type &rarr; Type)) (:M (Type &rarr; Type)). (<a href="#io-classes-monad-io-baseio-class">BaseIo</a> :IO) (<a href="#io-classes-thread-threads-class">Threads</a> :RT :T :IO) (<a href="#io-classes-monad-io-monadio-class">MonadIo</a> :M) &rArr; :IO <a href="#coalton-boolean-type">Boolean</a> * <a href="#coalton-list-type">List</a> <a href="#io-gen-impl-conc-parking-parkingset-type">ParkingSet</a> &key (:timeout <a href="#io-classes-thread-timeoutstrategy-type">TimeoutStrategy</a>) &rarr; :M <a href="#coalton-unit-type">Unit</a></code>

Parks the current thread in PSETS if SHOULD-PARK? returns True. Will park the thread
until woken by an unpark from another thread. Upon an unpark, the thread will resume even
if SHOULD-PARK? is False! SHOULD-PARK? is only checked to determine if the thread should
park, *not* if it should resume. Can specify a timeout.

Concurrent:
  - WARNING: SHOULD-PARK? must not block, or the thread could be left blocked in a masked
    state.



***

#### <a href="#io-io-impl-conc-parking-park-in-sets-if_-value"><code>(PARK-IN-SETS-IF_)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/io-impl/conc/parking.lisp#L25-L35">src</a></sub></sup><a name="io-io-impl-conc-parking-park-in-sets-if_-value"></a>
<code>&forall; (:M (Type &rarr; Type)). <a href="#io-classes-monad-io-monadio-class">MonadIo</a> :M &rArr; <a href="#io-io-impl-simple-io-io-type">IO</a> <a href="#coalton-boolean-type">Boolean</a> * <a href="#coalton-list-type">List</a> <a href="#io-gen-impl-conc-parking-parkingset-type">ParkingSet</a> &key (:timeout <a href="#io-classes-thread-timeoutstrategy-type">TimeoutStrategy</a>) &rarr; :M <a href="#coalton-unit-type">Unit</a></code>

Parks the current thread in PSETS if SHOULD-PARK? returns True. Will park the thread
until woken by an unpark from another thread. Upon an unpark, the thread will resume even
if SHOULD-PARK? is False! SHOULD-PARK? is only checked to determine if the thread should
park, *not* if it should resume. Can specify a timeout.

Concurrent:
  - WARNING: SHOULD-PARK? must not block, or the thread could be left blocked in a masked
    state.
  - Can briefly block while trying to park the thread, if contended.



***

#### <a href="#io-gen-impl-conc-parking-parking-set-empty?-value"><code>(PARKING-SET-EMPTY? PSET)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/parking.lisp#L322-L325">src</a></sub></sup><a name="io-gen-impl-conc-parking-parking-set-empty?-value"></a>
<code>&forall; (:M (Type &rarr; Type)). <a href="#io-classes-monad-io-monadio-class">MonadIo</a> :M &rArr; <a href="#io-gen-impl-conc-parking-parkingset-type">ParkingSet</a> &rarr; :M <a href="#coalton-boolean-type">Boolean</a></code>

Check if `pset` is empty.



***

#### <a href="#io-gen-impl-conc-parking-unpark-one-value"><code>(UNPARK-ONE PSET)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/parking.lisp#L292-L299">src</a></sub></sup><a name="io-gen-impl-conc-parking-unpark-one-value"></a>
<code>&forall; :RT :T (:M (Type &rarr; Type)). <a href="#io-classes-thread-threads-class">Threads</a> :RT :T :M &rArr; <a href="#io-gen-impl-conc-parking-parkingset-type">ParkingSet</a> &key (:fair <a href="#coalton-boolean-type">Boolean</a>) &rarr; :M <a href="#coalton-unit-type">Unit</a></code>

Unpark one thread parked in `pset`. If no threads are parked, does nothing.

If `fair` is `True`, unparks the thread which has been parked the longest. If `fair` is
`False`, unparks an arbitrary parked thread.



***

#### <a href="#io-gen-impl-conc-parking-unpark-set-value"><code>(UNPARK-SET PSET)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/parking.lisp#L248-L252">src</a></sub></sup><a name="io-gen-impl-conc-parking-unpark-set-value"></a>
<code>&forall; :RT :T (:M (Type &rarr; Type)). <a href="#io-classes-thread-threads-class">Threads</a> :RT :T :M &rArr; <a href="#io-gen-impl-conc-parking-parkingset-type">ParkingSet</a> &rarr; :M <a href="#coalton-unit-type">Unit</a></code>

Atomically reset PSET, then attempt to unpark all threads parked on the set.



***

#### <a href="#io-gen-impl-conc-parking-new-parking-set-value"><code>NEW-PARKING-SET</code></a> <sup><sub>[VALUE] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/parking.lisp#L67-L69">src</a></sub></sup><a name="io-gen-impl-conc-parking-new-parking-set-value"></a>
<code>&forall; (:M (Type &rarr; Type)). <a href="#io-classes-monad-io-monadio-class">MonadIo</a> :M &rArr; :M <a href="#io-gen-impl-conc-parking-parkingset-type">ParkingSet</a></code>

Create a new ParkingSet.



***

# Package `IO/CONC/GROUP`<a name="io-conc-group-package"></a>

### Structs

#### <a href="#io-gen-impl-conc-group-concurrentgroup-type"><code>ConcurrentGroup :C :A</code></a> <sup><sub>[STRUCT] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/concurrent-group.lisp#L36-L60">src</a></sub></sup><a name="io-gen-impl-conc-group-concurrentgroup-type"></a>

Handles masking, stopping, and awaiting a group of Concurrents as a unit. ConcurrentGroup
does not pass messages/data. For more structured uses, see WorkerPool.

ConcurrentGroup provides the following guarantees:
* Masking/unmasking the group is atomic. If another thread attempts to stop the group, it will
  either stop all of the Concurrents or none of them.
* Stopping the group sends the stop signal to all of the enclosed Concurrents.
* Awaiting the group blocks until all of the enclosed Concurrents have completed.
* Awaiting the group will error if any one of the enclosed Concurrents errored.
* Calling unmask-finally on the group runs the callback on each Concurrent separately, not
  once on the thread calling (unmask-finally).

ConcurrentGroup's guarantees are only valid if management of the enclosed Concurrents is
done through the group. For example, if one thread tries to mask the group while another
thread tries to stop one of the individual Concurrents in the group, then the second
thread might stop the individual Concurrent before the first thread has a chance to mask
it.

Concurrent:
  - The `(Concurrent ConcurrentGroup :a)' instance masks `stop`, `mask`, `unmask`,
    and `unmask-finally` to guarantee atomicity of the enclosed Concurrents
  - `await` is not masked

<details>
<summary>Instances</summary>

- <code><a href="#io-classes-thread-concurrent-class">Concurrent</a> :C :A &rArr; <a href="#io-classes-thread-concurrent-class">Concurrent</a> (<a href="#io-gen-impl-conc-group-concurrentgroup-type">ConcurrentGroup</a> :C :A) (<a href="#coalton-list-type">List</a> :A)</code>
- <code><a href="#coalton-types-runtimerepr-class">RuntimeRepr</a> (<a href="#io-gen-impl-conc-group-concurrentgroup-type">ConcurrentGroup</a> :|0| :|1|)</code>

</details>



***

### Values

#### <a href="#io-gen-impl-conc-group-enclose-group-value"><code>(ENCLOSE-GROUP CONCURRENTS)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/concurrent-group.lisp#L99-L103">src</a></sub></sup><a name="io-gen-impl-conc-group-enclose-group-value"></a>
<code>&forall; :RT :T :C (:M (Type &rarr; Type)) :A. (<a href="#io-classes-thread-threads-class">Threads</a> :RT :T :M) (<a href="#io-classes-thread-concurrent-class">Concurrent</a> :C :A) &rArr; <a href="#coalton-list-type">List</a> :C &rarr; :M (<a href="#io-gen-impl-conc-group-concurrentgroup-type">ConcurrentGroup</a> :C :A)</code>

Enclose already forked Concurrents in a ConcurrentGroup.

Warning: After calling, the enclosed Concurrents should only be managed through the group.



***

#### <a href="#io-gen-impl-conc-group-fork-group-value"><code>(FORK-GROUP FORK-CONCURRENTS)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/concurrent-group.lisp#L86-L95">src</a></sub></sup><a name="io-gen-impl-conc-group-fork-group-value"></a>
<code>&forall; :RT :T (:M (Type &rarr; Type)) :C :A. (<a href="#io-classes-thread-threads-class">Threads</a> :RT :T :M) (<a href="#io-classes-thread-concurrent-class">Concurrent</a> :C :A) &rArr; <a href="#coalton-list-type">List</a> (:M :C) &rarr; :M (<a href="#io-gen-impl-conc-group-concurrentgroup-type">ConcurrentGroup</a> :C :A)</code>

Run a list of IO operations that each forks a Concurrent. Enclose the forked Concurrents in a
ConcurrentGroup.

Concurrent:
  - Does not mask the forking operation, so it is possible to partially fork if stopped.
    To guarantee forking completion, caller should mask the call to `fork-group`.



***

#### <a href="#io-gen-impl-conc-group-fork-n-threads-value"><code>(FORK-N-THREADS N-THREADS OP)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/concurrent-group.lisp#L74-L82">src</a></sub></sup><a name="io-gen-impl-conc-group-fork-n-threads-value"></a>
<code>&forall; (:M (Type &rarr; Type)) :RT (:R (Type &rarr; Type)) :A :T. (<a href="#io-classes-monad-io-unliftio-class">UnliftIo</a> :R :M) (<a href="#io-classes-thread-threads-class">Threads</a> :RT :T :R) &rArr; <a href="#coalton-ufix-type">UFix</a> * (<a href="#coalton-ufix-type">UFix</a> &rarr; :R :A) &rarr; :R (<a href="#io-gen-impl-conc-group-concurrentgroup-type">ConcurrentGroup</a> :T :A)</code>

Fork `n-threads` running `op` in a `ConcurrentGroup`. Passes a thread index,
from [0, `n-threads`) to `op`.



***

### Macros

#### <a href="#io-gen-impl-conc-group-do-fork-n-threads-macro"><code>DO-FORK-N-THREADS ((SYM N-THREADS) &amp;BODY BODY)</code></a> <sup><sub>[MACRO]</sub></sup><a name="io-gen-impl-conc-group-do-fork-n-threads-macro"></a>


***

# Package `IO/CONC/SCHEDULER`<a name="io-conc-scheduler-package"></a>

### Types

#### <a href="#io-gen-impl-conc-bounded-scheduler-boundedscheduler-type"><code>BoundedScheduler</code></a> <sup><sub>[TYPE] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/bounded-scheduler.lisp#L30-L32">src</a></sub></sup><a name="io-gen-impl-conc-bounded-scheduler-boundedscheduler-type"></a>

A `BoundedScheduler` has a limited capacity, and task submissions block if full.

<details>
<summary>Instances</summary>

- <code><a href="#coalton-types-runtimerepr-class">RuntimeRepr</a> (<a href="#io-gen-impl-conc-bounded-scheduler-boundedscheduler-type">BoundedScheduler</a> :|0|)</code>
- <code><a href="#io-classes-conc-scheduler-scheduler-class">Scheduler</a> <a href="#io-gen-impl-conc-bounded-scheduler-boundedscheduler-type">BoundedScheduler</a></code>

</details>



***

#### <a href="#io-gen-impl-conc-bounded-scheduler-schedulererror-type"><code>SchedulerError</code></a> <sup><sub>[TYPE] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/bounded-scheduler.lisp#L26-L27">src</a></sub></sup><a name="io-gen-impl-conc-bounded-scheduler-schedulererror-type"></a>
<details>
<summary>Instances</summary>

- <code><a href="#coalton-classes-exception-class">Exception</a> <a href="#io-gen-impl-conc-bounded-scheduler-schedulererror-type">SchedulerError</a></code>
- <code><a href="#coalton-types-runtimerepr-class">RuntimeRepr</a> <a href="#io-gen-impl-conc-bounded-scheduler-schedulererror-type">SchedulerError</a></code>

</details>



***

#### <a href="#io-gen-impl-conc-unbounded-scheduler-unboundedscheduler-type"><code>UnboundedScheduler</code></a> <sup><sub>[TYPE] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/unbounded-scheduler.lisp#L25-L28">src</a></sub></sup><a name="io-gen-impl-conc-unbounded-scheduler-unboundedscheduler-type"></a>

An `UnboundedScheduler` never blocks when submitting tasks, but can overflow memory
if too many tasks are submitted.

<details>
<summary>Instances</summary>

- <code><a href="#coalton-types-runtimerepr-class">RuntimeRepr</a> (<a href="#io-gen-impl-conc-unbounded-scheduler-unboundedscheduler-type">UnboundedScheduler</a> :|0|)</code>
- <code><a href="#io-classes-conc-scheduler-scheduler-class">Scheduler</a> <a href="#io-gen-impl-conc-unbounded-scheduler-unboundedscheduler-type">UnboundedScheduler</a></code>

</details>



***

### Classes

#### <a href="#io-classes-conc-scheduler-scheduler-class"><code>Scheduler</code></a> <sup><sub>[CLASS] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/classes/conc/scheduler.lisp#L24-L65">src</a></sub></sup><a name="io-classes-conc-scheduler-scheduler-class"></a>
<code><a href="#io-classes-conc-scheduler-scheduler-class">Scheduler</a> :S</code>


A Scheduler distributes work from producer threads to worker threads. Generally, the
Scheduler is allowed to completely control the worker threads: it can sleep, block, spin,
etc., as dictated by the Scheduler algorithm. However, it must give producer threads the
choice of how to respond to the state of the Scheduler. For example, if the Scheduler is
bounded and its internal queue is full, the producer thread chooses whether to block or
fail based on its choice of which submit function to call.

The main purpose of a Scheduler is to pass into a WorkerPool to configure its scheduling
algorithm. A Scheduler could have uses in other contexts.

A Scheduler may be bounded or unbounded and still satisfy the Scheduler interface. For
unbounded schedulers, `submit` will never block and `try-submit` will always succeed.

Threads asking for work will submit a thread-index to the scheduler. The Scheduler can
choose to use this, particularly if it maintains a separate queue for each thread, or
choose to ignore it, if it only uses a single queue for all threads. Thread index values
should be 0-indexed, from [0, n-threads).

Methods:
- <code>SUBMIT :: <a href="#io-classes-thread-threads-class">Threads</a> :RT :T :M &rArr; :A * :S :A &rarr; :M <a href="#coalton-unit-type">Unit</a></code><br/>Submit a new item to the Scheduler.&#xA;&#xA;Concurrent:&#xA;  - Blocks if the Scheduler is full. Only bounded Schedulers will ever be full.
- <code>SUBMIT-WITH :: <a href="#io-classes-thread-threads-class">Threads</a> :RT :T :M &rArr; :A * <a href="#io-classes-thread-timeoutstrategy-type">TimeoutStrategy</a> * :S :A &rarr; :M <a href="#coalton-unit-type">Unit</a></code><br/>Submit a new item to the Scheduler.&#xA;&#xA;Concurrent:&#xA;  - Blocks if the Scheduler is full, possibly timing out based on STRATEGY. Only bounded&#xA;Schedulers will ever be full.
- <code>TRY-SUBMIT :: <a href="#io-classes-thread-threads-class">Threads</a> :RT :T :M &rArr; :A * :S :A &rarr; :M <a href="#coalton-boolean-type">Boolean</a></code><br/>Attempt to submit a new item to the Scheduler. Returns `True` if the item was added,&#xA;or `False` if the Scheduler was full. Only bounded Schedulers can be full.
- <code>TAKE-ITEM :: <a href="#io-classes-thread-threads-class">Threads</a> :RT :T :M &rArr; <a href="#coalton-ufix-type">UFix</a> * :S :A &rarr; :M :A</code><br/>Take the next item from the Scheduler for the given thread.&#xA;&#xA;Concurrent:&#xA;  - May block, sleep, spin, or do anything else to the requesting thread, except&#xA;    (1) leave it masked after returning, or (2) stop the thread.
<details>
<summary>Instances</summary>

- <code><a href="#io-classes-conc-scheduler-scheduler-class">Scheduler</a> <a href="#io-gen-impl-conc-unbounded-scheduler-unboundedscheduler-type">UnboundedScheduler</a></code>
- <code><a href="#io-classes-conc-scheduler-scheduler-class">Scheduler</a> <a href="#io-gen-impl-conc-bounded-scheduler-boundedscheduler-type">BoundedScheduler</a></code>

</details>



***

### Values

#### <a href="#io-gen-impl-conc-bounded-scheduler-new-bounded-scheduler-value"><code>(NEW-BOUNDED-SCHEDULER CAPACITY)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/bounded-scheduler.lisp#L37-L40">src</a></sub></sup><a name="io-gen-impl-conc-bounded-scheduler-new-bounded-scheduler-value"></a>
<code>&forall; :RT :T (:M (Type &rarr; Type)) :A. (<a href="#io-classes-thread-threads-class">Threads</a> :RT :T :M) (<a href="#io-classes-exceptions-exceptions-class">Exceptions</a> :M) &rArr; <a href="#coalton-ufix-type">UFix</a> &rarr; :M (<a href="#io-gen-impl-conc-bounded-scheduler-boundedscheduler-type">BoundedScheduler</a> :A)</code>


***

#### <a href="#io-gen-impl-conc-unbounded-scheduler-new-unbounded-scheduler-value"><code>NEW-UNBOUNDED-SCHEDULER</code></a> <sup><sub>[VALUE] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/unbounded-scheduler.lisp#L32-L33">src</a></sub></sup><a name="io-gen-impl-conc-unbounded-scheduler-new-unbounded-scheduler-value"></a>
<code>&forall; :RT :T (:M (Type &rarr; Type)) :A. <a href="#io-classes-thread-threads-class">Threads</a> :RT :T :M &rArr; :M (<a href="#io-gen-impl-conc-unbounded-scheduler-unboundedscheduler-type">UnboundedScheduler</a> :A)</code>


***

# Package `IO/CONC/WORKER-POOL`<a name="io-conc-worker-pool-package"></a>

### Structs

#### <a href="#io-gen-impl-conc-worker-pool-indexchunk-type"><code>IndexChunk</code></a> <sup><sub>[STRUCT] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/worker-pool.lisp#L167-L170">src</a></sub></sup><a name="io-gen-impl-conc-worker-pool-indexchunk-type"></a>

A chunk of indexed work with a start and end (exclusive).

<details>
<summary>Instances</summary>

- <code>(<a href="#coalton-classes-eq-class">Eq</a> <a href="#coalton-ufix-type">UFix</a>) (<a href="#coalton-classes-eq-class">Eq</a> <a href="#coalton-ufix-type">UFix</a>) &rArr; <a href="#coalton-classes-eq-class">Eq</a> <a href="#io-gen-impl-conc-worker-pool-indexchunk-type">IndexChunk</a></code>
- <code><a href="#coalton-iterator-intoiterator-class">IntoIterator</a> <a href="#io-gen-impl-conc-worker-pool-indexchunk-type">IndexChunk</a> <a href="#coalton-ufix-type">UFix</a></code>
- <code><a href="#coalton-types-runtimerepr-class">RuntimeRepr</a> <a href="#io-gen-impl-conc-worker-pool-indexchunk-type">IndexChunk</a></code>

</details>



***

#### <a href="#io-gen-impl-conc-worker-pool-workerpool-type"><code>WorkerPool :S :I :T</code></a> <sup><sub>[STRUCT] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/worker-pool.lisp#L47-L51">src</a></sub></sup><a name="io-gen-impl-conc-worker-pool-workerpool-type"></a>

A pool of worker threads that execute jobs submitted to the pool.

<details>
<summary>Instances</summary>

- <code>(<a href="#io-classes-thread-concurrent-class">Concurrent</a> (<a href="#io-gen-impl-conc-group-concurrentgroup-type">ConcurrentGroup</a> :T <a href="#coalton-unit-type">Unit</a>) (<a href="#coalton-list-type">List</a> <a href="#coalton-unit-type">Unit</a>)) (<a href="#io-classes-conc-scheduler-scheduler-class">Scheduler</a> :S) &rArr; <a href="#io-classes-thread-concurrent-class">Concurrent</a> (<a href="#io-gen-impl-conc-worker-pool-workerpool-type">WorkerPool</a> :S :I :T) <a href="#coalton-unit-type">Unit</a></code>
- <code><a href="#coalton-types-runtimerepr-class">RuntimeRepr</a> (<a href="#io-gen-impl-conc-worker-pool-workerpool-type">WorkerPool</a> :|0| :|1| :|2|)</code>

</details>



***

### Values

#### <a href="#io-gen-impl-conc-worker-pool-new-worker-pool-value"><code>(NEW-WORKER-POOL N-THREADS SCHEDULER)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/worker-pool.lisp#L74-L86">src</a></sub></sup><a name="io-gen-impl-conc-worker-pool-new-worker-pool-value"></a>
<code>&forall; :RT (:S (Type &rarr; Type)) (:I (Type &rarr; Type)) (:M (Type &rarr; Type)) :T. (<a href="#io-classes-thread-threads-class">Threads</a> :RT :T :M) (<a href="#io-classes-monad-io-unliftio-class">UnliftIo</a> :I :I) (<a href="#io-classes-thread-threads-class">Threads</a> :RT :T :I) (<a href="#io-classes-exceptions-exceptions-class">Exceptions</a> :I) (<a href="#io-classes-exceptions-exceptions-class">Exceptions</a> :M) (<a href="#io-classes-thread-concurrent-class">Concurrent</a> :T <a href="#coalton-unit-type">Unit</a>) (<a href="#io-classes-monad-io-liftio-class">LiftIo</a> :I :M) (<a href="#io-classes-conc-scheduler-scheduler-class">Scheduler</a> :S) &rArr; <a href="#coalton-ufix-type">UFix</a> * :S (<a href="#coalton-optional-type">Optional</a> (:I <a href="#coalton-unit-type">Unit</a>)) &rarr; :M (<a href="#io-gen-impl-conc-worker-pool-workerpool-type">WorkerPool</a> :S :I :T)</code>

Create a new worker pool. Automatically forks N-THREADS worker threads.



***

#### <a href="#io-io-impl-conc-worker-pool-new-worker-pool_-value"><code>(NEW-WORKER-POOL_)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/io-impl/conc/worker-pool.lisp#L34-L34">src</a></sub></sup><a name="io-io-impl-conc-worker-pool-new-worker-pool_-value"></a>
<code>&forall; (:S (Type &rarr; Type)) (:M (Type &rarr; Type)). (<a href="#io-classes-thread-threads-class">Threads</a> <a href="#io-io-impl-runtime-ioruntime-type">IoRuntime</a> <a href="#io-io-impl-io-runtime-runtime-iothread-type">IoThread</a> :M) (<a href="#io-classes-exceptions-exceptions-class">Exceptions</a> :M) (<a href="#io-classes-monad-io-liftio-class">LiftIo</a> <a href="#io-io-impl-simple-io-io-type">IO</a> :M) (<a href="#io-classes-conc-scheduler-scheduler-class">Scheduler</a> :S) &rArr; <a href="#coalton-ufix-type">UFix</a> * :S (<a href="#coalton-optional-type">Optional</a> (<a href="#io-io-impl-simple-io-io-type">IO</a> <a href="#coalton-unit-type">Unit</a>)) &rarr; :M (<a href="#io-gen-impl-conc-worker-pool-workerpool-type">WorkerPool</a> :S <a href="#io-io-impl-simple-io-io-type">IO</a> <a href="#io-io-impl-io-runtime-runtime-iothread-type">IoThread</a>)</code>


***

#### <a href="#io-gen-impl-conc-worker-pool-request-shutdown-value"><code>(REQUEST-SHUTDOWN POOL)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/worker-pool.lisp#L120-L126">src</a></sub></sup><a name="io-gen-impl-conc-worker-pool-request-shutdown-value"></a>
<code>&forall; :RT (:S (Type &rarr; Type)) (:I (Type &rarr; Type)) :T (:M (Type &rarr; Type)). (<a href="#io-classes-thread-threads-class">Threads</a> :RT :T :M) (<a href="#io-classes-exceptions-exceptions-class">Exceptions</a> :M) (<a href="#io-classes-conc-scheduler-scheduler-class">Scheduler</a> :S) &rArr; <a href="#io-gen-impl-conc-worker-pool-workerpool-type">WorkerPool</a> :S :I :T &rarr; :M <a href="#coalton-unit-type">Unit</a></code>

Request a shutdown. The threads in the pool will shutdown when all of the jobs already
in the queue are completed.

To immediately stop the threads, use `stop`.



***

#### <a href="#io-gen-impl-conc-worker-pool-submit-indexed-chunks-value"><code>(SUBMIT-INDEXED-CHUNKS POOL START END CHUNK-SIZE CHUNK-&gt;JOB)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/worker-pool.lisp#L182-L214">src</a></sub></sup><a name="io-gen-impl-conc-worker-pool-submit-indexed-chunks-value"></a>
<code>&forall; :RT (:S (Type &rarr; Type)) (:I (Type &rarr; Type)) :T (:R (Type &rarr; Type)) :A (:M (Type &rarr; Type)). (<a href="#io-classes-monad-io-unliftio-class">UnliftIo</a> :R :I) (<a href="#coalton-monad-classes-liftto-class">LiftTo</a> :R :M) (<a href="#io-classes-thread-threads-class">Threads</a> :RT :T :I) (<a href="#io-classes-exceptions-exceptions-class">Exceptions</a> :I) (<a href="#io-classes-conc-scheduler-scheduler-class">Scheduler</a> :S) (<a href="#io-classes-exceptions-exceptions-class">Exceptions</a> :M) &rArr; <a href="#io-gen-impl-conc-worker-pool-workerpool-type">WorkerPool</a> :S :I :T * <a href="#coalton-ufix-type">UFix</a> * <a href="#coalton-ufix-type">UFix</a> * <a href="#coalton-ufix-type">UFix</a> * (<a href="#io-gen-impl-conc-worker-pool-indexchunk-type">IndexChunk</a> &rarr; :R :A) &key (:timeout <a href="#io-classes-thread-timeoutstrategy-type">TimeoutStrategy</a>) &rarr; :M <a href="#coalton-unit-type">Unit</a></code>

Submit chunks of indexed-work to the worker pool. The first chunk starts at `start`,
and the last chunk ends at `end`, exclusive. Each chunk has size `chunk-size`, except the
last, which has the remaining work. A job is responsible for executing all work inside the
chunk it is passed.

Any jobs submitted after a shutdown request will be ignored.

Concurrent:
  - If the pool's scheduler is backed by a bounded data structure, then this can block
    while the scheduler is full.



***

#### <a href="#io-io-impl-conc-worker-pool-submit-indexed-chunks_-value"><code>(SUBMIT-INDEXED-CHUNKS_)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/io-impl/conc/worker-pool.lisp#L50-L61">src</a></sub></sup><a name="io-io-impl-conc-worker-pool-submit-indexed-chunks_-value"></a>
<code>&forall; (:S (Type &rarr; Type)) :A (:M (Type &rarr; Type)). (<a href="#coalton-monad-classes-liftto-class">LiftTo</a> <a href="#io-io-impl-simple-io-io-type">IO</a> :M) (<a href="#io-classes-conc-scheduler-scheduler-class">Scheduler</a> :S) (<a href="#io-classes-exceptions-exceptions-class">Exceptions</a> :M) &rArr; <a href="#io-gen-impl-conc-worker-pool-workerpool-type">WorkerPool</a> :S <a href="#io-io-impl-simple-io-io-type">IO</a> <a href="#io-io-impl-io-runtime-runtime-iothread-type">IoThread</a> * <a href="#coalton-ufix-type">UFix</a> * <a href="#coalton-ufix-type">UFix</a> * <a href="#coalton-ufix-type">UFix</a> * (<a href="#io-gen-impl-conc-worker-pool-indexchunk-type">IndexChunk</a> &rarr; <a href="#io-io-impl-simple-io-io-type">IO</a> :A) &key (:timeout <a href="#io-classes-thread-timeoutstrategy-type">TimeoutStrategy</a>) &rarr; :M <a href="#coalton-unit-type">Unit</a></code>

Submit chunks of indexed-work to the worker pool. The first chunk starts at `start`,
and the last chunk ends at `end`, exclusive. Each chunk has size `chunk-size`, except the
last, which has the remaining work. A job is responsible for executing all work inside the
chunk it is passed.

Any jobs submitted after a shutdown request will be ignored.

Concurrent:
  - If the pool's scheduler is backed by a bounded data structure, then this can block
    while the scheduler is full.



***

#### <a href="#io-gen-impl-conc-worker-pool-submit-job-value"><code>(SUBMIT-JOB POOL JOB)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/worker-pool.lisp#L109-L116">src</a></sub></sup><a name="io-gen-impl-conc-worker-pool-submit-job-value"></a>
<code>&forall; :RT (:S (Type &rarr; Type)) (:I (Type &rarr; Type)) :T (:R (Type &rarr; Type)) :A (:M (Type &rarr; Type)). (<a href="#io-classes-monad-io-unliftio-class">UnliftIo</a> :R :I) (<a href="#coalton-monad-classes-liftto-class">LiftTo</a> :R :M) (<a href="#io-classes-thread-threads-class">Threads</a> :RT :T :I) (<a href="#io-classes-exceptions-exceptions-class">Exceptions</a> :I) (<a href="#io-classes-conc-scheduler-scheduler-class">Scheduler</a> :S) &rArr; <a href="#io-gen-impl-conc-worker-pool-workerpool-type">WorkerPool</a> :S :I :T * :R :A &rarr; :M <a href="#coalton-unit-type">Unit</a></code>

Submit a job to the worker pool. Any jobs submitted after a shutdown request will
be ignored.

Concurrent:
  - If the pool's scheduler is backed by a bounded data structure, then this can block
    while the scheduler is full.



***

#### <a href="#io-gen-impl-conc-worker-pool-submit-job-with-value"><code>(SUBMIT-JOB-WITH STRATEGY POOL JOB)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/worker-pool.lisp#L91-L103">src</a></sub></sup><a name="io-gen-impl-conc-worker-pool-submit-job-with-value"></a>
<code>&forall; :RT (:S (Type &rarr; Type)) (:I (Type &rarr; Type)) :T (:R (Type &rarr; Type)) :A (:M (Type &rarr; Type)). (<a href="#io-classes-monad-io-unliftio-class">UnliftIo</a> :R :I) (<a href="#coalton-monad-classes-liftto-class">LiftTo</a> :R :M) (<a href="#io-classes-thread-threads-class">Threads</a> :RT :T :I) (<a href="#io-classes-exceptions-exceptions-class">Exceptions</a> :I) (<a href="#io-classes-conc-scheduler-scheduler-class">Scheduler</a> :S) &rArr; <a href="#io-classes-thread-timeoutstrategy-type">TimeoutStrategy</a> * <a href="#io-gen-impl-conc-worker-pool-workerpool-type">WorkerPool</a> :S :I :T * :R :A &rarr; :M <a href="#coalton-unit-type">Unit</a></code>

Submit a job to the worker pool. Any jobs submitted after a shutdown request will
be ignored.

Concurrent:
  - If the pool's scheduler is backed by a bounded data structure, then this can block
    while the scheduler is full, possibly timing out based on STRATEGY.



***

#### <a href="#io-io-impl-conc-worker-pool-submit-job-with_-value"><code>(SUBMIT-JOB-WITH_)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/io-impl/conc/worker-pool.lisp#L44-L44">src</a></sub></sup><a name="io-io-impl-conc-worker-pool-submit-job-with_-value"></a>
<code>&forall; (:S (Type &rarr; Type)) :A (:M (Type &rarr; Type)). (<a href="#io-classes-thread-threads-class">Threads</a> <a href="#io-io-impl-runtime-ioruntime-type">IoRuntime</a> <a href="#io-io-impl-io-runtime-runtime-iothread-type">IoThread</a> :M) (<a href="#coalton-monad-classes-liftto-class">LiftTo</a> <a href="#io-io-impl-simple-io-io-type">IO</a> :M) (<a href="#io-classes-conc-scheduler-scheduler-class">Scheduler</a> :S) &rArr; <a href="#io-classes-thread-timeoutstrategy-type">TimeoutStrategy</a> * <a href="#io-gen-impl-conc-worker-pool-workerpool-type">WorkerPool</a> :S <a href="#io-io-impl-simple-io-io-type">IO</a> <a href="#io-io-impl-io-runtime-runtime-iothread-type">IoThread</a> * <a href="#io-io-impl-simple-io-io-type">IO</a> :A &rarr; :M <a href="#coalton-unit-type">Unit</a></code>


***

#### <a href="#io-io-impl-conc-worker-pool-submit-job_-value"><code>(SUBMIT-JOB_)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/io-impl/conc/worker-pool.lisp#L39-L39">src</a></sub></sup><a name="io-io-impl-conc-worker-pool-submit-job_-value"></a>
<code>&forall; (:S (Type &rarr; Type)) :A (:M (Type &rarr; Type)). (<a href="#io-classes-thread-threads-class">Threads</a> <a href="#io-io-impl-runtime-ioruntime-type">IoRuntime</a> <a href="#io-io-impl-io-runtime-runtime-iothread-type">IoThread</a> :M) (<a href="#coalton-monad-classes-liftto-class">LiftTo</a> <a href="#io-io-impl-simple-io-io-type">IO</a> :M) (<a href="#io-classes-conc-scheduler-scheduler-class">Scheduler</a> :S) &rArr; <a href="#io-gen-impl-conc-worker-pool-workerpool-type">WorkerPool</a> :S <a href="#io-io-impl-simple-io-io-type">IO</a> <a href="#io-io-impl-io-runtime-runtime-iothread-type">IoThread</a> * <a href="#io-io-impl-simple-io-io-type">IO</a> :A &rarr; :M <a href="#coalton-unit-type">Unit</a></code>


***

### Macros

#### <a href="#io-gen-impl-conc-worker-pool-do-submit-indexed-chunks-macro"><code>DO-SUBMIT-INDEXED-CHUNKS (POOL (CHUNK-SYM (START END CHUNK-SIZE &amp;KEY (TIMEOUT (QUOTE NOTIMEOUT)))) &amp;BODY BODY)</code></a> <sup><sub>[MACRO]</sub></sup><a name="io-gen-impl-conc-worker-pool-do-submit-indexed-chunks-macro"></a>

Submit chunks of indexed-work to the worker pool. The first chunk starts at `start`,
and the last chunk ends at `end`, exclusive. Each chunk has size `chunk-size`, except the
last, which has the remaining work. A job is responsible for executing all work inside the
chunk it is passed.

Any jobs submitted after a shutdown request will be ignored.

Concurrent:
  - If the pool's scheduler is backed by a bounded data structure, then this can block
    while the scheduler is full.



***

#### <a href="#io-io-impl-conc-worker-pool-do-submit-indexed-chunks_-macro"><code>DO-SUBMIT-INDEXED-CHUNKS_ (POOL (CHUNK-SYM (START END CHUNK-SIZE &amp;KEY (TIMEOUT (QUOTE NOTIMEOUT)))) &amp;BODY BODY)</code></a> <sup><sub>[MACRO]</sub></sup><a name="io-io-impl-conc-worker-pool-do-submit-indexed-chunks_-macro"></a>

Submit chunks of indexed-work to the worker pool. The first chunk starts at `start`,
and the last chunk ends at `end`, exclusive. Each chunk has size `chunk-size`, except the
last, which has the remaining work. A job is responsible for executing all work inside the
chunk it is passed.

Any jobs submitted after a shutdown request will be ignored.

Concurrent:
  - If the pool's scheduler is backed by a bounded data structure, then this can block
    while the scheduler is full.



***

#### <a href="#io-gen-impl-conc-worker-pool-do-submit-job-macro"><code>DO-SUBMIT-JOB (POOL &amp;BODY BODY)</code></a> <sup><sub>[MACRO]</sub></sup><a name="io-gen-impl-conc-worker-pool-do-submit-job-macro"></a>


***

#### <a href="#io-gen-impl-conc-worker-pool-do-submit-job-with-macro"><code>DO-SUBMIT-JOB-WITH (STRATEGY POOL &amp;BODY BODY)</code></a> <sup><sub>[MACRO]</sub></sup><a name="io-gen-impl-conc-worker-pool-do-submit-job-with-macro"></a>


***

#### <a href="#io-io-impl-conc-worker-pool-do-submit-job-with_-macro"><code>DO-SUBMIT-JOB-WITH_ (STRATEGY POOL &amp;BODY BODY)</code></a> <sup><sub>[MACRO]</sub></sup><a name="io-io-impl-conc-worker-pool-do-submit-job-with_-macro"></a>


***

#### <a href="#io-io-impl-conc-worker-pool-do-submit-job_-macro"><code>DO-SUBMIT-JOB_ (POOL &amp;BODY BODY)</code></a> <sup><sub>[MACRO]</sub></sup><a name="io-io-impl-conc-worker-pool-do-submit-job_-macro"></a>


***

# Package `IO/CONC/FUTURE`<a name="io-conc-future-package"></a>

### Types

#### <a href="#io-gen-impl-conc-future-future-type"><code>Future</code></a> <sup><sub>[TYPE] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/io-future.lisp#L34-L37">src</a></sub></sup><a name="io-gen-impl-conc-future-future-type"></a>

A concurrent value that starts empty, and can be satisfied or failed by the
associated `Promise`. Once a future is completed, it cannot be overwritten.

<details>
<summary>Instances</summary>

- <code><a href="#coalton-types-runtimerepr-class">RuntimeRepr</a> (<a href="#io-gen-impl-conc-future-future-type">Future</a> :|0|)</code>

</details>



***

#### <a href="#io-gen-impl-conc-future-promise-type"><code>Promise</code></a> <sup><sub>[TYPE] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/io-future.lisp#L45-L47">src</a></sub></sup><a name="io-gen-impl-conc-future-promise-type"></a>

Used to satisfy or fail the associated `Future`.

<details>
<summary>Instances</summary>

- <code><a href="#coalton-types-runtimerepr-class">RuntimeRepr</a> (<a href="#io-gen-impl-conc-future-promise-type">Promise</a> :|0|)</code>

</details>



***

### Values

#### <a href="#io-gen-impl-conc-future-await-future-value"><code>(AWAIT-FUTURE F)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/io-future.lisp#L69-L78">src</a></sub></sup><a name="io-gen-impl-conc-future-await-future-value"></a>
<code>&forall; :RT :T :A (:M (Type &rarr; Type)). (<a href="#io-classes-thread-threads-class">Threads</a> :RT :T :M) (<a href="#io-classes-exceptions-exceptions-class">Exceptions</a> :M) &rArr; <a href="#io-gen-impl-conc-future-future-type">Future</a> :A &key (:timeout <a href="#io-classes-thread-timeoutstrategy-type">TimeoutStrategy</a>) &rarr; :M :A</code>

Await for `f` to be completed. Raises an exception if `f` has failed. Can specify a
timeout.

Does not clear `f`. Repeated awaits to `f` will return the same result.

Concurrent:
  - If `f` is not completed, blocks until `f` is completed.



***

#### <a href="#io-gen-impl-conc-future-fail-future-value"><code>(FAIL-FUTURE P EXC)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/io-future.lisp#L91-L96">src</a></sub></sup><a name="io-gen-impl-conc-future-fail-future-value"></a>
<code>&forall; :RT :T :A :E (:M (Type &rarr; Type)). (<a href="#io-classes-thread-threads-class">Threads</a> :RT :T :M) (<a href="#coalton-classes-exception-class">Exception</a> :E) &rArr; <a href="#io-gen-impl-conc-future-promise-type">Promise</a> :A * :E &rarr; :M <a href="#coalton-boolean-type">Boolean</a></code>

Fail the future corresponding to `p` with `exc`.

Will not overwrite a previously completed future. Returns `True` if fails the future,
`False` if it was already completed.



***

#### <a href="#io-gen-impl-conc-future-fork-future-value"><code>(FORK-FUTURE THUNK)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/io-future.lisp#L123-L138">src</a></sub></sup><a name="io-gen-impl-conc-future-fork-future-value"></a>
<code>&forall; (:I (Type &rarr; Type)) :RT :A (:R (Type &rarr; Type)) :B :T (:M (Type &rarr; Type)). (<a href="#io-classes-monad-io-unliftio-class">UnliftIo</a> :R :I) (<a href="#coalton-monad-classes-liftto-class">LiftTo</a> :R :M) (<a href="#io-classes-thread-threads-class">Threads</a> :RT :T :R) (<a href="#io-classes-thread-threads-class">Threads</a> :RT :T :M) &rArr; (<a href="#io-gen-impl-conc-future-promise-type">Promise</a> :A &rarr; :R :B) &key (:scope <a href="#io-classes-thread-forkscope-type">ForkScope</a> :T) (:unhandled <a href="#io-classes-thread-unhandledexceptionstrategy-type">UnhandledExceptionStrategy</a>) &rarr; :M (<a href="#coalton-classes-tuple-type">Tuple</a> (<a href="#io-gen-impl-conc-future-future-type">Future</a> :A) :T)</code>

Create a future-promise pair. Start a new thread with the `Promise`. Return the
`Future` and a handle to the running thread.

Other than receiving the promise as an argument, the spawned thread is independent of the
future. If the thread encounters an unhandled exception or otherwise finishes without
completing the promise, then the future will never be completed unless the spawned thread
transferred the promise to another thread before terminating.

If the thread raises an unhandled exception, the default behavior is to log it to
*ERROR-OUTPUT* and swallow it until/if the thread is joined. The default fork scope is
structured.



***

#### <a href="#io-io-impl-future-fork-future_-value"><code>(FORK-FUTURE_ THUNK)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/io-impl/io-future.lisp#L29-L36">src</a></sub></sup><a name="io-io-impl-future-fork-future_-value"></a>
<code>&forall; :A :B (:M (Type &rarr; Type)). (<a href="#coalton-monad-classes-liftto-class">LiftTo</a> <a href="#io-io-impl-simple-io-io-type">IO</a> :M) (<a href="#io-classes-thread-threads-class">Threads</a> <a href="#io-io-impl-runtime-ioruntime-type">IoRuntime</a> <a href="#io-io-impl-io-runtime-runtime-iothread-type">IoThread</a> :M) &rArr; (<a href="#io-gen-impl-conc-future-promise-type">Promise</a> :A &rarr; <a href="#io-io-impl-simple-io-io-type">IO</a> :B) &key (:scope <a href="#io-classes-thread-forkscope-type">ForkScope</a> <a href="#io-io-impl-io-runtime-runtime-iothread-type">IoThread</a>) (:unhandled <a href="#io-classes-thread-unhandledexceptionstrategy-type">UnhandledExceptionStrategy</a>) &rarr; :M (<a href="#coalton-classes-tuple-type">Tuple</a> (<a href="#io-gen-impl-conc-future-future-type">Future</a> :A) <a href="#io-io-impl-io-runtime-runtime-iothread-type">IoThread</a>)</code>

Create a future-promise pair. Start a new thread with the `Promise`. Return the
`Future` and a handle to the running thread.

If the thread raises an unhandled exception, the default behavior is to log it to
*ERROR-OUTPUT* and swallow it until/if the thread is joined. The default fork scope is
structured.



***

#### <a href="#io-gen-impl-conc-future-satisfy-future-value"><code>(SATISFY-FUTURE P VAL)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/io-future.lisp#L82-L87">src</a></sub></sup><a name="io-gen-impl-conc-future-satisfy-future-value"></a>
<code>&forall; :RT :T :A (:M (Type &rarr; Type)). <a href="#io-classes-thread-threads-class">Threads</a> :RT :T :M &rArr; <a href="#io-gen-impl-conc-future-promise-type">Promise</a> :A * :A &rarr; :M <a href="#coalton-boolean-type">Boolean</a></code>

Satisfy the future corresponding to `p` with `val`.

Will not overwrite a previously completed future. Returns `True` if satisfies the future,
`False` if it was already completed.



***

#### <a href="#io-gen-impl-conc-future-try-read-future-value"><code>(TRY-READ-FUTURE F)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/io-future.lisp#L100-L114">src</a></sub></sup><a name="io-gen-impl-conc-future-try-read-future-value"></a>
<code>&forall; :RT :T :A (:M (Type &rarr; Type)). (<a href="#io-classes-thread-threads-class">Threads</a> :RT :T :M) (<a href="#io-classes-exceptions-exceptions-class">Exceptions</a> :M) &rArr; <a href="#io-gen-impl-conc-future-future-type">Future</a> :A &rarr; :M (<a href="#coalton-optional-type">Optional</a> :A)</code>

Attempt to immediately read from `f`, returning `None` if it is empty. Raises an
exception if `f` has failed.

Concurrent:
  - Does not block.



***

#### <a href="#io-gen-impl-conc-future-new-future-value"><code>NEW-FUTURE</code></a> <sup><sub>[VALUE] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/io-future.lisp#L56-L63">src</a></sub></sup><a name="io-gen-impl-conc-future-new-future-value"></a>
<code>&forall; :RT :T (:M (Type &rarr; Type)) :A. <a href="#io-classes-thread-threads-class">Threads</a> :RT :T :M &rArr; :M (<a href="#coalton-classes-tuple-type">Tuple</a> (<a href="#io-gen-impl-conc-future-future-type">Future</a> :A) (<a href="#io-gen-impl-conc-future-promise-type">Promise</a> :A))</code>

Create a new `Future` and an associated `Promise`.

The `Promise` is not owned by any thread. It can be transferred between threads, and will
always guarantee a thread safe satisfaction or failure of the `Future`.



***

### Macros

#### <a href="#io-gen-impl-conc-future-do-fork-future-macro"><code>DO-FORK-FUTURE ((PROMISE-SYM) &amp;BODY FORMS)</code></a> <sup><sub>[MACRO]</sub></sup><a name="io-gen-impl-conc-future-do-fork-future-macro"></a>


***

#### <a href="#io-io-impl-future-do-fork-future_-macro"><code>DO-FORK-FUTURE_ ((PROMISE-SYM) &amp;BODY FORMS)</code></a> <sup><sub>[MACRO]</sub></sup><a name="io-io-impl-future-do-fork-future_-macro"></a>


***

# Package `IO/CONC/ATOMIC`<a name="io-conc-atomic-package"></a>

### Types

#### <a href="#io-gen-impl-conc-atomic-atvar-type"><code>AtVar</code></a> <sup><sub>[TYPE] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/io-atomic.lisp#L39-L43">src</a></sub></sup><a name="io-gen-impl-conc-atomic-atvar-type"></a>

A container that can be read and modified atomically.

<details>
<summary>Instances</summary>

- <code><a href="#coalton-types-runtimerepr-class">RuntimeRepr</a> (<a href="#io-gen-impl-conc-atomic-atvar-type">AtVar</a> :|0|)</code>

</details>



***

### Values

#### <a href="#io-gen-impl-conc-atomic-modify-value"><code>(MODIFY ATM F)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/io-atomic.lisp#L70-L78">src</a></sub></sup><a name="io-gen-impl-conc-atomic-modify-value"></a>
<code>&forall; :A (:M (Type &rarr; Type)). (<a href="#io-classes-monad-io-monadio-class">MonadIo</a> :M) (<a href="#io-classes-exceptions-exceptions-class">Exceptions</a> :M) &rArr; <a href="#io-gen-impl-conc-atomic-atvar-type">AtVar</a> :A * (:A &rarr; :A) &rarr; :M :A</code>

Atomically modify `atm` by applying `f` and return the new value. `f` must be a pure
function. If `f` throws an error, `atm` will be unchanged and the error will be handleable
via `Exceptions`.

Concurrent:
  - WARNING: `f` will be retried each time the calling thread loses the race to update
    `atm`, so `f` must be pure.



***

#### <a href="#io-gen-impl-conc-atomic-modify-swap-value"><code>(MODIFY-SWAP ATM F)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/io-atomic.lisp#L82-L90">src</a></sub></sup><a name="io-gen-impl-conc-atomic-modify-swap-value"></a>
<code>&forall; :A (:M (Type &rarr; Type)). <a href="#io-classes-monad-io-monadio-class">MonadIo</a> :M &rArr; <a href="#io-gen-impl-conc-atomic-atvar-type">AtVar</a> :A * (:A &rarr; :A) &rarr; :M :A</code>

Atomically modify `atm` by applying `f` and return the old value. `f` must be a pure
function. If `f` throws an error, `atm` will be unchanged and the error will be handleable
via `Exceptions`.

Concurrent:
  - WARNING: `f` will be retried each time the calling thread loses the race to update
    `atm`, so `f` must be pure.



***

#### <a href="#io-gen-impl-conc-atomic-new-at-var-value"><code>(NEW-AT-VAR VAL)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/io-atomic.lisp#L52-L54">src</a></sub></sup><a name="io-gen-impl-conc-atomic-new-at-var-value"></a>
<code>&forall; :A (:M (Type &rarr; Type)). <a href="#io-classes-monad-io-monadio-class">MonadIo</a> :M &rArr; :A &rarr; :M (<a href="#io-gen-impl-conc-atomic-atvar-type">AtVar</a> :A)</code>

Create a new AtVar containing `val`.



***

#### <a href="#io-gen-impl-conc-atomic-pop-value"><code>(POP ATM)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/io-atomic.lisp#L100-L102">src</a></sub></sup><a name="io-gen-impl-conc-atomic-pop-value"></a>
<code>&forall; :A (:M (Type &rarr; Type)). <a href="#io-classes-monad-io-monadio-class">MonadIo</a> :M &rArr; <a href="#io-gen-impl-conc-atomic-atvar-type">AtVar</a> (<a href="#coalton-list-type">List</a> :A) &rarr; :M (<a href="#coalton-optional-type">Optional</a> :A)</code>

Atomically pop and retrieve the head of an atomic list.



***

#### <a href="#io-gen-impl-conc-atomic-push-value"><code>(PUSH ATM ELT)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/io-atomic.lisp#L94-L96">src</a></sub></sup><a name="io-gen-impl-conc-atomic-push-value"></a>
<code>&forall; :A (:M (Type &rarr; Type)). <a href="#io-classes-monad-io-monadio-class">MonadIo</a> :M &rArr; <a href="#io-gen-impl-conc-atomic-atvar-type">AtVar</a> (<a href="#coalton-list-type">List</a> :A) * :A &rarr; :M (<a href="#coalton-list-type">List</a> :A)</code>

Atomically push a value onto an atomic list. Returns the new list.



***

#### <a href="#io-gen-impl-conc-atomic-read-value"><code>(READ ATM)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/io-atomic.lisp#L58-L60">src</a></sub></sup><a name="io-gen-impl-conc-atomic-read-value"></a>
<code>&forall; :A (:M (Type &rarr; Type)). <a href="#io-classes-monad-io-monadio-class">MonadIo</a> :M &rArr; <a href="#io-gen-impl-conc-atomic-atvar-type">AtVar</a> :A &rarr; :M :A</code>

Atomically read the value from `atm`.



***

#### <a href="#io-gen-impl-conc-atomic-write-value"><code>(WRITE ATM VAL)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/io-atomic.lisp#L64-L66">src</a></sub></sup><a name="io-gen-impl-conc-atomic-write-value"></a>
<code>&forall; :A (:M (Type &rarr; Type)). <a href="#io-classes-monad-io-monadio-class">MonadIo</a> :M &rArr; <a href="#io-gen-impl-conc-atomic-atvar-type">AtVar</a> :A * :A &rarr; :M <a href="#coalton-unit-type">Unit</a></code>

Atomically write a new value to `atm`.



***

# Package `IO/CONC/MVAR`<a name="io-conc-mvar-package"></a>

### Structs

#### <a href="#io-gen-impl-conc-mvar-mvar-type"><code>MVar :A</code></a> <sup><sub>[STRUCT] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/io-mvar.lisp#L53-L62">src</a></sub></sup><a name="io-gen-impl-conc-mvar-mvar-type"></a>

A synchronized container that can be empty or hold an :a.

All critical MVar operations are masked. However, irresponsible stopping could still cause
deadlocks and other race conditions.

<details>
<summary>Instances</summary>

- <code><a href="#coalton-types-runtimerepr-class">RuntimeRepr</a> (<a href="#io-gen-impl-conc-mvar-mvar-type">MVar</a> :|0|)</code>

</details>



***

### Values

#### <a href="#io-gen-impl-conc-mvar-is-empty-mvar-value"><code>(IS-EMPTY-MVAR MVAR)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/io-mvar.lisp#L396-L399">src</a></sub></sup><a name="io-gen-impl-conc-mvar-is-empty-mvar-value"></a>
<code>&forall; :RT :T :A (:M (Type &rarr; Type)). <a href="#io-classes-thread-threads-class">Threads</a> :RT :T :M &rArr; <a href="#io-gen-impl-conc-mvar-mvar-type">MVar</a> :A &rarr; :M <a href="#coalton-boolean-type">Boolean</a></code>

Return True if the MVar is currently empty.



***

#### <a href="#io-gen-impl-conc-mvar-new-mvar-value"><code>(NEW-MVAR VAL)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/io-mvar.lisp#L75-L77">src</a></sub></sup><a name="io-gen-impl-conc-mvar-new-mvar-value"></a>
<code>&forall; :RT :T :A (:M (Type &rarr; Type)). <a href="#io-classes-thread-threads-class">Threads</a> :RT :T :M &rArr; :A &rarr; :M (<a href="#io-gen-impl-conc-mvar-mvar-type">MVar</a> :A)</code>

Create a new MVar containing VAL.



***

#### <a href="#io-gen-impl-conc-mvar-put-mvar-value"><code>(PUT-MVAR MVAR VAL)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/io-mvar.lisp#L173-L220">src</a></sub></sup><a name="io-gen-impl-conc-mvar-put-mvar-value"></a>
<code>&forall; :RT :T :A (:M (Type &rarr; Type)). <a href="#io-classes-thread-threads-class">Threads</a> :RT :T :M &rArr; <a href="#io-gen-impl-conc-mvar-mvar-type">MVar</a> :A * :A &key (:timeout <a href="#io-classes-thread-timeoutstrategy-type">TimeoutStrategy</a>) &rarr; :M <a href="#coalton-unit-type">Unit</a></code>

Fill an empty MVar, blocking until it becomes empty. Can specify a timeout.

Concurrent:
  - Blocks while the MVar is full
  - Writers (including `put-mvar`) are woken individual on succesful takes in order
    of acquisition
  - On succesful put, blocking read-consumers are woken individually in order of acquisition
  - On succesful put, all blocking read-non-consumers are woken simultaneously. New data
    is handed directly to woken read-non-consumers so they don't contend on the MVar.



***

#### <a href="#io-gen-impl-conc-mvar-read-mvar-value"><code>(READ-MVAR MVAR)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/io-mvar.lisp#L326-L342">src</a></sub></sup><a name="io-gen-impl-conc-mvar-read-mvar-value"></a>
<code>&forall; :RT :T :A (:M (Type &rarr; Type)). <a href="#io-classes-thread-threads-class">Threads</a> :RT :T :M &rArr; <a href="#io-gen-impl-conc-mvar-mvar-type">MVar</a> :A &key (:timeout <a href="#io-classes-thread-timeoutstrategy-type">TimeoutStrategy</a>) &rarr; :M :A</code>

Read a value from an MVar, blocking until one is available. Does not consume value.
Can specify a timeout.

Concurrent:
  - Blocks while the MVar is empty
  - Blocking read-non-consumers (including `read-mvar`) are woken simultaneously on
    succesful put. Data is handed directly to woken readers, which don't contend on mvar.



***

#### <a href="#io-gen-impl-conc-mvar-swap-mvar-value"><code>(SWAP-MVAR MVAR NEW-VAL)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/io-mvar.lisp#L353-L392">src</a></sub></sup><a name="io-gen-impl-conc-mvar-swap-mvar-value"></a>
<code>&forall; :RT :T :A (:M (Type &rarr; Type)). <a href="#io-classes-thread-threads-class">Threads</a> :RT :T :M &rArr; <a href="#io-gen-impl-conc-mvar-mvar-type">MVar</a> :A * :A &key (:timeout <a href="#io-classes-thread-timeoutstrategy-type">TimeoutStrategy</a>) &rarr; :M :A</code>

Atomically replace the value in an MVar and return the old value. Can specify an
optional timeout.

Concurrent:
  - Blocks while the MVar is empty
  - Wakes the next blocking read-consumer when `swap-mvar` completes



***

#### <a href="#io-gen-impl-conc-mvar-take-mvar-value"><code>(TAKE-MVAR MVAR)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/io-mvar.lisp#L154-L168">src</a></sub></sup><a name="io-gen-impl-conc-mvar-take-mvar-value"></a>
<code>&forall; :RT :T :A (:M (Type &rarr; Type)). <a href="#io-classes-thread-threads-class">Threads</a> :RT :T :M &rArr; <a href="#io-gen-impl-conc-mvar-mvar-type">MVar</a> :A &key (:timeout <a href="#io-classes-thread-timeoutstrategy-type">TimeoutStrategy</a>) &rarr; :M :A</code>

Take a value from an MVar, blocking until one is available. Can specify a timeout.

Concurrent:
  - Blocks while the MVar is empty
  - Read-consumers (including `take-mvar`) are woken individual on succesful puts,
    in order of acquisition
  - On succesful take, one blocking writer is woken in order of acquisition



***

#### <a href="#io-gen-impl-conc-mvar-take-mvar-masked-value"><code>(TAKE-MVAR-MASKED MVAR)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/io-mvar.lisp#L136-L148">src</a></sub></sup><a name="io-gen-impl-conc-mvar-take-mvar-masked-value"></a>
<code>&forall; :RT :T :A (:M (Type &rarr; Type)). <a href="#io-classes-thread-threads-class">Threads</a> :RT :T :M &rArr; <a href="#io-gen-impl-conc-mvar-mvar-type">MVar</a> :A &key (:timeout <a href="#io-classes-thread-timeoutstrategy-type">TimeoutStrategy</a>) &rarr; :M :A</code>

Take a value from an MVar, blocking until one is available. Can specify a timeout.

Concurrent:
  - WARNING: Leaves the thread masked when returns to protect caller's critical regions
    based on consuming and restoring MVar to a valid state. This is useful when building higher-level concurrent resources from an MVar.
  - Blocks while the MVar is empty
  - Read-consumers (including `take-mvar-masked`) are woken individual on succesfull puts,
    in order of acquisition
  - On succesful take, one blocking writer is woken in order of acquisition



***

#### <a href="#io-gen-impl-conc-mvar-try-put-mvar-value"><code>(TRY-PUT-MVAR MVAR VAL)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/io-mvar.lisp#L286-L321">src</a></sub></sup><a name="io-gen-impl-conc-mvar-try-put-mvar-value"></a>
<code>&forall; :RT :T :A (:M (Type &rarr; Type)). <a href="#io-classes-thread-threads-class">Threads</a> :RT :T :M &rArr; <a href="#io-gen-impl-conc-mvar-mvar-type">MVar</a> :A * :A &rarr; :M <a href="#coalton-boolean-type">Boolean</a></code>

Attempt to immediately put a value into an MVar. Returns True if succeeded.

Concurrent:
  - Can briefly block while waiting to fill the MVar, if contended
  - On succesful put, blocking read-consumers are woken individually in order of acquisition
  - On succesful put, all blocking read-non-consumers are woken simultaneously. New data
    is handed directly to woken read-non-consumers so they don't contend on the MVar.



***

#### <a href="#io-gen-impl-conc-mvar-try-read-mvar-value"><code>(TRY-READ-MVAR MVAR)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/io-mvar.lisp#L345-L348">src</a></sub></sup><a name="io-gen-impl-conc-mvar-try-read-mvar-value"></a>
<code>&forall; :RT :T :A (:M (Type &rarr; Type)). <a href="#io-classes-thread-threads-class">Threads</a> :RT :T :M &rArr; <a href="#io-gen-impl-conc-mvar-mvar-type">MVar</a> :A &rarr; :M (<a href="#coalton-optional-type">Optional</a> :A)</code>

Attempt to immediately read a value from an MVar. Returns None if empty.



***

#### <a href="#io-gen-impl-conc-mvar-try-take-mvar-value"><code>(TRY-TAKE-MVAR MVAR)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/io-mvar.lisp#L258-L270">src</a></sub></sup><a name="io-gen-impl-conc-mvar-try-take-mvar-value"></a>
<code>&forall; :RT :T :A (:M (Type &rarr; Type)). <a href="#io-classes-thread-threads-class">Threads</a> :RT :T :M &rArr; <a href="#io-gen-impl-conc-mvar-mvar-type">MVar</a> :A &rarr; :M (<a href="#coalton-optional-type">Optional</a> :A)</code>

Attempt to immediately take a value from an MVar. Returns None if empty.

Concurrent:
  - Can briefly block while waiting to empty the MVar, if contended
  - On succesful take, one blocking writer is woken in order of acquisition



***

#### <a href="#io-gen-impl-conc-mvar-try-take-mvar-masked-value"><code>(TRY-TAKE-MVAR-MASKED MVAR)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/io-mvar.lisp#L273-L283">src</a></sub></sup><a name="io-gen-impl-conc-mvar-try-take-mvar-masked-value"></a>
<code>&forall; :RT :T :A (:M (Type &rarr; Type)). <a href="#io-classes-thread-threads-class">Threads</a> :RT :T :M &rArr; <a href="#io-gen-impl-conc-mvar-mvar-type">MVar</a> :A &rarr; :M (<a href="#coalton-optional-type">Optional</a> :A)</code>

Attempt to immediately take a value from an MVar. Returns None if empty.

Concurrent:
  - WARNING: Leaves the thread masked when returns to protect caller's critical regions
    based on consuming and restoring MVar to a valid state. This is useful when building higher-level concurrent resources from an MVar.
  - Can briefly block while waiting to empty the MVar, if contended
  - On succesful take, one blocking writer is woken in order of acquisition



***

#### <a href="#io-gen-impl-conc-mvar-with-mvar-value"><code>(WITH-MVAR MVAR OP)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/io-mvar.lisp#L410-L433">src</a></sub></sup><a name="io-gen-impl-conc-mvar-with-mvar-value"></a>
<code>&forall; (:I (Type &rarr; Type)) :RT :T :A (:R (Type &rarr; Type)) :B (:M (Type &rarr; Type)). (<a href="#io-classes-monad-io-unliftio-class">UnliftIo</a> :R :I) (<a href="#coalton-monad-classes-liftto-class">LiftTo</a> :R :M) (<a href="#io-classes-thread-threads-class">Threads</a> :RT :T :I) &rArr; <a href="#io-gen-impl-conc-mvar-mvar-type">MVar</a> :A * (:A &rarr; :R :B) &rarr; :M :B</code>

Run an operation with the value from an MVar, blocking until one is available.
Restore the MVar value and return the result of the operation.

WARNING: If the computation raises an unhandled exception or is stopped, leaves the MVar
empty!

Concurrent:
  - WARNING: Does not mask during the computation. To ensure completion, caller must mask
  - Blocks while the MVar is empty
  - Inherits notify semantics from `put-mvar`
  - Does not leave the MVar locked during the computation. Thus, other threads can
    put the MVar during the computation and force `with-mvar` to block until empty.



***

#### <a href="#io-io-impl-mvar-with-mvar_-value"><code>(WITH-MVAR_)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/io-impl/io-mvar.lisp#L25-L38">src</a></sub></sup><a name="io-io-impl-mvar-with-mvar_-value"></a>
<code>&forall; :A :B (:M (Type &rarr; Type)). (<a href="#io-classes-thread-threads-class">Threads</a> <a href="#io-io-impl-runtime-ioruntime-type">IoRuntime</a> <a href="#io-io-impl-io-runtime-runtime-iothread-type">IoThread</a> :M) (<a href="#coalton-monad-classes-liftto-class">LiftTo</a> <a href="#io-io-impl-simple-io-io-type">IO</a> :M) &rArr; <a href="#io-gen-impl-conc-mvar-mvar-type">MVar</a> :A * (:A &rarr; <a href="#io-io-impl-simple-io-io-type">IO</a> :B) &rarr; :M :B</code>

Run an operation with the value from an MVar, blocking until one is available.
Restore the MVar value and return the result of the operation.

WARNING: If the computation raises an unhandled exception or is stopped, leaves the MVar
empty!

Concurrent:
  - WARNING: Does not mask during the computation. To ensure completion, caller must mask
  - Blocks while the MVar is empty
  - Inherits notify semantics from `put-mvar`
  - Does not leave the MVar locked during the computation. Thus, other threads can
    put the MVar during the computation and force `with-mvar` to block until empty.



***

#### <a href="#io-gen-impl-conc-mvar-new-empty-mvar-value"><code>NEW-EMPTY-MVAR</code></a> <sup><sub>[VALUE] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/io-mvar.lisp#L90-L92">src</a></sub></sup><a name="io-gen-impl-conc-mvar-new-empty-mvar-value"></a>
<code>&forall; :RT :T (:M (Type &rarr; Type)) :A. <a href="#io-classes-thread-threads-class">Threads</a> :RT :T :M &rArr; :M (<a href="#io-gen-impl-conc-mvar-mvar-type">MVar</a> :A)</code>

Create a new empty MVar.



***

### Macros

#### <a href="#io-io-impl-mvar-do-with-mvar_-macro"><code>DO-WITH-MVAR_ ((SYM MVAR) &amp;BODY BODY)</code></a> <sup><sub>[MACRO]</sub></sup><a name="io-io-impl-mvar-do-with-mvar_-macro"></a>

Run an operation with the value from an MVar, blocking until one is available.
Stores the result of the operation in the MVar and returns.

WARNING: If the computation raises an unhandled exception or is stopped, leaves the MVar
empty!

Concurrent:
  - WARNING: Does not mask during the computation. To ensure completion, caller must mask
  - Blocks while the MVar is empty
  - Inherits notify semantics from `put-mvar`
  - Does not leave the MVar locked during the computation. Thus, other threads can
    put the MVar during the computation and force `with-mvar` to block until empty.



***

# Package `IO/CONC/QUEUES`<a name="io-conc-queues-package"></a>

### Classes

#### <a href="#io-gen-impl-conc-queues-queue-class"><code>Queue</code></a> <sup><sub>[CLASS] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/queues/queues.lisp#L23-L50">src</a></sub></sup><a name="io-gen-impl-conc-queues-queue-class"></a>
<code><a href="#io-gen-impl-conc-queues-queue-class">Queue</a> :Q</code>


A First-in, First-out (FIFO) queue.

All `Queue` instances share the same API, but are allowed to have different invariants for
memory, synchronicity, etc. For example, a `Queue` is allowed to be either bounded or
unbounded; may be threadsafe to enqueue, dequeue, or both; etc.

There are some guarantees that must hold for certain types of `Queue`s. For example, if
a `Queue` is bounded then `enqueue` must block if it is full.

Methods:
- <code>ENQUEUE :: <a href="#io-classes-thread-threads-class">Threads</a> :RT :T :M &rArr; :A * :Q :A &key (:timeout <a href="#io-classes-thread-timeoutstrategy-type">TimeoutStrategy</a>) &rarr; :M <a href="#coalton-unit-type">Unit</a></code><br/>Add a value to the end of the queue. If the queue is bounded and full, must block&#xA;until the value can be added.&#xA;&#xA;Can specify a timeout.
- <code>TRY-ENQUEUE :: <a href="#io-classes-thread-threads-class">Threads</a> :RT :T :M &rArr; :A * :Q :A &rarr; :M <a href="#coalton-boolean-type">Boolean</a></code><br/>Attempt to add a value to the end of the queue. Returns `True` if it succeeded, or&#xA;`False` if it failed. If the queue is unbounded, must always return true.
- <code>DEQUEUE :: <a href="#io-classes-thread-threads-class">Threads</a> :RT :T :M &rArr; :Q :A &key (:timeout <a href="#io-classes-thread-timeoutstrategy-type">TimeoutStrategy</a>) &rarr; :M :A</code><br/>Pop the value at the front of the queue. If the queue is empty, must block until a&#xA;value is present.&#xA;&#xA;Can specify a timeout.
- <code>TRY-DEQUEUE :: <a href="#io-classes-thread-threads-class">Threads</a> :RT :T :M &rArr; :Q :A &rarr; :M (<a href="#coalton-optional-type">Optional</a> :A)</code><br/>Try to pop the value at the front of the queue. Returns `None` if empty.
<details>
<summary>Instances</summary>

- <code><a href="#io-gen-impl-conc-queues-queue-class">Queue</a> <a href="#io-gen-impl-conc-queues-unbounded-mpmc-unboundedmpmcqueue-type">UnboundedMpmcQueue</a></code>
- <code><a href="#io-gen-impl-conc-queues-queue-class">Queue</a> <a href="#io-gen-impl-conc-queues-bounded-mpmc-boundedmpmcqueue-type">BoundedMpmcQueue</a></code>

</details>



***

# Package `IO/CONC/QUEUES/BOUNDED-MPMC`<a name="io-conc-queues-bounded-mpmc-package"></a>

### Structs

#### <a href="#io-gen-impl-conc-queues-bounded-mpmc-boundedmpmcqueue-type"><code>BoundedMpmcQueue :A</code></a> <sup><sub>[STRUCT] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/queues/bounded-mpmc.lisp#L41-L54">src</a></sub></sup><a name="io-gen-impl-conc-queues-bounded-mpmc-boundedmpmcqueue-type"></a>

A bounded FIFO Multi-Producer, Multi-Consumer Queue.

<details>
<summary>Instances</summary>

- <code><a href="#io-gen-impl-conc-queues-queue-class">Queue</a> <a href="#io-gen-impl-conc-queues-bounded-mpmc-boundedmpmcqueue-type">BoundedMpmcQueue</a></code>
- <code><a href="#coalton-types-runtimerepr-class">RuntimeRepr</a> (<a href="#io-gen-impl-conc-queues-bounded-mpmc-boundedmpmcqueue-type">BoundedMpmcQueue</a> :|0|)</code>

</details>



***

### Values

#### <a href="#io-gen-impl-conc-queues-bounded-mpmc-new-bounded-mpmc-queue-value"><code>(NEW-BOUNDED-MPMC-QUEUE CAPACITY)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/queues/bounded-mpmc.lisp#L286-L290">src</a></sub></sup><a name="io-gen-impl-conc-queues-bounded-mpmc-new-bounded-mpmc-queue-value"></a>
<code>&forall; :RT :T (:M (Type &rarr; Type)) :A. (<a href="#io-classes-thread-threads-class">Threads</a> :RT :T :M) (<a href="#io-classes-exceptions-exceptions-class">Exceptions</a> :M) &rArr; <a href="#coalton-ufix-type">UFix</a> &rarr; :M (<a href="#io-gen-impl-conc-queues-bounded-mpmc-boundedmpmcqueue-type">BoundedMpmcQueue</a> :A)</code>

Create a new bounded MPMC queue with the given capacity.



***

# Package `IO/CONC/QUEUES/UNBOUNDED-MPMC`<a name="io-conc-queues-unbounded-mpmc-package"></a>

### Types

#### <a href="#io-gen-impl-conc-queues-unbounded-mpmc-unboundedmpmcqueue-type"><code>UnboundedMpmcQueue</code></a> <sup><sub>[TYPE] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/queues/unbounded-mpmc.lisp#L741-L743">src</a></sub></sup><a name="io-gen-impl-conc-queues-unbounded-mpmc-unboundedmpmcqueue-type"></a>

A synchronized FIFO queue to pass data between threads.

<details>
<summary>Instances</summary>

- <code><a href="#io-gen-impl-conc-queues-queue-class">Queue</a> <a href="#io-gen-impl-conc-queues-unbounded-mpmc-unboundedmpmcqueue-type">UnboundedMpmcQueue</a></code>
- <code><a href="#coalton-types-runtimerepr-class">RuntimeRepr</a> (<a href="#io-gen-impl-conc-queues-unbounded-mpmc-unboundedmpmcqueue-type">UnboundedMpmcQueue</a> :|0|)</code>

</details>



***

### Values

#### <a href="#io-gen-impl-conc-queues-unbounded-mpmc-new-unbounded-mpmc-queue-value"><code>NEW-UNBOUNDED-MPMC-QUEUE</code></a> <sup><sub>[VALUE] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/queues/unbounded-mpmc.lisp#L752-L755">src</a></sub></sup><a name="io-gen-impl-conc-queues-unbounded-mpmc-new-unbounded-mpmc-queue-value"></a>
<code>&forall; :RT :T (:M (Type &rarr; Type)) :A. <a href="#io-classes-thread-threads-class">Threads</a> :RT :T :M &rArr; :M (<a href="#io-gen-impl-conc-queues-unbounded-mpmc-unboundedmpmcqueue-type">UnboundedMpmcQueue</a> :A)</code>


***

# Package `IO/CONC/STM`<a name="io-conc-stm-package"></a>

### Types

#### <a href="#io-gen-impl-conc-stm-stm-type"><code>STM</code></a> <sup><sub>[TYPE] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/stm.lisp#L111-L113">src</a></sub></sup><a name="io-gen-impl-conc-stm-stm-type"></a>

A transaction that can be run using `run-tx`.

<details>
<summary>Instances</summary>

- <code><a href="#coalton-classes-applicative-class">Applicative</a> <a href="#io-gen-impl-conc-stm-stm-type">STM</a></code>
- <code><a href="#coalton-classes-functor-class">Functor</a> <a href="#io-gen-impl-conc-stm-stm-type">STM</a></code>
- <code><a href="#coalton-classes-monad-class">Monad</a> <a href="#io-gen-impl-conc-stm-stm-type">STM</a></code>
- <code><a href="#coalton-types-runtimerepr-class">RuntimeRepr</a> (<a href="#io-gen-impl-conc-stm-stm-type">STM</a> :|0|)</code>

</details>



***

#### <a href="#io-gen-impl-conc-stm-tvar-type"><code>TVar</code></a> <sup><sub>[TYPE] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/stm.lisp#L67-L69">src</a></sub></sup><a name="io-gen-impl-conc-stm-tvar-type"></a>

A Transaction Variable that can be read and modified inside an STM transaction.

<details>
<summary>Instances</summary>

- <code>(<a href="#coalton-classes-eq-class">Eq</a> (<a href="#coalton-cell-cell-type">Cell</a> :A)) (<a href="#coalton-classes-eq-class">Eq</a> <a href="#io-gen-impl-conc-parking-parkingset-type">ParkingSet</a>) &rArr; <a href="#coalton-classes-eq-class">Eq</a> (<a href="#io-gen-impl-conc-stm-tvar-type">TVar</a> :A)</code>
- <code><a href="#coalton-types-runtimerepr-class">RuntimeRepr</a> (<a href="#io-gen-impl-conc-stm-tvar-type">TVar</a> :|0|)</code>

</details>



***

### Values

#### <a href="#io-gen-impl-conc-stm-modify-swap-tvar-value"><code>(MODIFY-SWAP-TVAR TVAR F)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/stm.lisp#L554-L561">src</a></sub></sup><a name="io-gen-impl-conc-stm-modify-swap-tvar-value"></a>
<code>&forall; :A. <a href="#io-gen-impl-conc-stm-tvar-type">TVar</a> :A * (:A &rarr; :A) &rarr; <a href="#io-gen-impl-conc-stm-stm-type">STM</a> :A</code>

Modify a mutable variable inside an atomic transaction. Returns the old value.



***

#### <a href="#io-gen-impl-conc-stm-modify-tvar-value"><code>(MODIFY-TVAR TVAR F)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/stm.lisp#L544-L551">src</a></sub></sup><a name="io-gen-impl-conc-stm-modify-tvar-value"></a>
<code>&forall; :A. <a href="#io-gen-impl-conc-stm-tvar-type">TVar</a> :A * (:A &rarr; :A) &rarr; <a href="#io-gen-impl-conc-stm-stm-type">STM</a> :A</code>

Modify a mutable variable inside an atomic transaction. Returns the new value.



***

#### <a href="#io-gen-impl-conc-stm-new-tvar-value"><code>(NEW-TVAR VAL)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/stm.lisp#L358-L360">src</a></sub></sup><a name="io-gen-impl-conc-stm-new-tvar-value"></a>
<code>&forall; :A (:M (Type &rarr; Type)). <a href="#io-classes-monad-io-monadio-class">MonadIo</a> :M &rArr; :A &rarr; :M (<a href="#io-gen-impl-conc-stm-tvar-type">TVar</a> :A)</code>

Create a sychronized mutable variable that can be used inside an atomic transaction.



***

#### <a href="#io-gen-impl-conc-stm-new-tvar-tx-value"><code>(NEW-TVAR-TX VAL)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/stm.lisp#L364-L368">src</a></sub></sup><a name="io-gen-impl-conc-stm-new-tvar-tx-value"></a>
<code>&forall; :A. :A &rarr; <a href="#io-gen-impl-conc-stm-stm-type">STM</a> (<a href="#io-gen-impl-conc-stm-tvar-type">TVar</a> :A)</code>

Create a synchronized mutable variable in the middle of an atomic transaction.



***

#### <a href="#io-gen-impl-conc-stm-or-else-value"><code>(OR-ELSE TX-A TX-B)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/stm.lisp#L678-L693">src</a></sub></sup><a name="io-gen-impl-conc-stm-or-else-value"></a>
<code>&forall; :A. <a href="#io-gen-impl-conc-stm-stm-type">STM</a> :A * <a href="#io-gen-impl-conc-stm-stm-type">STM</a> :A &rarr; <a href="#io-gen-impl-conc-stm-stm-type">STM</a> :A</code>

Run TX-A. If it signals a retry, run TX-b. If both transactions signal a
retry, then the entire transaction retries.



***

#### <a href="#io-gen-impl-conc-stm-read-tvar-value"><code>(READ-TVAR TVAR)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/stm.lisp#L519-L523">src</a></sub></sup><a name="io-gen-impl-conc-stm-read-tvar-value"></a>
<code>&forall; :A. <a href="#io-gen-impl-conc-stm-tvar-type">TVar</a> :A &rarr; <a href="#io-gen-impl-conc-stm-stm-type">STM</a> :A</code>

Read a mutable variable inside an atomic transaction.



***

#### <a href="#io-gen-impl-conc-stm-retry-unless-value"><code>(RETRY-UNLESS OK?)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/stm.lisp#L644-L656">src</a></sub></sup><a name="io-gen-impl-conc-stm-retry-unless-value"></a>
<code><a href="#coalton-boolean-type">Boolean</a> &rarr; <a href="#io-gen-impl-conc-stm-stm-type">STM</a> <a href="#coalton-unit-type">Unit</a></code>

Retry unless `ok?` is true. If `ok?` is false, then waits for a write transaction
to commit to a TVar that has already been read during this transaction, and then tries
this transaction again.

Concurrent:
  - When the transaction runs, executing retry will abort the transaction and sleep the
    thread. The thread will sleep until any relevant write transaction commits to the STM,
    when the retrying thread will wake and retry its transaction. A write transaction
    only triggers a retry if it writes to a TVar that was read before `retry` was called.



***

#### <a href="#io-gen-impl-conc-stm-retry-unless-tvar-value"><code>(RETRY-UNLESS-TVAR OK?)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/stm.lisp#L660-L675">src</a></sub></sup><a name="io-gen-impl-conc-stm-retry-unless-tvar-value"></a>
<code><a href="#io-gen-impl-conc-stm-tvar-type">TVar</a> <a href="#coalton-boolean-type">Boolean</a> &rarr; <a href="#io-gen-impl-conc-stm-stm-type">STM</a> <a href="#coalton-unit-type">Unit</a></code>

Retry unless `ok?` is true. If `ok?` is false, then waits for a write transaction
to commit to a TVar that has already been read during this transaction, and then tries
this transaction again.

Concurrent:
  - When the transaction runs, executing retry will abort the transaction and sleep the
    thread. The thread will sleep until any relevant write transaction commits to the STM,
    when the retrying thread will wake and retry its transaction. A write transaction
    only triggers a retry if it writes to a TVar that was read before `retry` was called.



***

#### <a href="#io-gen-impl-conc-stm-retry-with-value"><code>(RETRY-WITH STRATEGY)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/stm.lisp#L602-L621">src</a></sub></sup><a name="io-gen-impl-conc-stm-retry-with-value"></a>
<code>&forall; :A. <a href="#io-classes-thread-timeoutstrategy-type">TimeoutStrategy</a> &rarr; <a href="#io-gen-impl-conc-stm-stm-type">STM</a> :A</code>

Retry the current operation because the observed state is invalid. Waits for a write
transaction to commit to a TVar that has already been read during this transaction, and
then tries this transaction again.

This is useful if the transaction needs to wait for other threads to update the data
before it can continue. For example, if the transaction reads from a (TVar Queue) and the
queue is empty, it must wait until another thread pushes onto the queue before it can
continue.

Concurrent:
  - When the transaction runs, executing retry will abort the transaction and sleep the
    thread. The thread will sleep until any relevant write transaction commits to the STM,
    when the retrying thread will wake and retry its transaction. A write transaction
    only triggers a retry if it writes to a TVar that was read before `retry` was called.
  - Will timeout depending on strategy.



***

#### <a href="#io-gen-impl-conc-stm-run-tx-value"><code>(RUN-TX TX)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/stm.lisp#L696-L724">src</a></sub></sup><a name="io-gen-impl-conc-stm-run-tx-value"></a>
<code>&forall; :RT :T :A (:M (Type &rarr; Type)). <a href="#io-classes-thread-threads-class">Threads</a> :RT :T :M &rArr; <a href="#io-gen-impl-conc-stm-stm-type">STM</a> :A &rarr; :M :A</code>

Run an atomic transaction.

WARNING: The STM can abort and re-run the transaction repeatedly, until it completes with
a consistent snapshot of the data. Therefore, TX must be pure.



***

#### <a href="#io-gen-impl-conc-stm-swap-tvar-value"><code>(SWAP-TVAR TVAR NEW-VAL)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/stm.lisp#L534-L541">src</a></sub></sup><a name="io-gen-impl-conc-stm-swap-tvar-value"></a>
<code>&forall; :A. <a href="#io-gen-impl-conc-stm-tvar-type">TVar</a> :A * :A &rarr; <a href="#io-gen-impl-conc-stm-stm-type">STM</a> :A</code>

Swap the value of a mutable variable inside an atomic transaction. Returns the old
value.



***

#### <a href="#io-gen-impl-conc-stm-write-tvar-value"><code>(WRITE-TVAR TVAR VAL)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/stm.lisp#L527-L531">src</a></sub></sup><a name="io-gen-impl-conc-stm-write-tvar-value"></a>
<code>&forall; :A. <a href="#io-gen-impl-conc-stm-tvar-type">TVar</a> :A * :A &rarr; <a href="#io-gen-impl-conc-stm-stm-type">STM</a> <a href="#coalton-unit-type">Unit</a></code>

Write to a mutable variable inside an atomic transaction.



***

#### <a href="#io-gen-impl-conc-stm-retry-value"><code>RETRY</code></a> <sup><sub>[VALUE] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/stm.lisp#L625-L640">src</a></sub></sup><a name="io-gen-impl-conc-stm-retry-value"></a>
<code>&forall; :A. <a href="#io-gen-impl-conc-stm-stm-type">STM</a> :A</code>

Retry the current operation because the observed state is invalid. Waits for a write
transaction to commit to a TVar that has already been read during this transaction, and
then tries this transaction again.

This is useful if the transaction needs to wait for other threads to update the data
before it can continue. For example, if the transaction reads from a (TVar Queue) and the
queue is empty, it must wait until another thread pushes onto the queue before it can
continue.

Concurrent:
  - When the transaction runs, executing retry will abort the transaction and sleep the
    thread. The thread will sleep until any relevant write transaction commits to the STM,
    when the retrying thread will wake and retry its transaction. A write transaction
    only triggers a retry if it writes to a TVar that was read before `retry` was called.



***

### Macros

#### <a href="#io-gen-impl-conc-stm-do-run-tx-macro"><code>DO-RUN-TX (&amp;BODY BODY)</code></a> <sup><sub>[MACRO]</sub></sup><a name="io-gen-impl-conc-stm-do-run-tx-macro"></a>

Run an atomic transaction.

WARNING: The STM can abort and re-run the transaction repeatedly, until it completes with
a consistent snapshot of the data. Therefore, TX must be pure.



***

# Package `IO/CONC/STM/TARRAY`<a name="io-conc-stm-tarray-package"></a>

### Types

#### <a href="#io-gen-impl-conc-stm-tarray-tarray-type"><code>TArray</code></a> <sup><sub>[TYPE] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/tarray.lisp#L37-L39">src</a></sub></sup><a name="io-gen-impl-conc-stm-tarray-tarray-type"></a>

A transactional array, where each element is a synchronized transactional variable.

<details>
<summary>Instances</summary>

- <code><a href="#coalton-types-runtimerepr-class">RuntimeRepr</a> (<a href="#io-gen-impl-conc-stm-tarray-tarray-type">TArray</a> :|0|)</code>

</details>



***

### Values

#### <a href="#io-gen-impl-conc-stm-tarray-at-value"><code>(AT TARR I)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/tarray.lisp#L91-L99">src</a></sub></sup><a name="io-gen-impl-conc-stm-tarray-at-value"></a>
<code>&forall; :A. <a href="#io-gen-impl-conc-stm-tarray-tarray-type">TArray</a> :A * <a href="#coalton-ufix-type">UFix</a> &rarr; <a href="#io-gen-impl-conc-stm-stm-type">STM</a> (<a href="#coalton-optional-type">Optional</a> :A)</code>

Read the value in `tarr` at index `i`.



***

#### <a href="#io-gen-impl-conc-stm-tarray-at#-value"><code>(AT# TARR I)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/tarray.lisp#L103-L105">src</a></sub></sup><a name="io-gen-impl-conc-stm-tarray-at#-value"></a>
<code>&forall; :A. <a href="#io-gen-impl-conc-stm-tarray-tarray-type">TArray</a> :A * <a href="#coalton-ufix-type">UFix</a> &rarr; <a href="#io-gen-impl-conc-stm-stm-type">STM</a> :A</code>

Read the value in `tarr` at index `i`. Errors if out of bounds.



***

#### <a href="#io-gen-impl-conc-stm-tarray-modify-value"><code>(MODIFY TARR I F)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/tarray.lisp#L130-L133">src</a></sub></sup><a name="io-gen-impl-conc-stm-tarray-modify-value"></a>
<code>&forall; :A. <a href="#io-gen-impl-conc-stm-tarray-tarray-type">TArray</a> :A * <a href="#coalton-ufix-type">UFix</a> * (:A &rarr; :A) &rarr; <a href="#io-gen-impl-conc-stm-stm-type">STM</a> :A</code>

Update the value in `tarr` at index `i` with `f`. Returns the new value.



***

#### <a href="#io-gen-impl-conc-stm-tarray-modify-swap-value"><code>(MODIFY-SWAP TARR I F)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/tarray.lisp#L137-L140">src</a></sub></sup><a name="io-gen-impl-conc-stm-tarray-modify-swap-value"></a>
<code>&forall; :A. <a href="#io-gen-impl-conc-stm-tarray-tarray-type">TArray</a> :A * <a href="#coalton-ufix-type">UFix</a> * (:A &rarr; :A) &rarr; <a href="#io-gen-impl-conc-stm-stm-type">STM</a> :A</code>

Update the value in `tarr` at index `i` with `f`. Returns the old value.



***

#### <a href="#io-gen-impl-conc-stm-tarray-new-tarray-value"><code>(NEW-TARRAY LENGTH INIT-ELEM)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/tarray.lisp#L54-L56">src</a></sub></sup><a name="io-gen-impl-conc-stm-tarray-new-tarray-value"></a>
<code>&forall; :A (:M (Type &rarr; Type)). <a href="#io-classes-monad-io-monadio-class">MonadIo</a> :M &rArr; <a href="#coalton-ufix-type">UFix</a> * :A &rarr; :M (<a href="#io-gen-impl-conc-stm-tarray-tarray-type">TArray</a> :A)</code>

Create a new `TArray` with size `length` and all values set to `init-elem`.



***

#### <a href="#io-gen-impl-conc-stm-tarray-new-tarray-apply-value"><code>(NEW-TARRAY-APPLY LENGTH FACTORY)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/tarray.lisp#L65-L75">src</a></sub></sup><a name="io-gen-impl-conc-stm-tarray-new-tarray-apply-value"></a>
<code>&forall; (:M (Type &rarr; Type)) :A. <a href="#io-classes-monad-io-monadio-class">MonadIo</a> :M &rArr; <a href="#coalton-ufix-type">UFix</a> * (<a href="#coalton-ufix-type">UFix</a> &rarr; :M :A) &rarr; :M (<a href="#io-gen-impl-conc-stm-tarray-tarray-type">TArray</a> :A)</code>

Create a new `TArray` with size `length` and all values populated by applying
`factory` with the index.



***

#### <a href="#io-gen-impl-conc-stm-tarray-new-tarray-tx-value"><code>(NEW-TARRAY-TX LENGTH INIT-ELEM)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/tarray.lisp#L59-L62">src</a></sub></sup><a name="io-gen-impl-conc-stm-tarray-new-tarray-tx-value"></a>
<code>&forall; :A. <a href="#coalton-ufix-type">UFix</a> * :A &rarr; <a href="#io-gen-impl-conc-stm-stm-type">STM</a> (<a href="#io-gen-impl-conc-stm-tarray-tarray-type">TArray</a> :A)</code>

Create a new `TArray` with size `length` and all values set to `init-elem` inside of
an atomic transaction.



***

#### <a href="#io-gen-impl-conc-stm-tarray-set-value"><code>(SET TARR I ELEM)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/tarray.lisp#L123-L126">src</a></sub></sup><a name="io-gen-impl-conc-stm-tarray-set-value"></a>
<code>&forall; :A. <a href="#io-gen-impl-conc-stm-tarray-tarray-type">TArray</a> :A * <a href="#coalton-ufix-type">UFix</a> * :A &rarr; <a href="#io-gen-impl-conc-stm-stm-type">STM</a> <a href="#coalton-unit-type">Unit</a></code>

Set the value in `tarr` at index `i` to `elem`.



***

#### <a href="#io-gen-impl-conc-stm-tarray-to-arr-value"><code>(TO-ARR TARR)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/tarray.lisp#L78-L87">src</a></sub></sup><a name="io-gen-impl-conc-stm-tarray-to-arr-value"></a>
<code>&forall; :A. <a href="#coalton-types-runtimerepr-class">RuntimeRepr</a> :A &rArr; <a href="#io-gen-impl-conc-stm-tarray-tarray-type">TArray</a> :A &rarr; <a href="#io-gen-impl-conc-stm-stm-type">STM</a> (<a href="#coalton-lisparray-lisparray-type">LispArray</a> :A)</code>

Extract synchronized values in `tarr` into a `LispArray`.



***

#### <a href="#io-gen-impl-conc-stm-tarray-tvar-at-value"><code>(TVAR-AT TARR I)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/tarray.lisp#L109-L113">src</a></sub></sup><a name="io-gen-impl-conc-stm-tarray-tvar-at-value"></a>
<code>&forall; :A. <a href="#io-gen-impl-conc-stm-tarray-tarray-type">TArray</a> :A * <a href="#coalton-ufix-type">UFix</a> &rarr; <a href="#coalton-optional-type">Optional</a> (<a href="#io-gen-impl-conc-stm-tvar-type">TVar</a> :A)</code>

Get the synchronized variable at `i` in `tarr`.



***

#### <a href="#io-gen-impl-conc-stm-tarray-tvar-at#-value"><code>(TVAR-AT# TARR I)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/gen-impl/conc/tarray.lisp#L117-L119">src</a></sub></sup><a name="io-gen-impl-conc-stm-tarray-tvar-at#-value"></a>
<code>&forall; :A. <a href="#io-gen-impl-conc-stm-tarray-tarray-type">TArray</a> :A * <a href="#coalton-ufix-type">UFix</a> &rarr; <a href="#io-gen-impl-conc-stm-tvar-type">TVar</a> :A</code>

Get the synchronized variable at `i` in `tarr`. Errors if out of bounds.



***

# Package `IO/IO-ALL`<a name="io-io-all-package"></a>

### Macros

#### <a href="#io-io-all-derive-monad-io-all-macro"><code>DERIVE-MONAD-IO-ALL (MONAD-PARAM MONADT-FORM)</code></a> <sup><sub>[MACRO]</sub></sup><a name="io-io-all-derive-monad-io-all-macro"></a>


***

#### <a href="#io-io-all-implement-io-all-macro"><code>IMPLEMENT-IO-ALL (MONAD)</code></a> <sup><sub>[MACRO]</sub></sup><a name="io-io-all-implement-io-all-macro"></a>


***

# Package `IO/STUBS/TERM`<a name="io-stubs-term-package"></a>

### Types

#### <a href="#io-stubs-term-termstub-type"><code>TermStub</code></a> <sup><sub>[TYPE] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/stubs/term.lisp#L47-L47">src</a></sub></sup><a name="io-stubs-term-termstub-type"></a>


***

#### <a href="#io-stubs-term-termstubm-type"><code>TermStubM</code></a> <sup><sub>[TYPE] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/stubs/term.lisp#L46-L46">src</a></sub></sup><a name="io-stubs-term-termstubm-type"></a>


***

### Values

#### <a href="#io-stubs-term-run-term-stub-value"><code>(RUN-TERM-STUB STUB-OP READ-LINE-INPUTS)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/stubs/term.lisp#L115-L116">src</a></sub></sup><a name="io-stubs-term-run-term-stub-value"></a>
<code>&forall; :A. <a href="#coalton-monad-freet-freet-type">FreeT</a> <a href="#io-stubs-term-termstubf-type">TermStubF</a> <a href="#coalton-monad-identity-identity-type">Identity</a> :A * <a href="#coalton-list-type">List</a> <a href="#coalton-string-type">String</a> &rarr; <a href="#coalton-classes-tuple-type">Tuple</a> (<a href="#coalton-list-type">List</a> <a href="#coalton-string-type">String</a>) :A</code>


***

#### <a href="#io-stubs-term-run-term-stubm-value"><code>(RUN-TERM-STUBM OPM READ-LINE-INPUTS)</code></a> <sup><sub>[FUNCTION] · <a href="https://github.com/Jason94/coalton-io/tree/master/src/stubs/term.lisp#L73-L112">src</a></sub></sup><a name="io-stubs-term-run-term-stubm-value"></a>
<code>&forall; (:M (Type &rarr; Type)) :A. <a href="#coalton-classes-monad-class">Monad</a> :M &rArr; <a href="#coalton-monad-freet-freet-type">FreeT</a> <a href="#io-stubs-term-termstubf-type">TermStubF</a> :M :A * <a href="#coalton-list-type">List</a> <a href="#coalton-string-type">String</a> &rarr; :M (<a href="#coalton-classes-tuple-type">Tuple</a> (<a href="#coalton-list-type">List</a> <a href="#coalton-string-type">String</a>) :A)</code>


***

