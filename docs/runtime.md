# Runtime Specification

The `Runtime` class is an interface to the core threading operations that build all of `coalton-io`'s other concurrent programming machinery. This document gives the specific guarantees that a Runtime implementation must provide on how threads are stopped, masked, awaited, etc. It also documents any behavior specific to `IoRuntime`, the thread runtime included in the library.

Though not documented here, the `Concurrent` class provides a similar interface to the thread interface that `Runtime` provides. In general, a `Concurrent` implementation should provide semantics as close to the underlying thread model as possible. There will be some differences - for example, `ConcurrentGroup` manages a collection of threads. Each `Concurrent` should document its own guarantees, particularly to the extent they differ from those described here.

#### Asynchronous Stops

Much of the complexity in the `Runtime`'s guarantees and the concurrency code in `coalton-io` comes from the support of asynchronous stops between threads. Many other languages, such as any language on the JVM, only allows _cooperative_ asynchronous communication: one thread must explicitly accept any incoming stops, but a thread can never be stopped unless it decides to be. This means that those languages can't interrupt threads that are blocking on CPU-bound work.

Most Common Lisp implementations _do_ support this feature, and it's thoroughly integrated with `coalton-io`. On its own, this adds significant complexity because you have to account for the possibility of your thread being stopped at every line of code. One of the goals of `coalton-io` is to manage this complexity. Every concurrent tool in the library guarantees that it can't be stopped in an inoperable state. (For example, a thread committing an STM transaction will never stop mid-commit.) Additionally, the functions in `io/resource` abstract away much of the complexity in dealing with asynchronous stops for user-created resources.

## The Runtime Class

`Runtime` is defined in `src/classes/monad-io-thread.lisp`. In the definition, `:r` is the runtime and `:t` is the underlying "thread." A runtime object (`:r`) is never actually created. It's simply a type that is passed around to tell the compiler which versions of the class's functions should be used.

```lisp
  (define-class (Runtime :r :t (:r -> :t))
    (current-thread!
     (Proxy :r -> :t))
    (sleep!
     (Proxy :r -> UFix -> Unit))
    (fork!
     (Proxy :r -> (Unit -> Result Dynamic :a) -> :t))
    (fork-throw!
     (Proxy :r -> (Unit -> Result Dynamic :a) -> :t))
    (join!
     (Proxy :r -> :t -> Result Dynamic Unit))
    (stop!
     (Proxy :r -> :t -> Unit))
    (mask!
     (Proxy :r -> :t -> Unit))
    (unmask!
     (Proxy :r -> :t -> Unit))
    (unmask-finally!
      (Proxy :r -> :t -> (UnmaskFinallyMode -> :a) -> Unit)))
```

Unlike the runtime type, an instance of the underlying thread type is created whenever any of the `fork!` functions are called. Because of the functional dependency in the class definition (the `:r -> :t`), you can only ever have _one_ underlying thread type for a given Runtime. For example, the `IoRuntime` is defined like this:

```lisp
  (define-instance (Runtime IoRuntime IoThread)
    ...)
```

This definition tells the compiler that any `IoRuntime` will always fork threads of type `IoThread`. However, it is possible to create multiple runtimes that use the same underlying thread type. In practice, that won't be very useful.

## Runtime Guarantees

The only requirements of a runtime _in general_ are:
1. Forked functions must be run eventually.
2. All forked functions will execute on the same computer.

In principle, the second restriction could be relaxed, which would allow distributed runtimes to run with the same code as non-distributed. However, this would require exposing a lot more functionality through the `Runtime` class, such as locks and condition variables, which would all need to have distributed versions supplied.

Beyond this, there are no assumptions about the underlying "thread" in a runtime. The thread could be a system thread or a green thread managed by the runtime itself. The runtime could even not run code in parallel _at all_, and just queue forked functions to run sequentially on the main thread.

The `IoRuntime` uses system threads, as provided by the underlying Common Lisp implementation. An `IoThread` is a small wrapper around a system thread from `bordeaux-threads`, with some metadata about its mask and pending-stop status. As with any system thread-based runtime, this means that forking new threads is a (comparatively) expensive operation.
