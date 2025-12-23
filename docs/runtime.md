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

### Forking a Thread

`fork!` guarantees that the forked function will eventually be run, per above. Second, `fork!` guarantees that if the forked thread raises an unhandled exception, that the thread will (1) log the exception to `cl:*error-output*` and (2) immediately stop itself. It will not re-raise the exception, and it will not stop the entire program because that thread failed. Threads start unmasked.

`fork-throw!` has identical behavior to `fork!`, except in how it handles raised exceptions. If a thread forked with `forked-throw!` raises an unhandled exception, it will be re-raised to the Common Lisp environment. Exactly what impact that has on other threads and the program is determined by the underlying Common Lisp implementation and the execution environment.

### Joining a Thread

`join!` blocks the current thread until a target thread has completed, encountered an unraised exception, or been stopped. Joining a thread that has already completed is only guaranteed to complete in a reasonable amount of time, but could block for a small time. In `IoRuntime`, joining a completed thread is defined by the corresponding behavior of the underlying Common Lisp implementation.

`join!` returns a `Result Dynamic Unit`, where the Dynamic error case is an error that was raised and unhandled by the target thread. A common idiom is to use `(raise-result (join-thread thread))` (see `MonadIoThread` for the `MonadIo` wrapped version of `join!`), to simply re-raise any unhandled exceptions.

### Stopping, Masking, and Unmasking a Thread

#### Stopping

`stop!` immediately sends a stop signal to the target thread and returns. Stopping does not block until the target thread finishes stopping (it does not block at all). Stopping an already stopped/completed thread is a no-op. Stopping a thread guarantees it will not do any more work on the processor, except for three possible exceptions:
* the small amount of cleanup work done by the `MonadIo` wrapping
* if the thread is masked (see below)
* if the thread is in a context that is masked under an asyc interrupt masking mechanism provided by the underlying Common Lisp implementation.

As such, a runtime that implements a purely cooperative stopping model violates the requirements of a runtime. A runtime that immediately terminated the target thread would also violate the requirements, because it would not allow the target thread to (1) perform the cleanup done by `MonadIo`, or (2) respect masking.

In `IoRuntime`, because asynchronous exceptions are not part of the Common Lisp standard, the specific behavior of stopping is defined by the underlying Common Lisp implementation. In particular, `IoRuntime` uses `bordeaux-threads:error-in-thread` to deliver the asynchronous stop. On SBCL, this translates into a call to [sb-thread:interrupt-thread](https://www.sbcl.org/manual/#index-interrupt_002dthread).

#### Masking

`mask!` masks a thread. If a thread is masked, then `stop!` does not stop it - instead, it pends a "pending stop" on the target thread. Masking does not protect a thread from anything else, other than an asynchronous stop. For example, masking does not protect a thread from unhandled exceptions.

Masking can be nested, and a runtime must track the number of times the current thread has been masked so that it knows when it has been unmasked completely and is stoppable again. For performance reasons, a runtime is allowed to have a maximum possible number of masks, after which masking again is undefined behavior. Masking and unmasking should be fast (and ideally contention-free) because it's used comprehensively and at a low-level throughout the concurrency code. On `IoRuntime`, masking is quite fast: it performs an atomic (+ 2) operation to an atomic unsigned integer. On `IoRuntime`, the maximum possible number of nested masks is the maximum unsigned integer value of the architecture / 2. After that, integer overflow will cause undefined behavior and could result in the thread getting stopped immediately.

`unmask!` removes one level of masking. If the thread becomes completely not-masked, then it is now stoppable. If unmasking (1) removes the last level of maksing and (2) the thread received a pending stop while masked, then `unmask!` immediately stops the thread.

It's worth noting that the potential for `unmask!` to stop the thread doesn't actually create any new critical boundaries. Consider the following snippet:

```lisp
(mask! runtime current-thread)
(let resource = (acquire-resource))
(perform-work resource)
(unmask! runtime current-thread)
(release-resource resource)
```

This code is unsafe because the `unmask!` call on line four could stop the thread before `release-resource` has a chance to run on line five. However, even if `unmask!` did not honor pending stops and never stopped the thread, this code would still be unsafe because the current thread could receive an asynchronous stop after line four but before line five.

## Concurrent Function Documentation

`coalton-io` follows the following user-facing and internal documentation practices for any functions that run safely in a concurrent environment.

### Docstrings

The following default assumptions apply to all concurrent functions:

1. The function masks itself during any critical sections.
2. The function unmasks any masks it applied before it exits.
3. The function does not block.
4. If it does block, the function does not mask any blocking operation.
5. The function does not mask execution of any callback passed into it, in case it blocks.

Functions may violate these assumptions. However, any non-standard behavior must be documented in a `Concurrent:` section of the docstring. Pay particular attention to the `Concurrent` documentation for any functions used, particular if they leave the thread masked. Emphasize any non-standard behavior requiring the caller to take action.

Here is an example of a comprehensive `Concurrent:` docstring from the MVar implementation:

```lisp
  (inline)
  (declare take-mvar-masked (MonadIoThread :rt :t :m => MVar :a -> :m :a))
  (define (take-mvar-masked mvar)
    "Take a value from an MVar, blocking until one is available.

Concurrent:
  - WARNING: Leaves the thread masked when returns to protect caller's critical regions
    based on consuming and restoring MVar to a valid state. See MChan for an example.
  - Blocks while the MVar is empty
  - Read-consumers (including `take-mvar-masked`) are woken individual on succesfull puts,
    in order of acquisition
  - On succesful take, one blocking writer is woken in order of acquisition"
    ;; CONCURRENT: Inherits CONCURRENT semantics from take-mvar-masked-inner%
    (wrap-io-with-runtime (rt-prx)
      (take-mvar-masked-inner% mvar rt-prx)))
```

### Internal Documentation

Any concurrent function should have a top-level, internal `CONCURRENT:` comment briefly (1) justifying the soundness of its masking behavior and (2) noting any invariant behavior. These comments should be treated similarly to SAFETY comments in Rust.

Here is an example of a comprehensive `CONCURRENT:` comment from the MVar implementation:

```lisp
  (declare take-mvar-masked-inner% (Runtime :rt :t => MVar :a -> Proxy :rt -> :a))
  (define (take-mvar-masked-inner% mvar rt-prx)
    "Concurrent: Leaves the thread masked once."
    ;; CONCURRENT: Masks before entering the critical region.
    ;; unmask-and-await-safely% unmasks and awaits, then wakes and re-masks in a
    ;; catch block guaranteeing lock release.
    ;; The thread can only be stopped during unmask-and-await-safely%, thus cannot
    ;; be stopped between emptying the MVar and notifying listeners.
    ;; On the post-wakeup success path, does not unmask and leaves the applied mask
    ;; to the caller to handle.
    (mask-current! rt-prx)
    (lk:acquire (.lock mvar))
    (let ((lp (fn ()
                (match (at:read (.data mvar))
                  ((Some val)
                   (at:atomic-write (.data mvar) None)
                   (lk:release (.lock mvar))
                   (cv:notify (.notify-empty mvar))
                   val)
                  ((None)
                   (unmask-and-await-safely% rt-prx (.notify-full mvar) (.lock mvar))
                   (lp))))))
      (lp)))
```
