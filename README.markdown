# `coalton-io` - Functional Applications in Coalton
<p align="center">
    <img alt="Coalton IO Build Status" src="https://github.com/Jason94/coalton-io/actions/workflows/ci.yml/badge.svg?branch=master" />
</p>

_coalton-io_ provides tools to write safe, functional programs in Coalton that can perform real work, like:
* Robust exception handling & Resource safety
* Mutating data
* Generating random numbers
* Terminal & File IO
* Multithreading, with comprehensive support for:
  - Stopping threads at any time
  - Structured concurrency to prevent orphaned threads
  - Masking to protect critical areas
* Safely sharing data between threads (_coalton-io_ provides Atomic variables, MVars, MChans, a Software Transactional Memory, Futures, Thread Pools, and more)

`IO` is fast. Even in high-frequency hot loops, [benchmarks](benchmarks/benchmark_simple_io.csv) show that `IO` is competitive (_within 50%-80% speed of impure Coalton_) with iterative, non-pure Coalton code. If hot loops don't need to execute effects inside the loop, then `IO` runs with no significant overhead.

_coalton-io_ also allows you to extend all of this functionality for free if you want to write your own underlying effect type.

### Documentation

[**Read the API docs here**](https://jason94.github.io/coalton-io/)

[Read the asynchronous runtime specification here](docs/runtime.md)

## Example Usage

```lisp
  (declare sum-file (IO Integer))
  (define sum-file
    (do
     (write-line "Writing data file...")
     write-data-file
     (write-line "Done writing file...")
     (input-chan <- mv:new-empty-chan)
     (ints-chan <- mv:new-empty-chan)
     (sum-mvar <- mv:new-empty-mvar)
     (write-line "Forking threads...")
     (fork (reader-thread input-chan))
     (do-loop-times (_ n-workers)
       (fork (parser-thread input-chan ints-chan)))
     (fork (summer-thread ints-chan sum-mvar))
     (write-line "Waiting for sum...")
     (sum <- (mv:take-mvar sum-mvar))
     (write-line (<> "Calculated sum: " (into sum)))
     (pure sum)))
```

## Getting Started & Installation

The [coalton-io-template](https://github.com/Jason94/coalton-io-template) is a ready-made project to get started writing code with `coalton-io`. It uses [Qlot](https://github.com/fukamachi/qlot) to manage the project's dependencies.

Install Qlot, checkout the template, and download the dependencies by running:

```sh
curl -L https://qlot.tech/installer | sh
git clone https://github.com/Jason94/coalton-io-template.git coalton-io-project
cd coalton-io-project
qlot install
```

See [the coalton-io-template Readme](https://github.com/Jason94/coalton-io-template) for more instructions on how to run the program and its tests.

### Installing Manually

`coalton-io` depends on a later version of Coalton than the current Quicklisp release. You can easily install it by checking it out to your `local-projects` directory:

```bash
git clone https://github.com/coalton-lang/coalton.git ~/quicklisp/local-projects/coalton
```

Once you have the latest version of Coalton, you can install `coalton-io` from [Ultralisp](https://ultralisp.org/). See the Ultralisp website for setup instructions. Once the "ultralisp" distribution is set up, simply install it with:

```lisp
(ql:quickload "coalton-io")
```

## Examples

_coalton-io_ has a several example programs to demonstrate how to use `IO`:

* [Redis](examples/redis/) - A CLI client and multithreaded key/value server that implement a small portion of the RESP binary protocol.
  - _Demonstrates_: Resource handling, terminal IO, multithreading, networking, STM usage
* [Hangman](examples/hangman.lisp) - Play a game of hangman in the terminal. Shows `IO` basics and terminal IO.
  - _Demonstrates_: Terminal IO, file IO, how to implement the common "ReaderT" pattern for structuring functional programs, how to use "capability class" style to increase flexibility of IO functions
* [Channels & Threading](examples/channels-threading.lisp) - Multithreaded application to process an input data file.
  - _Demonstrates_: Multithreading, file IO, passing data safely between threads at a low level using channels and MVar's
* [Networking](examples/network-demo.lisp) - Run a simple client and server that allows the client to send messages to the server.
  - _Demonstrates_: Networking, resource safety, terminal IO

## Feature Breakdown

_coalton-io_ provides the following features in these packages:

* `io/exceptions` - Raise and handle exceptions within `IO`, automatically capture unhandled Lisp/Coalton errors, and easily convert between exceptions and `Result`/`Optional`
* `io/resource`   - Operations to safely acquire, use, and release resources
* `io/terminal`       - Read/write to the Console
* `io/files`       - Read/write to files and interact with the file system
* `io/random`     - Generate random numbers
* `io/mutable-var`        - Use unsynchronized (non-thread safe) mutable variables in pure code
* `io/unique-gen`     - Generate guaranteed unique values (thread safe)
* `io/threads`     - Fork new threads which run their own `IO` operations
* `io/sockets` - Connect sockets and send data over TCP/IP
* `io/conc/future`- Futures that run an `IO` computation in another thread and return the value to the calling thread
* `io/conc/atomic`- Atomic mutable variables for sharing state across threads
* `io/conc/mvar`  - Provides `MVar`s (synchronized single-value mutable stores to hand off data between threads) and `MChan`s (thread safe FIFO queues to stream data between threads)
* `io/conc/parking` - Park and unpark threads to wait on multiple conditions
* `io/conc/stm`   - Atomically run transactions on mutable memory shared between threads
* `io/conc/group` - `ConcurrentGroup`s that atomically manage the masking, stopping, and awaiting of a group of `Concurrent`s
* `io/conc/worker-pool` - Pool of worker threads that execute tasks submitted to the pool.

If you just want to use `IO` to write an application, use `io/simple-io` to get the standard `IO` type.

If you want to write _your own_ effect type, use `io/monad-io` and `io/io-all` to cover your own type with all of the features in the list above.

### Exceptions

You can raise exceptions in `IO`. Any unhandled exceptions are thrown when the IO is run. Anything that can be thrown in Coalton can be raised in `IO`:

```lisp
  (do
   (str <- retrieve-str-data)
   (do-when (== str "")
     (raise "An empty string was returned."))
   (write-line "This won't run if str == ''"))
```

Exceptions can be handled in several ways, including only handling exceptions of particular types. For example, `handle-all` recovers from all exceptions:

```lisp
  (do
   (file-data <-
     (handle-all (read-file-data "data.csv")
                 (const (pure Nil))))
   (do-foreach (str file-data)
     (write-line str)))
```

`wrap-io`, the main way to run normal Coalton functions in `IO`, automatically handles any errors thrown from Coalton or Lisp:

```lisp
  (do
   (file-data <-
     (handle-all (wrap-io
                   (lisp (List String) ()
                     (cl-read-file-data "data.csv")))
                 (const (pure Nil))))
   (do-foreach (str file-data)
     (write-line str)))
```

### Resource Safety

Use `bracket-io` to guarantee resources are released, even when exceptions are thrown. This wraps acquire, use, and cleanup in a single flow so files, connections, or locks never leak.

```lisp
  (bracket-io
    (open-data-source)
    (fn (source exit-case)
      (if (== Completed exit-case)
          (close-data-source source)
          (report-data-source source)))
    (fn (source)
      (process-source source)))
```

### Terminal IO

Read and write to/from the terminal. Writing to the terminal supports any type with an `Into :a String` instance, not just `String`.

```lisp
  (define (prompt-integer)
    (do
     (write "Please enter an integer: ")
     (input <- read-line)
     (do-match (parse-int input)
       ((None)
        (prompt-integer))
       ((Some x)
        (write "You entered: ")
        (write-line x)
        (pure x)))))
```

### Random Numbers

You can get, copy, and set the current random state. The `random` functions support generating numbers of several types from _[0, x)_:

```lisp
  (do
   (rs <- make-random-state)
   (set-current-random-state rs)
   (random_ 0.5))
```

### Threads

The `fork` family of functions & macros spawn new threads that run an IO operation. 

```lisp
  (do
   (do-fork
     (write-line "Hello from thread A"))
   (do-fork
     (write-line "Hello from thread B"))
   (sleep 2)
   (write-line "Hello from main thread"))
```

Mutable variables can be shared across threads. Plain mutable variables from the `io/mutable-var` package are **not** threadsafe, so this should generally be avoided. _coalton-io_ provides several other forms of mutable state that are suitable for sharing between threads.

```lisp
  (do
   (msg <- (new-var ""))
   (do-fork
     (write msg "Hello from thread A"))
   (sleep 1)
   (msg-str <- (read msg))
   (write-line msg-str)) ;; --> Hello from thread A (probably)
```

The `fork` functions and macros return a handle to the thread object, which can be used to interact with the thread, such as stopping its execution.

```lisp
  (do
   (thread <-
     (do-thread
       (sleep 10)
       end-the-world))
   (stop thread)
   (sleep 20)
   (write-line "That was a close one"))
```

Coalton-io uses _structured_ concurrency, which helps prevent orphaned threads. For example, suppose a thread sets up a `WorkerPool` and prepares to distribute tasks to the pool. But when it begins parsing the data, it raises an unhandled exception and the thread ends. The worker pool threads will automatically be cleaned up when the parent thread ends from the exception.

Threads are forked as a child of a parent thread, which sets their scope. When a parent thread finishes, it stops and joins all of its child threads. The top-level `run!` also sets a `Detached` scope. Threads forked under the `Detached` scope will outlive the thread that forked them, but will be stopped when the top-level `run!` finishes. For more on structured concurrency, [read here](docs/runtime.md#structured-concurrency).

### Atomic Transactions (STM)

Transaction Variables (`TVar`s) are mutable variables that can only be read/written in a transaction. Transactions are guaranteed to be atomic with respect to any of the TVar's referenced inside the transaction. STM transactions are usually much simpler and less error-prone than corresponding lock-based solutions, and the STM is particularly well suited for code that needs to access several different points of shared state.

This (slightly longer) example program manages ticket sales with transactions. The STM package provides more advanced features, such as signalling a `retry` to wait for particular state conditions.

```lisp
  (do
   (tickets <- (new-tvar 3))
   (money-paid <- (new-tvar 0))
   (let cost = 40.0)
   (let customers = (make-list "A" "B" "C" "D" "E"))
   (bought-a-ticket <- (new-tvar Nil))
   (do-foreach-io_ (customer customers)
     (do-fork
       (initial-balance <- (random_ 100.0))
       (balance <- (new-tvar initial-balance))
       (money-left <-
         (do-run-tx
           (current-balance <- (read-tvar balance))
           (tickets-remaining <- (read-tvar tickets))
           (do-when (and (> current-balance cost)
                         (> tickets-remaining 0))
             (modify-tvar money-paid (+ cost))
             (modify-tvar bought-a-ticket (Cons customer))
             (write-tvar tickets (1- tickets-remaining))
             (write-tvar balance (- current-balance cost)))
           (read-tvar balance)))
       (write-line (build-str "Customer " customer " has " money-left " left"))))
    (sleep 10)
    ((Tuple money-earned customers-with-tickets) <-
       (do-run-tx
         (money-earned <- (read-tvar money-paid))
         (customers-with-tickets <- (read-tvar bought-a-ticket))
         (pure (Tuple money-earned customers-with-tickets))))
    (write-line (build-str "Earned $" money-earned))
    (write-line "Customers who bought tickets:")
    (foreach-io_ customers-with-tickets write-line))
```

## Contributing

The file structure of the source code is complex, but the project is organized consistently. How the files are laid out in `src/` is documented [here](src/README.md).

Contributions to the concurrent codebase are welcome. However, because asynchronous exceptions are particularly tricky to develop around, please follow the standards for _Concurent Function Documentation_ given in [the runtime documentation](docs/runtime.md). 

### Running the tests

Run the tests in the REPL with:
```lisp
(asdf:test-system "coalton-io")
```
