# `coalton-io` - Functional Applications in Coalton

_coalton-io_ provides tools to write pure, functional programs in Coalton that can perform necessary tasks like:
* Robust exception handling & Resource safety
* Mutable data
* Random numbers
* Terminal & File IO
* Multithreading
* Safely sharing data between threads (_coalton-io_ provides Atomic variables, MVars, MChans, and a Software Transactional Memory system)

_coalton-io_ also extends all of this functionality for free if you want to write your own underlying effect type.

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

## Installation


`coalton-io` depends on a later version of Coalton than the current Quicklisp release (as of 11/13/2025). You can easily install it by checking it out to your `local-projects` directory:

```bash
git clone https://github.com/coalton-lang/coalton.git ~/quicklisp/local-projects/coalton
```

Once you have the latest version of Coalton, you can install `coalton-io` from [Ultralisp](https://ultralisp.org/). See the Ultralisp website for setup instructions. Once the "ultralisp" distribution is set up, simply install it with:

```lisp
(ql:quickload "coalton-io")
```

## Examples

_coalton-io_ has two example programs to demonstrate how to use `IO`:

* [Hangman](examples/hangman.lisp) - Play a game of hangman in the terminal. Shows `IO` basics and terminal IO.
* [Channels & Threading](examples/channels-threading.lisp) - Multithreaded application to process an input data file. Shows how to mix different `IO` effects, multithreading, and passing data safely between threads.

## Feature Breakdown

_coalton-io_ provides the following features in these packages:

* `io/exception` - Raise and handle exceptions within `IO`, automatically capture unhandled Lisp/Coalton errors, and easily convert between exceptions and `Result`/`Optional`
* `io/resource`  - Operations to safely acquire, use, and release resources.
* `io/term`      - Read/write to the Console
* `io/file`      - Read/write to files and interact with the file system
* `io/random`    - Generate random numbers
* `io/mut`       - Use unsynchronized (non-thread safe) mutable variables in pure code
* `io/unique`    - Generate guaranteed unique values (thread safe)
* `io/thread`    - Fork new threads which run their own `IO` operations
* `io/atomic`    - Atomic mutable variables for sharing state across threads
* `io/mvar`      - Provides `MVar`s (synchronized single-value mutable stores to hand off data between threads) and `MChan`s (thread safe FIFO queues to stream data between threads)
* `io/future`    - Futures that run an `IO` computation in another thread and return the value to the calling thread
* `io/stm`       - Atomically run transactions on mutable memory shared between threads

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

## Examples

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

Mutable variables can be shared across threads. Plain mutable variables from the `io/mut` package are **not** threadsafe, so this should generally be avoided. _coalton-io_ provides several other forms of mutable state that are suitable for sharing between threads.

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

## TODOs

* [ ] Standardize naming convention and type signatures with `_` suffix. (Should probably always mean pegged to IO)
