coalton-io's directory structure is complex to support the project's three goals:

1. Provide a generic interface over different kinds of effects and generic implementation for as many of those effects as possible.
2. Provide a specific effect monad, `IO`, and a runtime implementation, `IoRuntime`.
3. Package 1 & 2 together in a way that hides this complexity from end-users that just want to use `IO`.

Also, load-order issues provide a strong incentive to move generic definitions and type-classes earlier in the load order.

The project is broadly organized into two kinds of sub-directories:

* Library private - Folders containing packages that contain code for goal #1 or #2.
  * Essentially all of the actual _code_ in coalton-io lives in these folders.
* Library public - Import all of the generic machinery and IO specific implementation that is useful for end-users into a single "module" that bundles all the necessary exports to work with one kind of effect.
  * Currently, only library public packages are included in the documentation generator.

## Library Private Subfolders

### `src/classes/`

Contains packages defining the core `MonadIo` classes, the `Exceptions` class, and the effect classes. Contains essentially no implementation.

### `src/thread-impl/`

This folder has a few files defining some of the low-level concurrency tools used throughout and the implementation of `IoRuntime`.

### `src/gen-impl/`

Contains two types of packages:

1. Packages providing default implementations of the capability classes that support them (most)
   and the associated `implement-*` macros.
2. Packages that provide features that build off of one or more of the capability classes.
    * _Example:_ `src/gen-impl/conc/io-future` provides futures for any `:m` implementing
    both `Exceptions` and `Threads`.

#### `src/gen-impl/conc/`

Contains packages providing:

* New `Concurrent` instances (like `Future` and `ConcurrentGroup`)
* Data synchronization tools, like `MVar`
* Control-flow synchronization tools, locks or barriers

### `src/io-impl/`

Contains packages providing:

* The `IO` implementation of the effect classes. This is usually, but not always, a simple invocation of the `implement-x` macro.
* `IO` specific versions of the generic functions. This is particularly useful for functions using `UnliftIo`, which can cause inference issues without manual annotation and/or proxy usage.

## Library Public Subfolders

### `src/`

Contains packages:

1. All of the main public "modules" for the core effect types are here.
   * _Example:_ All symbols needed to work with terminal IO are re-exported in `io/io-term.lisp`.
2. A few library public packages that are important enough to belong in the root `src/` directory, particularly `io/io-resource.lisp`.

#### `src/conc/`

Re-exports packages from `src/gen-impl/conc/` and any `IO` specific functions from `src/io-impl/`.

#### `src/stubs/`

Contains packages providing mock implementations of a few of the effect classes for testing purposes. This is the only directory that doesn't use the re-export organization - stubs are implemented here.
