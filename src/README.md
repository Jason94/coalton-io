## `src/gen-impl/`

Contains two types of packages:

1. Packages providing default implementations of the capability classes that support them (most)
   and the associated `implement-monad-x` macros.

2. Packages that provide features that build off of one or more of the capability classes.
    * _Example:_ `src/gen-impl/conc/io-future` provides futures for any `:m` implementing
    both `MonadException` and `MonadIoThread`.

### `src/gen-impl/conc/`

Contains packages providing:

* New `Concurrent` instances (like `Future` and `ConcurrentGroup`)
* Data synchronization tools, like `MVar`
* Control-flow synchronization tools, locks or barriers
