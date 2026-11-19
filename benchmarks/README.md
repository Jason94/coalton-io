```lisp
(asdf:load-system "coalton-io/benchmarks")
(io/benchmarks:run-benchmarks-ci)
```

The full suite runs four times in sequence from a Git checkout. Results are
archived in `benchmarks/<UTC timestamp>_<full Git commit hash>/`, with one
subdirectory each for `A`, `B`, `C`, and `D`. The UTC timestamp uses the
`YYYYMMDDTHHMMSSZ` format. After run `D` completes, its CSVs replace the
matching CSVs in `benchmarks/` under their existing filenames.

To run a single benchmark package and write its CSV in the current directory:

```lisp
(io/benchmarks::run-benchmark-ci 'benchmark-schedulers)
(io/benchmarks::run-benchmark-ci 'benchmark-simple-io)
(io/benchmarks::run-benchmark-ci 'benchmark-runtime)
(io/benchmarks::run-benchmark-ci 'benchmark-stm)
(io/benchmarks::run-benchmark-ci 'benchmark-queues)
(io/benchmarks::run-benchmark-ci 'benchmark-mvars)
```
