```lisp
(asdf:load-system "coalton-io/benchmarks")
(io/benchmarks::run-benchmarks-ci)
(io/benchmarks::run-benchmark-ci 'benchmark-schedulers)
(io/benchmarks::run-benchmark-ci 'benchmark-simple-io)
```
