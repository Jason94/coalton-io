```lisp
(asdf:load-system "coalton-io/benchmarks")
(in-package :io/benchmarks)
(run-benchmarks-ci)
(run-benchmark-ci 'benchmark-schedulers)
```
