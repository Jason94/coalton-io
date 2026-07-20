To run all tests:
```
ros dynamic-space-size=4000 run
(asdf:test-system "coalton-io/stress-tests")
```

To run one package's tests:
```
(asdf:load-system "coalton-io/stress-tests")
(in-package :io/stress/stm-fiasco)
(run-package-tests)
```
