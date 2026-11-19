# coalton-queue-comparison

A standalone Common Lisp ASDF project with matched cl-freelock, lparallel,
and STMX workloads for queue, scheduler, and STM benchmarks already defined by
`coalton-io`.

The project does not wrap foreign data structures in Coalton classes or add
benchmarks to `coalton-io`.

## Queue comparisons

| Existing `benchmark-queues` benchmark | Native comparison workloads |
| --- | --- |
| `unbounded-enqueue-x-tasks-1-thread` | cl-freelock and lparallel enqueue, 1 thread |
| `unbounded-dequeue-x-tasks-1-thread` | pre-populated cl-freelock and lparallel dequeue, 1 thread |
| `unbounded-enqueue-try-dequeue-pairs-x-tasks-1-thread` | cl-freelock and lparallel paired enqueue/try-dequeue, 1 thread |
| `unbounded-enqueue-try-dequeue-x-tasks-1-enqueue-thread-1-dequeue-thread` | cl-freelock and lparallel, 1P/1C |
| `unbounded-enqueue-try-dequeue-x-tasks-6-enqueue-threads-6-dequeue-threads` | cl-freelock and lparallel, 6P/6C |
| `bounded-enqueue-x-tasks-1-thread` | cl-freelock and lparallel bounded enqueue, 1 thread |

Each sample performs 24,000 successful enqueues and, for pipeline workloads,
24,000 successful dequeues. Queue construction and worker construction are
outside the timed region. The start gate and the two-second settling period
mirror the existing `coalton-io` queue benchmark workers.

The existing Coalton bounded benchmark uses capacity 24,000. Since cl-freelock
requires a power-of-two capacity, both native bounded queues use capacity
32,768. The workload performs only 24,000 pushes, so none of the queues becomes
full or exercises a blocking path. No new Coalton benchmark is introduced.

## Scheduler comparisons

The lparallel and Coalton threads scheduler cases match the producer/worker
matrix of the coalton-io bounded and unbounded scheduler benchmarks. Each sample
submits 199,992 tasks, each performing one shared atomic increment.

The public cases time submission and completion through `submit-task` and
`receive-result` for lparallel, and `async` and `await` for Coalton threads.
The Common Lisp harness calls the public Coalton workload once per sample.
The `-optim` cases use internal submission paths and stop timing when the
shared count reaches the task total. Producer joins are timed in the public
cases and follow timing in the `-optim` cases. Scheduling one increment per
task makes the cost of result channels and futures visible even though the
workload does not use the returned values.

## STM comparisons

The STM set compares every existing `benchmark-stm` workload with STMX using
the same 576,000 transactional operations, 30 samples, and worker counts:

| Workload | Workers |
| --- | --- |
| one shared transactional variable | 1, 2, 4, 6 |
| eight shared transactional variables per transaction | 1, 2, 4, 6 |
| sequential transactions over 6 transactional cells | 1, 2, 4, 6 |
| sequential transactions over 60 transactional cells | 1, 2, 4, 6 |
| one independently written transactional variable per worker | 1, 2, 4, 6 |
| retry-driven round-robin increment | 2, 4, 6 |

Transactional data and worker threads are created outside the timed region. A
semaphore gate ensures every worker is ready before timing begins; the timed
region includes releasing the workers and waiting for all work to complete.
Correctness checks run after timing, matching the existing Coalton benchmarks.
The comparison uses STMX's standard `atomic`, `tvar`, `$`, and `retry` APIs.

## Run

```lisp
(asdf:load-system "coalton-queue-comparison")
```

Write the native results to `benchmark_queue_comparison.csv`:

```lisp
(benchmark-queue-comparison:run-ci)
```

Write the scheduler results to `benchmark_scheduler_comparison.csv`:

```lisp
(benchmark-scheduler-comparison:run-ci)
```

Write the STMX results to `benchmark_stm_comparison.csv`:

```lisp
(benchmark-stm-comparison:run-ci)
```

