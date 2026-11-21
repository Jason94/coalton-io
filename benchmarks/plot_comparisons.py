#!/usr/bin/env python3
r"""Reproduce the six comparative-throughput JPG charts.

Install this file as coalton-io/benchmarks/plot_comparisons.py, alongside the
coalton-io CSVs. The comparison project belongs at
coalton-io/benchmark-comparisons/, with its CSVs at that directory's top level.

From benchmarks/:
    python3 -m pip install numpy==2.3.5 pandas==2.2.3 matplotlib==3.10.8 Pillow==13.0.0
    python3 plot_comparisons.py

These dependency versions, Python 3.12, and Matplotlib's bundled DejaVu Sans
font reproduce the original rendering. No Lisp benchmarks are run.

Inputs:
  ../benchmark-comparisons/benchmark_queue_comparison.csv
  ../benchmark-comparisons/benchmark_scheduler_comparison.csv
  <newest timestamped archive>/*/benchmark_queues.csv
  <newest timestamped archive>/*/benchmark_schedulers.csv

All runs inside ONE archive are pooled by sample count. The top-level
coalton-io CSVs are excluded when an archive exists: in the supplied data they
duplicate run D. Without any archive, the two top-level CSVs are used instead.
The comparison project's historical src/ CSVs are never included.

To select the original archive explicitly:
    python3 plot_comparisons.py --io-runs \
        20261119T021631Z_24dd9b022c0d97a67ce8351cdbd51fcd14f5486e

Output: comparative_throughput/{queues,schedulers}/*.jpg, six files total.
Override with --output PATH (relative to the current working directory).

Method: throughput = task count / mean REAL-TIME seconds / 1,000,000.
Queue task count is 48,000 transferred items, including paired/pipeline cases;
scheduler task count is 199,992 completed tasks. These are the recorded suite's
workload sizes. -OPTIM scheduler names identify raw implementations; unsuffixed
comparison scheduler names identify public APIs. coalton-io includes bounded
and unbounded schedulers. Coverage checks reject missing or extra comparison
workloads instead of silently dropping them. Chart limits match the originals;
new measurements outside those limits cause an error instead of being clipped.
"""

import argparse
from pathlib import Path
import re

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.text import Text
import numpy as np
import pandas as pd


QUEUE_TASKS = 48_000
SCHEDULER_TASKS = 199_992
IO_FILES = ("benchmark_queues.csv", "benchmark_schedulers.csv")
QCOL = {
    "coalton-io": "#087F8C",
    "cl-freelock": "#E69832",
    "lparallel": "#7764AD",
    "Coalton channel": "#C65775",
}
QMARK = {"coalton-io": "o", "cl-freelock": "s", "lparallel": "^", "Coalton channel": "D"}
QPREFIX = {"cl-freelock": "FREELOCK", "lparallel": "LPARALLEL", "Coalton channel": "COALTON"}
VARIANTS = [
    ("io_unbounded", "coalton-io unbounded", "#087F8C", "-", "o"),
    ("io_bounded", "coalton-io bounded", "#E69832", "-", "D"),
    ("lp_raw", "lparallel raw", "#7764AD", "--", "^"),
    ("lp_public", "lparallel public", "#7764AD", "-", "s"),
    ("coalton_raw", "Coalton parallel raw", "#C65775", "--", "^"),
    ("coalton_public", "Coalton parallel public", "#C65775", "-", "s"),
]
CONFIGS = [
    (1, 1), (1, 2), (1, 4), (1, 6),
    (2, 2), (2, 4), (2, 6),
    (4, 2), (4, 4), (4, 6),
    (6, 2), (6, 4), (6, 6),
]


def require(condition, message):
    if not condition:
        raise ValueError(message)


def recorded(path):
    """One positive wall-clock mean per benchmark; other metrics are ignored."""
    if not path.is_file():
        raise FileNotFoundError(f"Missing input: {path}")
    data = pd.read_csv(path)
    required = {"benchmark", "metric", "samples", "average"}
    require(required <= set(data.columns), f"Missing CSV columns in {path}: {required - set(data.columns)}")
    data = data.loc[data.metric == "REAL-TIME"].copy()
    require(not data.empty, f"No REAL-TIME records in {path}")
    require(data.benchmark.notna().all(), f"Unnamed benchmark in {path}")
    require(not data.benchmark.duplicated().any(), f"Duplicate REAL-TIME benchmark in {path}")
    for column in ("average", "samples"):
        values = pd.to_numeric(data[column], errors="raise")
        require(np.isfinite(values).all() and (values > 0).all(), f"Invalid {column} in {path}")
        data[column] = values
    require((data.samples % 1 == 0).all(), f"Noninteger sample count in {path}")
    return data.set_index("benchmark")


def run_directories(benchmarks, selected):
    if selected is not None:
        root = Path(selected).expanduser()
        if not root.is_absolute():
            root = benchmarks / root
        root = root.resolve()
    else:
        archives = sorted(
            p for p in benchmarks.iterdir()
            if p.is_dir() and re.fullmatch(r"\d{8}T\d{6}Z_.+", p.name)
        )
        root = archives[-1] if archives else benchmarks
    require(root.is_dir(), f"No coalton-io run directory: {root}")
    # Child runs take precedence over any convenience CSV copies at the root.
    runs = sorted(
        p for p in root.iterdir()
        if p.is_dir() and any((p / name).is_file() for name in IO_FILES)
    )
    if not runs:
        runs = [root]
    for run in runs:
        for name in IO_FILES:
            if not (run / name).is_file():
                raise FileNotFoundError(f"Incomplete coalton-io run; missing {run / name}")
    return root, runs


def pooled_means(runs, filename):
    frames = [recorded(run / filename).reset_index() for run in runs]
    names = set(frames[0].benchmark)
    for run, frame in zip(runs, frames):
        require(set(frame.benchmark) == names, f"Mismatched workload coverage in {run / filename}")
    data = pd.concat(frames)
    return data.groupby("benchmark").apply(
        lambda group: np.average(group.average, weights=group.samples),
        include_groups=False,
    )


def configure_style():
    # Discard local matplotlibrc customizations before applying the original style.
    plt.rcdefaults()
    plt.rcParams.update({
        "font.family": "DejaVu Sans", "font.size": 12,
        "axes.spines.top": False, "axes.spines.right": False,
        "axes.titleweight": "bold", "text.color": "#172942",
        "axes.labelcolor": "#253449", "axes.edgecolor": "#aab5c2",
        "grid.color": "#e0e6ed", "figure.facecolor": "white",
        "savefig.facecolor": "white",
    })


def save(fig, path):
    fig.canvas.draw()
    renderer = fig.canvas.get_renderer()
    width, height = fig.canvas.get_width_height()
    for text in fig.findobj(match=lambda item: isinstance(item, Text)):
        if text.get_visible() and text.get_text().strip():
            box = text.get_window_extent(renderer)
            require(
                box.x0 >= -1 and box.y0 >= -1 and box.x1 <= width + 1 and box.y1 <= height + 1,
                f"Clipped chart text in {path.name}: {text.get_text()}",
            )
    path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(path, dpi=230, pil_kwargs={"quality": 95})
    plt.close(fig)
    return path


def check_range(values, maximum, title):
    require(max(values) <= maximum, f"{title} exceeds the original {maximum} million/s chart limit")


def queue_charts(native, io, output):
    used = set()
    files = []

    def point(lib, io_name, suffix):
        if lib == "coalton-io":
            require(io_name in io.index, f"Missing coalton-io queue workload: {io_name}")
            seconds = io.loc[io_name]
        else:
            name = QPREFIX[lib] + "-" + suffix
            require(name in native.index, f"Missing comparison queue workload: {name}")
            used.add(name)
            seconds = native.loc[name, "average"]
        return QUEUE_TASKS / seconds / 1e6

    def axes_style(ax, xs, xlabel, ymax):
        ax.set_xticks(xs)
        ax.set_xlabel(xlabel)
        ax.set_ylabel("Throughput (million items/s)")
        ax.set_ylim(0, ymax)
        ax.grid(axis="y")
        ax.set_axisbelow(True)

    def legend(fig, ax):
        fig.legend(*ax.get_legend_handles_labels(), loc="lower center",
                   bbox_to_anchor=(.5, .055), ncol=4, frameon=False, fontsize=12)

    fig, ax = plt.subplots(figsize=(11.8, 6.4))
    fig.subplots_adjust(left=.10, right=.98, top=.84, bottom=.22)
    for lib in QCOL:
        ys = []
        for threads in [1, 6, 12]:
            name = f"UNBOUNDED-ENQUEUE-TRY-DEQUEUE-PAIRS-X-TASKS-{threads}-" + ("THREAD" if threads == 1 else "THREADS")
            ys.append(point(lib, name, f"UNBOUNDED-PAIRS-{threads}T"))
        check_range(ys, 27, "Paired queues")
        ax.plot([1, 6, 12], ys, color=QCOL[lib], marker=QMARK[lib], ms=7, lw=2.5, label=lib)
    axes_style(ax, [1, 6, 12], "Threads (each enqueues and dequeues)", 27)
    fig.suptitle("Queues: paired enqueue / dequeue", fontsize=21, fontweight="bold", y=.96)
    legend(fig, ax)
    files.append(save(fig, output / "01_paired_thread_scaling.jpg"))

    fig, ax = plt.subplots(figsize=(11.8, 6.4))
    fig.subplots_adjust(left=.10, right=.98, top=.84, bottom=.22)
    for lib in QCOL:
        ys = []
        for producers in [1, 3, 6]:
            word = "THREAD" if producers == 1 else "THREADS"
            name = f"UNBOUNDED-ENQUEUE-TRY-DEQUEUE-X-TASKS-{producers}-ENQUEUE-{word}-{producers}-DEQUEUE-{word}"
            ys.append(point(lib, name, f"UNBOUNDED-TRY-PIPELINE-{producers}P{producers}C"))
        check_range(ys, 17, "Balanced queues")
        ax.plot([1, 3, 6], ys, color=QCOL[lib], marker=QMARK[lib], ms=7, lw=2.5, label=lib)
    axes_style(ax, [1, 3, 6], "Producers / consumers", 17)
    ax.set_xticklabels(["1 / 1", "3 / 3", "6 / 6"])
    fig.suptitle("Queues: balanced producer / consumer scaling", fontsize=21, fontweight="bold", y=.96)
    legend(fig, ax)
    files.append(save(fig, output / "02_balanced_pipeline_scaling.jpg"))

    fig, axes = plt.subplots(1, 3, figsize=(16.4, 5.2))
    fig.subplots_adjust(left=.12, right=.975, top=.77, bottom=.18, wspace=.65)
    profiles = [
        ("Unbounded enqueue", "UNBOUNDED-ENQUEUE-X-TASKS-1-THREAD", "UNBOUNDED-ENQUEUE-1T", list(QCOL)),
        ("Unbounded prefilled dequeue", "UNBOUNDED-DEQUEUE-X-TASKS-1-THREAD", "UNBOUNDED-DEQUEUE-1T", list(QCOL)),
        ("Bounded enqueue", "BOUNDED-ENQUEUE-X-TASKS-1-THREAD", "BOUNDED-ENQUEUE-1T", ["coalton-io", "cl-freelock", "lparallel"]),
    ]
    for ax, (title, name, suffix, libs) in zip(axes, profiles):
        values = [point(lib, name, suffix) for lib in libs]
        ax.barh(np.arange(len(libs)), values, color=[QCOL[lib] for lib in libs], height=.55)
        ax.set_yticks(np.arange(len(libs)), libs)
        ax.invert_yaxis()
        ax.set_xlim(0, max(values) * 1.25)
        for row, value in enumerate(values):
            ax.text(value + max(values) * .03, row, f"{value:.2f}", va="center", fontsize=11)
        ax.set_title(title, fontsize=13, pad=16)
        ax.set_xlabel("Million items/s")
        ax.grid(axis="x")
        ax.set_axisbelow(True)
    fig.suptitle("Queues: single-thread throughput", fontsize=22, fontweight="bold", y=.96)
    fig.text(.085, .035, "Bounded capacity: coalton-io 48,000; cl-freelock and lparallel 65,536.", fontsize=10, color="#526276")
    files.append(save(fig, output / "03_single_thread_comparisons.jpg"))
    require(used == set(native.index), f"Uncharted comparison queue workloads: {sorted(set(native.index) - used)}")
    return files


def scheduler_table(native, io):
    rows = []
    for name, record in native.iterrows():
        match = re.fullmatch(r"(LPARALLEL|COALTON)-SCHEDULER-(\d+)-PRODUCERS?-(\d+)-WORKERS?(-OPTIM)?", name)
        require(match is not None, f"Unknown comparison scheduler workload: {name}")
        base, producers, workers, optim = match.groups()
        variant = ("lp" if base == "LPARALLEL" else "coalton") + ("_raw" if optim else "_public")
        rows.append((variant, int(producers), int(workers), name, float(record["average"])))
    table = pd.DataFrame(rows, columns=["variant", "producers", "workers", "name", "seconds"])
    expected = {(v, p, w) for v, *_ in VARIANTS if not v.startswith("io_") for p, w in CONFIGS}
    actual = set(zip(table.variant, table.producers, table.workers))
    require(actual == expected and len(table) == len(expected),
            f"Comparison scheduler matrix differs from the original: missing={sorted(expected - actual)}, extra={sorted(actual - expected)}")
    for kind in ["unbounded", "bounded"]:
        for producers, workers in CONFIGS:
            if producers == 1:
                name = f"SINGLE-PRODUCER-{workers}-CONSUMER-X-TASKS-{kind.upper()}-SCHEDULER"
            else:
                name = f"{producers}-PRODUCERS-{workers}-CONSUMERS-X-TASKS-{kind.upper()}-SCHEDULER"
            require(name in io.index, f"Missing coalton-io scheduler workload: {name}")
            table.loc[len(table)] = ["io_" + kind, producers, workers, name, float(io.loc[name])]
    table["throughput"] = SCHEDULER_TASKS / table.seconds / 1e6
    check_range(table.throughput, 14, "Schedulers")
    return table.set_index(["variant", "producers", "workers"])


def scheduler_charts(table, output):
    used = set()
    files = []

    def series(ax, sequence, xs):
        for variant, label, color, style, marker in VARIANTS:
            values = []
            for producers, workers in sequence:
                key = (variant, producers, workers)
                values.append(table.loc[key, "throughput"])
                used.add(key)
            ax.plot(xs, values, label=label, color=color, ls=style, marker=marker, lw=2.2, ms=6)
        ax.set_ylim(0, 14)
        ax.set_xticks(xs)
        ax.set_ylabel("Million tasks/s")
        ax.grid(axis="y")
        ax.set_axisbelow(True)

    def legend(fig, ax, y=.03):
        fig.legend(*ax.get_legend_handles_labels(), loc="lower center", bbox_to_anchor=(.5, y),
                   ncol=3, frameon=False, fontsize=11.5, handlelength=2.3, columnspacing=2)

    fig, axes = plt.subplots(2, 2, figsize=(13.8, 8.3))
    fig.subplots_adjust(left=.08, right=.98, top=.84, bottom=.20, hspace=.52, wspace=.28)
    for ax, producers in zip(axes.flat, [1, 2, 4, 6]):
        workers = [w for p, w in CONFIGS if p == producers]
        series(ax, [(producers, w) for w in workers], workers)
        ax.set_title(f"{producers} " + ("producer" if producers == 1 else "producers"), fontsize=14, pad=12)
        ax.set_xlabel("Worker threads")
    fig.suptitle("Schedulers: worker scaling with producers fixed", fontsize=22, fontweight="bold", y=.97)
    legend(fig, axes.flat[0])
    files.append(save(fig, output / "01_worker_scaling_fixed_producers.jpg"))

    fig, axes = plt.subplots(1, 3, figsize=(16.4, 5.9))
    fig.subplots_adjust(left=.065, right=.985, top=.78, bottom=.28, wspace=.26)
    for ax, workers in zip(axes, [2, 4, 6]):
        producers = [p for p, w in CONFIGS if w == workers]
        series(ax, [(p, workers) for p in producers], producers)
        ax.set_title(f"{workers} workers", fontsize=14, pad=14)
        ax.set_xlabel("Producer threads")
    fig.suptitle("Schedulers: producer scaling with workers fixed", fontsize=22, fontweight="bold", y=.97)
    legend(fig, axes[0], .035)
    files.append(save(fig, output / "02_producer_scaling_fixed_workers.jpg"))

    fig, ax = plt.subplots(figsize=(13.2, 6.6))
    fig.subplots_adjust(left=.085, right=.98, top=.83, bottom=.25)
    sequence = [(p, w) for p, w in CONFIGS if p == w]
    series(ax, sequence, [p for p, w in sequence])
    ax.set_xlabel("Producers / workers")
    ax.set_xticklabels([f"{p} / {w}" for p, w in sequence])
    fig.suptitle("Schedulers: balanced producer / worker scaling", fontsize=22, fontweight="bold", y=.97)
    legend(fig, ax)
    files.append(save(fig, output / "03_balanced_producer_worker_scaling.jpg"))
    require(used == set(table.index), "Not all scheduler measurements were plotted")
    return files


def main():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--io-runs", help="Archive or single-run directory, relative to benchmarks/ or absolute; default: newest archive")
    parser.add_argument("--output", type=Path, help="Output directory; default: benchmarks/comparative_throughput/")
    args = parser.parse_args()
    benchmarks = Path(__file__).resolve().parent
    comparison = benchmarks.parent / "benchmark-comparisons"
    output = args.output.resolve() if args.output else benchmarks / "comparative_throughput"
    try:
        native_queues = recorded(comparison / "benchmark_queue_comparison.csv")
        native_schedulers = recorded(comparison / "benchmark_scheduler_comparison.csv")
        root, runs = run_directories(benchmarks, args.io_runs)
        queues = pooled_means(runs, IO_FILES[0])
        schedulers = pooled_means(runs, IO_FILES[1])
        table = scheduler_table(native_schedulers, schedulers)
        configure_style()
        files = queue_charts(native_queues, queues, output / "queues")
        files += scheduler_charts(table, output / "schedulers")
    except (OSError, ValueError, KeyError) as error:
        parser.exit(1, f"Error: {error}\n")
    print(f"coalton-io: {root} ({len(runs)} run(s), sample-weighted means)")
    print(f"Comparison CSVs: {comparison}")
    print(f"Plotted all {len(native_queues)} queue and {len(native_schedulers)} scheduler comparison workloads.")
    for path in files:
        print(path)


if __name__ == "__main__":
    main()
