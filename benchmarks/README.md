# Benchmarks

The benchmark harness measures one retained 27-task suite across R, registered C,
zigr, Rcpp, cpp11, extendr, and Savvy. Every runner is checked against R and C
truth before timing. Package fixtures are runner build and loading details, not a
second benchmark suite.

The timed event excludes input construction, correctness comparison, timers, and
result retention. Each timing sample is one timer interval around the declared
fixed-count event batch. The batch count is selected once per task across all
runners from the declared 1, 8, 64 ladder. Tasks with state, RNG, or ALTREP
representation changes remain at one event per batch.

## Commands

```sh
bash build_all.sh
Rscript tests/test_measurement.R
Rscript tests/test_specification.R
Rscript check_coverage.R
Rscript run_benchmarks.R --runners=r,c_call,zigr --tasks=vector_sum
Rscript run_benchmarks.R --tasks=attributes --memory-task=attributes
```

`--correctness-only` writes only `run_manifest.json` and `correctness.csv`.
Timed runs publish only:

1. `run_manifest.json`
2. `correctness.csv`
3. `timing_samples.csv`
4. `timing_summary.csv`
5. `memory_summary.csv` when `--memory-task` is selected

The manifest seals source-tree and built-artifact identities, selected task seeds,
the timing policy, and output digests. A changed source tree, artifact, raw sample,
or summary cannot be accepted as the same run.

`memory_summary.csv` is a separate, one-event fresh-process safety measurement for
one selected large-output task. On Linux it records loaded `VmRSS`, pre-event and
post-event `VmHWM`, and swap. Its high-water difference is a process-growth cap,
not an allocation claim.

The harness reports medians and distribution diagnostics for each runner and task.
It does not assign product tiers, aggregate winners, or comparative report tracks.
