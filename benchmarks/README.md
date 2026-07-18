# Benchmarks

The benchmark harness measures one retained 27-task suite across zigr, Rcpp, cpp11, extendr, Savvy, the R baseline, and registered C. Every runner is checked against R and C truth before timing. Package fixtures are runner build and loading details, not a second benchmark suite.

## Frozen comparison scope

The current product comparison set is zigr, Rcpp, cpp11, extendr, and Savvy. Pure R and optimized base R are separate task-level baseline roles. The current registry has no standalone pure-R runner. A row is `pure_r` only when its declared kernel is implemented explicitly in R source. A row is `optimized_base_r` when its R implementation delegates the kernel to a compiled base-R, BLAS, LAPACK, Fortran, or runtime-backed operation. The `r` runner is the owner of those R baseline rows; the runner name alone does not decide the row role.

`c_call` is the registered handwritten C control. It is included only for tasks with a matching registered `c_revision_<task>` entry and never counts as a product win. A task without an applicable control remains in the suite with an explicit non-applicability disposition.

The timed event excludes input construction, correctness comparison, timers, and result retention. Each timing sample is one timer interval around the declared fixed-count event batch. The batch count is selected once per task across all runners from the declared 1, 8, 64 ladder. Tasks with state, RNG, or ALTREP representation changes remain at one event per batch.

## Commands

Run these commands from the `benchmarks/` directory; the harness resolves its
source, fixture, and artifact paths relative to that working directory.

```sh
bash build_all.sh
Rscript tests/test_measurement.R
Rscript tests/test_specification.R
Rscript check_coverage.R
Rscript run_benchmarks.R --runners=r,c_call,zigr --tasks=vector_sum
Rscript run_benchmarks.R --tasks=attributes --memory-task=attributes
```

`--correctness-only` writes only `run_manifest.json` and `correctness.csv`. Timed runs publish only:

1. `run_manifest.json`
2. `correctness.csv`
3. `timing_samples.csv`
4. `timing_summary.csv`
5. `memory_summary.csv` when `--memory-task` is selected

The manifest seals source-tree and built-artifact identities, selected task seeds, the timing policy, and output digests. A changed source tree, artifact, raw sample, or summary cannot be accepted as the same run.

`results/CANONICAL_RUN.json` is a migrated receipt from the superseded 83-task promotion system. It is retained for historical provenance only. The current harness does not read it, and its source identity, task/report counts, promotion metadata, and acceptance fields cannot be used as current benchmark evidence. Current acceptance requires the schema 4 `direct-v1` manifest and artifacts produced by the current run.

`memory_summary.csv` is a separate, one-event fresh-process safety measurement for one selected large-output task. On Linux it records loaded `VmRSS`, pre-event and post-event `VmHWM`, and swap. Its high-water difference is a process-growth cap, not an allocation claim.

The harness reports medians and distribution diagnostics for each runner and task.
It does not assign product tiers, aggregate winners, or comparative report tracks.
