# Benchmarks

The benchmark harness measures one retained 27-task suite across zigr, Rcpp, cpp11, extendr, Savvy, the R baseline, and registered C. Every runner is checked against R and C truth before timing. Package fixtures are runner build and loading details, not a second benchmark suite.

## Comparison scope

The product comparison set is zigr, Rcpp, cpp11, extendr, and Savvy. Pure R and optimized base R are separate task-level baseline roles. The registry has no standalone pure-R runner. A row is `pure_r` only when its declared kernel is implemented explicitly in R source. A row is `optimized_base_r` when its R implementation delegates the kernel to a compiled base-R, BLAS, LAPACK, Fortran, or runtime-backed operation. The `r` runner supplies those R baseline rows; the runner name alone does not decide the row role.

`c_call` is the registered handwritten C control. It is included only for tasks with a matching registered `c_revision_<task>` entry and never counts as a product win. A task without an applicable control remains in the suite with an explicit non-applicability disposition.

Reduction fixtures must match R's ordered extended-precision result. The C and C++ fixtures implement that algorithm directly. The fixture Rust toolchains do not expose a matching extended-precision scalar, so Extendr and Savvy use their public R-call interfaces; those rows include the delegated base-R kernel.

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
5. `cost_account.csv`
6. `memory_summary.csv` when `--memory-task` is selected

The manifest seals source-tree and built-artifact identities, selected task seeds, the timing policy, and output digests. A changed source tree, artifact, raw sample, or summary cannot be accepted as the same run.

`cost_account.csv` records the exact attributable work for the fixed revision inputs: logical input elements and passes; fixture-level borrowed, materialized, copied, and written bytes; native allocation requests; known R payload sizes; and list-slot reads. The byte columns describe separate transfers, not one additive total. `input_elements` is the input cardinality, so the RNG row records its scalar count argument while its output size appears in the R payload columns. `input_passes` counts complete fixture-level traversals and excludes work inside opaque R calls. Native request sizes use the current x86_64 fixture ABI. R headers, allocator rounding, metadata allocations, and work hidden inside opaque R serialization or attribute calls are marked instead of being presented as measured totals.

`results/CANONICAL_RUN.json` is an old receipt that the harness does not read. Do not use it for a current comparison. Use the schema 4 `direct-v1` manifest and artifacts produced by the run.

`memory_summary.csv` is a separate, one-event fresh-process safety measurement for one selected large-output task. On Linux it records loaded `VmRSS`, pre-event and post-event `VmHWM`, and swap. Its high-water difference is a process-growth cap, not an allocation claim.

The harness reports medians and distribution diagnostics for each runner and task.
It does not assign product tiers, aggregate winners, or comparative report tracks.
