<!-- markdownlint-disable MD024 -->

# zigr Benchmark Harness

Six runner backends. The current published baseline is `p0-7-20260710-full`, referenced by `results/CANONICAL_RUN.json`. The canonical P0 comparison remains 44 tasks; P1.3 adds 26 focused generated-boundary rows and P1.7 adds 11 representation diagnostics without changing that aggregate. extendr uses raw FFI wrappers for 4 tasks (matmul, struct_convert, external_ptr, rng_stress) where extendr's `#[extendr]` Robj wrapper caused Rust double-panics.

- **r** (R baseline): 81 manifest task references in `src/r/run_all.R`
- **zigr** (Zig): 44 direct task files plus registered P1.3 and P1.7 fixtures under `src/zig/`, built via `build.zig`
- **c_call** (C): 44 task files plus P1.3 fixtures in `register.c` under `src/c_call/`
- **rcpp** (C++): Single `main.cpp` under `src/cpp/`
- **savvy** (Rust): `rust/src/lib.rs` + `init.c` under `src/savvy/`
- **extendr** (Rust): `rust/src/lib.rs` + `entrypoint.c` under `src/extendr/`, with raw FFI entrypoints for 4 tasks

## Task matrix

81 manifest rows: 44 P0 tasks (`01` through `43`, plus `07a`/`07b` splitting original task 07), 26 P1.3 boundary rows (`50` through `75`), and 11 P1.7 representation diagnostics (`76` through `86`).

- **Layer 1** (tasks 01-06): vector sum, element-wise ops, memory bandwidth, sort, recursion, broadcast
- **Layer 2** (tasks 07a, 07b, 08-11): PROTECT shallow/scaling, type dispatch, longjmp, SEXP create/inspect
- **Layer 3** (tasks 12-24): matrix ops, data frame filter, list access, string ops, factor ops, attributes, S4 slots, NA propagation, long vector indexing
- **Layer 4** (tasks 25-29): L1 arithmetic, matmul, crossprod, Cholesky, linear model
- **Layer 5** (tasks 30-37): ALTREP create, materialize, element walk, region read, sum (R and native), min/max, no-NA query
- **Layer 6** (tasks 38-41, 43): struct convert, R eval, try eval, serialize roundtrip, RNG stress
- **Layer 6 extra** (task 42): external pointer (non-deterministic, H.2 skipped)
- **Layer 7** (tasks 44-47): system diagnostics (build time, binary size, cross-compile time, memory allocation counts) (zigr only)
- **P1.3 boundary pairs** (tasks 50-75): generated/handwritten zero-arg, scalar, optional, materialized numeric, ALTREP conversion diagnostic, string, raw, complex, fixed-schema named-list validation, external-pointer method, and `.External` fixtures. These are `api_overhead` and `non_comparable`; generated and handwritten variants are separated in `analysis_summary.csv`.
- **P1.7 representation diagnostics** (tasks 76-86): one-pass and declared four-pass `StringSliceView`, cached metadata, and allocated headers; cached metadata construction alone; borrowed and copied RAWSXP; borrowed complex input; and an R-owned complex return. The string input is an ordinary ASCII STRSXP so timing isolates representation rather than R encoding translation. These are `api_overhead` and `non_comparable` because they compare distinct ownership representations rather than substitute implementations.

The P1.3 fixtures are implemented for `r`, `c_call`, and `zigr`. The P1.7 diagnostics are implemented for `r` and `zigr`. C, Rcpp, extendr, and Savvy declare the P1.7 rows as optional until their equivalent representation contracts are owned by later P1 work; a full run records those rows as explicit `N/A` rather than timing a mismatched substitute.

The materialized numeric rows use ordinary REALSXPs. The integer ALTREP rows deliberately compare zigr's contiguous owned-copy conversion with a handwritten region-stream implementation, so they are a P1.5 conversion-policy diagnostic rather than wrapper-overhead evidence. The schema rows validate a fixed named-list through `SEXP`; generated typed struct conversion is P1.8. The generated method row invokes `generateMethods` on a valid `.Call` receiver; generated tag, foreign-pointer, finalizer, and lifetime guarantees remain P1.9 work.

`task_manifest.csv` is the canonical task-policy source. It owns stable task IDs, layers, display names, workload categories, expected result contracts, correctness policy, comparability, aggregate membership, and exclusion notes. The executable argument closures remain in `runner_subprocess.R` as task specs selected by manifest ID; the `input_factory` value `task_spec.args` names that adapter boundary without duplicating R code in CSV.

The manifest distinguishes `r_reference`, `native_invariant`, and `nondeterministic` correctness policies. It marks API-overhead and nondeterministic tasks as `non_comparable` for the primary aggregate while retaining them in task-level reports. Runner configurations and task specs must match the manifest before timing starts. Run `Rscript check_coverage.R` for the focused preflight; `run_benchmarks.R` runs it automatically before any runner.

Runner JSONs in `runners/` map task IDs to exported symbols. Input generators live in `runner_subprocess.R`. They are shared across all runners.

## Correctness validation (H.2)

Before timing, every native task is validated against the R baseline or its manifest result contract. The R reference gets the same arguments. The return value comparison checks type, attributes, recursive list structure, numeric tolerance, exact missing-value kind (NA versus NaN), and character encoding. Reference errors, native errors, missing references, contract mismatches, and value mismatches stop timing for that task.

Each summary and raw timing file records `correctness_status`, `correctness_policy`, `correctness_message`, and the effective `call_type`; `.Call` and `.External` rows are therefore not conflated. `PASS` permits timing, `REFERENCE` identifies the R baseline runner, and `NOT_VALIDATED` is never eligible for comparative aggregation. A correctness or timing `FAIL` causes the runner subprocess and parent run to fail; it cannot become a complete, exportable, or promotable run. `N/A` is rejected unless explicitly allowed by the run manifest. Native-only and nondeterministic tasks use structural result contracts from `task_manifest.csv`.

Eight tasks skip H.2. Layer 2 (07a through 11) uses C API idioms that R cannot express. PROTECT, longjmp, SEXP create/inspect use R stubs returning `0L`. Non-deterministic tasks (42 external pointer, 43 RNG stress) differ on every call.

## Pipeline

```sh
# Build all native runners
bash build_all.sh

# Run all configured runners into a unique results/runs/<run_id> directory
Rscript run_benchmarks.R

# Quick subset
Rscript run_benchmarks.R --runners=zigr,c_call
Rscript run_benchmarks.R --tasks=1,2,6
Rscript run_benchmarks.R --runners=r,c_call,zigr --tasks=50,51,52,53
Rscript run_benchmarks.R --build    # rebuild then run

# Promote a completed full-matrix run explicitly
Rscript promote_run.R --run-dir=results/runs/<run_id>

# Summarize one completed run
Rscript analysis/summarize.R --run-dir=results/runs/<run_id>

# Run system diagnostics (zigr-only)
Rscript run_system_tasks.R
```

`run_benchmarks.R` creates a run manifest before execution, spawns `runner_subprocess.R` per runner with that run directory, validates coverage, marks the run complete, and exports comparisons from that one directory. Failed or interrupted runs remain `incomplete` and cannot be exported or promoted; a later project run marks `running` manifests older than six hours as incomplete while retaining their artifacts. Existing root-level CSVs are legacy evidence.

Each run manifest records a source-tree digest, host and CPU identity, R and Zig versions, declared target/optimization/CPU features, BLAS details and thread settings, locale, relevant environment variables, runner configuration, shared-library fingerprints, and the explicitly allowed `N/A` task set. Set `ZIGR_TARGET`, `ZIGR_OPTIMIZE`, and `ZIGR_CPU_FEATURES` when the build differs from the native `ReleaseFast` defaults.

## Output files

The pipeline writes per-task timing CSVs, per-runner summaries, a run manifest, and cross-runner comparisons under one run directory. Comparative export also writes `task_comparisons.csv` with noise and median-interval fields plus `category_metrics.csv`. `analysis/summarize.R` writes `analysis_summary.csv` beside the selected run.

## Results (canonical baseline)

Run `p0-7-20260710-full` completed on 2026-07-10 with `ReleaseFast`, native target, Zig 0.16.0, R 4.6.1, and single-threaded OpenBLAS. All six runners passed all 44 tasks: `264 PASS, 0 FAIL, 0 N/A`. The primary aggregate contains 36 manifest-approved tasks; eight strategy-sensitive or nondeterministic tasks remain visible in task reports but are excluded from aggregate ratios.

### Comparative metrics

Ratios are runner median divided by the reference median; lower is better. Aggregate ratios use only the 36 manifest-approved tasks.

| Runner  | Tasks won | Median vs R | Geomedian vs R | Median vs best native | Geomedian vs best native | Meaningful native wins | Low-noise wins |
| ------- | --------- | ----------- | -------------- | --------------------- | ------------------------ | ---------------------- | -------------- |
| r       | 5         | 1.000       | 1.000          | NA                    | NA                       | NA                     | NA             |
| c_call  | 1         | 0.398       | 0.299          | 1.125                 | 1.525                    | 0                      | 0              |
| rcpp    | 1         | 0.397       | 0.303          | 1.151                 | 1.548                    | 1                      | 1              |
| extendr | 3         | 0.389       | 0.321          | 1.212                 | 1.638                    | 2                      | 1              |
| savvy   | 9         | 0.388       | 0.283          | 1.100                 | 1.445                    | 0                      | 7              |
| zigr    | 17        | 0.263       | 0.212          | 1.003                 | 1.082                    | 16                     | 10             |

`Tasks won` includes exact median ties. The low-noise set contains 19 aggregate tasks with CV <= 20%. zigr's aggregate result is approximately 3.8x faster than R by median and 4.7x by geomedian, while remaining 1.003x slower by median and 1.082x slower by geomedian than the per-task best native runner.

### Category results for zigr

| Category       | Tasks | Aggregate tasks | Low-noise aggregate tasks | Median vs R | Geomedian vs R | Median vs best native | Geomedian vs best native |
| ------------- | -----: | --------------: | --------------: | -----------: | --------------: | ---------------------: | ------------------------: |
| kernels       | 11    | 11              | 8               | 0.297        | 0.222           | 1.110                  | 1.110                     |
| boundary      | 13    | 13              | 3               | 0.166        | 0.108           | 1.043                  | 1.043                     |
| altrep        | 8     | 8               | 6               | 1.232        | 0.519           | 1.149                  | 1.116                     |
| r_runtime     | 6     | 4               | 2               | 0.975        | 0.805           | 1.010                  | 1.065                     |
| synthetic_api | 6     | 0               | 0               | NA           | NA              | NA                     | NA                        |

### Task-level wins and parity

zigr is the fastest native runner on 17 aggregate tasks: `01_vectorsum`, `04_sort`, `06_broadcast`, `13_matrix_rowsums`, `14_matrix_rowcol_means`, `15_dataframe_filter`, `16_list_access`, `17_string_concat`, `18_string_nchar`, `19_string_encoding`, `20_factor_ops`, `22_s4_slot_access`, `23_na_propagation`, `25_l1_arithmetic`, `26_matmul`, `29_lm_fit`, and `33_altrep_region_read`.

### Material losses and low-noise review

| Task                  | Best native | zigr vs best | Signal |
| --------------------- | ----------- | -----------: | ------ |
| 03_memcpy_bandwidth   | extendr     | 1.868x       | noisy, CV 42.92% |
| 21_attrib_ops         | savvy       | 1.429x       | below timer floor, CV 151.69% |
| 05_fib_recursive      | rcpp        | 1.424x       | low-noise, CV 6.69% |
| 38_struct_convert     | extendr     | 1.258x       | below timer floor, CV 147.91% |
| 30_altrep_create      | savvy       | 1.207x       | below timer floor, CV 156.81% |
| 34_altrep_sum_via_R   | c_call      | 1.182x       | below timer floor, CV 133.75% |
| 32_altrep_elt_walk    | extendr     | 1.156x       | low-noise, CV 3.27% |
| 36_altrep_min_max     | savvy       | 1.150x       | low-noise, CV 5.21% |
| 35_altrep_sum_native  | savvy       | 1.149x       | low-noise, CV 3.84% |
| 02_elem_ops           | c_call      | 1.148x       | noisy, CV 35.92% |
| 24_long_vector_idx    | savvy       | 1.145x       | noisy/below floor, CV 60.29% |
| 37_altrep_no_na_query | savvy       | 1.101x       | low-noise, CV 9.06% |

The remaining low-noise losses are near parity: `39_r_eval` (1.001x), `31_altrep_materialize` (1.005x), and `28_cholesky` (1.007x). `09_longjmp_safety` and `43_rng_stress` are also low-noise task-level losses but are excluded because their strategies or outputs are not directly comparable. The largest non-comparable outlier is `07b_protect_scaling`; it measures a 100k-element protection loop, where zigr performs per-element protection while savvy batches protection work. This is a binding-design gap, not a timing artifact.

### Exclusions and skipped correctness comparisons

| Tasks | Policy | Reason |
| ----- | ------ | ------ |
| 07a, 07b | native invariant | R API protection loops have no shared R semantic reference |
| 08, 10, 11 | native invariant | Direct SEXP inspection/allocation has no shared R semantic reference |
| 09 | native invariant | Native error and unwind strategies differ |
| 42 | native invariant | Pointer identity and finalization are not shared R values |
| 43 | nondeterministic | Random output is not compared by value |

These tasks still passed their declared structural or native-invariant correctness contracts and remain available in `task_comparisons.csv`; they simply do not define the primary aggregate.

### System diagnostics (L7)

Layer 7 tasks are zigr-only, not comparative. They measure build and memory characteristics of the zigr backend. Run via `run_system_tasks.R`.

The table below is retained historical Layer 7 evidence; P0.7's canonical run covers the 44-task comparative matrix and does not rerun these four standalone diagnostics.

| Task                  | Metric                          | Result                |
| --------------------- | ------------------------------- | --------------------- |
| 44_build_time         | cold build (clean cache)        | 2.41s                 |
| 44_build_time         | warm build (no changes)         | 0.08s                 |
| 44_build_time         | incremental (one file modified) | 0.86s                 |
| 45_binary_size        | x86_64-linux .so                | 179 KB                |
| 45_binary_size        | aarch64-linux .so               | N/A (link fails)      |
| 45_binary_size        | x86_64-windows .dll             | N/A (link fails)      |
| 46_cross_compile_time | x86_64-linux (warm from t45)    | 1.04s                 |
| 46_cross_compile_time | aarch64-linux (cold)            | 0.42s                 |
| 46_cross_compile_time | x86_64-windows (cold)           | 0.06s (lib not found) |
| 46_cross_compile_time | aarch64-macos (cold)            | 0.13s (LTO needs LLD) |
| 47_mem_alloc_count    | vectorsum (10M)                 | 1 alloc, 80 MB        |

Cross-target .so sizes are N/A because R and BLAS shared libraries on this host are x86_64 binaries, incompatible with foreign targets. zigr's compilation step succeeds for aarch64-linux (the translate-c output is target-independent); linking fails because native `.so` files cannot be linked into cross-target output. A full cross-compilation pipeline would require R headers and libraries for each target.

Task 47 demonstrates zigr's custom allocator instrumentation: wrapping any operation with a `CountingAllocator` that intercepts every alloc/free call. Results are for a standalone Zig binary, no R runtime needed. Only `c_allocator` calls are counted (the Zig-level interface to libc `malloc`/`free`); R API allocations (`R_chk_calloc`, `allocVector3`) are not instrumented because they require an active R session.

### Cross-compilation status for CI

Zig can cross-compile the zigr source for any LLVM target from any host. The translate-c step produces target-independent output from the R headers. The practical limitation is linking: the shared library must link against `libR.so` (or `.dylib`/`.dll`) for the target platform, which requires either:

- A native build on each target (R installed), or
- Target-specific R libraries available on the build host

A GitHub Actions workflow building zigr on Linux (x86_64), macOS (aarch64), and Windows (x86_64) is feasible: each runner installs R via its package manager, then runs `zig build`. Cross-compilation from a single Linux host can verify compilation for all targets but cannot produce loadable `.so` files without target-specific R libraries.

## Methodology

- Timing: 10 warm-up iterations, then `microbenchmark` blocks of 10. The runner stops when CV over the last 5 blocks falls below 1%, or at 500 measured iterations; summaries record the warmups, block size, measured sample count (`n_iterations`), stopping condition, and stopping-window CV.
- Timer floor: medians below the fixed 0.01 ms floor are labeled `below_floor`; the floor is a reporting label and does not force extra repeats.
- RSS: post-GC VmRSS endpoint delta (max 0, after - before), read from `/proc/self/status VmRSS`; it is not a peak-memory measurement.
- Cold start: one post-correctness call before warm-up, logged separately with the run ID; it is a call cold-start measure, not process launch time.
- GC control: full GC before warm-up and both RSS endpoints; no forced GC occurs between timed samples.
- Allocation: Task 47's standalone allocator counts remain a separate diagnostic; RSS is never used as an allocation or timing substitute.
- BLAS: single-threaded (`OPENBLAS_NUM_THREADS=1`)
- Low-noise threshold: CV <= 20% across all runners
- Meaningful margin: gap must exceed 5% (ratio > 1.05)
- Confidence intervals: exact 95% order-statistic intervals for per-task medians, computed from existing samples without rerunning tasks; task-level ratio bounds use those intervals, while aggregate rankings remain descriptive over the fixed manifest task set.
- Category reports: `category_metrics.csv` groups results into kernels, boundary, ALTREP, R runtime, and synthetic API categories.
