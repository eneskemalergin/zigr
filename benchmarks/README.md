<!-- markdownlint-disable MD024 -->

# zigr Benchmark Harness

Six runner backends. All 44 tasks pass across all runners. extendr uses raw FFI wrappers for 4 tasks (matmul, struct_convert, external_ptr, rng_stress) where extendr's `#[extendr]` Robj wrapper caused Rust double-panics.

- **r** (R baseline): 44 task implementations in `src/r/run_all.R`
- **zigr** (Zig): 44 `task_*.zig` under `src/zig/`, built via `build.zig`
- **c_call** (C): 44 `task_*.c` + `register.c` under `src/c_call/`
- **rcpp** (C++): Single `main.cpp` under `src/cpp/`
- **savvy** (Rust): `rust/src/lib.rs` + `init.c` under `src/savvy/`
- **extendr** (Rust): `rust/src/lib.rs` + `entrypoint.c` under `src/extendr/`, with raw FFI entrypoints for 4 tasks

## Task matrix

44 tasks (`01` through `43`, plus `07a`/`07b` splitting original task 07).

- **Layer 1** (tasks 01-06): vector sum, element-wise ops, memory bandwidth, sort, recursion, broadcast
- **Layer 2** (tasks 07a, 07b, 08-11): PROTECT shallow/scaling, type dispatch, longjmp, SEXP create/inspect
- **Layer 3** (tasks 12-24): matrix ops, data frame filter, list access, string ops, factor ops, attributes, S4 slots, NA propagation, long vector indexing
- **Layer 4** (tasks 25-29): L1 arithmetic, matmul, crossprod, Cholesky, linear model
- **Layer 5** (tasks 30-37): ALTREP create, materialize, element walk, region read, sum (R and native), min/max, no-NA query
- **Layer 6** (tasks 38-41, 43): struct convert, R eval, try eval, serialize roundtrip, RNG stress
- **Layer 6 extra** (task 42): external pointer (non-deterministic, H.2 skipped)
- **Layer 7** (tasks 44-47): system diagnostics (build time, binary size, cross-compile time, memory allocation counts) (zigr only)

`task_manifest.csv` is the canonical task-policy source. It owns stable task IDs, layers, display names, workload categories, expected result contracts, correctness policy, comparability, aggregate membership, and exclusion notes. The executable argument closures remain in `runner_subprocess.R` as task specs selected by manifest ID; the `input_factory` value `task_spec.args` names that adapter boundary without duplicating R code in CSV.

The manifest distinguishes `r_reference`, `native_invariant`, and `nondeterministic` correctness policies. It marks API-overhead and nondeterministic tasks as `non_comparable` for the primary aggregate while retaining them in task-level reports. Runner configurations and task specs must match the manifest before timing starts.

Runner JSONs in `runners/` map task IDs to exported symbols. Input generators live in `runner_subprocess.R`. They are shared across all runners.

## Correctness validation (H.2)

Before timing, every native task is validated against the R baseline. The R reference gets the same arguments. The return value is compared per type: numeric uses relative tolerance (`sqrt(.Machine$double.eps)`), integer/logical/character uses exact `identical`, lists recurse element-wise.

Eight tasks skip H.2. Layer 2 (07a through 11) uses C API idioms that R cannot express. PROTECT, longjmp, SEXP create/inspect use R stubs returning `0L`. Non-deterministic tasks (42 external pointer, 43 RNG stress) differ on every call.

## Pipeline

```sh
# Build all native runners
bash build_all.sh

# Run all configured runners
Rscript run_benchmarks.R

# Quick subset
Rscript run_benchmarks.R --runners=zigr,c_call
Rscript run_benchmarks.R --tasks=1,2,6
Rscript run_benchmarks.R --build    # rebuild then run

# Run system diagnostics (zigr-only)
Rscript run_system_tasks.R
```

`run_benchmarks.R` iterates over runner JSONs, spawns `runner_subprocess.R` per runner, then runs `export_comparative_metrics.R` to produce cross-runner comparisons.

## Output files

The pipeline writes per-task timing CSVs, per-runner summaries, and cross-runner comparisons. `analysis/summarize.R` regenerates the per-task summary from the raw data.

## Results (latest run)

All 6 runners pass all 44 tasks. extendr required raw FFI workarounds for 4 tasks where the `#[extendr]` Robj wrapper triggered Rust double-panics.

### Comparative metrics

| Runner  | Tasks won | Geomedian vs R | Geomedian vs best native | Meaningful native wins | Low-noise wins |
| ------- | --------- | -------------- | ------------------------ | ---------------------- | -------------- |
| r       | 8         | 1.000          | NA                       | NA                     | NA             |
| c_call  | 1         | 0.338          | 2.072                    | 0                      | 0              |
| rcpp    | 2         | 0.346          | 2.123                    | 0                      | 1              |
| extendr | 1         | 0.360          | 2.207                    | 1                      | 0              |
| savvy   | 13        | 0.249          | 1.530                    | 8                      | 7              |
| zigr    | 19        | 0.224          | 1.373                    | 18                     | 11             |

Column notes:

- **Tasks won**: tasks where the runner tied for the lowest median (including exact ties)
- **Geomedian vs R**: geometric mean of `runner_median / r_median` across all tasks. Lower is better. 0.231 means zigr is 1 / 0.231 = 4.3x faster than R
- **Geomedian vs best native**: same ratio vs the fastest native runner per task. 1.398 means zigr averages 1.4x slower than the per-task best
- **Meaningful native wins**: tasks where the runner won by more than 5% (the noise floor)
- **Low-noise wins**: tasks won within the low-noise subset (CV <= 20%), where comparisons are most reliable

zigr leads overall: 19 of 44 tasks won, 4.5x faster than R, 1.4x slower than the best native on average. savvy holds second place (13 wins), especially on ALTREP tasks and BLAS wrappers. R wins 8 tasks (Layer 2 stubs and a few it matches at).

### Task-level highlights (zigr wins)

| Task                   | median (ms) | Best native runner | zigr vs best |
| ---------------------- | ----------- | ------------------ | ------------ |
| 01_vectorsum           | 4.32        | zigr               | 1.000        |
| 04_sort                | 33.17       | zigr               | 1.000        |
| 06_broadcast           | 4.09        | zigr               | 1.000        |
| 08_type_dispatch       | 0.01        | zigr               | 1.000        |
| 11_sexp_inspect        | 0.04        | zigr               | 1.000        |
| 13_matrix_rowsums      | 0.08        | zigr               | 1.000        |
| 14_matrix_rowcol_means | 0.10        | zigr               | 1.000        |
| 15_dataframe_filter    | 0.51        | zigr               | 1.000        |
| 16_list_access         | 0.01        | zigr               | 1.000        |
| 17_string_concat       | 0.42        | zigr               | 1.000        |
| 18_string_nchar        | 0.02        | zigr               | 1.000        |
| 19_string_encoding     | 0.03        | zigr               | 1.000        |
| 20_factor_ops          | 0.12        | zigr               | 1.000        |
| 22_s4_slot_access      | 0.01        | zigr               | 1.000        |
| 23_na_propagation      | 0.14        | zigr               | 1.000        |
| 25_l1_arithmetic       | 1.06        | zigr               | 1.000        |
| 26_matmul              | 0.73        | zigr               | 1.000        |
| 29_lm_fit              | 0.21        | zigr               | 1.000        |
| 33_altrep_region_read  | 0.33        | zigr               | 1.000        |

savvy wins on ALTREP element walk, min/max, no-NA query, and crossprod/Cholesky wraps. c_call and rcpp split wins on BLAS wrappers, ALTREP materialize, and serialization.

### Where zigr trails

| Task                  | Best native      | zigr vs best |
| --------------------- | ---------------- | ------------ |
| 07a_protect_shallow   | savvy (0.002 ms) | 3.31x        |
| 07b_protect_scaling   | savvy (0.002 ms) | 15833x       |
| 24_long_vector_idx    | savvy            | 1.41x        |
| 32_altrep_elt_walk    | savvy            | 1.44x        |
| 35_altrep_sum_native  | savvy            | 1.40x        |
| 36_altrep_min_max     | savvy            | 1.42x        |
| 37_altrep_no_na_query | savvy            | 1.52x        |

07b is the outlier. savvy uses compact PROTECT tracking (O(1) batch calls to `Rf_protect`) while zigr calls `Rf_protect` per element (O(100k)). This is a structural gap in the binding design, not a measurement artifact.

### System diagnostics (L7)

Layer 7 tasks are zigr-only, not comparative. They measure build and memory characteristics of the zigr backend. Run via `run_system_tasks.R`.

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

- Timing: adaptive convergence via `microbenchmark` (runs in blocks of 10, stops when rolling CV drops below 1% across last 5 blocks (50 iterations) or max 500 iterations). 10 warm-up iterations before measurement.
- RSS: per-task VmRSS delta (max 0, after - before), read from `/proc/self/status VmRSS`. `gc(full=TRUE)` runs before each RSS reading.
- Cold start: single run before warm-up, logged separately
- GC control: `gc(full=TRUE)` runs before each measurement block
- BLAS: single-threaded (`OPENBLAS_NUM_THREADS=1`)
- Low-noise threshold: CV <= 20% across all runners
- Meaningful margin: gap must exceed 5% (ratio > 1.05)
