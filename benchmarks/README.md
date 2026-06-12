<!-- markdownlint-disable MD024 -->
# zigr Benchmark Harness

This directory contains the in-repo benchmark harness plus six active runner backends:

| Runner    | Implementation | Notes                                         |
| --------- | -------------- | --------------------------------------------- |
| `r`       | R baseline     | Pure R reference implementation               |
| `zigr`    | Zig            | Native Zig bindings and runtime helpers       |
| `c_call`  | C (.Call)      | Hand-written C reference backend              |
| `rcpp`    | Rcpp (C++)     | C++ backend through Rcpp                      |
| `extendr` | Rust + extendr | Rust backend using extendr-generated wrappers |
| `savvy`   | Rust + Savvy   | Rust backend using Savvy-generated wrappers   |

## Current state

The full task matrix in `plan/BENCHMARK.md` defines 47 shared tasks plus 4 zigr-only Layer 7 system tasks. The current source tree implements the first 43 of those tasks (`task_01_vectorsum` through `task_43_rng_stress`) across all six backends:

- `benchmarks/src/zig/`: 43 `task_*.zig` + `main.zig`, builds via `benchmarks/build.zig`.
- `benchmarks/src/c_call/`: 43 `task_*.c` + `register.c` + `Makefile`.
- `benchmarks/src/cpp/`: `main.cpp` (Rcpp, single translation unit).
- `benchmarks/src/extendr/`: `rust/src/lib.rs` + `entrypoint.c` + `Makefile`.
- `benchmarks/src/savvy/`: `rust/src/lib.rs` + `init.c` + `api.h` + `Makefile`.
- `benchmarks/src/r/`: `run_all.R` (pure R baseline).

`benchmarks/runner_subprocess.R` enumerates the 43 tasks. `benchmarks/runner_subprocess.R` is the single source of truth for task IDs and input generators; the JSONs in `runners/` and the Makefiles in each backend source dir are checked against it.

The benchmark spec in `plan/BENCHMARK.md` lists tasks 44-47 (Layer 7 system diagnostics: build time, binary size, cross-compile time, allocation count) which are not yet wired into the harness. The eight `owned_altrep_*` min/max/argmin/argmax diagnostics from a prior numbering scheme have been removed.

## Results

All per-task timing CSVs in `benchmarks/results/<runner>/` were cleared during the benchmark cleanup. The `cold_start.csv` files remain. The `benchmarks/analysis/summary.csv` and `comparison.csv` outputs were also cleared because they indexed the old task numbering.

To regenerate results:

```sh
# Build all native runners
bash build_all.sh

# Run all configured runners
Rscript run_benchmarks.R
```

The harness will write per-task CSVs to `benchmarks/results/<runner>/task_*.csv` and consolidated metrics to `benchmarks/analysis/summary.csv` and `comparison.csv` on the next full run.

## Methodology and known limitations

The current harness is functional but not yet at the level of rigor described in `plan/BENCHMARK.md` sections 2.3 (correctness validation), 2.4 (GC management), 2.5 (behavioral divergence analysis), and 7 (definitions of done). In particular:

- **No per-task result correctness validation.** A task is currently considered "PASS" if it returns a value. There is no harness code that compares each task's output against the R baseline per the tolerance table in `BENCHMARK.md` section 2.3. Adding this is the next structural item.
- **`times=100L` hard-coded**, no adaptive convergence detection.
- **GC management is `bench::mark`-only** on a per-runner basis; the harness does not explicitly call `gc(full = TRUE)` between blocks.
- **No input-randomization control** (no `--seed`).

These are tracked under workstream W2 in `plan/PLAN.md` and will be addressed when the tasks themselves are finalized.

## Output files

- Per-task timing CSVs live in `results/<runner>/task_*.csv` (regenerated on every run).
- Cold-start timings live in `results/<runner>/cold_start.csv`.
- Consolidated summary lives in `analysis/summary.csv` (regenerated on every full run).

## Usage

```sh
# Build all native runners
bash build_all.sh

# Run all configured runners
Rscript run_benchmarks.R

# Rebuild, then run everything
Rscript run_benchmarks.R --build

# Run a subset of runners or tasks
Rscript run_benchmarks.R --runners=zigr,extendr,savvy
Rscript run_benchmarks.R --tasks=1,2,5
```
