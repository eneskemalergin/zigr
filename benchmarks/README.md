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

## Current Landscape

The full results table below was refreshed on `2026-05-24` with:

```sh
Rscript run_benchmarks.R --build
```

All six runners currently pass the full 39-task shared matrix. `zigr` also passes 8 additional owned-backing ALTREP diagnostics, for 47 PASS rows total.

The full comparative tables below still describe that last unfiltered six-runner refresh. Focused parity rechecks are called out separately so they do not get conflated with the full matrix.

## Focused Rust Parity Recheck

On `2026-05-24` the harness also reran a focused three-round subprocess slice for `zigr`, `extendr`, and `savvy` on tasks `10_blas_matmul`, `11_crossprod`, and `12_cholesky`.

Saved focused outputs:

- `results/focused/rust_parity_round_1_2026-05-24.csv`
- `results/focused/rust_parity_round_2_2026-05-24.csv`
- `results/focused/rust_parity_round_3_2026-05-24.csv`
- `results/focused/rust_parity_matrix_2026-05-24.csv`
- `results/focused/task12_repeat_rows_2026-05-24.csv`
- `results/focused/task12_repeat_band_2026-05-24.csv`

Task `12_cholesky` stayed in the same narrow band across all three fresh subprocess rounds:

| Runner    | Min mean (ms) | Max mean (ms) | Avg mean (ms) | Median mean (ms) |
| --------- | ------------: | ------------: | ------------: | ---------------: |
| `extendr` |        0.2927 |        0.2958 |        0.2947 |           0.2957 |
| `zigr`    |        0.2882 |        0.3026 |        0.2962 |           0.2978 |
| `savvy`   |        0.3048 |        0.3121 |        0.3075 |           0.3056 |

In the same focused three-task slice, tasks `10_blas_matmul` and `11_crossprod` also stayed tightly clustered across the three runners. Those focused CSVs are supplemental only. The standard per-runner summaries and the comparative tables below are now back in sync with the latest unfiltered six-runner export.

Headline aggregate metrics in this README use the 32-task aggregate-comparable subset. The remaining 7 shared tasks still appear in the full results table, but they are excluded from aggregate leaderboards because they are not measuring the same strategy across runners:

- `07_parallel` and `36_parallel_scaling`: native backends use multithreaded sums while the R baseline is serial.
- `29_scale_law`: runners intentionally use different dispatch strategies.
- `30_arena_vs_rmalloc`: runners intentionally use different allocation strategies.
- `38_owned_altrep_real_sum`, `39_owned_altrep_int_sum`, and `40_owned_altrep_logical_sum`: owned ALTREP capability benchmarks rather than a common user-level path.

Task `23_altrep_sum` was also normalized in this refresh. `c_call`, `rcpp`, and `zigr` all route through R's `sum()` generic so the benchmark measures the same ALTREP-aware dispatch path across backends.

Lower times are better. The `Geomean vs R` column is the geometric mean of `mean_ms / R_baseline_mean_ms` across the 32 aggregate-comparable shared tasks, so values below `1.0x` are faster than the R baseline overall.

The native-comparison columns use the fastest native runner on each aggregate-comparable task as the reference point. `Geomean vs Best Native` and `Median vs Best Native` therefore measure how far a runner sits from the native front, where `1.0x` is the best possible value. `Meaningful Native Wins` only counts tasks where a runner beats the next-fastest native backend by more than `5%`. `Low-Noise Wins` only counts native task wins on aggregate-comparable tasks where the maximum coefficient of variation across all runners is at most `20%`.

## Overall Vs R

| Runner    | Tasks Won | Geomean vs R |
| --------- | --------: | -----------: |
| `zigr`    |         9 |       0.553x |
| `rcpp`    |         7 |       0.625x |
| `c_call`  |         7 |       0.642x |
| `extendr` |         5 |       0.660x |
| `savvy`   |         1 |       0.663x |
| `R`       |         3 |       1.000x |

## Native Comparison

| Runner    | Geomean vs Best Native | Median vs Best Native | Meaningful Native Wins | Low-Noise Wins |
| --------- | ---------------------: | --------------------: | ---------------------: | -------------: |
| `zigr`    |                 1.189x |                1.031x |                      6 |              6 |
| `rcpp`    |                 1.344x |                1.025x |                      5 |              1 |
| `c_call`  |                 1.382x |                1.040x |                      2 |              5 |
| `extendr` |                 1.421x |                1.084x |                      0 |              4 |
| `savvy`   |                 1.426x |                1.108x |                      0 |              1 |

High-level takeaways from the current run:

- `zigr` remains the leader on the 32-task aggregate-comparable set. It posts a `0.553x` geomean versus pure R and wins `9` of those `32` tasks.
- `zigr` also stays closest to the native front on the comparable subset. Its `1.189x` geomean versus the best native backend and `1.031x` median ratio show that most comparable tasks still sit near parity, with the largest remaining drag concentrated in `21_string_nchar`, `34_translate_c_cost`, `25_altrep_create`, and `24_altrep_read`.
- Task `12_cholesky` is no longer a standout zigr regression in the full matrix. `zigr` lands at `0.2969 ms`, versus `0.2875 ms` for `rcpp`, `0.2999 ms` for `extendr`, `0.3037 ms` for `c_call`, and `0.3057 ms` for `savvy`.
- The middle native pack remains tight, but the ordering in this full refresh is `rcpp`, `c_call`, `extendr`, then `savvy` on aggregate-comparable geomean versus R.
- Raw `Tasks Won` is only one perspective. On the stricter native-only counts, `zigr` now has `6` meaningful wins by a margin greater than `5%` and `6` wins on the `17` aggregate-comparable low-noise tasks.
- Task `10_blas_matmul` now lands in zigr's favor in the full matrix at `0.9255 ms`, narrowly ahead of `extendr` at `0.9299 ms` and `c_call` at `0.9399 ms`.
- The owned-backing ALTREP sum tasks still decisively favor `zigr`, but they remain capability rows rather than headline aggregate wins: task `38` real sum is `0.1164 ms`, task `39` integer sum is `0.0684 ms`, and task `40` logical sum is `0.1103 ms`.
- Task `33_binary_growth` is still useful as a footprint check, not just as a timed harness task. `zigr` remains the largest single artifact at `3,973,368` bytes, but its `47` benchmark exports pull it down to `84,539.7` bytes per export, slightly leaner than `extendr` on that normalized view.

## Artifact Footprint

Current task `33_binary_growth` artifact metrics:

| Runner    | Artifact Bytes | Callable Exports | Bytes per Export |
| --------- | -------------: | ---------------: | ---------------: |
| `r`       |          6,985 |               39 |            179.1 |
| `c_call`  |        175,280 |               39 |          4,494.4 |
| `rcpp`    |        274,408 |               39 |          7,036.1 |
| `savvy`   |      2,153,672 |               39 |         55,222.4 |
| `extendr` |      3,336,304 |               39 |         85,546.3 |
| `zigr`    |      3,973,368 |               47 |         84,539.7 |

## Task Set

| ID                               | Task                                     |
| -------------------------------- | ---------------------------------------- |
| `01_fib`                         | Fibonacci (`n=50`)                       |
| `02_vectorsum`                   | Vector sum (`1e7`)                       |
| `03_transpose`                   | Matrix transpose (`500x500`)             |
| `04_strings`                     | String concatenation (`1e4`)             |
| `05_dataframe`                   | Data frame filtering (`1e5`)             |
| `06_na_prop`                     | NA propagation (`1e6`)                   |
| `07_parallel`                    | Parallel sum (`1e7`)                     |
| `09_protect`                     | PROTECT stress (`49k`)                   |
| `10_blas_matmul`                 | BLAS matmul (`256x256`)                  |
| `11_crossprod`                   | Cross-product (`500x50`)                 |
| `12_cholesky`                    | Cholesky (`200x200`)                     |
| `13_lm`                          | Linear model (`n=5000`, `p=20`)          |
| `14_rowsums`                     | Row sums (`1000x500`)                    |
| `15_elem_ops`                    | Element-wise ops (`1e6`)                 |
| `16_rowcol_means`                | Row means and column sums (`500x1000`)   |
| `17_broadcast`                   | Vector + scalar (`1e7`)                  |
| `18_sort`                        | Sort (`1e6`)                             |
| `19_cumsum`                      | Cumulative sum (`1e7`)                   |
| `20_rnorm`                       | Random normal (`1e6`)                    |
| `21_string_nchar`                | String `nchar` (`1e4`)                   |
| `22_which_na`                    | Which NA (`1e6`)                         |
| `23_altrep_sum`                  | ALTREP sum (`1:1e7`)                     |
| `24_altrep_read`                 | ALTREP read (`1:1e7`)                    |
| `25_altrep_create`               | ALTREP create (`n=1e6`)                  |
| `26_comptime_dispatch`           | Comptime dispatch (`3x2048`, reps=`256`) |
| `27_struct_convert`              | Struct convert (`10` fields)             |
| `28_na_prop_vary`                | NA proportion sweep (`6x1e6`)            |
| `29_scale_law`                   | Scale-law mix (`1K..4M`)                 |
| `30_arena_vs_rmalloc`            | Allocation mix (`100x1024`)              |
| `31_prot_overhead`               | Protection mix (`5x4096x64`)             |
| `32_longjmp_safety`              | Longjmp safety (`4x512x64`)              |
| `33_binary_growth`               | Binary footprint (`bytes/export`)        |
| `34_translate_c_cost`            | Math call cost (`4x512x2048`)            |
| `35_string_variants`             | String suite (`1e4x24`)                  |
| `36_parallel_scaling`            | Thread sweep (`1/2/4/8/16 x 2^20`)       |
| `37_memory_bandwidth`            | Memory paths (`3x2x2^20`)                |
| `38_owned_altrep_real_sum`       | Owned ALTREP real sum (`n=1e6`)          |
| `39_owned_altrep_int_sum`        | Owned ALTREP integer sum (`n=1e6`)       |
| `40_owned_altrep_logical_sum`    | Owned ALTREP logical sum (`n=1e6`)       |
| `41_owned_altrep_int_min`        | Owned ALTREP integer min (`n=1e6`)       |
| `42_owned_altrep_int_max`        | Owned ALTREP integer max (`n=1e6`)       |
| `43_owned_altrep_int_argmin`     | Owned ALTREP integer argmin (`n=1e6`)    |
| `44_owned_altrep_int_argmax`     | Owned ALTREP integer argmax (`n=1e6`)    |
| `45_owned_altrep_logical_min`    | Owned ALTREP logical min (`n=1e6`)       |
| `46_owned_altrep_logical_max`    | Owned ALTREP logical max (`n=1e6`)       |
| `47_owned_altrep_logical_argmin` | Owned ALTREP logical argmin (`n=1e6`)    |
| `48_owned_altrep_logical_argmax` | Owned ALTREP logical argmax (`n=1e6`)    |

Tasks `38` to `40` are shared owned-backing ALTREP sum workloads, but the aggregate exports treat them as capability benchmarks rather than common user-level paths. Tasks `41` to `48` remain zigr-only diagnostics for owned-backing ALTREP min/max/argmin/argmax paths. Comparative metrics ignore any task unless every runner has a PASS row for the same task, and they further annotate whether a shared task is aggregate-comparable.

## Full Results

Mean wall time in milliseconds for the 39 shared tasks. Lower is better. Task `33_binary_growth` is timed like the rest of the matrix, but the raw footprint metrics that matter for that task are listed in the artifact table above. Rows marked `Excluded` are reported for completeness but are not part of the 32-task aggregate leaderboards.

| Task                        |       R |    zigr | zigr vs Best Native | C .Call |    Rcpp | extendr |   Savvy | Aggregate |
| --------------------------- | ------: | ------: | ------------------: | ------: | ------: | ------: | ------: | :-------: |
| 01_fib                      |  0.0034 |  0.0029 |              1.706x |  0.0030 |  0.0017 |  0.0042 |  0.0022 | Yes       |
| 02_vectorsum                | 15.1832 |  3.9322 |              1.000x |  9.4859 |  9.4820 |  8.8960 |  8.9561 | Yes       |
| 03_transpose                |  1.1346 |  1.0952 |              1.125x |  0.9734 |  1.0307 |  0.9914 |  1.0982 | Yes       |
| 04_strings                  |  2.2480 |  1.4390 |              1.553x |  1.1849 |  1.1679 |  0.9268 |  0.9730 | Yes       |
| 05_dataframe                | 22.5811 |  0.5098 |              1.000x |  0.9921 |  0.9873 |  0.9981 |  0.9860 | Yes       |
| 06_na_prop                  | 10.8729 |  0.1513 |              1.000x |  2.7345 |  2.6815 |  2.8821 |  3.0256 | Yes       |
| 07_parallel                 | 15.1429 |  2.4972 |              1.000x |  2.8325 |  2.9187 |  2.5229 |  2.6495 | Excluded  |
| 09_protect                  |  0.0016 |  1.0170 |              1.307x |  0.7783 |  0.8397 |  0.8453 |  0.8444 | Yes       |
| 10_blas_matmul              |  0.9420 |  0.9255 |              1.000x |  0.9399 |  0.9486 |  0.9299 |  1.0001 | Yes       |
| 11_crossprod                |  0.0730 |  0.0686 |              1.030x |  0.0666 |  0.0707 |  0.0670 |  0.0705 | Yes       |
| 12_cholesky                 |  0.2793 |  0.2969 |              1.033x |  0.3037 |  0.2875 |  0.2999 |  0.3057 | Yes       |
| 13_lm                       |  1.1583 |  0.2301 |              1.000x |  0.2303 |  0.2303 |  0.2314 |  0.2360 | Yes       |
| 14_rowsums                  |  1.2434 |  0.0827 |              1.000x |  0.1765 |  0.1845 |  0.1153 |  0.1183 | Yes       |
| 15_elem_ops                 | 83.1151 | 38.0822 |              1.271x | 30.0220 | 29.9624 | 33.4000 | 32.0355 | Yes       |
| 16_rowcol_means             |  1.9091 |  0.4759 |              1.000x |  0.8130 |  0.8098 |  0.8140 |  0.8154 | Yes       |
| 17_broadcast                | 49.4076 | 48.5664 |              1.000x | 48.7509 | 48.7095 | 48.5456 | 55.0899 | Yes       |
| 18_sort                     | 65.6943 | 41.5821 |              1.163x | 35.7433 | 36.4148 | 37.9554 | 38.4279 | Yes       |
| 19_cumsum                   | 58.9330 | 52.8457 |              1.011x | 52.2756 | 53.1194 | 53.8323 | 57.3950 | Yes       |
| 20_rnorm                    | 37.8210 | 32.4311 |              1.011x | 32.0627 | 32.4260 | 32.1989 | 32.8379 | Yes       |
| 21_string_nchar             |  0.6811 |  0.2209 |              2.836x |  0.0798 |  0.0799 |  0.0779 |  0.1328 | Yes       |
| 22_which_na                 |  4.4973 |  1.2657 |              1.010x |  2.1916 |  2.1969 |  1.2532 |  2.3753 | Yes       |
| 23_altrep_sum               |  0.0021 |  0.0033 |              1.650x |  0.0024 |  0.0020 |  0.0027 |  0.0023 | Yes       |
| 24_altrep_read              |  0.0021 |  0.0029 |              1.706x |  0.0021 |  0.0017 |  0.0023 |  0.0022 | Yes       |
| 25_altrep_create            |  0.0018 |  0.0033 |              1.941x |  0.0023 |  0.0017 |  0.0027 |  0.0019 | Yes       |
| 26_comptime_dispatch        |  1.6644 |  1.4317 |              1.113x |  1.5391 |  1.5363 |  1.2860 |  1.2884 | Yes       |
| 27_struct_convert           |  0.0040 |  0.0036 |              1.161x |  0.0034 |  0.0031 |  0.0063 |  0.0058 | Yes       |
| 28_na_prop_vary             | 51.4719 |  2.7324 |              1.000x | 22.1362 | 21.2561 | 22.0602 | 21.8399 | Yes       |
| 29_scale_law                |  8.6157 |  1.3571 |              1.000x |  5.2262 |  5.1641 |  4.9440 |  4.9769 | Excluded  |
| 30_arena_vs_rmalloc         |  0.5132 |  0.1026 |              1.000x |  0.1178 |  0.5089 |  0.1154 |  0.1117 | Excluded  |
| 31_prot_overhead            |  2.3651 |  3.1999 |              1.125x |  2.8587 |  2.8431 |  3.2804 |  2.9552 | Yes       |
| 32_longjmp_safety           | 17.0816 |  5.0545 |              1.016x |  4.9794 |  4.9740 |  5.4504 |  5.1901 | Yes       |
| 33_binary_growth            |  0.0483 |  0.0482 |              1.000x |  0.0484 |  0.0484 |  0.0520 |  0.0497 | Yes       |
| 34_translate_c_cost         | 32.6118 | 19.1580 |              1.601x | 12.9119 | 12.9872 | 12.4141 | 11.9693 | Yes       |
| 35_string_variants          |  6.3786 |  1.9459 |              1.078x |  1.8055 |  2.2864 |  2.6257 |  2.4204 | Yes       |
| 36_parallel_scaling         |  7.2977 |  3.5001 |              1.439x |  3.1707 |  3.2561 |  2.4329 |  2.4333 | Excluded  |
| 37_memory_bandwidth         | 41.1820 | 11.4804 |              1.021x | 11.2397 | 12.1924 | 12.1627 | 12.1340 | Yes       |
| 38_owned_altrep_real_sum    | 11.0518 |  0.1164 |              1.000x |  0.8225 |  0.8304 |  0.8640 |  0.8243 | Excluded  |
| 39_owned_altrep_int_sum     |  9.1054 |  0.0684 |              1.000x |  0.8199 |  0.8197 |  0.8242 |  0.8208 | Excluded  |
| 40_owned_altrep_logical_sum |  1.7692 |  0.1103 |              1.000x |  0.7395 |  0.7509 |  0.4178 |  0.4137 | Excluded  |

## Zigr-Only Diagnostics

Tasks `41` to `48` currently exercise owned-backing ALTREP min/max/argmin/argmax paths that only `zigr` implements. They therefore do not participate in the shared comparative exports yet.

| Task                             | zigr mean (ms) |
| -------------------------------- | -------------: |
| `41_owned_altrep_int_min`        |         0.0640 |
| `42_owned_altrep_int_max`        |         0.0644 |
| `43_owned_altrep_int_argmin`     |         0.1925 |
| `44_owned_altrep_int_argmax`     |         0.1933 |
| `45_owned_altrep_logical_min`    |         0.0641 |
| `46_owned_altrep_logical_max`    |         0.0640 |
| `47_owned_altrep_logical_argmin` |         0.1926 |
| `48_owned_altrep_logical_argmax` |         0.1940 |

## Low-Noise Appendix

These 17 aggregate-comparable tasks satisfy the current low-noise rule of maximum CV `<= 20%` across all runners. Low-noise but excluded rows such as `07_parallel`, `29_scale_law`, `36_parallel_scaling`, and `40_owned_altrep_logical_sum` are intentionally omitted here because they are not part of the aggregate metrics.

| Task                          | Best Native Runner | Max CV |
| ----------------------------- | -----------------: | -----: |
| `02_vectorsum`                |             `zigr` |  7.17% |
| `04_strings`                  |          `extendr` | 14.77% |
| `05_dataframe`                |             `zigr` |  9.88% |
| `10_blas_matmul`              |             `zigr` | 10.81% |
| `11_crossprod`                |           `c_call` | 16.63% |
| `13_lm`                       |             `zigr` | 19.80% |
| `16_rowcol_means`             |             `zigr` |  4.99% |
| `17_broadcast`                |          `extendr` | 17.36% |
| `18_sort`                     |           `c_call` | 10.53% |
| `19_cumsum`                   |           `c_call` | 17.71% |
| `20_rnorm`                    |           `c_call` |  9.20% |
| `21_string_nchar`             |          `extendr` | 17.86% |
| `26_comptime_dispatch`        |          `extendr` |  7.61% |
| `28_na_prop_vary`             |             `zigr` | 11.77% |
| `32_longjmp_safety`           |             `rcpp` | 13.66% |
| `34_translate_c_cost`         |            `savvy` |  4.47% |
| `35_string_variants`          |           `c_call` | 11.90% |

## Output Files

- Runner summaries live in `results/*_summary.csv`.
- Comparative runner metrics live in `results/comparative_metrics.csv`.
- Per-task native comparisons, aggregate-comparability flags, and exclusion notes live in `results/task_comparisons.csv`.
- Comparative CSVs are auto-refreshed only on full unfiltered `run_benchmarks.R` runs.
- Per-task timing CSVs live in `results/<runner>/task_*.csv`.
- Cold-start timings live in `results/<runner>/cold_start.csv`.

## Usage

```sh
# Build all native runners
bash build_all.sh

# Run all configured runners
Rscript run_benchmarks.R

# Rebuild, then run everything
Rscript run_benchmarks.R --build

# Refresh comparative CSVs from existing summary files only
Rscript export_comparative_metrics.R

# Run a subset of runners or tasks
Rscript run_benchmarks.R --runners=zigr,extendr,savvy
Rscript run_benchmarks.R --tasks=1,2,5
```
