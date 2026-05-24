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

The full results table below was refreshed on `2026-05-23` with:

```sh
Rscript run_benchmarks.R --build
```

All six runners currently pass the full 39-task shared matrix. `zigr` also passes 8 additional owned-backing ALTREP diagnostics, for 47 PASS rows total. This README now reflects the current full-matrix refresh, not a mix of older full-run numbers plus newer focused checks.

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
| `zigr`    |         9 |       0.571x |
| `savvy`   |         5 |       0.638x |
| `extendr` |         4 |       0.659x |
| `rcpp`    |         5 |       0.655x |
| `c_call`  |         4 |       0.684x |
| `R`       |         5 |       1.000x |

## Native Comparison

| Runner    | Geomean vs Best Native | Median vs Best Native | Meaningful Native Wins | Low-Noise Wins |
| --------- | ---------------------: | --------------------: | ---------------------: | -------------: |
| `zigr`    |                 1.259x |                1.068x |                      6 |              6 |
| `savvy`   |                 1.407x |                1.079x |                      2 |              4 |
| `extendr` |                 1.453x |                1.079x |                      2 |              2 |
| `rcpp`    |                 1.443x |                1.051x |                      3 |              1 |
| `c_call`  |                 1.508x |                1.158x |                      1 |              2 |

High-level takeaways from the current run:

- `zigr` remains the leader on the 32-task aggregate-comparable set. It posts a `0.571x` geomean versus pure R and wins `9` of those `32` tasks.
- `zigr` also stays closest to the native front on the comparable subset. Its `1.259x` geomean versus the best native backend and `1.068x` median ratio show that most comparable tasks sit near parity, with the largest remaining drag concentrated in `10_blas_matmul`, `21_string_nchar`, `27_struct_convert`, and `34_translate_c_cost`.
- `savvy`, `rcpp`, and `extendr` now form a fairly tight middle cluster at `0.638x`, `0.655x`, and `0.659x` versus R. `c_call` trails that group at `0.684x`, but still stays within the same broad band once the strategy-mismatched tasks are removed from the aggregate.
- Raw `Tasks Won` is only one perspective. On the stricter native-only counts, `zigr` now has `6` meaningful wins by a margin greater than `5%` and `6` wins on the `15` aggregate-comparable low-noise tasks.
- Task `23_altrep_sum` is no longer a methodology outlier. After routing `c_call` and `rcpp` through R's `sum()` generic, the native backends all land in the same `0.0023` to `0.0026 ms` band.
- The owned-backing ALTREP sum tasks still decisively favor `zigr`, but they are now treated as capability rows instead of headline aggregate wins: task `38` real sum is `0.1444 ms`, task `39` integer sum is `0.0645 ms`, and task `40` logical sum is `0.1088 ms`.
- Task `33_binary_growth` is useful as a footprint check, not just as a timed harness task. `zigr` is still the largest single artifact at `3,982,864` bytes, but its `46` benchmark exports pull it down to `86,584.0` bytes per export, now slightly leaner than `extendr` on that normalized view.

## Artifact Footprint

Current task `33_binary_growth` artifact metrics:

| Runner    | Artifact Bytes | Callable Exports | Bytes per Export |
| --------- | -------------: | ---------------: | ---------------: |
| `r`       |          6,985 |               38 |            183.8 |
| `c_call`  |        175,336 |               38 |          4,614.1 |
| `rcpp`    |        274,496 |               38 |          7,223.6 |
| `savvy`   |      2,153,672 |               38 |         56,675.6 |
| `extendr` |      3,336,304 |               38 |         87,797.5 |
| `zigr`    |      3,982,864 |               46 |         86,584.0 |

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
| 01_fib                      |  0.0035 |  0.0021 |              1.235x |  0.0021 |  0.0017 |  0.0041 |  0.0020 | Yes       |
| 02_vectorsum                | 15.4746 |  3.6166 |              1.000x |  9.9794 |  9.3780 |  8.8594 |  8.8711 | Yes       |
| 03_transpose                |  1.1420 |  1.0088 |              1.025x |  0.9840 |  0.9992 |  1.0104 |  1.0223 | Yes       |
| 04_strings                  |  2.2495 |  1.4012 |              1.555x |  1.1902 |  0.9111 |  0.9127 |  0.9013 | Yes       |
| 05_dataframe                | 21.5161 |  0.5158 |              1.000x |  0.9845 |  0.9847 |  0.9940 |  0.9564 | Yes       |
| 06_na_prop                  | 10.8545 |  0.1474 |              1.000x |  2.7898 |  2.6606 |  2.9027 |  2.9754 | Yes       |
| 07_parallel                 | 15.5179 |  2.2275 |              1.000x |  2.8000 |  2.7722 |  2.5033 |  2.4740 | Excluded  |
| 09_protect                  |  0.0016 |  0.7741 |              1.040x |  0.8533 |  0.7595 |  0.7936 |  0.7446 | Yes       |
| 10_blas_matmul              |  3.2149 |  3.5445 |              4.323x |  3.4467 |  3.0719 |  3.1336 |  0.8199 | Yes       |
| 11_crossprod                |  0.0746 |  0.0714 |              1.075x |  0.0664 |  0.0695 |  0.0685 |  0.0732 | Yes       |
| 12_cholesky                 |  0.4073 |  1.1225 |              2.095x |  1.1443 |  0.6775 |  0.5918 |  0.5358 | Yes       |
| 13_lm                       |  1.2754 |  0.3912 |              1.118x |  0.4093 |  0.3657 |  0.3499 |  0.4003 | Yes       |
| 14_rowsums                  |  1.2464 |  0.0820 |              1.000x |  0.1934 |  0.1822 |  0.1173 |  0.1149 | Yes       |
| 15_elem_ops                 | 82.6438 | 37.4240 |              1.241x | 30.1666 | 30.3104 | 33.1846 | 30.4828 | Yes       |
| 16_rowcol_means             |  1.9142 |  0.4689 |              1.000x |  0.8286 |  0.8131 |  0.8161 |  0.8098 | Yes       |
| 17_broadcast                | 49.1426 | 46.6617 |              1.000x | 48.4801 | 49.2205 | 47.9367 | 54.7622 | Yes       |
| 18_sort                     | 63.8455 | 40.1360 |              1.130x | 36.6991 | 36.1207 | 37.5463 | 35.5310 | Yes       |
| 19_cumsum                   | 58.3645 | 51.6063 |              1.001x | 51.6513 | 51.5589 | 51.6978 | 56.5588 | Yes       |
| 20_rnorm                    | 37.8472 | 32.1541 |              1.013x | 32.5727 | 48.5317 | 31.7472 | 32.3384 | Yes       |
| 21_string_nchar             |  0.6660 |  0.2173 |              3.312x |  0.0797 |  0.0817 |  0.0656 |  0.1314 | Yes       |
| 22_which_na                 |  4.0219 |  1.3007 |              1.144x |  2.2121 |  2.2429 |  1.1368 |  2.3671 | Yes       |
| 23_altrep_sum               |  0.0020 |  0.0024 |              1.043x |  0.0026 |  0.0023 |  0.0025 |  0.0024 | Yes       |
| 24_altrep_read              |  0.0021 |  0.0022 |              1.222x |  0.0022 |  0.0018 |  0.0022 |  0.0022 | Yes       |
| 25_altrep_create            |  0.0017 |  0.0024 |              1.333x |  0.0023 |  0.0018 |  0.0025 |  0.0019 | Yes       |
| 26_comptime_dispatch        |  1.6686 |  1.3951 |              1.095x |  1.5246 |  1.5282 |  1.2798 |  1.2740 | Yes       |
| 27_struct_convert           |  0.0041 |  0.0122 |              3.697x |  0.0034 |  0.0033 |  0.0059 |  0.0058 | Yes       |
| 28_na_prop_vary             | 50.2117 |  2.4408 |              1.000x | 22.2511 | 21.3105 | 21.9573 | 22.0224 | Yes       |
| 29_scale_law                |  8.5778 |  1.2499 |              1.000x |  5.2256 |  5.4271 |  4.9496 |  4.9916 | Excluded  |
| 30_arena_vs_rmalloc         |  0.5195 |  0.1030 |              1.000x |  0.1182 |  0.5221 |  0.1115 |  0.1106 | Excluded  |
| 31_prot_overhead            |  2.3778 |  2.9336 |              1.060x |  2.8394 |  2.8914 |  2.9856 |  2.7669 | Yes       |
| 32_longjmp_safety           | 17.0997 |  5.0914 |              1.000x |  5.0976 |  5.1195 |  5.4911 |  5.1387 | Yes       |
| 33_binary_growth            |  0.0489 |  0.0496 |              1.029x |  0.0509 |  0.0482 |  0.0490 |  0.0486 | Yes       |
| 34_translate_c_cost         | 32.9461 | 19.0223 |              1.603x | 13.0012 | 13.2235 | 11.9579 | 11.8689 | Yes       |
| 35_string_variants          |  6.2733 |  2.0034 |              1.096x |  1.8271 |  2.2446 |  2.3968 |  2.3134 | Yes       |
| 36_parallel_scaling         |  7.1586 |  3.4734 |              1.453x |  3.3062 |  3.2127 |  2.3906 |  2.4061 | Excluded  |
| 37_memory_bandwidth         | 38.7299 | 11.3609 |              1.000x | 11.5234 | 12.5897 | 11.7969 | 12.0648 | Yes       |
| 38_owned_altrep_real_sum    | 10.8254 |  0.1444 |              1.000x |  0.8368 |  0.8267 |  0.8280 |  0.8221 | Excluded  |
| 39_owned_altrep_int_sum     |  9.0588 |  0.0645 |              1.000x |  0.8219 |  0.8201 |  0.8219 |  0.8198 | Excluded  |
| 40_owned_altrep_logical_sum |  1.7115 |  0.1088 |              1.000x |  0.7616 |  0.7543 |  0.4133 |  0.4135 | Excluded  |

## Zigr-Only Diagnostics

Tasks `41` to `48` currently exercise owned-backing ALTREP min/max/argmin/argmax paths that only `zigr` implements. They therefore do not participate in the shared comparative exports yet.

| Task                             | zigr mean (ms) |
| -------------------------------- | -------------: |
| `41_owned_altrep_int_min`        |         0.0635 |
| `42_owned_altrep_int_max`        |         0.0635 |
| `43_owned_altrep_int_argmin`     |         0.1949 |
| `44_owned_altrep_int_argmax`     |         0.1928 |
| `45_owned_altrep_logical_min`    |         0.0647 |
| `46_owned_altrep_logical_max`    |         0.0642 |
| `47_owned_altrep_logical_argmin` |         0.1933 |
| `48_owned_altrep_logical_argmax` |         0.1933 |

## Low-Noise Appendix

These 15 aggregate-comparable tasks satisfy the current low-noise rule of maximum CV `<= 20%` across all runners. Low-noise but excluded rows such as `07_parallel`, `29_scale_law`, `36_parallel_scaling`, and `40_owned_altrep_logical_sum` are intentionally omitted here because they are not part of the aggregate metrics.

| Task                          | Best Native Runner | Max CV |
| ----------------------------- | -----------------: | -----: |
| `02_vectorsum`                |             `zigr` |  4.72% |
| `04_strings`                  |            `savvy` |  9.11% |
| `05_dataframe`                |             `zigr` |  6.32% |
| `11_crossprod`                |           `c_call` | 14.53% |
| `16_rowcol_means`             |             `zigr` |  5.07% |
| `17_broadcast`                |             `zigr` | 17.34% |
| `18_sort`                     |            `savvy` |  9.46% |
| `19_cumsum`                   |             `rcpp` | 16.46% |
| `20_rnorm`                    |          `extendr` |  9.11% |
| `21_string_nchar`             |          `extendr` | 16.06% |
| `26_comptime_dispatch`        |            `savvy` |  7.26% |
| `28_na_prop_vary`             |             `zigr` | 11.75% |
| `32_longjmp_safety`           |             `zigr` | 14.37% |
| `34_translate_c_cost`         |            `savvy` |  9.01% |
| `35_string_variants`          |           `c_call` | 11.80% |

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
