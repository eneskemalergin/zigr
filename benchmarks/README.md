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

Lower times are better. The `Geomean vs R` column is the geometric mean of `mean_ms / R_baseline_mean_ms` across all tasks, so values below `1.0x` are faster than the R baseline overall.

The native-comparison columns use the fastest native runner on each task as the reference point. `Geomean vs Best Native` and `Median vs Best Native` therefore measure how far a runner sits from the native front, where `1.0x` is the best possible value. `Meaningful Native Wins` only counts tasks where a runner beats the next-fastest native backend by more than `5%`. `Low-Noise Wins` only counts native task wins on tasks where the maximum coefficient of variation across all runners is at most `20%`.

## Overall Vs R

| Runner    | Tasks Won | Geomean vs R |
| --------- | --------: | -----------: |
| `zigr`    |        13 |       0.396x |
| `savvy`   |         5 |       0.516x |
| `extendr` |         5 |       0.530x |
| `rcpp`    |         8 |       0.667x |
| `c_call`  |         3 |       0.715x |
| `R`       |         5 |       1.000x |

## Native Comparison

| Runner    | Geomean vs Best Native | Median vs Best Native | Meaningful Native Wins | Low-Noise Wins |
| --------- | ---------------------: | --------------------: | ---------------------: | -------------: |
| `zigr`    |                 1.228x |                1.044x |                     12 |              8 |
| `savvy`   |                 1.602x |                1.111x |                      1 |              4 |
| `extendr` |                 1.645x |                1.096x |                      2 |              3 |
| `rcpp`    |                 2.069x |                1.161x |                      5 |              2 |
| `c_call`  |                 2.218x |                1.278x |                      2 |              2 |

High-level takeaways from the current run:

- `zigr` remains the overall leader. It posts a `0.396x` geomean versus pure R and wins `13` of the `39` shared tasks.
- `zigr` also stays closest to the native front. Its `1.228x` geomean versus the best native backend and `1.044x` median ratio show that most tasks sit near parity, with the remaining drag concentrated in `10_blas_matmul`, `21_string_nchar`, `27_struct_convert`, `34_translate_c_cost`, and `36_parallel_scaling`.
- `savvy` and `extendr` remain the next-best aggregate backends at `0.516x` and `0.530x` versus R. `rcpp` still collects `8` raw task wins, but a few outliers keep its geomean behind the Rust pair.
- Raw `Tasks Won` is only one perspective. On the stricter native-only counts, `zigr` now has `12` meaningful wins by a margin greater than `5%` and `8` wins on the `19` low-noise shared tasks.
- The shared owned-backing ALTREP sum tasks now decisively favor `zigr`: task `38` real sum is `0.1444 ms` versus native peers near `0.82 ms`, task `39` integer sum is `0.0645 ms` versus native peers near `0.82 ms`, and task `40` logical sum is `0.1088 ms` versus native peers around `0.41` to `0.46 ms`.
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

Tasks `38` to `40` are now shared owned-backing ALTREP sum workloads. Tasks `41` to `48` remain zigr-only diagnostics for owned-backing ALTREP min/max/argmin/argmax paths. Comparative metrics ignore any task unless every runner has a PASS row for the same task.

## Full Results

Mean wall time in milliseconds for the 39 shared tasks. Lower is better. Task `33_binary_growth` is timed like the rest of the matrix, but the raw footprint metrics that matter for that task are listed in the artifact table above.

| Task                        |       R |    zigr | zigr vs Best Native | C .Call |    Rcpp | extendr |   Savvy |
| --------------------------- | ------: | ------: | ------------------: | ------: | ------: | ------: | ------: |
| 01_fib                      |  0.0035 |  0.0021 |              1.312x |  0.0022 |  0.0016 |  0.0041 |  0.0020 |
| 02_vectorsum                | 15.4746 |  3.6166 |              1.000x |  9.9499 |  9.9683 |  8.8594 |  8.8711 |
| 03_transpose                |  1.1420 |  1.0088 |              1.044x |  0.9788 |  0.9664 |  1.0104 |  1.0223 |
| 04_strings                  |  2.2495 |  1.4012 |              1.555x |  1.2017 |  0.9221 |  0.9127 |  0.9013 |
| 05_dataframe                | 21.5161 |  0.5158 |              1.000x |  0.9846 |  0.9949 |  0.9940 |  0.9564 |
| 06_na_prop                  | 10.8545 |  0.1474 |              1.000x |  2.7703 |  2.7198 |  2.9027 |  2.9754 |
| 07_parallel                 | 15.5179 |  2.2275 |              1.000x |  2.8935 |  2.7206 |  2.5033 |  2.4740 |
| 09_protect                  |  0.0016 |  0.7741 |              1.040x |  0.8169 |  0.8804 |  0.7936 |  0.7446 |
| 10_blas_matmul              |  3.2149 |  3.5445 |              4.323x |  5.6744 |  2.5270 |  3.1336 |  0.8199 |
| 11_crossprod                |  0.0746 |  0.0714 |              1.066x |  0.0673 |  0.0670 |  0.0685 |  0.0732 |
| 12_cholesky                 |  0.4073 |  1.1225 |              2.296x |  3.6157 |  0.4888 |  0.5918 |  0.5358 |
| 13_lm                       |  1.2754 |  0.3912 |              1.118x |  0.4225 |  0.3577 |  0.3499 |  0.4003 |
| 14_rowsums                  |  1.2464 |  0.0820 |              1.000x |  0.1879 |  0.1764 |  0.1173 |  0.1149 |
| 15_elem_ops                 | 82.6438 | 37.4240 |              1.248x | 30.2164 | 29.9792 | 33.1846 | 30.4828 |
| 16_rowcol_means             |  1.9142 |  0.4689 |              1.000x |  0.8121 |  0.8104 |  0.8161 |  0.8098 |
| 17_broadcast                | 49.1426 | 46.6617 |              1.000x | 49.1945 | 49.7718 | 47.9367 | 54.7622 |
| 18_sort                     | 63.8455 | 40.1360 |              1.130x | 37.5961 | 36.9749 | 37.5463 | 35.5310 |
| 19_cumsum                   | 58.3645 | 51.6063 |              1.003x | 52.1997 | 51.4276 | 51.6978 | 56.5588 |
| 20_rnorm                    | 37.8472 | 32.1541 |              1.013x | 32.0786 | 32.0551 | 31.7472 | 32.3384 |
| 21_string_nchar             |  0.6660 |  0.2173 |              3.312x |  0.0823 |  0.0788 |  0.0656 |  0.1314 |
| 22_which_na                 |  4.0219 |  1.3007 |              1.144x |  2.1941 |  2.2073 |  1.1368 |  2.3671 |
| 23_altrep_sum               |  0.0020 |  0.0024 |              1.000x |  8.8993 |  8.9345 |  0.0025 |  0.0024 |
| 24_altrep_read              |  0.0021 |  0.0022 |              1.222x |  0.0021 |  0.0018 |  0.0022 |  0.0022 |
| 25_altrep_create            |  0.0017 |  0.0024 |              1.333x |  0.0023 |  0.0018 |  0.0025 |  0.0019 |
| 26_comptime_dispatch        |  1.6686 |  1.3951 |              1.095x |  1.5330 |  1.3939 |  1.2798 |  1.2740 |
| 27_struct_convert           |  0.0041 |  0.0122 |              3.812x |  0.0035 |  0.0032 |  0.0059 |  0.0058 |
| 28_na_prop_vary             | 50.2117 |  2.4408 |              1.000x | 22.4974 | 21.8158 | 21.9573 | 22.0224 |
| 29_scale_law                |  8.5778 |  1.2499 |              1.000x |  5.0806 |  5.1172 |  4.9496 |  4.9916 |
| 30_arena_vs_rmalloc         |  0.5195 |  0.1030 |              1.000x |  0.1457 |  0.4786 |  0.1115 |  0.1106 |
| 31_prot_overhead            |  2.3778 |  2.9336 |              1.071x |  2.7526 |  2.7394 |  2.9856 |  2.7669 |
| 32_longjmp_safety           | 17.0997 |  5.0914 |              1.013x |  5.0261 |  5.0799 |  5.4911 |  5.1387 |
| 33_binary_growth            |  0.0489 |  0.0496 |              1.027x |  0.0495 |  0.0483 |  0.0490 |  0.0486 |
| 34_translate_c_cost         | 32.9461 | 19.0223 |              1.603x | 12.8248 | 12.8321 | 11.9579 | 11.8689 |
| 35_string_variants          |  6.2733 |  2.0034 |              1.131x |  1.7721 |  2.2091 |  2.3968 |  2.3134 |
| 36_parallel_scaling         |  7.1586 |  3.4734 |              1.453x |  3.1383 |  3.2223 |  2.3906 |  2.4061 |
| 37_memory_bandwidth         | 38.7299 | 11.3609 |              1.055x | 10.7643 | 12.4944 | 11.7969 | 12.0648 |
| 38_owned_altrep_real_sum    | 10.8254 |  0.1444 |              1.000x |  0.8223 |  0.8300 |  0.8280 |  0.8221 |
| 39_owned_altrep_int_sum     |  9.0588 |  0.0645 |              1.000x |  0.8198 |  0.8201 |  0.8219 |  0.8198 |
| 40_owned_altrep_logical_sum |  1.7115 |  0.1088 |              1.000x |  0.4633 |  0.4721 |  0.4133 |  0.4135 |

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

These tasks satisfy the current low-noise rule of maximum CV `<= 20%` across all runners.

| Task                          | Best Native Runner | Max CV |
| ----------------------------- | -----------------: | -----: |
| `02_vectorsum`                |             `zigr` |  4.33% |
| `04_strings`                  |            `savvy` |  9.11% |
| `05_dataframe`                |             `zigr` |  6.32% |
| `07_parallel`                 |             `zigr` |  4.99% |
| `11_crossprod`                |             `rcpp` | 14.53% |
| `16_rowcol_means`             |             `zigr` |  4.00% |
| `17_broadcast`                |             `zigr` | 17.34% |
| `18_sort`                     |            `savvy` |  9.46% |
| `19_cumsum`                   |             `rcpp` | 16.46% |
| `20_rnorm`                    |          `extendr` |  9.11% |
| `21_string_nchar`             |          `extendr` |  6.67% |
| `26_comptime_dispatch`        |            `savvy` |  7.26% |
| `28_na_prop_vary`             |             `zigr` | 11.75% |
| `29_scale_law`                |             `zigr` | 14.23% |
| `32_longjmp_safety`           |           `c_call` | 14.37% |
| `34_translate_c_cost`         |            `savvy` |  3.09% |
| `35_string_variants`          |           `c_call` | 11.80% |
| `36_parallel_scaling`         |          `extendr` |  3.36% |
| `40_owned_altrep_logical_sum` |             `zigr` | 19.61% |

## Output Files

- Runner summaries live in `results/*_summary.csv`.
- Comparative runner metrics live in `results/comparative_metrics.csv`.
- Per-task native comparisons and low-noise flags live in `results/task_comparisons.csv`.
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
