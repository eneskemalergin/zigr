#!/usr/bin/env Rscript
# Runner subprocess: loads a runner.json config, runs tasks with convergence
# detection, outputs CSV. Called once per runner by run_benchmarks.R.
# Usage: Rscript runner_subprocess.R --runner=zigr [--tasks=1,2]

source("lib/harness.R")
library(methods)
library(jsonlite)

cli <- commandArgs(trailingOnly = TRUE)
runner_name <- NA
task_filter <- NULL
for (a in cli) {
  if (grepl("^--runner=", a)) runner_name <- sub("^--runner=", "", a)
  if (grepl("^--tasks=", a))  task_filter <- as.integer(strsplit(sub("^--tasks=", "", a), ",")[[1]])
}
if (is.na(runner_name)) stop("--runner= required")

# Load runner config
cfg_dir <- "runners"
cfg_path <- file.path(cfg_dir, paste0(runner_name, ".json"))
if (!file.exists(cfg_path)) stop(sprintf("runner config not found: %s", cfg_path))
cfg <- fromJSON(cfg_path, simplifyVector = FALSE)

root_dir <- normalizePath(file.path(cfg_dir, ".."))

unlink(file.path(root_dir, "results", runner_name, "errors.csv"))

# ── Task definitions (shared across all runners) ──────
all_tasks <- list(
  list(id = "01_fib",       name = "Fibonacci (n=50)",
       args = function() list(50L)),
  list(id = "02_vectorsum", name = "Vector Sum (1e7)",
       args = function() list(runif(1e7))),
  list(id = "03_transpose",    name = "Matrix Transpose (500x500)",
       args = function() list(matrix(runif(500*500), 500, 500))),
  list(id = "04_strings",   name = "String Concatenation (1e4)",
       args = function() list(replicate(10000, paste0(sample(letters, 50, T), collapse = "")), ",")),
  list(id = "05_dataframe", name = "Data Frame Filtering (1e5)",
       args = function() list(data.frame(x = rnorm(1e5), y = abs(rnorm(1e5)) + 0.1,
                                         grp = factor(sample(letters[1:10], 1e5, T)),
                                         stringsAsFactors = FALSE))),
  list(id = "06_na_prop",   name = "NA Propagation (1e6)",
       args = function() { x <- runif(1e6); x[sample(1e6, 5e4)] <- NA; list(x) }),
  list(id = "07_parallel",  name = "Parallel Sum (1e7)",
       args = function() list(runif(1e7))),
  list(id = "09_protect",   name = "PROTECT Stress (49k)",
       args = function() list(49000L)),
  list(id = "10_blas_matmul", name = "BLAS Matmul (N=256)",
       args = function() { n <- 256L; list(matrix(runif(n * n), n, n),
                                           matrix(runif(n * n), n, n)) }),
  list(id = "11_crossprod", name = "Cross-product (500x50)",
       args = function() list(matrix(runif(25000), 500, 50))),
  list(id = "12_cholesky",  name = "Cholesky (200x200)",
       args = function() { n <- 200L; A <- crossprod(matrix(runif(n * n), n, n)) + diag(n); list(A) }),
  list(id = "13_lm",        name = "Linear Model (n=5000, p=20)",
       args = function() { n <- 5000L; p <- 20L; X <- matrix(runif(n * p), n, p); y <- runif(n); list(X, y) }),
  list(id = "14_rowsums",   name = "Row Sums (1000x500)",
       args = function() list(matrix(runif(500000), 1000, 500))),
  list(id = "15_elem_ops",  name = "Element-wise ops (1e6)",
       args = function() list(runif(1e6, -5, 5))),
  list(id = "16_rowcol_means", name = "Row/Col Means (500x1000)",
       args = function() list(matrix(runif(500000), 500, 1000))),
  list(id = "17_broadcast", name = "Vector + Scalar (1e7)",
       args = function() list(runif(1e7), 3.14)),
  list(id = "18_sort",      name = "Sort (1e6)",
       args = function() list(runif(1e6))),
  list(id = "19_cumsum",    name = "Cumulative Sum (1e7)",
       args = function() list(runif(1e7))),
  list(id = "20_rnorm",     name = "Random Normal (1e6)",
       args = function() list(1000000L)),
  list(id = "21_string_nchar", name = "String Nchar (1e4)",
       args = function() list(replicate(10000, paste0(sample(letters, 50, TRUE), collapse = "")))),
  list(id = "22_which_na",  name = "Which NA (1e6)",
       args = function() { x <- runif(1e6); x[sample(1e6, 5e4)] <- NA; list(x) }),
  list(id = "23_altrep_sum", name = "ALTREP Sum (1:1e7)",
       args = function() list(1:1e7)),
  list(id = "24_altrep_read", name = "ALTREP Read (1:1e7)",
       args = function() list(1:1e7)),
  list(id = "25_altrep_create", name = "ALTREP Create (n=1e6)",
       args = function() list(1000000L)),
  list(id = "26_comptime_dispatch", name = "Comptime Dispatch (3x2048, reps=256)",
       args = function() list(list(
         runif(2048),
         sample.int(100L, 2048, replace = TRUE),
         sample(c(FALSE, TRUE), 2048, replace = TRUE)
       ))),
  list(id = "27_struct_convert", name = "Struct Convert (10 fields)",
       args = function() list(list(
         id = 42L,
         count = 7L,
         level = 3L,
         flag = TRUE,
         enabled = FALSE,
         ratio = 1.25,
         offset = -3.5,
         scale = 9.75,
         weights = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5),
         indices = c(3L, 1L, 4L, 1L, 5L, 9L, 2L, 6L)
       ))),
  list(id = "28_na_prop_vary", name = "NA Proportion Sweep (6x1e6)",
       args = function() {
         base <- runif(1e6)
         with_na <- function(prop) {
           out <- base
           n_na <- as.integer(length(out) * prop)
           if (n_na > 0L) out[sample.int(length(out), n_na)] <- NA_real_
           out
         }
         list(list(
           p0 = with_na(0.000),
           p0_1 = with_na(0.001),
           p1 = with_na(0.010),
           p5 = with_na(0.050),
           p25 = with_na(0.250),
           p50 = with_na(0.500)
         ))
       }),
  list(id = "29_scale_law", name = "Scale Law Mix (1K..4M)",
       args = function() list(list(
         n1k = runif(1024),
         n16k = runif(16384),
         n256k = runif(262144),
         n1m = runif(1048576),
         n4m = runif(4194304)
       ))),
  list(id = "30_arena_vs_rmalloc", name = "Arena vs R_alloc vs malloc (100x1K)",
       args = function() list(runif(1024))),
  list(id = "31_prot_overhead", name = "Protection Overhead (5x4096x64)",
       args = function() list(runif(64))),
  list(id = "32_longjmp_safety", name = "Longjmp Safety (4x512x64)",
       args = function() list(runif(64))),
  list(id = "34_translate_c_cost", name = "Math Call Cost (4x512x2048)",
       args = function() list(runif(2048, 0.1, 1.0))),
  list(id = "35_string_variants", name = "String Variants (1e4x24)",
       args = function() {
         x <- replicate(10000, paste0(sample(letters, 24, TRUE), collapse = ""))
         tagged <- seq.int(1L, length(x), by = 10L)
         x[tagged] <- paste0("abc", substring(x[tagged], 4L))
         list(x)
       }),
  list(id = "36_parallel_scaling", name = "Parallel Scaling (1/2/4/8/16 x 2^20)",
       args = function() list(rep.int(as.double(1:8), 131072L))),
  list(id = "37_memory_bandwidth", name = "Memory Bandwidth (3x2x2^20)",
       args = function() list(rep.int(as.double(1:8), 131072L))),
  list(id = "38_owned_altrep_real_sum", name = "Owned ALTREP Real Sum (n=1e6)",
       args = function() list(1000000L)),
  list(id = "39_owned_altrep_int_sum", name = "Owned ALTREP Integer Sum (n=1e6)",
       args = function() list(1000000L)),
  list(id = "40_owned_altrep_logical_sum", name = "Owned ALTREP Logical Sum (n=1e6)",
       args = function() list(1000000L)),
  list(id = "41_owned_altrep_int_min", name = "Owned ALTREP Integer Min (n=1e6)",
       args = function() list(1000000L)),
  list(id = "42_owned_altrep_int_max", name = "Owned ALTREP Integer Max (n=1e6)",
       args = function() list(1000000L)),
  list(id = "43_owned_altrep_int_argmin", name = "Owned ALTREP Integer Argmin (n=1e6)",
       args = function() list(1000000L)),
  list(id = "44_owned_altrep_int_argmax", name = "Owned ALTREP Integer Argmax (n=1e6)",
       args = function() list(1000000L)),
  list(id = "45_owned_altrep_logical_min", name = "Owned ALTREP Logical Min (n=1e6)",
       args = function() list(1000000L)),
  list(id = "46_owned_altrep_logical_max", name = "Owned ALTREP Logical Max (n=1e6)",
       args = function() list(1000000L)),
  list(id = "47_owned_altrep_logical_argmin", name = "Owned ALTREP Logical Argmin (n=1e6)",
       args = function() list(1000000L)),
  list(id = "48_owned_altrep_logical_argmax", name = "Owned ALTREP Logical Argmax (n=1e6)",
       args = function() list(1000000L)),
  list(id = "33_binary_growth", name = "Binary Footprint (bytes/export)",
       args = function() list(),
       expr = function(cfg, root_dir) substitute(runner_artifact_metrics(cfg_arg, root_arg),
                                                 list(cfg_arg = cfg, root_arg = root_dir)))
)

if (!is.null(task_filter)) {
     task_numbers <- vapply(
          all_tasks,
          function(task) as.integer(sub("_.*$", "", task$id)),
          integer(1)
     )
     all_tasks <- all_tasks[task_numbers %in% task_filter]
}

cat(sprintf("\n======== %s (%s) ========\n", runner_name, cfg$label))

`%||%` <- function(x, y) if (is.null(x)) y else x
call_type <- cfg$call_type %||% ".Call"

# Load .so (skip for R baseline)
if (call_type != "r") {
  so_path <- file.path(root_dir, cfg$so_path)
  if (!file.exists(so_path)) stop(sprintf("library not found: %s", so_path))
  tryCatch(dyn.load(so_path),
           error = function(e) stop(sprintf("load error: %s", conditionMessage(e))))
} else {
  source(file.path(root_dir, "src/r/run_all.R"))
}

results_list <- list()
exports <- cfg$exports
n_pass <- 0; n_fail <- 0; n_na <- 0

for (task in all_tasks) {
  tid <- task$id
     task_expr <- if (is.function(task$expr)) task$expr(cfg, root_dir) else NULL
     cfun <- exports[[tid]]
     if (is.null(task_expr) && is.null(cfun)) {
    n_na <- n_na + 1
    cat(sprintf("  %-14s [N/A]\n", tid))
    results_list[[length(results_list) + 1]] <- data.frame(
      runner = runner_name, task = tid, status = "N/A",
      mean_ms = NA, sd_ms = NA, cv_pct = NA, rss_kb = NA,
      cold_start_ms = NA, n_iterations = NA, error = NA_character_,
      stringsAsFactors = FALSE)
    next
  }

  args <- task$args()

  # Cold start
     cs <- timed_call(cfun, args, call_type, expr = task_expr)
  log_cold_start(runner_name, tid, cs$wall_ms, dir = file.path(root_dir, "results"))
  if (!is.na(cs$error)) {
    n_fail <- n_fail + 1
    cat(sprintf("  %-14s [FAIL] %s\n", tid, cs$error))
    log_error(runner_name, tid, cs$error, dir = file.path(root_dir, "results"))
    results_list[[length(results_list) + 1]] <- data.frame(
      runner = runner_name, task = tid, status = "FAIL",
      mean_ms = NA, sd_ms = NA, cv_pct = NA, rss_kb = NA,
      cold_start_ms = round(cs$wall_ms, 3), n_iterations = NA,
      error = cs$error, stringsAsFactors = FALSE)
    next
  }

  # Microbenchmark measurement
     bm <- benchmark_call(cfun, args, call_type, warmup = 10L, times = 100L, expr = task_expr)
  if (!is.na(bm$error)) {
    n_fail <- n_fail + 1
    cat(sprintf("  %-14s [FAIL] %s\n", tid, bm$error))
    log_error(runner_name, tid, bm$error, dir = file.path(root_dir, "results"))
    results_list[[length(results_list) + 1]] <- data.frame(
      runner = runner_name, task = tid, status = "FAIL",
      mean_ms = NA, sd_ms = NA, cv_pct = NA, rss_kb = NA,
      cold_start_ms = round(cs$wall_ms, 3), n_iterations = NA,
      error = bm$error, stringsAsFactors = FALSE)
    next
  }

  n_pass <- n_pass + 1
  cat(sprintf("  %-14s mean=%8.4fms sd=%7.4fms cv=%5.2f%% rss=%dKB runs=%d\n",
              tid, bm$mean_ms, bm$sd_ms, bm$cv_pct, bm$peak_rss, bm$n_runs))

  # Write per-run CSVs
  runs_df <- data.frame(
    runner      = runner_name,
    task        = tid,
    iteration   = seq_len(length(bm$times)),
    wall_ms     = round(bm$times, 4),
    peak_rss_kb = c(rep(NA_integer_, length(bm$times) - 1), bm$peak_rss),
    error       = NA_character_,
    stringsAsFactors = FALSE
  )
  write_csv(runs_df, file.path(root_dir, "results", runner_name, sprintf("task_%s.csv", tid)))

  results_list[[length(results_list) + 1]] <- data.frame(
    runner        = runner_name,
    task          = tid,
    status        = "PASS",
    mean_ms       = round(bm$mean_ms, 4),
    sd_ms         = round(bm$sd_ms, 4),
    cv_pct        = round(bm$cv_pct, 2),
    rss_kb        = bm$peak_rss,
    cold_start_ms = round(cs$wall_ms, 3),
    n_iterations  = bm$n_runs,
    error         = NA_character_,
    stringsAsFactors = FALSE
  )
}

# Write per-runner summary
summary <- do.call(rbind, results_list)
write_csv(summary, file.path(root_dir, "results", sprintf("%s_summary.csv", runner_name)))

# Print summary table
cat(sprintf("\n  Results: %d PASS, %d FAIL, %d N/A\n\n", n_pass, n_fail, n_na))
cat(sprintf("  %-8s %8s %7s %5s %8s %5s\n",
            "Task", "Mean(ms)", "SD", "CV%", "RSS", "Runs"))
cat(sprintf("  %-8s %8s %7s %5s %8s %5s\n",
            "------", "------", "--", "---", "---", "----"))
for (i in seq_len(nrow(summary))) {
  s <- summary[i, ]
  cat(sprintf("  %-8s %8.4f %7.4f %5.2f %8d %5d\n",
              s$task, s$mean_ms, s$sd_ms, s$cv_pct, s$rss_kb, s$n_iterations))
}

invisible()
