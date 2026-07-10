#!/usr/bin/env Rscript
# Loads a runner.json, runs tasks, outputs CSV. Called per-runner by run_benchmarks.R.
# Usage: Rscript runner_subprocess.R --runner=zigr --results-dir=results/runs/<run_id> [--tasks=1,2]

source("lib/harness.R")
library(methods)
library(jsonlite)

cli <- commandArgs(trailingOnly = TRUE)
runner_name <- NA
task_filter <- NULL
check_only <- FALSE
results_dir_arg <- NULL
for (a in cli) {
  if (grepl("^--runner=", a)) runner_name <- sub("^--runner=", "", a)
  if (grepl("^--tasks=", a))  task_filter <- as.integer(strsplit(sub("^--tasks=", "", a), ",")[[1]])
  if (a == "--check-only") check_only <- TRUE
  if (grepl("^--results-dir=", a)) results_dir_arg <- sub("^--results-dir=", "", a)
}
if (is.na(runner_name)) stop("--runner= required")

cfg_dir <- "runners"
cfg_path <- file.path(cfg_dir, paste0(runner_name, ".json"))
if (!file.exists(cfg_path)) stop(sprintf("runner config not found: %s", cfg_path))
cfg <- fromJSON(cfg_path, simplifyVector = FALSE)

root_dir <- normalizePath(file.path(cfg_dir, ".."))
source(file.path(root_dir, "lib", "run_manifest.R"))
if (!check_only && is.null(results_dir_arg)) stop("--results-dir= is required for benchmark execution")
results_dir <- normalizePath(if (is.null(results_dir_arg)) file.path(root_dir, "results") else results_dir_arg, mustWork = FALSE)
run_id <- NA_character_
allowed_na_tasks <- character(0)
timing_policy <- benchmark_timing_policy()
if (!check_only) {
  run_metadata <- read_run_manifest(results_dir)
  run_id <- as.character(run_metadata$run_id)
  allowed_na_tasks <- sort(run_manifest_values(run_metadata$allowed_na_tasks))
  if (!is.null(run_metadata$timing_policy)) timing_policy <- run_metadata$timing_policy
  validate_timing_policy(timing_policy)
  if (!(runner_name %in% run_manifest_values(run_metadata$runners))) {
    stop(sprintf("runner %s is not declared by run manifest %s", runner_name, run_manifest_path(results_dir)))
  }
  staging_results_dir <- file.path(results_dir, ".staging")
  dir.create(file.path(staging_results_dir, runner_name), recursive = TRUE, showWarnings = FALSE)
  unlink(file.path(staging_results_dir, runner_name, "errors.csv"))
}

# Tasks are shared across all runners. IDs match exports in runner JSONs.
all_tasks <- list(
  list(id = "01_vectorsum", name = "Vector Sum (1e7)",
       args = function() list(runif(1e7))),
  list(id = "02_elem_ops", name = "Element-wise ops (1e6)",
       args = function() list(runif(1e6, -5, 5))),
  list(id = "03_memcpy_bandwidth", name = "Memory Bandwidth (1e7)",
       args = function() list(rep.int(as.double(1:8), 131072L))),
  list(id = "04_sort", name = "Sort (1e6)",
       args = function() list(runif(1e6))),
  list(id = "05_fib_recursive", name = "Fibonacci (n=25)",
       args = function() list(25L)),
  list(id = "06_broadcast", name = "Vector + Scalar (1e7)",
       args = function() list(runif(1e7), 3.14)),
  list(id = "07a_protect_shallow", name = "PROTECT Shallow (10 items)",
       args = function() list(runif(10))),
  list(id = "07b_protect_scaling", name = "PROTECT Scaling (100k items)",
       args = function() list(runif(100000))),
  list(id = "08_type_dispatch", name = "Type Dispatch (3 types x 2048)",
       args = function() list(list(runif(2048), 1:2048, rep("x", 2048)))),
  list(id = "09_longjmp_safety", name = "Longjmp Safety (4x512x64)",
       args = function() list(runif(64))),
  list(id = "10_sexp_create", name = "SEXP Create (100k)",
       args = function() list(100000L)),
  list(id = "11_sexp_inspect", name = "SEXP Inspect (10k)",
       args = function() list(list(runif(1), 1L, "x", list(), NULL))),
  list(id = "12_matrix_transpose", name = "Matrix Transpose (500x500)",
       args = function() list(matrix(runif(500*500), 500, 500))),
  list(id = "13_matrix_rowsums", name = "Row Sums (1000x500)",
       args = function() list(matrix(runif(500000), 1000, 500))),
  list(id = "14_matrix_rowcol_means", name = "Row/Col Means (500x1000)",
       args = function() list(matrix(runif(500000), 500, 1000))),
  list(id = "15_dataframe_filter", name = "Data Frame Filter (100k rows)",
       args = function() list(data.frame(x = rnorm(1e5), y = abs(rnorm(1e5)) + 0.1,
                                         grp = factor(sample(letters[1:10], 1e5, T)),
                                         stringsAsFactors = FALSE))),
  list(id = "16_list_access", name = "List Access (1000 elements)",
       args = function() list(replicate(1000, runif(100), simplify = FALSE))),
  list(id = "17_string_concat", name = "String Concatenation (10k x 24 chars)",
       args = function() list(replicate(10000, paste0(sample(letters, 24, T), collapse = "")))),
  list(id = "18_string_nchar", name = "String Nchar (10k, 5% NA)",
       args = function() { x <- replicate(10000, paste0(sample(letters, 50, TRUE), collapse = "")); x[sample(10000, 500)] <- NA_character_; list(x) }),
  list(id = "19_string_encoding", name = "String Encoding (10k ASCII)",
       args = function() list(replicate(10000, paste0(sample(letters, 24, TRUE), collapse = "")))),
  list(id = "20_factor_ops", name = "Factor Ops (10k / 100 levels)",
       args = function() list(sample(letters[1:100], 10000, replace = TRUE))),
  list(id = "21_attrib_ops", name = "Attribute Ops (1M vector)",
       args = function() list(runif(1e6))),
  list(id = "22_s4_slot_access", name = "S4 Slot Access",
       args = function() list(1.0)),
  list(id = "23_na_propagation", name = "NA Propagation (1e6, 5% NA)",
       args = function() { x <- runif(1e6); x[sample(1e6, 5e4)] <- NA; list(x) }),
  list(id = "24_long_vector_idx", name = "Long Vector Index (2^31+1 ALTREP)",
       args = function() list(1:1e7)),
  list(id = "25_l1_arithmetic", name = "L1 Arithmetic (4000 x 2500 passes)",
       args = function() list(runif(4000))),
  list(id = "26_matmul", name = "Matmul (256x256)",
       args = function() { n <- 256L; list(matrix(runif(n * n), n, n),
                                           matrix(runif(n * n), n, n)) }),
  list(id = "27_crossprod", name = "Cross-product (500x50)",
       args = function() list(matrix(runif(25000), 500, 50))),
  list(id = "28_cholesky", name = "Cholesky (200x200)",
       args = function() { n <- 200L; A <- crossprod(matrix(runif(n * n), n, n)) + diag(n); list(A) }),
  list(id = "29_lm_fit", name = "Linear Model (n=5000, p=20)",
       args = function() { n <- 5000L; p <- 20L; X <- matrix(runif(n * p), n, p); y <- runif(n); list(X, y) }),
  list(id = "30_altrep_create", name = "ALTREP Create (n=1e6)",
       args = function() list(1000000L)),
  list(id = "31_altrep_materialize", name = "ALTREP Materialize (1e6)",
       args = function() list(1000000L)),
  list(id = "32_altrep_elt_walk", name = "ALTREP Elt Walk (1e6)",
       args = function() list(1000000L)),
  list(id = "33_altrep_region_read", name = "ALTREP Region Read (1e6)",
       args = function() list(1000000L)),
  list(id = "34_altrep_sum_via_R", name = "ALTREP Sum via R (1e6)",
       args = function() list(1000000L)),
  list(id = "35_altrep_sum_native", name = "ALTREP Sum Native (1e6)",
       args = function() list(1000000L)),
  list(id = "36_altrep_min_max", name = "ALTREP Min/Max (1e6)",
       args = function() list(1000000L)),
  list(id = "37_altrep_no_na_query", name = "ALTREP No-NA Query (1e6)",
       args = function() list(1000000L)),
  list(id = "38_struct_convert", name = "Struct Convert (10 fields)",
       args = function() list(list(id = 42L, count = 7L, level = 3L, flag = TRUE,
                                   enabled = FALSE, ratio = 1.25, offset = -3.5, scale = 9.75,
                                   weights = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5),
                                   indices = c(3L, 1L, 4L, 1L, 5L, 9L, 2L, 6L)))),
  list(id = "39_r_eval", name = "R Eval (sum(x) + mean(x))",
       args = function() list(runif(1e6))),
  list(id = "40_r_tryeval", name = "R TryEval (stop error catch)",
       args = function() list(runif(1))),
  list(id = "41_serialize_roundtrip", name = "Serialize Roundtrip (1e6)",
       args = function() list(runif(1e6))),
  list(id = "42_external_ptr", name = "External Pointer",
       args = function() list(1L)),
  list(id = "43_rng_stress", name = "RNG Stress (1M norm_rand)",
       args = function() list(1000000L))
)

source(file.path(root_dir, "lib", "task_manifest.R"))
manifest <- load_task_manifest(root_dir)
validate_runner_config(manifest, cfg, runner_name)
validate_task_specs(manifest, all_tasks)
all_tasks <- order_task_specs(manifest, all_tasks)

if (!is.null(task_filter)) {
  task_numbers <- vapply(
    all_tasks,
    function(task) as.integer(sub("([0-9]+).*", "\\1", task$id)),
    integer(1))
  all_tasks <- all_tasks[task_numbers %in% task_filter]
}

cat(sprintf("Runner: %s (%s)\n", runner_name, cfg$label))

`%||%` <- function(x, y) if (is.null(x)) y else x
call_type <- cfg$call_type %||% ".Call"

timing_summary_fields <- function(bm = NULL) {
  if (is.null(bm)) {
    return(list(
      warmup_iterations = as.integer(timing_policy$warmup_iterations),
      block_size = as.integer(timing_policy$block_size),
      max_iterations = as.integer(timing_policy$max_iterations),
      convergence_window_blocks = as.integer(timing_policy$convergence_window_blocks),
      convergence_cv_threshold_pct = as.numeric(timing_policy$convergence_cv_threshold_pct),
      convergence_cv_pct = NA_real_,
      stopping_condition = "not_measured",
      converged = NA,
      timer_noise_floor_ms = as.numeric(timing_policy$timer_noise_floor_ms),
      timer_noise_status = "not_measured",
      rss_metric = as.character(timing_policy$rss_metric),
      gc_policy = as.character(timing_policy$gc_policy)
    ))
  }
  list(
    warmup_iterations = as.integer(bm$warmup_iterations),
    block_size = as.integer(bm$block_size),
    max_iterations = as.integer(bm$max_iterations),
    convergence_window_blocks = as.integer(bm$convergence_window_blocks),
    convergence_cv_threshold_pct = as.numeric(bm$convergence_cv_threshold_pct),
    convergence_cv_pct = as.numeric(bm$convergence_cv_pct),
    stopping_condition = as.character(bm$stopping_condition),
    converged = isTRUE(bm$converged),
    timer_noise_floor_ms = as.numeric(bm$timer_noise_floor_ms),
    timer_noise_status = as.character(bm$timer_noise_status),
    rss_metric = as.character(bm$rss_metric),
    gc_policy = as.character(timing_policy$gc_policy)
  )
}

if (call_type != "r") {
  so_path <- file.path(root_dir, cfg$so_path)
  if (!file.exists(so_path)) stop(sprintf("library not found: %s", so_path))
  tryCatch(dyn.load(so_path),
           error = function(e) stop(sprintf("load error: %s", conditionMessage(e))))

  extra_so_paths <- cfg$extra_so_paths %||% list()
  for (extra_so in extra_so_paths) {
    extra_path <- file.path(root_dir, extra_so)
    if (!file.exists(extra_path)) stop(sprintf("library not found: %s", extra_path))
    tryCatch(dyn.load(extra_path),
             error = function(e) stop(sprintf("load error: %s", conditionMessage(e))))
  }
}

# ── H.2: Load R baseline as correctness reference ──
source(file.path(root_dir, "src/r/run_all.R"))
r_cfg_path <- file.path(root_dir, "runners", "r.json")
r_ref <- fromJSON(r_cfg_path, simplifyVector = FALSE)$exports
validate_r_reference_map(manifest, r_ref)

if (check_only) {
  validate_task_arguments(manifest, all_tasks)
  cat(sprintf("Coverage preflight passed for %s (%d task specs)\n", runner_name, length(all_tasks)))
  quit(save = "no", status = 0, runLast = FALSE)
}

# H.2 comparison stays beside the runner because it has one production caller.
same_attributes <- function(expected, actual) {
  expected_names <- names(expected)
  actual_names <- names(actual)
  if (!identical(sort(expected_names), sort(actual_names))) return(FALSE)
  if (length(expected_names) == 0L) return(TRUE)
  all(vapply(expected_names, function(name) identical(expected[[name]], actual[[name]]), logical(1)))
}

compare_correctness <- function(expected, actual, path = "result") {
  result <- function(ok, message = "") list(ok = isTRUE(ok), message = message)
  if (is.null(expected) && is.null(actual)) return(result(TRUE))
  if (is.null(expected) || is.null(actual)) return(result(FALSE, sprintf("%s is NULL on one side", path)))
  if (!identical(typeof(expected), typeof(actual))) {
    return(result(FALSE, sprintf("%s type differs", path)))
  }
  if (!same_attributes(attributes(expected), attributes(actual))) {
    return(result(FALSE, sprintf("%s attributes differ", path)))
  }
  if (length(expected) != length(actual)) return(result(FALSE, sprintf("%s length differs", path)))

  if (is.numeric(expected) || is.complex(expected)) {
    if (!identical(is.na(expected), is.na(actual))) return(result(FALSE, sprintf("%s NA positions differ", path)))
    if (!identical(is.nan(expected), is.nan(actual))) return(result(FALSE, sprintf("%s NA and NaN kinds differ", path)))
    if (!isTRUE(all.equal(
      expected,
      actual,
      tolerance = sqrt(.Machine$double.eps),
      check.attributes = FALSE
    ))) {
      return(result(FALSE, sprintf("%s values differ", path)))
    }
    return(result(TRUE))
  }
  if (is.character(expected)) {
    if (!identical(is.na(expected), is.na(actual))) return(result(FALSE, sprintf("%s NA positions differ", path)))
    if (!identical(Encoding(expected), Encoding(actual))) return(result(FALSE, sprintf("%s encodings differ", path)))
    if (!identical(expected[!is.na(expected)], actual[!is.na(actual)])) {
      return(result(FALSE, sprintf("%s string values differ", path)))
    }
    return(result(TRUE))
  }
  if (is.list(expected)) {
    for (index in seq_along(expected)) {
      nested <- compare_correctness(expected[[index]], actual[[index]], sprintf("%s[[%d]]", path, index))
      if (!isTRUE(nested$ok)) return(nested)
    }
    return(result(TRUE))
  }
  if (identical(expected, actual)) return(result(TRUE))
  result(FALSE, sprintf("%s values differ", path))
}

capture_result <- function(fn) {
  error <- NA_character_
  value <- tryCatch(fn(), error = function(e) {
    error <<- conditionMessage(e)
    NULL
  })
  list(ok = is.na(error), value = value, error = error)
}

result_preview <- function(value) {
  gsub(",", " ", substr(paste(deparse(value), collapse = ""), 1, 120))
}

results_list <- list()
exports <- cfg$exports
n_pass <- 0; n_fail <- 0; n_na <- 0

for (task in all_tasks) {
  tid <- task$id
  manifest_row <- match(tid, manifest$task)
  if (is.na(manifest_row)) stop(sprintf("task %s is absent from the manifest", tid))
  correctness_policy <- manifest$correctness_policy[[manifest_row]]
  expected_return <- manifest$expected_return[[manifest_row]]
  correctness_status <- if (call_type == "r") "REFERENCE" else "NOT_VALIDATED"
  correctness_message <- if (call_type == "r") "R reference runner" else ""
  task_expr <- if (is.function(task$expr)) task$expr(cfg, root_dir) else NULL
  cfun <- exports[[tid]]
  if (call_type == ".Call" && !is.null(cfun)) {
    package_overrides <- cfg$package_overrides %||% list()
    package_name <- package_overrides[[tid]]
    if (!is.null(package_name)) {
      cfun <- getNativeSymbolInfo(cfun, PACKAGE = package_name)$address
    }
  }
  if (is.null(task_expr) && is.null(cfun)) {
    n_na <- n_na + 1
    na_allowed <- tid %in% allowed_na_tasks
    if (!na_allowed) {
      n_fail <- n_fail + 1
      correctness_message <- "no executable for this runner and task is not explicitly allowed N/A"
    }
    cat(sprintf("  %-14s [N/A]\n", tid))
    results_list[[length(results_list) + 1]] <- data.frame(
      runner = runner_name, task = tid, status = "N/A",
      mean_ms = NA, median_ms = NA, min_ms = NA, max_ms = NA,
      sd_ms = NA, cv_pct = NA, rss_kb = NA,
      cold_start_ms = NA, n_iterations = NA, error = NA_character_,
      correctness_status = "NOT_APPLICABLE",
      correctness_policy = correctness_policy,
      correctness_message = if (na_allowed) "no executable for this runner" else correctness_message,
      stringsAsFactors = FALSE,
      timing_summary_fields())
    next
  }

  args <- task$args()

  # ── H.2 Correctness validation is a hard timing prerequisite ──
  if (call_type != "r") {
    invoke_native <- function() {
      if (call_type == ".Call") return(do.call(.Call, c(list(cfun), args)))
      if (call_type == ".C") return(do.call(.C, c(list(cfun), args)))
      stop(sprintf("unsupported correctness call type: %s", call_type))
    }
    native_eval <- capture_result(invoke_native)
    if (!native_eval$ok) {
      correctness_status <- "FAIL"
      correctness_message <- sprintf("native call failed: %s", native_eval$error)
    } else {
      native_contract <- validate_result_contract(native_eval$value, expected_return)
      if (!native_contract$ok) {
        correctness_status <- "FAIL"
        correctness_message <- paste("native result:", native_contract$message)
      } else if (identical(correctness_policy, "r_reference")) {
        ref_name <- r_ref[[tid]]
        if (is.null(ref_name) || !nzchar(ref_name) || !exists(ref_name, mode = "function")) {
          correctness_status <- "NOT_VALIDATED"
          correctness_message <- "R reference function is missing"
        } else {
          ref_eval <- capture_result(function() do.call(get(ref_name, mode = "function"), args))
          if (!ref_eval$ok) {
            correctness_status <- "FAIL"
            correctness_message <- sprintf("R reference failed: %s", ref_eval$error)
          } else {
            ref_contract <- validate_result_contract(ref_eval$value, expected_return)
            if (!ref_contract$ok) {
              correctness_status <- "FAIL"
              correctness_message <- paste("R reference result:", ref_contract$message)
            } else {
              comparison <- compare_correctness(ref_eval$value, native_eval$value)
              if (!isTRUE(comparison$ok)) {
                correctness_status <- "FAIL"
                correctness_message <- sprintf(
                  "H.2 mismatch: %s; expected '%s' got '%s'",
                  comparison$message,
                  result_preview(ref_eval$value),
                  result_preview(native_eval$value)
                )
              } else {
                correctness_status <- "PASS"
              }
            }
          }
        }
      } else {
        correctness_status <- "PASS"
      }
    }
  }

  if (correctness_status %in% c("FAIL", "NOT_VALIDATED")) {
    n_fail <- n_fail + 1
    cat(sprintf("  %-14s [%s] correctness: %s\n", tid, correctness_status, correctness_message))
    log_error(runner_name, tid, correctness_message, dir = staging_results_dir)
    results_list[[length(results_list) + 1]] <- data.frame(
      runner = runner_name, task = tid, status = "FAIL",
      mean_ms = NA, median_ms = NA, min_ms = NA, max_ms = NA,
      sd_ms = NA, cv_pct = NA, rss_kb = NA,
      cold_start_ms = NA, n_iterations = NA, error = correctness_message,
      correctness_status = correctness_status,
      correctness_policy = correctness_policy,
      correctness_message = correctness_message,
      stringsAsFactors = FALSE,
      timing_summary_fields())
    next
  }

  # Do not retain large correctness results while timing the same input.
  native_eval <- NULL
  ref_eval <- NULL
  native_contract <- NULL
  ref_contract <- NULL
  comparison <- NULL

  cs <- timed_call(cfun, args, call_type, expr = task_expr)
  log_cold_start(runner_name, tid, cs$wall_ms, run_id = run_id, dir = staging_results_dir)
  if (!is.na(cs$error)) {
    n_fail <- n_fail + 1
    cat(sprintf("  %-14s [FAIL] %s\n", tid, cs$error))
    log_error(runner_name, tid, cs$error, dir = staging_results_dir)
    results_list[[length(results_list) + 1]] <- data.frame(
      runner = runner_name, task = tid, status = "FAIL",
      mean_ms = NA, median_ms = NA, min_ms = NA, max_ms = NA,
      sd_ms = NA, cv_pct = NA, rss_kb = NA,
      cold_start_ms = round(cs$wall_ms, 3), n_iterations = NA,
      error = cs$error,
      correctness_status = correctness_status,
      correctness_policy = correctness_policy,
      correctness_message = correctness_message,
      stringsAsFactors = FALSE,
      timing_summary_fields())
    next
  }

  bm <- benchmark_call(
    cfun,
    args,
    call_type,
    warmup = as.integer(timing_policy$warmup_iterations),
    block_size = as.integer(timing_policy$block_size),
    max_iter = as.integer(timing_policy$max_iterations),
    cv_threshold = as.numeric(timing_policy$convergence_cv_threshold_pct),
    convergence_blocks = as.integer(timing_policy$convergence_window_blocks),
    timer_noise_floor_ms = as.numeric(timing_policy$timer_noise_floor_ms),
    rss_metric = as.character(timing_policy$rss_metric),
    expr = task_expr
  )
  if (!is.na(bm$error)) {
    n_fail <- n_fail + 1
    cat(sprintf("  %-14s [FAIL] %s\n", tid, bm$error))
    log_error(runner_name, tid, bm$error, dir = staging_results_dir)
    results_list[[length(results_list) + 1]] <- data.frame(
      runner = runner_name, task = tid, status = "FAIL",
      mean_ms = NA, median_ms = NA, min_ms = NA, max_ms = NA,
      sd_ms = NA, cv_pct = NA, rss_kb = NA,
      cold_start_ms = round(cs$wall_ms, 3), n_iterations = NA,
      error = bm$error,
      correctness_status = correctness_status,
      correctness_policy = correctness_policy,
      correctness_message = correctness_message,
      stringsAsFactors = FALSE,
      timing_summary_fields())
    next
  }

  n_pass <- n_pass + 1
  cat(sprintf("  %-14s mean=%8.4fms median=%8.4fms sd=%7.4fms cv=%5.2f%% rss=%dKB runs=%d\n",
              tid, bm$mean_ms, bm$median_ms, bm$sd_ms, bm$cv_pct, bm$peak_rss, bm$n_runs))

  runs_df <- data.frame(
    runner      = runner_name,
    task        = tid,
    iteration   = seq_len(length(bm$times)),
    wall_ms     = round(bm$times, 4),
    peak_rss_kb = c(rep(NA_integer_, length(bm$times) - 1), bm$peak_rss),
    error       = NA_character_,
    run_id      = run_id,
    correctness_status = correctness_status,
    correctness_policy = correctness_policy,
    correctness_message = correctness_message,
    stringsAsFactors = FALSE
  )
  write_csv(runs_df, file.path(staging_results_dir, runner_name, sprintf("task_%s.csv", tid)))

  results_list[[length(results_list) + 1]] <- data.frame(
    runner        = runner_name,
    task          = tid,
    status        = "PASS",
    mean_ms       = round(bm$mean_ms, 4),
    median_ms     = round(bm$median_ms, 4),
    min_ms        = round(bm$min_ms, 4),
    max_ms        = round(bm$max_ms, 4),
    sd_ms         = round(bm$sd_ms, 4),
    cv_pct        = round(bm$cv_pct, 2),
    rss_kb        = bm$peak_rss,
    cold_start_ms = round(cs$wall_ms, 3),
    n_iterations  = bm$n_runs,
    error         = NA_character_,
    correctness_status = correctness_status,
    correctness_policy = correctness_policy,
    correctness_message = correctness_message,
    stringsAsFactors = FALSE,
    timing_summary_fields(bm)
  )
}

summary <- do.call(rbind, results_list)
summary$run_id <- run_id
staged_summary <- file.path(staging_results_dir, sprintf("%s_summary.csv", runner_name))
write_csv(summary, staged_summary)
if (n_fail > 0L) {
  stop(sprintf(
    "runner %s failed correctness or timing validation for %d task(s); run cannot be completed",
    runner_name, n_fail
  ))
}
final_runner_dir <- file.path(results_dir, runner_name)
final_summary <- file.path(results_dir, sprintf("%s_summary.csv", runner_name))
if (dir.exists(final_runner_dir) || file.exists(final_summary)) {
  stop(sprintf("final result path already exists for runner %s", runner_name))
}
if (!file.rename(file.path(staging_results_dir, runner_name), final_runner_dir)) {
  stop(sprintf("cannot promote staged results for runner %s", runner_name))
}
if (!file.rename(staged_summary, final_summary)) {
  stop(sprintf("cannot promote staged summary for runner %s", runner_name))
}

cat(sprintf("  Results: %d PASS, %d FAIL, %d N/A\n", n_pass, n_fail, n_na))
for (i in seq_len(nrow(summary))) {
  s <- summary[i, ]
  cat(sprintf("  %-14s %8.4f %7.4f %8.4f %5.2f %8d %5d  %s\n",
              s$task, s$mean_ms, s$median_ms, s$sd_ms, s$cv_pct, s$rss_kb, s$n_iterations, s$status))
}
