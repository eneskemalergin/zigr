#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
run_dir <- NULL
for (arg in args) {
  if (grepl("^--run-dir=", arg)) run_dir <- sub("^--run-dir=", "", arg)
}
if (is.null(run_dir)) stop("--run-dir= is required")

root_dir <- normalizePath(".")
source(file.path(root_dir, "lib", "task_manifest.R"))
source(file.path(root_dir, "lib", "input_contract.R"))
source(file.path(root_dir, "lib", "r_provenance.R"))
source(file.path(root_dir, "lib", "source_ledger.R"))
source(file.path(root_dir, "lib", "run_manifest.R"))
results_dir <- normalizePath(run_dir, mustWork = FALSE)
run_metadata <- read_run_manifest(results_dir)
if (!identical(as.character(run_metadata$status), "complete")) stop("run must be complete before analysis")
validate_run_artifacts(results_dir, run_metadata)
out_csv <- file.path(results_dir, "analysis_summary.csv")
manifest <- load_task_manifest(root_dir)

# Use validated per-runner summaries as the statistical source of truth.
summary_column <- function(data, name, default = NA) {
  if (name %in% names(data)) data[[name]] else rep(default, nrow(data))
}

runner_dirs <- file.path(results_dir, run_manifest_values(run_metadata$runners))
rows <- list()

for (runner_dir in runner_dirs) {
  runner <- basename(runner_dir)
  summary <- read.csv(file.path(results_dir, paste0(runner, "_summary.csv")), stringsAsFactors = FALSE)
  manifest_rows <- match(summary$task, manifest$task)
  if (anyNA(manifest_rows)) stop(sprintf("summary for %s contains an unknown task", runner))
  rows[[length(rows) + 1L]] <- data.frame(
    runner = runner,
    task = as.character(summary$task),
    call_type = summary_column(summary, "call_type", NA_character_),
    matrix_group = task_matrix_group(summary$task),
    matrix_variant = task_matrix_variant(summary$task),
    category = manifest$category[manifest_rows],
    report_category = task_report_category(manifest$category[manifest_rows]),
    aggregate_comparable = manifest$aggregate[manifest_rows],
    comparison_note = manifest$comparison_note[manifest_rows],
    mean_ms = summary$mean_ms,
    median_ms = summary$median_ms,
    min_ms = summary$min_ms,
    max_ms = summary$max_ms,
    sd_ms = summary$sd_ms,
    cv_pct = summary$cv_pct,
    mean_rss_kb = summary$rss_kb,
    rss_metric = summary_column(summary, "rss_metric", "legacy_endpoint_delta_kb"),
    gc_policy = summary_column(summary, "gc_policy", "legacy"),
    cold_start_ms = summary$cold_start_ms,
    n_runs = summary$n_iterations,
    warmup_iterations = summary_column(summary, "warmup_iterations"),
    block_size = summary_column(summary, "block_size"),
    max_iterations = summary_column(summary, "max_iterations"),
    convergence_window_blocks = summary_column(summary, "convergence_window_blocks"),
    convergence_cv_threshold_pct = summary_column(summary, "convergence_cv_threshold_pct"),
    convergence_cv_pct = summary_column(summary, "convergence_cv_pct"),
    stopping_condition = summary_column(summary, "stopping_condition", "legacy"),
    converged = summary_column(summary, "converged"),
    timer_noise_floor_ms = summary_column(summary, "timer_noise_floor_ms"),
    timer_noise_status = summary_column(summary, "timer_noise_status", "legacy"),
    status = summary$status,
    stringsAsFactors = FALSE
  )
}

if (length(rows) == 0) {
  cat("No results found in ", results_dir, "\n")
  quit(save = "no", status = 0)
}

agg <- do.call(rbind, rows)

write.csv(agg, out_csv, row.names = FALSE)
cat(sprintf("Summary written to %s\n", out_csv))

cat("\nSummary\n")
cat(sprintf("%-8s %-14s %-14s %10s %8s %8s\n",
    "Runner", "Task", "Category", "Mean (ms)", "SD (ms)", "RSS (KB)"))
cat(sprintf("%-8s %-14s %-14s %10s %8s %8s\n",
    "------", "---", "--------", "--------", "------", "-------"))

for (i in seq_len(nrow(agg))) {
  cat(sprintf("%-8s %-14s %-14s %10.1f %8.2f %8.0f\n",
      agg$runner[i], agg$task[i], agg$report_category[i],
      agg$mean_ms[i], agg$sd_ms[i], agg$mean_rss_kb[i]))
}
