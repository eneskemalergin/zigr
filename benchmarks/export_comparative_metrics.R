#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
results_dir <- NULL
low_noise_cv_threshold <- NULL
meaningful_margin <- NULL
root_dir <- normalizePath(".")
source(file.path(root_dir, "lib", "task_manifest.R"))
source(file.path(root_dir, "lib", "input_contract.R"))
source(file.path(root_dir, "lib", "r_provenance.R"))
source(file.path(root_dir, "lib", "source_ledger.R"))
source(file.path(root_dir, "lib", "environment_manifest.R"))
source(file.path(root_dir, "lib", "run_manifest.R"))
manifest <- load_task_manifest(root_dir)

for (arg in args) {
  if (grepl("^--run-dir=", arg)) {
    results_dir <- sub("^--run-dir=", "", arg)
  } else if (grepl("^--results-dir=", arg)) {
    stop("--results-dir is retired; pass one completed run with --run-dir=")
  } else if (grepl("^--low-noise-cv=", arg)) {
    low_noise_cv_threshold <- as.numeric(sub("^--low-noise-cv=", "", arg))
  } else if (grepl("^--meaningful-margin=", arg)) {
    meaningful_margin <- as.numeric(sub("^--meaningful-margin=", "", arg))
  }
}

if (is.null(results_dir)) stop("--run-dir= is required; export never reads a results glob")
results_dir <- normalizePath(results_dir, mustWork = FALSE)
run_metadata <- read_run_manifest(results_dir)
if (!identical(as.character(run_metadata$status), "complete")) {
  stop(sprintf("run %s is not complete; comparative export is refused", run_metadata$run_id))
}
validate_run_artifacts(results_dir, run_metadata)
timing_policy <- if (is.null(run_metadata$timing_policy)) benchmark_timing_policy() else run_metadata$timing_policy
validate_timing_policy(timing_policy)
if (is.null(low_noise_cv_threshold)) low_noise_cv_threshold <- as.numeric(timing_policy$low_noise_cv_threshold_pct)
if (is.null(meaningful_margin)) meaningful_margin <- as.numeric(timing_policy$meaningful_margin_ratio)
timer_noise_floor_ms <- as.numeric(timing_policy$timer_noise_floor_ms)
expected_tasks <- sort(run_manifest_values(run_metadata$tasks))
expected_runners <- sort(run_manifest_values(run_metadata$runners))

summary_files <- file.path(results_dir, paste0(expected_runners, "_summary.csv"))
missing_summary_files <- summary_files[!file.exists(summary_files)]
if (length(missing_summary_files) > 0L) {
  stop(sprintf("run is missing runner summaries: %s", paste(basename(missing_summary_files), collapse = ", ")))
}

summaries <- do.call(
  rbind,
  lapply(summary_files, read.csv, stringsAsFactors = FALSE)
)

if (!"timer_noise_floor_ms" %in% names(summaries)) summaries$timer_noise_floor_ms <- timer_noise_floor_ms
if (!"timer_noise_status" %in% names(summaries)) {
  summaries$timer_noise_status <- ifelse(summaries$median_ms < timer_noise_floor_ms, "below_floor", "above_floor")
}

correctness_columns <- c("correctness_status", "correctness_policy", "correctness_message")
missing_correctness_columns <- setdiff(correctness_columns, names(summaries))
if (length(missing_correctness_columns) > 0L) {
  stop(sprintf("runner summaries missing correctness columns: %s", paste(missing_correctness_columns, collapse = ", ")))
}
invalid_pass_rows <- summaries[summaries$status == "PASS" & !(summaries$correctness_status %in% c("PASS", "REFERENCE")), , drop = FALSE]
if (nrow(invalid_pass_rows) > 0L) {
  stop(sprintf("timing summaries contain unapproved correctness rows: %s", paste(unique(invalid_pass_rows$task), collapse = ", ")))
}

actual_runners <- sort(unique(summaries$runner))
if (!identical(expected_runners, actual_runners)) {
  stop(sprintf("summary runner set differs from runner configs; expected: %s; got: %s",
               paste(expected_runners, collapse = ", "), paste(actual_runners, collapse = ", ")))
}
for (runner in expected_runners) {
  actual_tasks <- sort(unique(summaries$task[summaries$runner == runner]))
  if (!identical(expected_tasks, actual_tasks)) {
    stop(sprintf("summary coverage for %s differs from task manifest; missing: %s; extra: %s",
                 runner, paste(setdiff(expected_tasks, actual_tasks), collapse = ", "),
                 paste(setdiff(actual_tasks, expected_tasks), collapse = ", ")))
  }
}

runner_tasks <- split(summaries$task, summaries$runner)
runner_tasks <- lapply(runner_tasks, function(tasks) sort(unique(tasks)))
reference_runner <- names(runner_tasks)[[1]]
reference_tasks <- runner_tasks[[reference_runner]]

for (runner in names(runner_tasks)[-1]) {
  if (!identical(reference_tasks, runner_tasks[[runner]])) {
    stop(
      paste(
        "comparative export requires identical task coverage across runner summaries;",
        sprintf("%s and %s differ", reference_runner, runner),
        "so rerun the full matrix before exporting comparative metrics"
      )
    )
  }
}

pass_summaries <- subset(
  summaries,
  status == "PASS" & correctness_status %in% c("PASS", "REFERENCE"),
  select = c(runner, task, mean_ms, median_ms, cv_pct, timer_noise_floor_ms, timer_noise_status)
)
if (nrow(pass_summaries) == 0L) {
  stop(sprintf("no PASS rows found in %s", results_dir))
}

median_confidence_interval <- function(values, level) {
  values <- sort(as.numeric(values[is.finite(values)]))
  n <- length(values)
  if (n < 2L) return(c(low = NA_real_, high = NA_real_))
  alpha <- 1 - level
  low_rank <- max(1L, as.integer(qbinom(alpha / 2, n, 0.5)))
  high_rank <- min(n, as.integer(qbinom(1 - alpha / 2, n, 0.5)) + 1L)
  c(low = values[[low_rank]], high = values[[high_rank]])
}

read_task_samples <- function(runner, task) {
  path <- file.path(results_dir, runner, sprintf("task_%s.csv", task))
  if (!file.exists(path)) stop(sprintf("raw timing samples missing: %s", path))
  samples <- read.csv(path, stringsAsFactors = FALSE)
  if (!"wall_ms" %in% names(samples)) stop(sprintf("raw timing samples missing wall_ms: %s", path))
  samples$wall_ms
}

all_runners_in_results <- sort(unique(summaries$runner))
pass_matrix <- xtabs(~ task + runner, data = pass_summaries)
missing_runners <- setdiff(all_runners_in_results, colnames(pass_matrix))
if (length(missing_runners) > 0L) {
  for (runner in missing_runners) {
    pass_matrix <- cbind(pass_matrix, stats::setNames(data.frame(rep(0L, nrow(pass_matrix))), runner))
  }
}
pass_matrix <- pass_matrix[, all_runners_in_results, drop = FALSE]
common_pass_tasks <- rownames(pass_matrix)[apply(pass_matrix > 0L, 1, all)]
if (length(common_pass_tasks) == 0L) {
  stop("no tasks passed across every runner; comparative export requires a shared PASS intersection")
}

excluded_tasks <- setdiff(sort(unique(summaries$task)), common_pass_tasks)
if (length(excluded_tasks) > 0L) {
  cat(sprintf(
    "Excluding non-shared benchmark tasks from comparative export: %s\n",
    paste(excluded_tasks, collapse = ", ")
  ))
}

summaries <- pass_summaries[pass_summaries$task %in% common_pass_tasks, , drop = FALSE]

ordered_tasks <- manifest$task[manifest$task %in% unique(summaries$task)]

mean_wide <- reshape(
  summaries[, c("runner", "task", "median_ms")],
  idvar = "task",
  timevar = "runner",
  direction = "wide"
)
names(mean_wide) <- c("task", paste0(sub("median_ms\\.", "", names(mean_wide)[-1]), "_median"))

cv_wide <- reshape(
  summaries[, c("runner", "task", "cv_pct")],
  idvar = "task",
  timevar = "runner",
  direction = "wide"
)
names(cv_wide) <- c("task", paste0(sub("cv_pct\\.", "", names(cv_wide)[-1]), "_cv"))

merged <- merge(mean_wide, cv_wide, by = "task")
merged <- merged[match(ordered_tasks, merged$task), ]

manifest_rows <- match(merged$task, manifest$task)
if (anyNA(manifest_rows)) stop("comparative data contains a task absent from the task manifest")
merged$category <- manifest$category[manifest_rows]
merged$matrix_group <- task_matrix_group(merged$task)
merged$matrix_variant <- task_matrix_variant(merged$task)
merged$report_category <- task_report_category(merged$category)
merged$aggregate_comparable <- manifest$aggregate[manifest_rows]
merged$comparison_note <- manifest$comparison_note[manifest_rows]
below_floor_by_task <- tapply(summaries$timer_noise_status == "below_floor", summaries$task, any)
merged$timer_noise_floor_ms <- timer_noise_floor_ms
below_floor <- as.logical(below_floor_by_task[merged$task])
below_floor[is.na(below_floor)] <- FALSE
merged$timer_noise_status <- ifelse(below_floor, "below_floor", "above_floor")

all_runners_in_data <- colnames(mean_wide)[-1]
all_runners_in_data <- gsub("_median$", "", all_runners_in_data)
native_runners <- setdiff(all_runners_in_data, "r")
all_runners <- c("r", native_runners)
native_median_cols <- paste0(native_runners, "_median")
all_median_cols <- paste0(all_runners, "_median")
all_cv_cols <- paste0(all_runners, "_cv")

ci_cache <- list()
for (runner in all_runners) {
  for (task in merged$task) {
    key <- paste(runner, task, sep = "\r")
    ci_cache[[key]] <- median_confidence_interval(
      read_task_samples(runner, task),
      level = timing_policy$median_ci_level
    )
  }
}
for (runner in all_runners) {
  ci_low_name <- paste0(runner, "_median_ci_low_ms")
  ci_high_name <- paste0(runner, "_median_ci_high_ms")
  merged[[ci_low_name]] <- vapply(
    merged$task,
    function(task) ci_cache[[paste(runner, task, sep = "\r")]][["low"]],
    numeric(1)
  )
  merged[[ci_high_name]] <- vapply(
    merged$task,
    function(task) ci_cache[[paste(runner, task, sep = "\r")]][["high"]],
    numeric(1)
  )
}

aggregate_merged <- merged[merged$aggregate_comparable, , drop = FALSE]
if (nrow(aggregate_merged) == 0L) {
  stop("no aggregate-comparable tasks remain after applying the benchmark comparison policy")
}

excluded_aggregate_tasks <- merged[!merged$aggregate_comparable, c("task", "comparison_note"), drop = FALSE]
if (nrow(excluded_aggregate_tasks) > 0L) {
  cat(sprintf(
    "Excluding strategy-sensitive tasks from aggregate metrics: %s\n",
    paste(sprintf("%s (%s)", excluded_aggregate_tasks$task, excluded_aggregate_tasks$comparison_note), collapse = ", ")
  ))
}

merged$best_native_median_ms <- apply(merged[, native_median_cols, drop = FALSE], 1, min)
merged$best_all_median_ms <- apply(merged[, all_median_cols, drop = FALSE], 1, min)
merged$max_cv_pct <- apply(merged[, all_cv_cols, drop = FALSE], 1, max, na.rm = TRUE)
merged$low_noise <- merged$max_cv_pct <= low_noise_cv_threshold

aggregate_merged$best_native_median_ms <- apply(aggregate_merged[, native_median_cols, drop = FALSE], 1, min)
aggregate_merged$best_all_median_ms <- apply(aggregate_merged[, all_median_cols, drop = FALSE], 1, min)
aggregate_merged$max_cv_pct <- apply(aggregate_merged[, all_cv_cols, drop = FALSE], 1, max, na.rm = TRUE)
aggregate_merged$low_noise <- aggregate_merged$max_cv_pct <= low_noise_cv_threshold

merged$best_native_runner <- apply(
  merged[, native_median_cols, drop = FALSE],
  1,
  function(row) native_runners[[which.min(row)]]
)

aggregate_merged$best_native_runner <- apply(
  aggregate_merged[, native_median_cols, drop = FALSE],
  1,
  function(row) native_runners[[which.min(row)]]
)

merged$best_native_median_ci_low_ms <- vapply(
  seq_len(nrow(merged)),
  function(index) merged[[paste0(merged$best_native_runner[[index]], "_median_ci_low_ms")]][[index]],
  numeric(1)
)
merged$best_native_median_ci_high_ms <- vapply(
  seq_len(nrow(merged)),
  function(index) merged[[paste0(merged$best_native_runner[[index]], "_median_ci_high_ms")]][[index]],
  numeric(1)
)
merged$zigr_vs_best_native_ci_low <- merged$zigr_median_ci_low_ms / merged$best_native_median_ci_high_ms
merged$zigr_vs_best_native_ci_high <- merged$zigr_median_ci_high_ms / merged$best_native_median_ci_low_ms
merged$zigr_vs_r_ci_low <- merged$zigr_median_ci_low_ms / merged$r_median_ci_high_ms
merged$zigr_vs_r_ci_high <- merged$zigr_median_ci_high_ms / merged$r_median_ci_low_ms

for (runner in native_runners) {
  merged[[paste0(runner, "_vs_best_native")]] <-
    merged[[paste0(runner, "_median")]] / merged$best_native_median_ms
  aggregate_merged[[paste0(runner, "_vs_best_native")]] <-
    aggregate_merged[[paste0(runner, "_median")]] / aggregate_merged$best_native_median_ms
}

runner_metrics <- data.frame(
  runner = c(native_runners, "r"),
  stringsAsFactors = FALSE
)

runner_metrics$tasks_won <- vapply(
  runner_metrics$runner,
  function(runner) {
    sum(abs(aggregate_merged[[paste0(runner, "_median")]] - aggregate_merged$best_all_median_ms) < 1e-12)
  },
  integer(1)
)

runner_metrics$geomedian_vs_r <- c(
  vapply(
    native_runners,
    function(runner) exp(mean(log(aggregate_merged[[paste0(runner, "_median")]] / aggregate_merged$r_median))),
    numeric(1)
  ),
  1.0
)

runner_metrics$median_vs_r <- c(
  vapply(
    native_runners,
    function(runner) median(aggregate_merged[[paste0(runner, "_median")]] / aggregate_merged$r_median),
    numeric(1)
  ),
  1.0
)

runner_metrics$geomedian_vs_best_native <- c(
  vapply(
    native_runners,
    function(runner) exp(mean(log(aggregate_merged[[paste0(runner, "_vs_best_native")]]))),
    numeric(1)
  ),
  NA_real_
)

runner_metrics$median_vs_best_native <- c(
  vapply(
    native_runners,
    function(runner) median(aggregate_merged[[paste0(runner, "_vs_best_native")]]),
    numeric(1)
  ),
  NA_real_
)

runner_metrics$meaningful_native_wins <- c(
  vapply(
    native_runners,
    function(runner) {
      vals <- aggregate_merged[[paste0(runner, "_median")]]
      other_runners <- setdiff(native_runners, runner)
      if (length(other_runners) == 0L) return(0L)
      others <- aggregate_merged[, paste0(other_runners, "_median"), drop = FALSE]
      sum(apply(others, 1, min) / vals > meaningful_margin)
    },
    integer(1)
  ),
  NA_integer_
)

low_noise_subset <- aggregate_merged[aggregate_merged$low_noise, , drop = FALSE]
runner_metrics$low_noise_wins <- c(
  vapply(
    native_runners,
    function(runner) {
      if (nrow(low_noise_subset) == 0L) return(0L)
      sum(abs(low_noise_subset[[paste0(runner, "_median")]] - apply(low_noise_subset[, native_median_cols, drop = FALSE], 1, min)) < 1e-12)
    },
    integer(1)
  ),
  NA_integer_
)

runner_metrics$low_noise_task_count <- if (nrow(low_noise_subset) == 0L) 0L else nrow(low_noise_subset)
runner_metrics$aggregate_task_count <- nrow(aggregate_merged)
runner_metrics$excluded_shared_task_count <- nrow(merged) - nrow(aggregate_merged)
runner_metrics$low_noise_cv_threshold_pct <- low_noise_cv_threshold
runner_metrics$meaningful_margin_ratio <- meaningful_margin
runner_metrics$timer_noise_floor_ms <- timer_noise_floor_ms
runner_metrics$median_ci_level <- timing_policy$median_ci_level
runner_metrics$median_ci_method <- timing_policy$median_ci_method

category_rows <- list()
for (category in sort(unique(merged$report_category))) {
  all_category <- merged[merged$report_category == category, , drop = FALSE]
  aggregate_category <- aggregate_merged[aggregate_merged$report_category == category, , drop = FALSE]
  for (runner in all_runners) {
    has_aggregate <- nrow(aggregate_category) > 0L
    vs_r <- if (has_aggregate) aggregate_category[[paste0(runner, "_median")]] / aggregate_category$r_median else numeric(0)
    vs_best <- if (has_aggregate && runner != "r") aggregate_category[[paste0(runner, "_vs_best_native")]] else numeric(0)
    category_rows[[length(category_rows) + 1L]] <- data.frame(
      report_category = category,
      runner = runner,
      task_count = nrow(all_category),
      aggregate_task_count = nrow(aggregate_category),
      low_noise_task_count = sum(all_category$low_noise),
      timer_floor_task_count = sum(all_category$timer_noise_status == "below_floor"),
      median_vs_r = if (has_aggregate) median(vs_r) else NA_real_,
      geomedian_vs_r = if (has_aggregate) exp(mean(log(vs_r))) else NA_real_,
      median_vs_best_native = if (length(vs_best) > 0L) median(vs_best) else NA_real_,
      geomedian_vs_best_native = if (length(vs_best) > 0L) exp(mean(log(vs_best))) else NA_real_,
      stringsAsFactors = FALSE
    )
  }
}
category_metrics <- do.call(rbind, category_rows)

task_comparisons <- data.frame(
  task = merged$task,
  matrix_group = merged$matrix_group,
  matrix_variant = merged$matrix_variant,
  category = merged$category,
  report_category = merged$report_category,
  best_native_runner = merged$best_native_runner,
  best_native_median_ms = merged$best_native_median_ms,
  best_native_median_ci_low_ms = merged$best_native_median_ci_low_ms,
  best_native_median_ci_high_ms = merged$best_native_median_ci_high_ms,
  zigr_vs_best_native = merged$zigr_vs_best_native,
  zigr_vs_best_native_ci_low = merged$zigr_vs_best_native_ci_low,
  zigr_vs_best_native_ci_high = merged$zigr_vs_best_native_ci_high,
  zigr_vs_r_ci_low = merged$zigr_vs_r_ci_low,
  zigr_vs_r_ci_high = merged$zigr_vs_r_ci_high,
  max_cv_pct = merged$max_cv_pct,
  low_noise = merged$low_noise,
  timer_noise_floor_ms = merged$timer_noise_floor_ms,
  timer_noise_status = merged$timer_noise_status,
  median_ci_level = timing_policy$median_ci_level,
  median_ci_method = timing_policy$median_ci_method,
  aggregate_comparable = merged$aggregate_comparable,
  comparison_note = merged$comparison_note,
  stringsAsFactors = FALSE
)

dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
write.csv(runner_metrics, file.path(results_dir, "comparative_metrics.csv"), row.names = FALSE)
write.csv(task_comparisons, file.path(results_dir, "task_comparisons.csv"), row.names = FALSE)
write.csv(category_metrics, file.path(results_dir, "category_metrics.csv"), row.names = FALSE)

cat(sprintf("Wrote %s\n", file.path(results_dir, "comparative_metrics.csv")))
cat(sprintf("Wrote %s\n", file.path(results_dir, "task_comparisons.csv")))
cat(sprintf("Wrote %s\n", file.path(results_dir, "category_metrics.csv")))
