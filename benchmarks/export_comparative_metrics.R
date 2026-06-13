#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
results_dir <- "results"
low_noise_cv_threshold <- 20
meaningful_margin <- 1.05

aggregate_exclusions <- data.frame(
  task = character(0),
  comparison_note = character(0),
  stringsAsFactors = FALSE
)

for (arg in args) {
  if (grepl("^--results-dir=", arg)) {
    results_dir <- sub("^--results-dir=", "", arg)
  } else if (grepl("^--low-noise-cv=", arg)) {
    low_noise_cv_threshold <- as.numeric(sub("^--low-noise-cv=", "", arg))
  } else if (grepl("^--meaningful-margin=", arg)) {
    meaningful_margin <- as.numeric(sub("^--meaningful-margin=", "", arg))
  }
}

summary_files <- Sys.glob(file.path(results_dir, "*_summary.csv"))
if (length(summary_files) == 0L) {
  stop(sprintf("no runner summaries found in %s", results_dir))
}

summaries <- do.call(
  rbind,
  lapply(summary_files, read.csv, stringsAsFactors = FALSE)
)

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

pass_summaries <- subset(summaries, status == "PASS", select = c(runner, task, mean_ms, median_ms, cv_pct))
if (nrow(pass_summaries) == 0L) {
  stop(sprintf("no PASS rows found in %s", results_dir))
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

task_order <- order(as.integer(sub("_.*$", "", unique(summaries$task))))
ordered_tasks <- unique(summaries$task)[task_order]

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

matched_exclusions <- match(merged$task, aggregate_exclusions$task)
merged$aggregate_comparable <- is.na(matched_exclusions)
merged$comparison_note <- ifelse(
  is.na(matched_exclusions),
  "",
  aggregate_exclusions$comparison_note[matched_exclusions]
)

native_runners <- c("zigr", "c_call", "rcpp", "extendr", "savvy")
all_runners <- c("r", native_runners)
native_median_cols <- paste0(native_runners, "_median")
all_median_cols <- paste0(all_runners, "_median")
all_cv_cols <- paste0(all_runners, "_cv")

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

merged$best_native_median_ms <- apply(merged[, native_median_cols], 1, min)
merged$best_all_median_ms <- apply(merged[, all_median_cols], 1, min)
merged$max_cv_pct <- apply(merged[, all_cv_cols], 1, max, na.rm = TRUE)
merged$low_noise <- merged$max_cv_pct <= low_noise_cv_threshold

aggregate_merged$best_native_median_ms <- apply(aggregate_merged[, native_median_cols], 1, min)
aggregate_merged$best_all_median_ms <- apply(aggregate_merged[, all_median_cols], 1, min)
aggregate_merged$max_cv_pct <- apply(aggregate_merged[, all_cv_cols], 1, max, na.rm = TRUE)
aggregate_merged$low_noise <- aggregate_merged$max_cv_pct <= low_noise_cv_threshold

merged$best_native_runner <- apply(
  merged[, native_median_cols],
  1,
  function(row) native_runners[[which.min(row)]]
)

aggregate_merged$best_native_runner <- apply(
  aggregate_merged[, native_median_cols],
  1,
  function(row) native_runners[[which.min(row)]]
)

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
    function(runner) exp(mean(log(aggregate_merged[[paste0(runner, "_median")]] / aggregate_merged$r_mean))),
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
      others <- aggregate_merged[, paste0(setdiff(native_runners, runner), "_median"), drop = FALSE]
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
      sum(abs(low_noise_subset[[paste0(runner, "_median")]] - apply(low_noise_subset[, native_median_cols], 1, min)) < 1e-12)
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

task_comparisons <- data.frame(
  task = merged$task,
  best_native_runner = merged$best_native_runner,
  best_native_median_ms = merged$best_native_median_ms,
  zigr_vs_best_native = merged$zigr_vs_best_native,
  max_cv_pct = merged$max_cv_pct,
  low_noise = merged$low_noise,
  aggregate_comparable = merged$aggregate_comparable,
  comparison_note = merged$comparison_note,
  stringsAsFactors = FALSE
)

dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
write.csv(runner_metrics, file.path(results_dir, "comparative_metrics.csv"), row.names = FALSE)
write.csv(task_comparisons, file.path(results_dir, "task_comparisons.csv"), row.names = FALSE)

cat(sprintf("Wrote %s\n", file.path(results_dir, "comparative_metrics.csv")))
cat(sprintf("Wrote %s\n", file.path(results_dir, "task_comparisons.csv")))
