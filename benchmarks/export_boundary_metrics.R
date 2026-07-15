#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
run_dir <- NULL
for (arg in args) {
  if (grepl("^--run-dir=", arg)) run_dir <- sub("^--run-dir=", "", arg)
}
if (is.null(run_dir)) stop("--run-dir= is required")

root_dir <- normalizePath(".")
source(file.path(root_dir, "lib", "specification.R"))
source(file.path(root_dir, "lib", "measurement.R"))
source(file.path(root_dir, "lib", "provenance.R"))
source(file.path(root_dir, "lib", "run_manifest.R"))
validate_cli_arguments(args, value_options = "run-dir", label = "boundary export")

run_dir <- normalizePath(run_dir, mustWork = FALSE)
metadata <- read_run_manifest(run_dir)
if (!identical(as.character(metadata$status), "complete")) stop("boundary export requires a complete run")
validate_run_artifacts(run_dir, metadata)
timing_policy <- metadata$timing_policy
validate_timing_policy(timing_policy)
policy_version <- boundary_budget_policy_version()
recorded_policy_version <- metadata$boundary_budget_policy_version
if (is.null(recorded_policy_version) || !identical(as.character(recorded_policy_version), policy_version)) {
  stop("run boundary budget policy version differs from the current policy")
}

required_runners <- c("c_call", "r", "zigr")
missing_runners <- setdiff(required_runners, run_manifest_values(metadata$runners))
if (length(missing_runners) > 0L) stop(sprintf("boundary export is missing runners: %s", paste(missing_runners, collapse = ", ")))

manifest <- load_task_manifest(root_dir)
boundary_tasks <- manifest$task[grepl("^[0-9]{2}_boundary_.*_(generated|handwritten)$", manifest$task)]
representation_tasks <- manifest$task[grepl("^(76|77|78|79|80|81|82|83|84|85|86)_", manifest$task)]
required_tasks <- c(boundary_tasks, representation_tasks)
missing_tasks <- setdiff(required_tasks, run_manifest_values(metadata$tasks))
if (length(missing_tasks) > 0L) stop(sprintf("boundary export is missing tasks: %s", paste(missing_tasks, collapse = ", ")))

summaries <- read_run_summary_table(run_dir, metadata, "task", required_runners)
summaries <- summaries[summaries$runner %in% required_runners, , drop = FALSE]

task_sample_stats <- function(runner, task, expected_n) {
  values <- read_run_wall_time_samples(
    run_dir, metadata, "task", runner, task, expected_n = expected_n
  )
  n <- length(values)
  interval <- median_confidence_interval(values, as.numeric(timing_policy$median_ci_level))
  sample_mean <- mean(values)
  list(
    median = median(values),
    cv_pct = if (sample_mean > 0) sd(values) / sample_mean * 100 else 0,
    ci_low = interval[["low"]],
    ci_high = interval[["high"]],
    n = n
  )
}

summary_row <- function(runner, task, pass_required = TRUE) {
  row <- summaries[summaries$runner == runner & summaries$task == task, , drop = FALSE]
  if (nrow(row) != 1L) stop(sprintf("%s/%s is not one summary row", runner, task))
  if (pass_required && row$status[[1]] != "PASS") stop(sprintf("%s/%s is not one PASS row", runner, task))
  if (pass_required && row$sample_stage[[1]] != "confirmation") {
    stop(sprintf("%s/%s has no fixed confirmation sample", runner, task))
  }
  row
}

generated_tasks <- boundary_tasks[task_matrix_variant(boundary_tasks) == "generated"]
boundary_rows <- lapply(generated_tasks, function(generated_task) {
  group <- task_matrix_group(generated_task)
  handwritten_task <- boundary_tasks[
    task_matrix_group(boundary_tasks) == group & task_matrix_variant(boundary_tasks) == "handwritten"
  ]
  if (length(handwritten_task) != 1L) stop(sprintf("boundary group %s has no unique handwritten row", group))

  generated <- summary_row("zigr", generated_task)
  handwritten <- summary_row("zigr", handwritten_task)
  c_reference <- summary_row("c_call", generated_task)
  r_reference <- summary_row("r", generated_task, pass_required = FALSE)
  generated_samples <- task_sample_stats("zigr", generated_task, generated$n_iterations[[1]])
  handwritten_samples <- task_sample_stats("zigr", handwritten_task, handwritten$n_iterations[[1]])
  c_samples <- task_sample_stats("c_call", generated_task, c_reference$n_iterations[[1]])
  r_samples <- if (identical(as.character(r_reference$status[[1L]]), "PASS")) {
    task_sample_stats("r", generated_task, r_reference$n_iterations[[1]])
  } else {
    NULL
  }
  floor_ms <- as.numeric(timing_policy$timer_noise_floor_ms)
  below_floor <- generated_samples$median < floor_ms || handwritten_samples$median < floor_ms
  low_noise <- max(generated_samples$cv_pct, handwritten_samples$cv_pct) <= as.numeric(timing_policy$low_noise_cv_threshold_pct)
  c_below_floor <- generated_samples$median < floor_ms || c_samples$median < floor_ms
  c_low_noise <- max(generated_samples$cv_pct, c_samples$cv_pct) <= as.numeric(timing_policy$low_noise_cv_threshold_pct)
  comparable_work <- group != "altrep_integer"
  eligible <- !below_floor && low_noise && comparable_work
  c_eligible <- !c_below_floor && c_low_noise && comparable_work

  data.frame(
    matrix_group = group,
    budget_class = boundary_budget_class(group),
    generated_task = generated_task,
    handwritten_task = handwritten_task,
    c_reference_task = generated_task,
    r_reference_task = generated_task,
    generated_median_ms = generated_samples$median,
    generated_median_ci_low_ms = generated_samples$ci_low,
    generated_median_ci_high_ms = generated_samples$ci_high,
    handwritten_median_ms = handwritten_samples$median,
    handwritten_median_ci_low_ms = handwritten_samples$ci_low,
    handwritten_median_ci_high_ms = handwritten_samples$ci_high,
    generated_minus_handwritten_ms = generated_samples$median - handwritten_samples$median,
    generated_vs_handwritten = generated_samples$median / handwritten_samples$median,
    c_reference_median_ms = c_samples$median,
    c_reference_median_ci_low_ms = c_samples$ci_low,
    c_reference_median_ci_high_ms = c_samples$ci_high,
    generated_minus_c_ms = generated_samples$median - c_samples$median,
    generated_vs_c = generated_samples$median / c_samples$median,
    c_reference_cv_pct = c_samples$cv_pct,
    c_reference_timer_noise_status = if (c_samples$median < floor_ms) "below_floor" else "above_floor",
    c_reference_first_call_ms = c_reference$first_call_ms[[1]],
    c_reference_rss_endpoint_delta_kb = c_reference$rss_endpoint_delta_kb[[1]],
    c_reference_rss_endpoint_support = c_reference$rss_endpoint_support[[1]],
    c_reference_rss_endpoint_support_reason = c_reference$rss_endpoint_support_reason[[1]],
    c_reference_n_iterations = c_reference$n_iterations[[1]],
    c_reference_sample_stage = c_reference$sample_stage[[1]],
    c_comparison_eligible = c_eligible,
    c_comparison_reason = if (!comparable_work) {
      "different ownership strategy"
    } else if (c_below_floor) {
      "below timer floor"
    } else if (!c_low_noise) {
      "CV above low-noise threshold"
    } else {
      "eligible"
    },
    r_reference_status = as.character(r_reference$status[[1L]]),
    r_reference_reason = if (identical(as.character(r_reference$status[[1L]]), "PASS")) {
      ""
    } else {
      as.character(r_reference$disposition_reason[[1L]])
    },
    r_reference_median_ms = if (is.null(r_samples)) NA_real_ else r_samples$median,
    generated_cv_pct = generated_samples$cv_pct,
    handwritten_cv_pct = handwritten_samples$cv_pct,
    generated_first_call_ms = generated$first_call_ms[[1]],
    handwritten_first_call_ms = handwritten$first_call_ms[[1]],
    generated_rss_endpoint_delta_kb = generated$rss_endpoint_delta_kb[[1]],
    handwritten_rss_endpoint_delta_kb = handwritten$rss_endpoint_delta_kb[[1]],
    generated_rss_endpoint_support = generated$rss_endpoint_support[[1]],
    handwritten_rss_endpoint_support = handwritten$rss_endpoint_support[[1]],
    generated_rss_endpoint_support_reason = generated$rss_endpoint_support_reason[[1]],
    handwritten_rss_endpoint_support_reason = handwritten$rss_endpoint_support_reason[[1]],
    generated_n_iterations = generated_samples$n,
    handwritten_n_iterations = handwritten_samples$n,
    generated_sample_stage = generated$sample_stage[[1]],
    handwritten_sample_stage = handwritten$sample_stage[[1]],
    timer_noise_status = if (below_floor) "below_floor" else "above_floor",
    low_noise = low_noise,
    comparison_eligible = eligible,
    comparison_reason = if (!comparable_work) {
      "different ownership strategy"
    } else if (below_floor) {
      "below timer floor"
    } else if (!low_noise) {
      "CV above low-noise threshold"
    } else {
      "eligible"
    },
    timer_noise_floor_ms = timing_policy$timer_noise_floor_ms,
    median_ci_level = timing_policy$median_ci_level,
    median_ci_method = timing_policy$median_ci_method,
    rss_endpoint_metric = generated$rss_endpoint_metric[[1]],
    budget_policy_version = policy_version,
    run_id = metadata$run_id,
    stringsAsFactors = FALSE
  )
})
boundary_metrics <- do.call(rbind, boundary_rows)

policy <- boundary_budget_policy()
class_rows <- lapply(seq_len(nrow(policy)), function(index) {
  budget <- policy[index, , drop = FALSE]
  rows <- boundary_metrics[boundary_metrics$budget_class == budget$budget_class, , drop = FALSE]
  eligible <- rows[rows$comparison_eligible, , drop = FALSE]
  baseline_median <- max(rows$generated_median_ms)
  baseline_overhead <- if (nrow(eligible) == 0L) NA_real_ else max(eligible$generated_minus_handwritten_ms)
  baseline_ratio <- if (nrow(eligible) == 0L) NA_real_ else max(eligible$generated_vs_handwritten)
  median_pass <- baseline_median <= budget$max_generated_median_ms
  overhead_pass <- is.na(budget$max_eligible_overhead_ms) || is.na(baseline_overhead) || baseline_overhead <= budget$max_eligible_overhead_ms
  ratio_pass <- is.na(budget$max_eligible_ratio) || is.na(baseline_ratio) || baseline_ratio <= budget$max_eligible_ratio
  needs_pair_signal <- !is.na(budget$max_eligible_overhead_ms) || !is.na(budget$max_eligible_ratio)
  status <- if (!median_pass || !overhead_pass || !ratio_pass) {
    "FAIL"
  } else if (needs_pair_signal && nrow(eligible) == 0L) {
    "PASS_NO_RATIO_SIGNAL"
  } else {
    "PASS"
  }
  data.frame(
    budget_class = budget$budget_class,
    tasks = paste(rows$matrix_group, collapse = ";"),
    baseline_max_generated_median_ms = baseline_median,
    max_generated_median_ms = budget$max_generated_median_ms,
    baseline_max_eligible_overhead_ms = baseline_overhead,
    max_eligible_overhead_ms = budget$max_eligible_overhead_ms,
    baseline_max_eligible_ratio = baseline_ratio,
    max_eligible_ratio = budget$max_eligible_ratio,
    eligible_pair_count = nrow(eligible),
    status = status,
    budget_policy_version = policy_version,
    run_id = metadata$run_id,
    stringsAsFactors = FALSE
  )
})
boundary_budgets <- do.call(rbind, class_rows)

representation_policy <- representation_budget_policy()
representation_rows <- lapply(representation_tasks, function(task) {
  row <- summary_row("zigr", task)
  samples <- task_sample_stats("zigr", task, row$n_iterations[[1]])
  limit <- representation_policy$max_median_ms[match(task, representation_policy$task)]
  data.frame(
    task = task,
    median_ms = samples$median,
    median_ci_low_ms = samples$ci_low,
    median_ci_high_ms = samples$ci_high,
    max_median_ms = limit,
    status = if (samples$median <= limit) "PASS" else "FAIL",
    cv_pct = samples$cv_pct,
    timer_noise_status = if (samples$median < as.numeric(timing_policy$timer_noise_floor_ms)) "below_floor" else "above_floor",
    first_call_ms = row$first_call_ms[[1]],
    rss_endpoint_delta_kb = row$rss_endpoint_delta_kb[[1]],
    rss_endpoint_support = row$rss_endpoint_support[[1]],
    rss_endpoint_support_reason = row$rss_endpoint_support_reason[[1]],
    n_iterations = samples$n,
    sample_stage = row$sample_stage[[1]],
    timer_noise_floor_ms = timing_policy$timer_noise_floor_ms,
    median_ci_level = timing_policy$median_ci_level,
    median_ci_method = timing_policy$median_ci_method,
    rss_endpoint_metric = row$rss_endpoint_metric[[1]],
    budget_policy_version = policy_version,
    run_id = metadata$run_id,
    stringsAsFactors = FALSE
  )
})
representation_metrics <- do.call(rbind, representation_rows)

if (any(!grepl("^PASS", boundary_budgets$status)) || any(representation_metrics$status != "PASS")) {
  stop("one or more performance budgets failed")
}

local({
report_manifest_path <- file.path(run_dir, "report_manifest.json")
if (!file.exists(report_manifest_path)) {
  stop("boundary export requires the comparative report manifest")
}
report_manifest <- jsonlite::fromJSON(report_manifest_path, simplifyVector = FALSE)
if (!identical(as.character(report_manifest$schema_version), comparative_report_schema_version()) ||
    !identical(as.character(report_manifest$run_id), as.character(metadata$run_id)) ||
    !setequal(run_manifest_values(report_manifest$declared_report_files), unname(declared_report_files()))) {
  stop("comparative report manifest does not declare the current report set")
}
files <- boundary_report_files()
final_paths <- file.path(run_dir, unname(files))
existing <- final_paths[file.exists(final_paths)]
if (length(existing) > 0L) {
  stop(sprintf("boundary report output already exists: %s", paste(basename(existing), collapse = ", ")))
}
outputs <- list(
  boundary = boundary_metrics,
  boundary_budget = boundary_budgets,
  representation_budget = representation_metrics
)
staging <- tempfile("boundary-reports-", tmpdir = run_dir)
dir.create(staging)
on.exit(unlink(staging, recursive = TRUE), add = TRUE)
staged_paths <- file.path(staging, unname(files))
for (index in seq_along(outputs)) write_csv(outputs[[index]], staged_paths[[index]])
boundary_records <- lapply(seq_along(files), function(index) list(
  role = names(files)[[index]], file = unname(files[[index]]),
  rows = nrow(outputs[[index]]),
  md5 = unname(as.character(tools::md5sum(staged_paths[[index]])[[1L]]))
))
report_manifest$reports <- c(report_manifest$reports, boundary_records)
boundary_source <- "export_boundary_metrics.R"
report_manifest$report_code_sources <- c(
  report_manifest$report_code_sources,
  list(list(
    path = boundary_source,
    md5 = unname(as.character(tools::md5sum(file.path(root_dir, boundary_source))[[1L]]))
  ))
)
recorded_files <- vapply(report_manifest$reports, function(record) as.character(record$file), character(1))
if (anyDuplicated(recorded_files) ||
    !setequal(recorded_files, run_manifest_values(report_manifest$declared_report_files))) {
  stop("report records do not cover the declared report set")
}
published <- character(0)
publication_complete <- FALSE
on.exit({
  if (!publication_complete) unlink(published)
}, add = TRUE)
for (index in seq_along(final_paths)) {
  if (!file.rename(staged_paths[[index]], final_paths[[index]])) {
    unlink(published)
    stop(sprintf("cannot publish boundary report: %s", final_paths[[index]]))
  }
  published <- c(published, final_paths[[index]])
}
tryCatch(
  write_run_manifest_json_atomic(report_manifest, report_manifest_path),
  error = function(error) {
    unlink(published)
    stop(error)
  }
)
publication_complete <- TRUE

cat(sprintf("Wrote %s\n", paste(final_paths, collapse = ", ")))
})
