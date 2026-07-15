#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
results_dir <- NULL
root_dir <- normalizePath(".")
source(file.path(root_dir, "lib", "specification.R"))
source(file.path(root_dir, "lib", "measurement.R"))
source(file.path(root_dir, "lib", "provenance.R"))
source(file.path(root_dir, "lib", "run_manifest.R"))
source(file.path(root_dir, "lib", "product_fixtures.R"))
manifest <- load_task_manifest(root_dir)
validate_cli_arguments(
  args,
  value_options = c("run-dir", "results-dir"),
  label = "comparative export"
)

for (arg in args) {
  if (grepl("^--run-dir=", arg)) {
    results_dir <- sub("^--run-dir=", "", arg)
  } else if (grepl("^--results-dir=", arg)) {
    stop("--results-dir is retired; pass one completed run with --run-dir=")
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
low_noise_cv_threshold <- as.numeric(timing_policy$low_noise_cv_threshold_pct)
meaningful_margin <- as.numeric(timing_policy$meaningful_margin_ratio)
timer_noise_floor_ms <- as.numeric(timing_policy$timer_noise_floor_ms)
expected_tasks <- sort(run_manifest_values(run_metadata$tasks))
expected_runners <- sort(run_manifest_values(run_metadata$runners))

summaries <- read_run_summary_table(results_dir, run_metadata, "task", expected_runners)

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

export_separated_metrics <- function() {
  evidence <- load_evidence_manifest(root_dir, manifest)
  correctness_identity <- validate_correctness_artifacts(results_dir, run_metadata, evidence)
  validate_fixture_measurement_artifacts(results_dir, run_metadata, evidence)
  source_bundle <- verify_fixture_source_paths(root_dir, evidence)

  raw_ci <- function(universe, runner, item_id, row_id, status) {
    if (!identical(as.character(status), "PASS")) return(c(low = NA_real_, high = NA_real_))
    id <- if (identical(universe, "task")) item_id else row_id
    median_confidence_interval(
      read_run_wall_time_samples(results_dir, run_metadata, universe, runner, id),
      as.numeric(timing_policy$median_ci_level)
    )
  }
  evidence_fallback <- function(values, fallback) {
    values <- as.character(values)
    values[!nzchar(values)] <- fallback
    values
  }

  task_keys <- paste(summaries$runner, summaries$task, sep = "\r")
  task_evidence_keys <- paste(evidence$tasks$runner, evidence$tasks$task, sep = "\r")
  task_index <- match(task_keys, task_evidence_keys)
  if (anyNA(task_index)) stop("task summaries cannot be joined to the evidence matrix")
  task_manifest_index <- match(summaries$task, manifest$task)
  task_cells <- data.frame(
    run_id = as.character(summaries$run_id), universe = "task",
    item_id = as.character(summaries$task), row_id = as.character(summaries$task),
    display_name = as.character(manifest$display_name[task_manifest_index]),
    category = task_report_category(manifest$category[task_manifest_index]),
    group_id = as.character(evidence$tasks$comparison_group[task_index]),
    runner = as.character(summaries$runner), variant = "public",
    implementation_role = as.character(evidence$tasks$implementation_role[task_index]),
    evidence_use = as.character(evidence$tasks$evidence_use[task_index]),
    path_kind = as.character(evidence$tasks$path_kind[task_index]),
    representation_strategy = as.character(evidence$tasks$representation_strategy[task_index]),
    comparison_tier = as.character(evidence$tasks$comparison_tier[task_index]),
    input_fingerprint = as.character(summaries$input_fingerprint),
    kernel_id = as.character(summaries$kernel_id),
    contract_version = as.character(summaries$contract_version),
    fixture_version = as.character(evidence$tasks$fixture_version[task_index]),
    mutation_policy = as.character(evidence$tasks$mutation_policy[task_index]),
    setup_policy = as.character(evidence$tasks$setup_policy[task_index]),
    disposition = as.character(evidence$tasks$status[task_index]),
    reason = as.character(evidence$tasks$reason[task_index]),
    owner = as.character(evidence$tasks$owner[task_index]),
    status = as.character(summaries$status), correctness_status = as.character(summaries$correctness_status),
    correctness_message = as.character(summaries$correctness_message),
    median_ms = as.numeric(summaries$median_ms), cv_pct = as.numeric(summaries$cv_pct),
    timer_noise_floor_ms = as.numeric(summaries$timer_noise_floor_ms),
    timer_noise_status = as.character(summaries$timer_noise_status),
    source_label = as.character(summaries$source_path_class),
    source_verification_digest = as.character(summaries$source_verification_digest),
    artifact_digest = as.character(summaries$artifact_digest),
    source_ledger_identity_digest = as.character(summaries$source_ledger_identity_digest),
    backend_provenance = as.character(summaries$r_implementation_provenance),
    stringsAsFactors = FALSE
  )

  fixture_summaries <- read_run_summary_table(results_dir, run_metadata, "fixture", expected_runners)
  fixture_keys <- paste(fixture_summaries$runner, fixture_summaries$fixture, sep = "\r")
  fixture_evidence_keys <- paste(evidence$fixture_rows$runner, evidence$fixture_rows$fixture, sep = "\r")
  fixture_index <- match(fixture_keys, fixture_evidence_keys)
  if (anyNA(fixture_index)) stop("fixture summaries cannot be joined to the evidence matrix")
  fixture_optimized <- fixture_summaries$variant == "optimized_base_r"
  fixture_role <- as.character(evidence$fixture_rows$implementation_role[fixture_index])
  fixture_role[fixture_optimized] <- "optimized_base_r"
  fixture_tier <- as.character(evidence$fixture_rows$comparison_tier[fixture_index])
  fixture_tier[fixture_optimized] <- "tier_c"
  fixture_group <- as.character(evidence$fixture_rows$comparison_group[fixture_index])
  fixture_group[fixture_optimized] <- paste0("normalized:", fixture_summaries$fixture[fixture_optimized], ":optimized-base-r")
  fixture_backend <- rep("not_applicable", nrow(fixture_summaries))
  for (index in which(fixture_summaries$runner == "r")) {
    fixture <- as.character(fixture_summaries$fixture[[index]])
    provenance <- if (fixture_optimized[[index]]) source_bundle$optimized_r_provenance[[fixture]] else source_bundle$r_provenance[[fixture]]
    fixture_backend[[index]] <- paste(
      as.character(provenance$implementation_class), as.character(provenance$compiled_backend), sep = ":"
    )
  }
  fixture_cells <- data.frame(
    run_id = as.character(fixture_summaries$run_id), universe = "fixture",
    item_id = as.character(fixture_summaries$fixture),
    row_id = as.character(fixture_summaries$row_id), display_name = as.character(fixture_summaries$fixture),
    category = "fixture", group_id = fixture_group,
    runner = as.character(fixture_summaries$runner), variant = as.character(fixture_summaries$variant),
    implementation_role = fixture_role,
    evidence_use = ifelse(fixture_optimized, "timed_baseline", as.character(evidence$fixture_rows$evidence_use[fixture_index])),
    path_kind = ifelse(fixture_optimized, "optimized_base_r", as.character(evidence$fixture_rows$path_kind[fixture_index])),
    representation_strategy = ifelse(
      fixture_optimized, "runtime_service", as.character(evidence$fixture_rows$representation_strategy[fixture_index])
    ),
    comparison_tier = fixture_tier,
    input_fingerprint = as.character(fixture_summaries$input_fingerprint),
    kernel_id = as.character(fixture_summaries$kernel_id),
    contract_version = as.character(fixture_summaries$contract_version),
    fixture_version = as.character(fixture_summaries$fixture_version),
    mutation_policy = as.character(evidence$fixture_rows$mutation_policy[fixture_index]),
    setup_policy = as.character(fixture_summaries$setup_policy),
    disposition = ifelse(fixture_optimized, "supported_and_executable", as.character(evidence$fixture_rows$status[fixture_index])),
    reason = ifelse(fixture_optimized, "declared optimized base-R baseline", as.character(evidence$fixture_rows$reason[fixture_index])),
    owner = ifelse(fixture_optimized, "performance", as.character(evidence$fixture_rows$owner[fixture_index])),
    status = as.character(fixture_summaries$status), correctness_status = as.character(fixture_summaries$correctness_status),
    correctness_message = as.character(fixture_summaries$correctness_message),
    median_ms = as.numeric(fixture_summaries$median_ms), cv_pct = as.numeric(fixture_summaries$cv_pct),
    timer_noise_floor_ms = as.numeric(fixture_summaries$timer_noise_floor_ms),
    timer_noise_status = as.character(fixture_summaries$timer_noise_status),
    source_label = as.character(fixture_summaries$path_kind),
    source_verification_digest = as.character(fixture_summaries$source_ledger_identity_digest),
    artifact_digest = as.character(fixture_summaries$fixture_artifact_digest),
    source_ledger_identity_digest = as.character(fixture_summaries$source_ledger_identity_digest),
    backend_provenance = fixture_backend,
    stringsAsFactors = FALSE
  )
  cells <- rbind(task_cells, fixture_cells)
  cells$median_ci_low_ms <- NA_real_
  cells$median_ci_high_ms <- NA_real_
  for (index in which(cells$status == "PASS")) {
    interval <- raw_ci(
      cells$universe[[index]], cells$runner[[index]], cells$item_id[[index]],
      cells$row_id[[index]], cells$status[[index]]
    )
    cells$median_ci_low_ms[[index]] <- interval[["low"]]
    cells$median_ci_high_ms[[index]] <- interval[["high"]]
  }

  add_zigr_comparison <- function(rows, compare_items = rows$item_id, direction = "row_over_zigr") {
    reference_rows <- cells[cells$runner == "zigr" & cells$variant == "public", , drop = FALSE]
    reference_keys <- paste(reference_rows$universe, reference_rows$item_id, sep = "\r")
    if (anyDuplicated(reference_keys)) stop("zigr comparison reference is not unique")
    reference_index <- match(paste(rows$universe, compare_items, sep = "\r"), reference_keys)
    if (anyNA(reference_index)) stop("zigr comparison reference is not unique")
    reference <- reference_rows[reference_index, , drop = FALSE]
    rows$zigr_reference_item_id <- as.character(compare_items)
    rows$zigr_median_ms <- reference$median_ms
    rows$zigr_median_ci_low_ms <- reference$median_ci_low_ms
    rows$zigr_median_ci_high_ms <- reference$median_ci_high_ms
    rows$zigr_cv_pct <- reference$cv_pct
    rows$zigr_timer_noise_status <- reference$timer_noise_status
    rows$ratio <- NA_real_
    rows$ratio_ci_low <- NA_real_
    rows$ratio_ci_high <- NA_real_
    rows$noise_status <- "not_comparable"
    rows$relative_result <- "NOT_COMPARABLE"
    comparable <- is.finite(rows$median_ms) & is.finite(reference$median_ms)
    if (identical(direction, "zigr_over_row")) {
      rows$ratio[comparable] <- reference$median_ms[comparable] / rows$median_ms[comparable]
      rows$ratio_ci_low[comparable] <- reference$median_ci_low_ms[comparable] / rows$median_ci_high_ms[comparable]
      rows$ratio_ci_high[comparable] <- reference$median_ci_high_ms[comparable] / rows$median_ci_low_ms[comparable]
    } else {
      rows$ratio[comparable] <- rows$median_ms[comparable] / reference$median_ms[comparable]
      rows$ratio_ci_low[comparable] <- rows$median_ci_low_ms[comparable] / reference$median_ci_high_ms[comparable]
      rows$ratio_ci_high[comparable] <- rows$median_ci_high_ms[comparable] / reference$median_ci_low_ms[comparable]
    }
    low_noise <- rows$cv_pct <= low_noise_cv_threshold & reference$cv_pct <= low_noise_cv_threshold
    rows$noise_status[comparable] <- ifelse(low_noise[comparable], "low_noise", "high_noise")
    rows$relative_result[comparable] <- ifelse(
      rows$ratio[comparable] > meaningful_margin, "LOSS",
      ifelse(rows$ratio[comparable] < 1 / meaningful_margin, "WIN", "TIE")
    )
    rows
  }

  product_groups <- unique(cells$group_id[
    cells$runner %in% report_product_runners() & cells$comparison_tier == "tier_a"
  ])
  product <- cells[cells$group_id %in% product_groups & cells$runner %in% report_linked_runners() & cells$variant == "public", ]
  product$product_status <- ifelse(
    product$runner %in% report_product_runners() & product$implementation_role == "product_public_path" &
      product$comparison_tier == "tier_a" & product$status == "PASS",
    "PRODUCT_PASS",
    ifelse(
      product$runner %in% report_product_runners() & product$implementation_role == "capability_gap",
      "PRODUCT_GAP",
      ifelse(product$runner %in% c("r", "c_call"), "LINKED_BASELINE", "EXCLUDED_DIAGNOSTIC")
    )
  )
  product$claim_eligible <- product$product_status == "PRODUCT_PASS"
  product$reason <- ifelse(
    product$product_status == "PRODUCT_PASS", "Tier A product-public path passed correctness and timing gates",
    evidence_fallback(product$reason, ifelse(
      product$product_status == "LINKED_BASELINE", "linked R or registered-C baseline retained for the comparison",
      "row is not a Tier A product-public result for this comparison"
    ))
  )
  product$owner <- evidence_fallback(product$owner, "performance")
  product <- add_zigr_comparison(product)
  product$measurement_status <- product$status
  product$report_status <- product$product_status
  product$row_over_zigr_ratio <- product$ratio
  product$row_over_zigr_ratio_ci_low <- product$ratio_ci_low
  product$row_over_zigr_ratio_ci_high <- product$ratio_ci_high
  product$row_relative_to_zigr <- product$relative_result

  strategy_groups <- unique(cells$group_id[
    cells$runner %in% report_product_runners() & cells$comparison_tier == "tier_b"
  ])
  strategy <- cells[cells$group_id %in% strategy_groups & cells$runner %in% report_linked_runners() & cells$variant == "public", ]
  strategy$strategy_status <- ifelse(
    strategy$runner %in% report_product_runners() & strategy$implementation_role == "product_public_path" &
      strategy$comparison_tier == "tier_b" & strategy$status == "PASS",
    "STRATEGY_PASS",
    ifelse(
      strategy$runner %in% report_product_runners() & strategy$implementation_role == "product_public_path" &
        strategy$comparison_tier == "tier_b" & strategy$status == "CORRECTNESS_ONLY",
      "STRATEGY_CORRECTNESS_ONLY", "LINKED_OR_EXCLUDED"
    )
  )
  strategy$claim_eligible <- FALSE
  strategy$reason <- ifelse(
    strategy$strategy_status %in% c("STRATEGY_PASS", "STRATEGY_CORRECTNESS_ONLY"),
    "Tier B strategy evidence retained outside product-level claims",
    evidence_fallback(strategy$reason, "linked baseline, gap, or non-Tier-B row retained for context")
  )
  strategy$owner <- evidence_fallback(strategy$owner, "performance")
  strategy <- add_zigr_comparison(strategy)
  strategy$measurement_status <- strategy$status
  strategy$report_status <- strategy$strategy_status
  strategy$row_over_zigr_ratio <- strategy$ratio
  strategy$row_over_zigr_ratio_ci_low <- strategy$ratio_ci_low
  strategy$row_over_zigr_ratio_ci_high <- strategy$ratio_ci_high
  strategy$row_relative_to_zigr <- strategy$relative_result

  r_baseline <- cells[cells$runner == "r", ]
  r_baseline$baseline_class <- ifelse(
    r_baseline$variant == "optimized_base_r", "optimized_base_r",
    ifelse(r_baseline$implementation_role == "capability_gap", "pure_r_unrepresentable", r_baseline$implementation_role)
  )
  r_baseline$claim_eligible <- FALSE
  r_baseline$owner <- evidence_fallback(r_baseline$owner, "performance")
  r_baseline <- add_zigr_comparison(r_baseline, direction = "zigr_over_row")
  r_baseline$item_id[r_baseline$variant == "optimized_base_r"] <- r_baseline$row_id[r_baseline$variant == "optimized_base_r"]
  r_baseline$owner[r_baseline$relative_result == "LOSS"] <- "performance"
  r_baseline$measurement_status <- r_baseline$status
  r_baseline$zigr_over_r_ratio <- r_baseline$ratio
  r_baseline$zigr_over_r_ratio_ci_low <- r_baseline$ratio_ci_low
  r_baseline$zigr_over_r_ratio_ci_high <- r_baseline$ratio_ci_high
  r_baseline$zigr_relative_to_r <- r_baseline$relative_result

  control <- cells[cells$implementation_role %in% c("c_control", "language_control") & cells$variant == "public", ]
  control$control_role <- control$implementation_role
  control$claim_eligible <- FALSE
  compare_items <- as.character(control$item_id)
  handwritten <- control$universe == "task" & control$runner == "zigr" &
    task_matrix_variant(control$item_id) == "handwritten"
  generated_by_group <- manifest$task[task_matrix_variant(manifest$task) == "generated"]
  names(generated_by_group) <- task_matrix_group(generated_by_group)
  compare_items[handwritten] <- unname(generated_by_group[task_matrix_group(control$item_id[handwritten])])
  if (anyNA(compare_items)) stop("handwritten control cannot be linked to its generated zigr path")
  control <- add_zigr_comparison(control, compare_items, direction = "zigr_over_row")
  control$owner <- evidence_fallback(control$owner, "performance")
  control$owner[control$relative_result == "LOSS"] <- "performance"
  control$measurement_status <- control$status
  control$report_status <- "CONTROL_ONLY"
  control$zigr_over_control_ratio <- control$ratio
  control$zigr_over_control_ratio_ci_low <- control$ratio_ci_low
  control$zigr_over_control_ratio_ci_high <- control$ratio_ci_high
  control$zigr_relative_to_control <- control$relative_result

  diagnostic <- cells[cells$universe == "task", ]
  diagnostic$claim_eligible <- FALSE
  diagnostic$exclusion_reason <- evidence_fallback(
    diagnostic$reason,
    "historical task cell retained only in the complete diagnostic matrix"
  )
  diagnostic$owner <- evidence_fallback(diagnostic$owner, "performance")
  diagnostic$measurement_status <- diagnostic$status
  diagnostic$noise_status <- ifelse(
    diagnostic$status == "PASS" & diagnostic$cv_pct <= low_noise_cv_threshold,
    "low_noise", ifelse(diagnostic$status == "PASS", "high_noise", "not_measured")
  )
  source_records <- source_bundle$records
  capability <- do.call(rbind, lapply(source_records, function(record) {
    row <- fixture_cells[
      fixture_cells$runner == record$runner & fixture_cells$item_id == record$fixture &
        fixture_cells$variant == "public",
      , drop = FALSE
    ]
    if (nrow(row) != 1L) stop("fixture source record has no unique measured result")
    data.frame(
      run_id = as.character(run_metadata$run_id), runner = as.character(record$runner),
      fixture = as.character(record$fixture), source_class = as.character(record$source_class),
      source_paths = {
        paths <- paste(as.character(unlist(record$source_paths, use.names = FALSE)), collapse = ";")
        if (nzchar(paths)) paths else "not_applicable"
      },
      verification_digest = as.character(record$verification_digest),
      fixture_result = if (isTRUE(record$executable)) as.character(row$status) else "GAP",
      gap_reason = if (isTRUE(record$executable)) "" else as.character(record$reason),
      owner = evidence_fallback(row$owner, "performance"), claim_eligible = FALSE,
      artifact_digest = as.character(row$artifact_digest),
      source_ledger_identity_digest = as.character(row$source_ledger_identity_digest),
      stringsAsFactors = FALSE
    )
  }))

  staging <- tempfile("separated-report-staging-", tmpdir = results_dir)
  dir.create(staging, recursive = TRUE)
  on.exit(unlink(staging, recursive = TRUE), add = TRUE)
  safety_paths <- file.path(staging, paste0("safety-", expected_runners, ".csv"))
  for (index in seq_along(expected_runners)) {
    runner <- expected_runners[[index]]
    command <- c(
      "benchmark_worker.R", "--kind=fixture", paste0("--runner=", runner), paste0("--run-dir=", results_dir),
      "--proof-only", paste0("--proof-output=", safety_paths[[index]])
    )
    output <- system2(
      "Rscript", shQuote(command), stdout = TRUE, stderr = TRUE,
      env = c("OPENBLAS_NUM_THREADS=1", "OMP_NUM_THREADS=1", "MKL_NUM_THREADS=1")
    )
    status <- attr(output, "status")
    if (!is.null(status) && status != 0L) {
      stop(sprintf("fixture safety subprocess failed for %s: %s", runner, paste(output, collapse = "\n")))
    }
    if (!file.exists(safety_paths[[index]])) stop(sprintf("fixture safety proof is missing for %s", runner))
  }
  safety <- do.call(rbind, lapply(safety_paths, read.csv, stringsAsFactors = FALSE))

  product_output <- product[, c(
    "run_id", "universe", "item_id", "group_id", "display_name", "runner", "implementation_role",
    "evidence_use", "path_kind", "representation_strategy", "comparison_tier", "input_fingerprint",
    "kernel_id", "contract_version", "fixture_version", "mutation_policy", "setup_policy",
    "measurement_status", "report_status", "claim_eligible",
    "reason", "owner", "correctness_status", "median_ms", "median_ci_low_ms", "median_ci_high_ms",
    "cv_pct", "timer_noise_floor_ms", "timer_noise_status", "zigr_median_ms", "zigr_median_ci_low_ms",
    "zigr_median_ci_high_ms", "zigr_cv_pct", "zigr_timer_noise_status", "row_over_zigr_ratio",
    "row_over_zigr_ratio_ci_low", "row_over_zigr_ratio_ci_high", "noise_status", "row_relative_to_zigr",
    "source_label", "source_verification_digest",
    "artifact_digest", "source_ledger_identity_digest"
  )]
  strategy_output <- strategy[, names(product_output)]
  r_output <- r_baseline[, c(
    "run_id", "universe", "item_id", "row_id", "runner", "baseline_class", "evidence_use", "path_kind",
    "representation_strategy", "input_fingerprint", "kernel_id", "contract_version", "fixture_version",
    "mutation_policy", "setup_policy", "measurement_status", "claim_eligible", "reason", "owner",
    "backend_provenance", "correctness_status", "median_ms", "median_ci_low_ms", "median_ci_high_ms",
    "cv_pct", "timer_noise_floor_ms", "timer_noise_status", "zigr_median_ms", "zigr_median_ci_low_ms",
    "zigr_median_ci_high_ms", "zigr_cv_pct", "zigr_timer_noise_status", "zigr_over_r_ratio",
    "zigr_over_r_ratio_ci_low", "zigr_over_r_ratio_ci_high", "noise_status", "zigr_relative_to_r",
    "source_label", "source_verification_digest",
    "artifact_digest", "source_ledger_identity_digest"
  )]
  control_output <- control[, c(
    "run_id", "universe", "item_id", "row_id", "runner", "control_role", "evidence_use", "path_kind",
    "representation_strategy", "input_fingerprint", "kernel_id", "contract_version", "fixture_version",
    "mutation_policy", "setup_policy", "measurement_status", "report_status", "claim_eligible",
    "reason", "owner", "correctness_status", "median_ms", "median_ci_low_ms", "median_ci_high_ms",
    "cv_pct", "timer_noise_floor_ms", "timer_noise_status", "zigr_reference_item_id", "zigr_median_ms",
    "zigr_median_ci_low_ms", "zigr_median_ci_high_ms", "zigr_cv_pct", "zigr_timer_noise_status",
    "zigr_over_control_ratio", "zigr_over_control_ratio_ci_low", "zigr_over_control_ratio_ci_high",
    "noise_status", "zigr_relative_to_control", "source_label",
    "source_verification_digest", "artifact_digest", "source_ledger_identity_digest"
  )]
  diagnostic_output <- diagnostic[, c(
    "run_id", "universe", "item_id", "row_id", "runner", "implementation_role", "evidence_use", "path_kind",
    "representation_strategy", "comparison_tier", "input_fingerprint", "kernel_id", "contract_version",
    "fixture_version", "mutation_policy", "setup_policy", "disposition", "measurement_status",
    "claim_eligible", "source_label", "source_verification_digest", "correctness_status",
    "correctness_message", "median_ms", "median_ci_low_ms", "median_ci_high_ms", "cv_pct",
    "timer_noise_floor_ms", "timer_noise_status", "noise_status", "exclusion_reason", "owner", "artifact_digest",
    "source_ledger_identity_digest"
  )]

  validate_product_metrics(product_output)
  validate_strategy_metrics(strategy_output)
  validate_r_baseline_metrics(
    r_output, expected_tasks, c(evidence$fixtures, "F03_optimized_base_r", "F04_optimized_base_r")
  )
  validate_control_metrics(control_output)
  expected_task_keys <- unlist(lapply(expected_runners, function(runner) paste(runner, expected_tasks, sep = "\r")))
  validate_diagnostic_metrics(diagnostic_output, expected_task_keys)
  validate_capability_matrix(capability, paste(evidence$fixture_rows$runner, evidence$fixture_rows$fixture, sep = "\r"))
  validate_safety_results(safety, expected_runners)

  analysis <- build_analysis_summary(summaries, manifest, run_metadata$run_id)

  outputs <- list(
    product = product_output, strategy = strategy_output, r_baseline = r_output,
    control = control_output, diagnostic = diagnostic_output, capability = capability, safety = safety,
    analysis = analysis
  )
  report_files <- separated_report_files()
  staged_reports <- file.path(staging, unname(report_files))
  for (index in seq_along(outputs)) write_csv(outputs[[index]], staged_reports[[index]])
  final_reports <- file.path(results_dir, unname(report_files))
  manifest_path <- file.path(results_dir, "report_manifest.json")
  existing <- c(final_reports[file.exists(final_reports)], manifest_path[file.exists(manifest_path)])
  if (length(existing) > 0L) {
    stop(sprintf("separated report output already exists: %s", paste(basename(existing), collapse = ", ")))
  }
  report_records <- lapply(seq_along(report_files), function(index) list(
    role = names(report_files)[[index]], file = unname(report_files[[index]]),
    rows = nrow(outputs[[index]]), md5 = unname(as.character(tools::md5sum(staged_reports[[index]]))[[1L]])
  ))
  manifest_record <- list(
    schema_version = "separated-report-v2", run_id = as.character(run_metadata$run_id),
    evidence_schema_version = as.integer(evidence$schema_version),
    evidence_vocabulary_version = as.character(evidence$vocabulary_version),
    recorded_source_tree_digest = as.character(run_metadata$environment$source_tree$digest),
    task_correctness_artifact_digest = as.character(correctness_identity$task_artifact_digest),
    fixture_correctness_artifact_digest = as.character(correctness_identity$fixture_artifact_digest),
    report_policy = list(
      low_noise_cv_threshold_pct = low_noise_cv_threshold,
      meaningful_margin_ratio = meaningful_margin,
      timer_noise_floor_ms = timer_noise_floor_ms,
      median_ci_level = as.numeric(timing_policy$median_ci_level),
      median_ci_method = as.character(timing_policy$median_ci_method)
    ),
    report_code_sources = lapply(c(
      "export_comparative_metrics.R", "benchmark_worker.R", "lib/specification.R",
      "lib/measurement.R", "lib/provenance.R", "lib/run_manifest.R", "lib/product_fixtures.R", "task_manifest.csv",
      "evidence_manifest.json", "source_ledger.json"
    ), function(path) list(path = path, md5 = unname(as.character(tools::md5sum(file.path(root_dir, path)))[[1L]]))),
    reports = report_records
  )
  staged_manifest <- file.path(staging, "report_manifest.json")
  jsonlite::write_json(manifest_record, staged_manifest, auto_unbox = TRUE, pretty = TRUE)
  published <- character(0)
  for (index in seq_along(final_reports)) {
    if (!file.rename(staged_reports[[index]], final_reports[[index]])) {
      unlink(published)
      stop(sprintf("cannot publish separated report: %s", final_reports[[index]]))
    }
    published <- c(published, final_reports[[index]])
  }
  if (!file.rename(staged_manifest, manifest_path)) {
    unlink(published)
    stop("cannot publish separated report manifest")
  }
  cat(sprintf(
    "Exported eight separated reports for run %s: %s.\n",
    run_metadata$run_id,
    paste(sprintf("%s=%d", names(outputs), vapply(outputs, nrow, integer(1))), collapse = ", ")
  ))
  invisible(outputs)
}

classified_matrix <- identical(expected_runners, sort(evidence_schema_vocabulary()$runners)) &&
  identical(expected_tasks, sort(as.character(manifest$task)))
if (!classified_matrix) {
  stop("comparative export requires the current complete runner and task matrix")
}
export_separated_metrics()
