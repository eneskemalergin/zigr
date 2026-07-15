#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
results_dir <- NULL
low_noise_cv_threshold <- NULL
meaningful_margin <- NULL
low_noise_override <- FALSE
meaningful_margin_override <- FALSE
root_dir <- normalizePath(".")
source(file.path(root_dir, "lib", "task_manifest.R"))
source(file.path(root_dir, "lib", "harness.R"))
source(file.path(root_dir, "lib", "input_contract.R"))
source(file.path(root_dir, "lib", "r_provenance.R"))
source(file.path(root_dir, "lib", "source_ledger.R"))
source(file.path(root_dir, "lib", "environment_manifest.R"))
source(file.path(root_dir, "lib", "run_manifest.R"))
source(file.path(root_dir, "lib", "evidence_schema.R"))
source(file.path(root_dir, "lib", "product_fixtures.R"))
source(file.path(root_dir, "lib", "fixture_measurement.R"))
manifest <- load_task_manifest(root_dir)

for (arg in args) {
  if (grepl("^--run-dir=", arg)) {
    results_dir <- sub("^--run-dir=", "", arg)
  } else if (grepl("^--results-dir=", arg)) {
    stop("--results-dir is retired; pass one completed run with --run-dir=")
  } else if (grepl("^--low-noise-cv=", arg)) {
    low_noise_cv_threshold <- as.numeric(sub("^--low-noise-cv=", "", arg))
    low_noise_override <- TRUE
  } else if (grepl("^--meaningful-margin=", arg)) {
    meaningful_margin <- as.numeric(sub("^--meaningful-margin=", "", arg))
    meaningful_margin_override <- TRUE
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

export_separated_metrics <- function() {
  evidence <- load_evidence_manifest(root_dir, manifest)
  correctness_identity <- validate_correctness_artifacts(results_dir, run_metadata, evidence)
  validate_fixture_measurement_artifacts(results_dir, run_metadata, evidence)
  source_bundle <- verify_fixture_source_paths(root_dir, evidence)

  report_ci <- function(values) {
    values <- sort(as.numeric(values[is.finite(values)]))
    n <- length(values)
    if (n < 2L) return(c(low = NA_real_, high = NA_real_))
    alpha <- 1 - as.numeric(timing_policy$median_ci_level)
    low_rank <- max(1L, as.integer(qbinom(alpha / 2, n, 0.5)))
    high_rank <- min(n, as.integer(qbinom(1 - alpha / 2, n, 0.5)) + 1L)
    c(low = values[[low_rank]], high = values[[high_rank]])
  }
  raw_ci <- function(universe, runner, item_id, row_id, status) {
    if (!identical(as.character(status), "PASS")) return(c(low = NA_real_, high = NA_real_))
    path <- if (identical(universe, "task")) {
      file.path(results_dir, runner, sprintf("task_%s.csv", item_id))
    } else {
      file.path(results_dir, "fixtures", runner, paste0(row_id, ".csv"))
    }
    if (!file.exists(path)) stop(sprintf("raw timing samples missing: %s", path))
    raw <- read.csv(path, stringsAsFactors = FALSE)
    if (!"wall_ms" %in% names(raw)) stop(sprintf("raw timing samples missing wall_ms: %s", path))
    report_ci(raw$wall_ms)
  }
  evidence_owner <- function(values, fallback = "performance") {
    values <- as.character(values)
    values[!nzchar(values)] <- fallback
    values
  }
  evidence_reason <- function(values, fallback) {
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

  fixture_files <- file.path(results_dir, paste0("fixture_", expected_runners, "_summary.csv"))
  fixture_summaries <- do.call(rbind, lapply(fixture_files, read.csv, stringsAsFactors = FALSE))
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
    rows$zigr_reference_item_id <- as.character(compare_items)
    rows$zigr_median_ms <- NA_real_
    rows$zigr_median_ci_low_ms <- NA_real_
    rows$zigr_median_ci_high_ms <- NA_real_
    rows$zigr_cv_pct <- NA_real_
    rows$zigr_timer_noise_status <- "not_measured"
    rows$ratio <- NA_real_
    rows$ratio_ci_low <- NA_real_
    rows$ratio_ci_high <- NA_real_
    rows$noise_status <- "not_comparable"
    rows$relative_result <- "NOT_COMPARABLE"
    for (index in seq_len(nrow(rows))) {
      reference <- cells[
        cells$universe == rows$universe[[index]] & cells$item_id == compare_items[[index]] &
          cells$runner == "zigr" & cells$variant == "public",
        , drop = FALSE
      ]
      if (nrow(reference) != 1L) stop("zigr comparison reference is not unique")
      rows$zigr_median_ms[[index]] <- reference$median_ms[[1L]]
      rows$zigr_median_ci_low_ms[[index]] <- reference$median_ci_low_ms[[1L]]
      rows$zigr_median_ci_high_ms[[index]] <- reference$median_ci_high_ms[[1L]]
      rows$zigr_cv_pct[[index]] <- reference$cv_pct[[1L]]
      rows$zigr_timer_noise_status[[index]] <- reference$timer_noise_status[[1L]]
      comparable <- is.finite(rows$median_ms[[index]]) && is.finite(reference$median_ms[[1L]])
      if (!comparable) next
      if (identical(direction, "zigr_over_row")) {
        rows$ratio[[index]] <- reference$median_ms[[1L]] / rows$median_ms[[index]]
        rows$ratio_ci_low[[index]] <- reference$median_ci_low_ms[[1L]] / rows$median_ci_high_ms[[index]]
        rows$ratio_ci_high[[index]] <- reference$median_ci_high_ms[[1L]] / rows$median_ci_low_ms[[index]]
      } else {
        rows$ratio[[index]] <- rows$median_ms[[index]] / reference$median_ms[[1L]]
        rows$ratio_ci_low[[index]] <- rows$median_ci_low_ms[[index]] / reference$median_ci_high_ms[[1L]]
        rows$ratio_ci_high[[index]] <- rows$median_ci_high_ms[[index]] / reference$median_ci_low_ms[[1L]]
      }
      rows$noise_status[[index]] <- if (
        rows$cv_pct[[index]] <= low_noise_cv_threshold && reference$cv_pct[[1L]] <= low_noise_cv_threshold
      ) "low_noise" else "high_noise"
      rows$relative_result[[index]] <- if (rows$ratio[[index]] > meaningful_margin) {
        "LOSS"
      } else if (rows$ratio[[index]] < 1 / meaningful_margin) {
        "WIN"
      } else {
        "TIE"
      }
    }
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
    evidence_reason(product$reason, ifelse(
      product$product_status == "LINKED_BASELINE", "linked R or registered-C baseline retained for the comparison",
      "row is not a Tier A product-public result for this comparison"
    ))
  )
  product$owner <- evidence_owner(product$owner)
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
    evidence_reason(strategy$reason, "linked baseline, gap, or non-Tier-B row retained for context")
  )
  strategy$owner <- evidence_owner(strategy$owner)
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
  r_baseline$owner <- evidence_owner(r_baseline$owner)
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
  control$owner <- evidence_owner(control$owner)
  control$owner[control$relative_result == "LOSS"] <- "performance"
  control$measurement_status <- control$status
  control$report_status <- "CONTROL_ONLY"
  control$zigr_over_control_ratio <- control$ratio
  control$zigr_over_control_ratio_ci_low <- control$ratio_ci_low
  control$zigr_over_control_ratio_ci_high <- control$ratio_ci_high
  control$zigr_relative_to_control <- control$relative_result

  diagnostic <- cells[cells$universe == "task", ]
  diagnostic$claim_eligible <- FALSE
  diagnostic$exclusion_reason <- evidence_reason(
    diagnostic$reason,
    "historical task cell retained only in the complete diagnostic matrix"
  )
  diagnostic$owner <- evidence_owner(diagnostic$owner)
  diagnostic$measurement_status <- diagnostic$status
  diagnostic$noise_status <- ifelse(
    diagnostic$status == "PASS" & diagnostic$cv_pct <= low_noise_cv_threshold,
    "low_noise", ifelse(diagnostic$status == "PASS", "high_noise", "not_measured")
  )
  probes <- data.frame(
    run_id = as.character(run_metadata$run_id), universe = "system_probe",
    item_id = c("44_build_time", "45_binary_size", "46_cross_compile", "47_allocator_count"),
    row_id = c("44_build_time", "45_binary_size", "46_cross_compile", "47_allocator_count"),
    display_name = c("Build time", "Binary size", "Cross compile", "Allocator count"),
    category = "system_probe", group_id = "system_probe", runner = "system",
    variant = "quarantined", implementation_role = "diagnostic_control",
    evidence_use = "diagnostic_control", path_kind = "unclassified",
    representation_strategy = "not_applicable", comparison_tier = "tier_d",
    input_fingerprint = "not_applicable", kernel_id = "not_applicable",
    contract_version = "not_applicable", fixture_version = "not_applicable",
    mutation_policy = "not_applicable", setup_policy = "not_applicable",
    disposition = "source_invalid_system_probe", reason = "legacy system probe mutates or rebuilds the measured source tree",
    owner = "portability", status = "EXCLUDED", correctness_status = "NOT_APPLICABLE",
    correctness_message = "excluded from product, strategy, baseline, and control evidence",
    median_ms = NA_real_, cv_pct = NA_real_, timer_noise_floor_ms = timer_noise_floor_ms,
    timer_noise_status = "not_measured",
    source_label = "source_invalid_system_probe", source_verification_digest = "not_applicable",
    artifact_digest = "not_applicable", source_ledger_identity_digest = "not_applicable",
    backend_provenance = "not_applicable", median_ci_low_ms = NA_real_, median_ci_high_ms = NA_real_,
    claim_eligible = FALSE,
    exclusion_reason = "legacy system probe mutates or rebuilds the measured source tree",
    noise_status = "not_measured", measurement_status = "EXCLUDED", stringsAsFactors = FALSE
  )
  diagnostic <- rbind(diagnostic, probes)

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
      owner = evidence_owner(row$owner), claim_eligible = FALSE,
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
      "fixture_subprocess.R", paste0("--runner=", runner), paste0("--run-dir=", results_dir),
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
  validate_diagnostic_metrics(
    diagnostic_output, expected_task_keys,
    c("44_build_time", "45_binary_size", "46_cross_compile", "47_allocator_count")
  )
  validate_capability_matrix(capability, paste(evidence$fixture_rows$runner, evidence$fixture_rows$fixture, sep = "\r"))
  validate_safety_results(safety, expected_runners)

  outputs <- list(
    product = product_output, strategy = strategy_output, r_baseline = r_output,
    control = control_output, diagnostic = diagnostic_output, capability = capability, safety = safety
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
    schema_version = "separated-report-v1", run_id = as.character(run_metadata$run_id),
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
      "export_comparative_metrics.R", "fixture_subprocess.R", "lib/evidence_schema.R",
      "lib/fixture_measurement.R", "lib/product_fixtures.R", "lib/task_manifest.R",
      "lib/input_contract.R", "lib/r_provenance.R", "lib/source_ledger.R",
      "lib/environment_manifest.R", "lib/run_manifest.R", "task_manifest.csv",
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
    "Exported seven separated reports for run %s: %s.\n",
    run_metadata$run_id,
    paste(sprintf("%s=%d", names(outputs), vapply(outputs, nrow, integer(1))), collapse = ", ")
  ))
  invisible(outputs)
}

classified_matrix <- identical(expected_runners, sort(evidence_schema_vocabulary()$runners)) &&
  length(expected_tasks) == 83L
if (classified_matrix) {
  if (low_noise_override || meaningful_margin_override) {
    stop("classified report export refuses policy overrides; use the completed run timing policy")
  }
  export_separated_metrics()
  quit(save = "no", status = 0L, runLast = FALSE)
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
