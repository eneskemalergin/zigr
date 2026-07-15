#!/usr/bin/env Rscript

library(jsonlite)
root_dir <- normalizePath(".")
source(file.path(root_dir, "lib", "specification.R"))
source(file.path(root_dir, "lib", "measurement.R"))
source(file.path(root_dir, "lib", "provenance.R"))
source(file.path(root_dir, "lib", "run_manifest.R"))
source(file.path(root_dir, "lib", "product_fixtures.R"))

args <- commandArgs(trailingOnly = TRUE)
validate_cli_arguments(
  args,
  value_options = c("runners", "tasks", "run-dir", "seed", "prune-runs"),
  flag_options = c("build", "correctness-only"),
  label = "benchmark"
)
runners_filter <- NULL
tasks_filter   <- NULL
do_build       <- FALSE
correctness_only <- FALSE
run_dir_arg    <- NULL
prune_runs     <- NULL
master_seed    <- benchmark_master_seed()
for (a in args) {
  if (grepl("^--runners=", a)) runners_filter <- parse_csv_option(sub("^--runners=", "", a), "runner filter")
  if (grepl("^--tasks=", a))  tasks_filter <- parse_task_filter(sub("^--tasks=", "", a))
  if (a == "--build")         do_build      <- TRUE
  if (a == "--correctness-only") correctness_only <- TRUE
  if (grepl("^--run-dir=", a)) run_dir_arg <- sub("^--run-dir=", "", a)
  if (grepl("^--seed=", a)) master_seed <- input_scalar_integer(sub("^--seed=", "", a), "master seed")
  if (grepl("^--prune-runs=", a)) prune_runs <- sub("^--prune-runs=", "", a)
}

if (!is.null(prune_runs)) {
  if (length(args) != 1L) stop("--prune-runs cannot be combined with benchmark arguments")
  receipt_path <- file.path(root_dir, "results", "CANONICAL_RUN.json")
  protected <- if (file.exists(receipt_path)) {
    receipt <- fromJSON(receipt_path, simplifyVector = FALSE)
    validate_run_promotion_receipt(receipt)
    if (length(receipt$run_id) != 1L || is.na(receipt$run_id) || !nzchar(as.character(receipt$run_id))) {
      stop("canonical acceptance receipt has no run ID")
    }
    as.character(receipt$run_id)
  } else {
    character(0)
  }
  removed <- prune_local_runs(file.path(root_dir, "results"), prune_runs, protected)
  cat(sprintf(
    "Removed %d local run director%s%s\n",
    length(removed),
    if (length(removed) == 1L) "y" else "ies",
    if (length(removed) == 0L) "." else paste0(": ", paste(removed, collapse = ", "))
  ))
  quit(save = "no", status = 0L, runLast = FALSE)
}
manifest <- load_task_manifest(root_dir)
evidence <- load_evidence_manifest(root_dir, manifest)
raw_runner_configs <- load_runner_configs(root_dir)
all_runners <- list()
for (runner_name in names(raw_runner_configs)) {
  cfg <- raw_runner_configs[[runner_name]]
  if (!is.null(cfg$status) && cfg$status == "broken") next
  cfg <- hydrate_runner_config(manifest, cfg, cfg$name, evidence)
  all_runners[[cfg$name]] <- cfg
}

if (!is.null(runners_filter)) {
  unknown_runners <- setdiff(runners_filter, names(all_runners))
  if (length(unknown_runners) > 0L) stop(sprintf("runner filter contains unknown runners: %s", paste(unknown_runners, collapse = ", ")))
  all_runners <- all_runners[intersect(names(all_runners), runners_filter)]
}
if (length(all_runners) == 0L) stop("no active runners selected")

task_numbers <- as.integer(sub("([0-9]+).*", "\\1", manifest$task))
selected_tasks <- manifest$task
if (!is.null(tasks_filter)) {
  selected_tasks <- manifest$task[task_numbers %in% tasks_filter]
  if (length(selected_tasks) == 0L) stop("task filter selected no manifest tasks")
}

source(file.path(root_dir, "src", "r", "run_all.R"))
r_config <- raw_runner_configs$r
r_runner_map <- r_config$exports
r_reference_exports <- r_reference_map(r_config)
r_evidence_rows <- evidence$tasks[evidence$tasks$runner == "r", , drop = FALSE]
r_provenance <- build_run_r_provenance(
  selected_tasks, r_runner_map, r_reference_exports, manifest, r_evidence_rows
)

coverage_args <- c("check_coverage.R")
if (!is.null(tasks_filter)) coverage_args <- c(coverage_args, sprintf("--tasks=%s", paste(tasks_filter, collapse = ",")))
if (!is.null(tasks_filter) || !is.null(runners_filter)) coverage_args <- c(coverage_args, "--quick")
blas_env <- c("OPENBLAS_NUM_THREADS=1")
cat(sprintf(
  "Preflight: %s\n",
  if ("--quick" %in% coverage_args) "quick manifest and selected-task admission" else "full static trust suite"
))
coverage_code <- system2("Rscript", args = coverage_args, env = blas_env, stdout = "", stderr = "")
if (!identical(coverage_code, 0L)) stop(sprintf("coverage preflight failed with exit code %d", coverage_code))

run_dir <- if (is.null(run_dir_arg)) {
  run_id <- paste0(format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"), "-pid", Sys.getpid())
  file.path(root_dir, "results", "runs", run_id)
} else {
  normalizePath(run_dir_arg, mustWork = FALSE)
}
run_id <- basename(run_dir)
if (file.exists(run_manifest_path(run_dir))) stop(sprintf("run directory already exists: %s", run_dir))
existing_entries <- if (dir.exists(run_dir)) list.files(run_dir, all.files = TRUE, no.. = TRUE) else character(0)
if (length(existing_entries) > 0L) stop(sprintf("run directory is not empty: %s", run_dir))

project_runs_root <- normalizePath(file.path(root_dir, "results", "runs"), mustWork = FALSE)
if (identical(dirname(run_dir), project_runs_root)) {
  stale_runs <- reconcile_running_runs(
    file.path(root_dir, "results"),
    replacement_run_id = run_id
  )
  if (length(stale_runs) > 0L) {
    cat(sprintf("Reconciled stale runs: %s\n\n", paste(stale_runs, collapse = ", ")))
  }
}

run_metadata <- list(
  schema_version = 3L,
  artifact_layout = "grouped-v1",
  run_id = run_id,
  status = "running",
  started_at = run_manifest_timestamp(),
  runners = sort(names(all_runners)),
  tasks = selected_tasks,
  master_seed = master_seed,
  input_manifest = list(relative_path = "input_manifest.json", digest = "pending"),
  runner_dispositions = run_disposition_records(evidence, sort(names(all_runners)), selected_tasks),
  r_provenance = compact_run_r_provenance(r_provenance),
  timing_policy = benchmark_timing_policy(),
  boundary_budget_policy_version = boundary_budget_policy_version(),
  full_matrix = is.null(runners_filter) && is.null(tasks_filter),
  measurement_mode = if (correctness_only) "correctness_only" else "timed",
  command = commandArgs()
)
write_run_manifest(run_dir, run_metadata)
run_complete <- FALSE
run_error <- NULL
previous_error_handler <- getOption("error")
mark_run_incomplete <- function() {
  if (!run_complete) {
    message <- if (is.null(run_error)) geterrmessage() else run_error
    try(update_run_manifest(run_dir, "incomplete", message), silent = TRUE)
  }
}
options(error = function() {
  mark_run_incomplete()
  options(error = previous_error_handler)
  if (is.function(previous_error_handler)) previous_error_handler()
  quit(save = "no", status = 1L, runLast = FALSE)
})

input_manifest_path <- file.path(run_dir, run_metadata$input_manifest$relative_path)
prepare_args <- c(
  "benchmark_worker.R", "--kind=task",
  sprintf("--prepare-inputs=%s", input_manifest_path),
  sprintf("--master-seed=%d", master_seed)
)
if (!is.null(tasks_filter)) {
  prepare_args <- c(prepare_args, sprintf("--tasks=%s", paste(tasks_filter, collapse = ",")))
}
prepare_code <- system2("Rscript", args = prepare_args, env = blas_env, stdout = "", stderr = "")
if (!identical(prepare_code, 0L)) stop(sprintf("canonical input preparation failed with exit code %d", prepare_code))
run_metadata$input_manifest$digest <- unname(as.character(tools::md5sum(input_manifest_path))[[1L]])
prepared_inputs <- read_input_recipe_manifest(input_manifest_path)
if (!identical(sort(names(prepared_inputs$tasks)), sort(as.character(selected_tasks)))) {
  stop("canonical input preparation produced the wrong task set")
}
write_run_manifest(run_dir, run_metadata)

cat(sprintf("Runners: %s\n\n", paste(names(all_runners), collapse = ", ")))
cat(sprintf("Run: %s\n\n", run_id))
runner_failures <- character(0)

if (do_build) {
  cat("Building runners\n")
  code <- system("bash build_all.sh", ignore.stdout = FALSE, ignore.stderr = FALSE)
  if (code != 0) stop(sprintf("runner build failed with exit code %d", code))
  cat("\n")
}

checked_sexp_value <- tolower(Sys.getenv("ZIGR_CHECKED_SEXP", unset = "false"))
if (!(checked_sexp_value %in% c("1", "true", "yes", "on", "0", "false", "no", "off", ""))) {
  stop("ZIGR_CHECKED_SEXP must be a boolean value")
}

build_settings <- list(
  optimization = Sys.getenv("ZIGR_OPTIMIZE", unset = "ReleaseFast"),
  target = Sys.getenv("ZIGR_TARGET", unset = "native"),
  cpu_features = Sys.getenv("ZIGR_CPU_FEATURES", unset = "default"),
  checked_sexp = checked_sexp_value %in% c("1", "true", "yes", "on"),
  cache_dir = normalizePath(
    Sys.getenv("ZIG_CACHE_DIR", unset = file.path(root_dir, ".zig-cache")),
    mustWork = FALSE
  ),
  global_cache_dir = normalizePath(
    Sys.getenv("ZIG_GLOBAL_CACHE_DIR", unset = file.path(root_dir, ".zig-global-cache")),
    mustWork = FALSE
  ),
  command = if (do_build) "bash build_all.sh" else "prebuilt runner libraries",
  requested_rebuild = do_build
)
run_metadata$environment <- capture_environment_manifest(
  root_dir,
  all_runners,
  blas_env,
  build_settings,
  evidence,
  r_provenance,
  source_root = normalizePath("..")
)
validate_environment_manifest(run_metadata$environment)
write_run_manifest(run_dir, run_metadata)

worker_process_args <- function(kind, runner_name, validation_only = FALSE, validation_output = NULL,
                                timing = NULL) {
  if (!(kind %in% c("task", "fixture"))) stop("worker kind must be task or fixture")
  worker_args <- c(
    "benchmark_worker.R", sprintf("--kind=%s", kind),
    sprintf("--runner=%s", runner_name),
    sprintf("--%s=%s", if (identical(kind, "task")) "results-dir" else "run-dir", run_dir)
  )
  if (identical(kind, "task")) worker_args <- c(
    worker_args,
    sprintf("--input-manifest=%s", input_manifest_path),
    sprintf("--expected-input-manifest-digest=%s", run_metadata$input_manifest$digest),
    sprintf("--master-seed=%d", master_seed)
  )
  if (validation_only) {
    worker_args <- c(worker_args, "--validation-only", sprintf("--validation-output=%s", validation_output))
  } else {
    correctness_path <- if (identical(kind, "task")) task_correctness_path else fixture_correctness_path
    worker_args <- c(
      worker_args,
      sprintf("--validated-correctness=%s", correctness_path)
    )
  }
  if (identical(kind, "task") && !is.null(tasks_filter)) {
    worker_args <- c(worker_args, sprintf("--tasks=%s", paste(tasks_filter, collapse = ",")))
  }
  if (!is.null(timing)) {
    ids <- as.character(timing$ids)
    counts <- as.integer(timing$counts[ids])
    group_orders <- as.integer(timing$group_orders[ids])
    if (anyNA(counts) || anyNA(group_orders)) stop("worker timing selection is incomplete")
    if (identical(kind, "task")) {
      worker_args <- worker_args[!grepl("^--tasks=", worker_args)]
      task_ids <- as.integer(sub("([0-9]+).*", "\\1", ids))
      worker_args <- c(worker_args, sprintf("--tasks=%s", paste(task_ids, collapse = ",")))
    } else {
      worker_args <- c(worker_args, sprintf("--fixtures=%s", paste(ids, collapse = ",")))
    }
    worker_args <- c(
      worker_args,
      sprintf("--timing-stage=%s", timing$stage),
      sprintf("--timing-counts=%s", paste(paste(ids, counts, sep = "="), collapse = ",")),
      sprintf("--batch-output=%s", timing$output),
      sprintf("--batch=%d", timing$batch), sprintf("--attempt=%d", timing$attempt),
      sprintf("--process-epoch=%d", timing$process_epoch),
      sprintf("--member-order=%d", timing$member_order),
      sprintf("--group-orders=%s", paste(paste(ids, group_orders, sep = "="), collapse = ","))
    )
  }
  worker_args
}

cat("Trust and correctness preflight\n")
correctness_root <- file.path(run_dir, "correctness")
task_correctness_path <- run_correctness_artifact_paths(run_dir, run_metadata, "task")
fixture_correctness_path <- run_correctness_artifact_paths(run_dir, run_metadata, "fixture")
correctness_staging_root <- file.path(run_dir, ".staging", "correctness")
task_correctness_staging <- file.path(correctness_staging_root, "tasks")
fixture_correctness_staging <- file.path(correctness_staging_root, "fixtures")
for (rn in names(all_runners)) {
  code <- system2(
    "Rscript",
    args = worker_process_args("fixture",
      rn, validation_only = TRUE, validation_output = file.path(fixture_correctness_staging, paste0(rn, ".csv"))
    ),
    env = blas_env, stdout = "", stderr = ""
  )
  if (code != 0L) {
    run_error <- sprintf("fixture validation preflight failed for %s with exit code %d", rn, code)
    stop(run_error)
  }
}
for (rn in names(all_runners)) {
  code <- system2(
    "Rscript",
    args = worker_process_args("task",
      rn, validation_only = TRUE, validation_output = file.path(task_correctness_staging, paste0(rn, ".csv"))
    ),
    env = blas_env, stdout = "", stderr = ""
  )
  if (code != 0L) {
    run_error <- sprintf("runner validation preflight failed for %s with exit code %d", rn, code)
    stop(run_error)
  }
}
combine_csv_files_once(
  file.path(task_correctness_staging, paste0(names(all_runners), ".csv")),
  task_correctness_path,
  "task correctness evidence"
)
combine_csv_files_once(
  file.path(fixture_correctness_staging, paste0(names(all_runners), ".csv")),
  fixture_correctness_path,
  "fixture correctness evidence"
)
unlink(correctness_staging_root, recursive = TRUE)
cat("\n")

correctness_evidence <- validate_correctness_artifacts(run_dir, run_metadata, evidence)
validate_source_tree_identity(normalizePath(".."), run_metadata$environment$source_tree)

run_metadata$correctness_stage <- c(list(
  completed_at = run_manifest_timestamp(),
  fixture_runners = sort(names(all_runners)),
  task_runners = sort(names(all_runners)),
  tasks = as.list(selected_tasks),
  source_tree_digest = as.character(run_metadata$environment$source_tree$digest),
  source_ledger_identity_digest = as.character(run_metadata$environment$tool_source_ledger$identity_digest)
), correctness_evidence)
write_run_manifest(run_dir, run_metadata)

if (correctness_only) {
  validate_source_tree_identity(normalizePath(".."), run_metadata$environment$source_tree)
  run_metadata$status <- "correctness_complete"
  write_run_manifest(run_dir, run_metadata)
  run_complete <- TRUE
  options(error = previous_error_handler)
  cat("Correctness-only stage completed without timing artifacts.\n")
  quit(save = "no", status = 0L, runLast = FALSE)
}

timing_root <- file.path(run_dir, ".staging", "timing")
dir.create(timing_root, recursive = TRUE, showWarnings = FALSE)
timing_started_at <- proc.time()[["elapsed"]]
timing_failures <- list()
worker_process_epoch <- 0L

prior_packing_hint <- function(universe, groups) {
  runs_root <- file.path(root_dir, "results", "runs")
  empty <- list(run_id = NULL, estimated_ms = stats::setNames(rep(0, length(groups)), groups))
  if (!dir.exists(runs_root)) return(empty)
  candidates <- list.dirs(runs_root, recursive = FALSE, full.names = TRUE)
  candidates <- setdiff(candidates, normalizePath(run_dir, mustWork = FALSE))
  if (length(candidates) == 0L) return(empty)
  candidates <- candidates[order(file.info(candidates)$mtime, decreasing = TRUE)]
  for (candidate in candidates) {
    prior <- tryCatch(read_run_manifest(candidate), error = function(error) NULL)
    if (is.null(prior) || !identical(as.character(prior$status), "complete") ||
        !identical(
          as.character(prior$environment$source_tree$digest),
          as.character(run_metadata$environment$source_tree$digest)
        ) || !all(names(all_runners) %in% run_manifest_values(prior$runners))) next
    trusted <- tryCatch({
      validate_run_completion_contract(prior)
      validate_run_completion_artifacts(candidate, prior)
      TRUE
    }, error = function(error) FALSE)
    if (!trusted) next
    summary <- tryCatch(
      read_run_summary_table(candidate, prior, universe, names(all_runners)),
      error = function(error) NULL
    )
    if (is.null(summary)) next
    id <- if (identical(universe, "task")) as.character(summary$task) else as.character(summary$fixture)
    usable <- summary$status == "PASS" & summary$runner %in% names(all_runners) & id %in% groups &
      is.finite(as.numeric(summary$median_ms)) & as.numeric(summary$median_ms) >= 0
    estimates <- tapply(as.numeric(summary$median_ms[usable]), id[usable], sum)
    hint <- empty$estimated_ms
    hint[names(estimates)] <- as.numeric(estimates)
    return(list(run_id = as.character(prior$run_id), estimated_ms = hint))
  }
  empty
}

stage_files <- function(universe, stage, pattern) {
  root <- file.path(timing_root, universe, stage)
  if (!dir.exists(root)) return(character(0))
  list.files(root, pattern = pattern, recursive = TRUE, full.names = TRUE)
}

prepare_timing_stage <- function(stage, groups, counts, schedule_seed, packing_estimates = NULL) {
  group_rows <- timing_group_schedule(groups, schedule_seed)
  group_rows$estimated_ms <- if (identical(stage, "pilot")) {
    if (is.null(packing_estimates)) rep(0, nrow(group_rows)) else {
      as.numeric(packing_estimates[as.character(group_rows$group_id)]) *
        as.integer(run_metadata$timing_policy$pilot_iterations)
    }
  } else {
    as.numeric(counts$estimated_confirmation_ms[match(group_rows$group_id, counts$group_id)])
  }
  batches <- pack_timing_batches(group_rows, run_metadata$timing_policy)
  schedule <- timing_batch_schedule(batches, names(all_runners))
  list(schedule = schedule, batches = batches)
}

run_timing_stage <- function(universe, stage, groups, counts, schedule_seed, prepared = NULL,
                             packing_estimates = NULL) {
  if (is.null(prepared)) {
    prepared <- prepare_timing_stage(stage, groups, counts, schedule_seed, packing_estimates)
  }
  schedule <- prepared$schedule
  batches <- prepared$batches
  executor <- function(rows, timeout_seconds, attempt, batch_epoch) {
    batch_id <- unique(as.integer(rows$batch))
    offset <- (batch_id - 1L) %% length(all_runners)
    runner_order <- names(all_runners)[((seq_along(all_runners) - 1L + offset) %% length(all_runners)) + 1L]
    output_root <- file.path(
      timing_root, universe, stage,
      sprintf("batch-%03d-attempt-%d-epoch-%d", batch_id, attempt, batch_epoch)
    )
    ids <- as.character(rows$group_id)
    selected_counts <- if (identical(stage, "pilot")) {
      stats::setNames(rep(as.integer(run_metadata$timing_policy$pilot_iterations), length(ids)), ids)
    } else {
      stats::setNames(as.integer(counts$confirmation_iterations[match(ids, counts$group_id)]), ids)
    }
    group_orders <- stats::setNames(as.integer(rows$group_order), ids)
    codes <- integer(length(runner_order))
    batch_started <- proc.time()[["elapsed"]]
    for (index in seq_along(runner_order)) {
      runner <- runner_order[[index]]
      remaining_timeout <- floor(timeout_seconds - (proc.time()[["elapsed"]] - batch_started))
      if (remaining_timeout < 1L) {
        codes[index:length(codes)] <- 124L
        break
      }
      output <- file.path(output_root, runner)
      worker_process_epoch <<- worker_process_epoch + 1L
      args <- worker_process_args(universe, runner, timing = list(
        ids = ids, counts = selected_counts, group_orders = group_orders,
        stage = stage, output = output, batch = batch_id, attempt = attempt,
        process_epoch = worker_process_epoch, member_order = index
      ))
      codes[[index]] <- system2(
        "Rscript", args = args, env = blas_env, stdout = "", stderr = "",
        timeout = as.integer(remaining_timeout)
      )
    }
    ok <- all(!is.na(codes) & codes == 0L)
    timed_out <- any(codes == 124L, na.rm = TRUE)
    if (!ok && dir.exists(output_root)) {
      sample_files <- list.files(output_root, pattern = "^samples\\.csv$", recursive = TRUE, full.names = TRUE)
      reason <- if (timed_out) "batch_timeout" else "worker_failure"
      for (path in sample_files) {
        rows <- read.csv(path, stringsAsFactors = FALSE)
        rows$excluded <- TRUE
        rows$exclusion_reason <- reason
        write_csv(rows, path)
      }
      unlink(list.files(output_root, pattern = "_summary\\.csv$", recursive = TRUE, full.names = TRUE))
    }
    list(ok = ok, timed_out = timed_out, exit_codes = codes)
  }
  outcomes <- run_timing_batches(
    batches, executor,
    as.integer(run_metadata$timing_policy$batch_timeout_seconds),
    as.integer(run_metadata$timing_policy$total_run_budget_seconds),
    started_at = timing_started_at
  )
  bad <- outcomes[!grepl("^complete$", outcomes$status), , drop = FALSE]
  if (nrow(bad) > 0L) timing_failures[[paste(universe, stage, sep = ":")]] <<- bad
  list(schedule = schedule, batches = batches, outcomes = outcomes)
}

read_stage_samples <- function(universe, stage) {
  files <- stage_files(universe, stage, "^samples\\.csv$")
  if (length(files) == 0L) return(data.frame())
  do.call(rbind, lapply(files, read.csv, stringsAsFactors = FALSE))
}

pilot_plan_for <- function(universe, groups, eligible_groups) {
  raw <- read_stage_samples(universe, "pilot")
  if (nrow(raw) == 0L) {
    return(data.frame(
      group_id = groups, pilot_complete = FALSE, pilot_median_group_ms = NA_real_,
      pilot_max_cv_pct = NA_real_, pilot_max_drift_pct = NA_real_,
      confirmation_iterations = NA_integer_, estimated_confirmation_ms = NA_real_,
      status = ifelse(groups %in% eligible_groups, "incomplete", "unsupported"), stringsAsFactors = FALSE
    ))
  }
  if ("excluded" %in% names(raw)) raw <- raw[!as.logical(raw$excluded), , drop = FALSE]
  if (identical(universe, "task")) raw <- raw[raw$phase == "timed", , drop = FALSE]
  raw$group_id <- if (identical(universe, "task")) as.character(raw$task) else as.character(raw$fixture)
  raw$member_id <- if (identical(universe, "task")) {
    as.character(raw$runner)
  } else paste(raw$runner, raw$row_id, sep = ":")
  plan <- pilot_group_plan(raw[c("group_id", "member_id", "iteration", "wall_ms")], run_metadata$timing_policy)
  missing <- setdiff(groups, plan$group_id)
  if (length(missing) > 0L) plan <- rbind(plan, data.frame(
    group_id = missing, pilot_complete = FALSE, pilot_median_group_ms = NA_real_,
    pilot_max_cv_pct = NA_real_, pilot_max_drift_pct = NA_real_,
    confirmation_iterations = NA_integer_, estimated_confirmation_ms = NA_real_,
    status = ifelse(missing %in% eligible_groups, "incomplete", "unsupported"), stringsAsFactors = FALSE
  ))
  plan[match(groups, plan$group_id), , drop = FALSE]
}

task_groups <- as.character(selected_tasks)
fixture_groups <- as.character(evidence$fixtures)
task_eligible_groups <- unique(as.character(evidence$tasks$task[
  evidence$tasks$runner %in% names(all_runners) & evidence$tasks$timing_eligible
]))
fixture_eligible_groups <- unique(as.character(evidence$fixture_rows$fixture[
  evidence$fixture_rows$runner %in% names(all_runners) & evidence$fixture_rows$timing_eligible
]))
task_seed <- as.integer((as.double(master_seed) + 3001) %% 2147483646L)
fixture_seed <- as.integer((as.double(master_seed) + 6001) %% 2147483646L)
if (task_seed < 1L) task_seed <- 1L
if (fixture_seed < 1L) fixture_seed <- 1L

cat("Bounded pilot measurement\n")
task_hint <- prior_packing_hint("task", task_groups)
fixture_hint <- prior_packing_hint("fixture", fixture_groups)
task_pilot <- run_timing_stage(
  "task", "pilot", task_groups, NULL, task_seed, packing_estimates = task_hint$estimated_ms
)
fixture_pilot <- run_timing_stage(
  "fixture", "pilot", fixture_groups, NULL, fixture_seed, packing_estimates = fixture_hint$estimated_ms
)
task_plan <- pilot_plan_for("task", task_groups, task_eligible_groups)
fixture_plan <- pilot_plan_for("fixture", fixture_groups, fixture_eligible_groups)

confirmation_candidates <- function(universe, plan, seed) {
  eligible <- as.character(plan$group_id[plan$status == "confirmation"])
  if (length(eligible) == 0L) {
    return(data.frame(
      universe = character(), group_id = character(), group_order = integer(),
      estimated_ms = numeric(), stringsAsFactors = FALSE
    ))
  }
  ordered <- timing_group_schedule(eligible, seed)
  ordered$universe <- universe
  ordered$estimated_ms <- as.numeric(
    plan$estimated_confirmation_ms[match(ordered$group_id, plan$group_id)]
  )
  ordered[c("universe", "group_id", "group_order", "estimated_ms")]
}
confirmation_candidates <- rbind(
  confirmation_candidates("task", task_plan, task_seed),
  confirmation_candidates("fixture", fixture_plan, fixture_seed)
)
confirmation_candidates$budget_order <- seq_len(nrow(confirmation_candidates))
remaining_confirmation_budget_ms <- max(
  0,
  (as.numeric(run_metadata$timing_policy$total_run_budget_seconds) -
    (proc.time()[["elapsed"]] - timing_started_at)) * 1000
)
confirmation_budget <- admit_timing_budget(
  confirmation_candidates, remaining_confirmation_budget_ms
)
apply_confirmation_budget <- function(plan, universe) {
  decisions <- confirmation_budget[
    confirmation_budget$universe == universe & !confirmation_budget$admitted,
    , drop = FALSE
  ]
  if (nrow(decisions) > 0L) {
    plan$status[match(decisions$group_id, plan$group_id)] <- "incomplete"
  }
  plan
}
task_plan <- apply_confirmation_budget(task_plan, "task")
fixture_plan <- apply_confirmation_budget(fixture_plan, "fixture")

task_confirmation_groups <- task_plan$group_id[task_plan$status == "confirmation"]
fixture_confirmation_groups <- fixture_plan$group_id[fixture_plan$status == "confirmation"]
task_confirmation_frozen <- if (length(task_confirmation_groups) > 0L) {
  prepare_timing_stage("confirmation", task_confirmation_groups, task_plan, task_seed)
} else list(schedule = data.frame(), batches = data.frame())
fixture_confirmation_frozen <- if (length(fixture_confirmation_groups) > 0L) {
  prepare_timing_stage("confirmation", fixture_confirmation_groups, fixture_plan, fixture_seed)
} else list(schedule = data.frame(), batches = data.frame())

run_metadata$timing_execution <- list(
  schedule_seeds = list(task = task_seed, fixture = fixture_seed),
  packing_hint_run_ids = list(task = task_hint$run_id, fixture = fixture_hint$run_id),
  confirmation_budget = list(
    available_ms = remaining_confirmation_budget_ms,
    decisions = confirmation_budget
  ),
  task_plan = task_plan, fixture_plan = fixture_plan,
  task_pilot_schedule = task_pilot$schedule, fixture_pilot_schedule = fixture_pilot$schedule,
  task_pilot_batches = task_pilot$batches, fixture_pilot_batches = fixture_pilot$batches,
  task_confirmation_schedule = task_confirmation_frozen$schedule,
  fixture_confirmation_schedule = fixture_confirmation_frozen$schedule,
  task_confirmation_batches = task_confirmation_frozen$batches,
  fixture_confirmation_batches = fixture_confirmation_frozen$batches,
  frozen_at = run_manifest_timestamp()
)
write_run_manifest(run_dir, run_metadata)

task_confirmation <- if (length(task_confirmation_groups) > 0L) {
  run_timing_stage(
    "task", "confirmation", task_confirmation_groups, task_plan, task_seed,
    prepared = task_confirmation_frozen
  )
} else list(schedule = data.frame(), batches = data.frame(), outcomes = data.frame())
fixture_confirmation <- if (length(fixture_confirmation_groups) > 0L) {
  run_timing_stage(
    "fixture", "confirmation", fixture_confirmation_groups, fixture_plan, fixture_seed,
    prepared = fixture_confirmation_frozen
  )
} else list(schedule = data.frame(), batches = data.frame(), outcomes = data.frame())

run_metadata$timing_execution$outcomes <- list(
  task_pilot = task_pilot$outcomes, fixture_pilot = fixture_pilot$outcomes,
  task_confirmation = task_confirmation$outcomes,
  fixture_confirmation = fixture_confirmation$outcomes
)
run_metadata$timing_execution$finished_at <- run_manifest_timestamp()
write_run_manifest(run_dir, run_metadata)

write_timing_samples <- function(universe) {
  pilot_samples <- read_stage_samples(universe, "pilot")
  confirmation_samples <- read_stage_samples(universe, "confirmation")
  samples <- if (nrow(confirmation_samples) == 0L) pilot_samples else rbind(pilot_samples, confirmation_samples)
  if (nrow(samples) == 0L) return(invisible(NULL))
  write_csv_once(
    samples,
    run_sample_artifact_paths(run_dir, run_metadata, universe, names(all_runners)[[1L]], "."),
    paste(universe, "samples")
  )
}

write_timing_samples("task")
write_timing_samples("fixture")

if (length(timing_failures) > 0L) {
  unlink(timing_root, recursive = TRUE)
  run_error <- paste(
    "bounded timing left incomplete batches after continuing the queue:",
    paste(names(timing_failures), collapse = ", ")
  )
  stop(run_error)
}

consolidate_timing <- function(universe, plan) {
  pilot_files <- stage_files(universe, "pilot", "_summary\\.csv$")
  confirmation_files <- stage_files(universe, "confirmation", "_summary\\.csv$")
  pilot <- do.call(rbind, lapply(pilot_files, read.csv, stringsAsFactors = FALSE))
  confirmation <- if (length(confirmation_files) == 0L) pilot[0, , drop = FALSE] else {
    do.call(rbind, lapply(confirmation_files, read.csv, stringsAsFactors = FALSE))
  }
  id_field <- if (identical(universe, "task")) "task" else "fixture"
  confirmed <- plan$group_id[plan$status == "confirmation"]
  summary <- rbind(
    pilot[!(as.character(pilot[[id_field]]) %in% confirmed), , drop = FALSE],
    confirmation
  )
  key_fields <- if (identical(universe, "task")) c("runner", "task") else c("runner", "row_id")
  keys <- do.call(paste, c(summary[key_fields], sep = "\r"))
  if (anyDuplicated(keys)) stop(sprintf("%s staged summaries contain duplicate rows", universe))
  write_csv_once(summary, run_summary_artifact_paths(run_dir, run_metadata, universe), paste(universe, "summary"))
}

consolidate_timing("task", task_plan)
consolidate_timing("fixture", fixture_plan)
unlink(timing_root, recursive = TRUE)

final_correctness_evidence <- validate_correctness_artifacts(run_dir, run_metadata, evidence)
if (!identical(final_correctness_evidence, correctness_evidence)) {
  stop("correctness evidence changed after validation preflight")
}
validate_source_tree_identity(normalizePath(".."), run_metadata$environment$source_tree)
validate_run_artifacts(run_dir, run_metadata)
validate_fixture_measurement_artifacts(run_dir, run_metadata, evidence)
validate_source_tree_identity(normalizePath(".."), run_metadata$environment$source_tree)
run_metadata$completion_artifacts <- capture_run_completion_artifacts(run_dir, run_metadata)
run_metadata$status <- "complete"
run_metadata$finished_at <- run_manifest_timestamp()
run_metadata$status_message <- NULL
run_metadata$completion_contract <- capture_run_completion_contract(run_metadata)
write_run_manifest(run_dir, run_metadata)
validate_run_completion_contract(run_metadata)
validate_run_completion_artifacts(run_dir, run_metadata)
run_complete <- TRUE
options(error = previous_error_handler)
cat("Report generation is a separate explicit command.\n")
cat("Done.\n")
