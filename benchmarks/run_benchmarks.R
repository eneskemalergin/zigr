#!/usr/bin/env Rscript

library(jsonlite)

root_dir <- normalizePath(".")
source(file.path(root_dir, "lib", "measurement.R"))
source(file.path(root_dir, "lib", "run_manifest.R"))
source(file.path(root_dir, "lib", "product_fixtures.R"))

arguments <- commandArgs(trailingOnly = TRUE)
validate_cli_arguments(
  arguments,
  value_options = c("runners", "tasks", "memory-task", "run-dir", "seed"),
  flag_options = c("build", "correctness-only"),
  label = "benchmark"
)

option_value <- function(name) {
  matches <- grep(paste0("^--", name, "="), arguments, value = TRUE)
  if (length(matches) == 0L) {
    return(NULL)
  }
  sub(paste0("^--", name, "="), "", matches[[1L]])
}

runner_names <- direct_runner_names(root_dir)
selected_runners <- option_value("runners")
selected_runners <- if (is.null(selected_runners)) {
  runner_names
} else {
  strsplit(selected_runners, ",", fixed = TRUE)[[1L]]
}
if (any(!nzchar(selected_runners)) || anyDuplicated(selected_runners) ||
  !all(selected_runners %in% runner_names)) {
  stop("runner selection contains an unknown or duplicate runner")
}

specs <- benchmark_revision_task_specs()
task_ids <- vapply(specs, `[[`, character(1), "id")
selected_tasks <- option_value("tasks")
selected_tasks <- if (is.null(selected_tasks)) {
  task_ids
} else {
  strsplit(selected_tasks, ",", fixed = TRUE)[[1L]]
}
if (any(!nzchar(selected_tasks)) || anyDuplicated(selected_tasks) ||
  !all(selected_tasks %in% task_ids)) {
  stop("task selection contains an unknown or duplicate task")
}

master_seed <- option_value("seed")
master_seed <- if (is.null(master_seed)) {
  benchmark_master_seed()
} else {
  input_scalar_integer(master_seed, "master seed")
}
do_build <- "--build" %in% arguments
correctness_only <- "--correctness-only" %in% arguments
memory_task <- option_value("memory-task")
if (!is.null(memory_task) && (correctness_only || !(memory_task %in% selected_tasks) ||
  !isTRUE(direct_task_suitability_row(memory_task)$large_output))) {
  stop("memory task must be a selected large-output task in a timed run")
}

if (do_build) {
  build_status <- system("bash build_all.sh", ignore.stdout = FALSE, ignore.stderr = FALSE)
  if (!identical(build_status, 0L)) stop(sprintf("runner build failed with exit code %d", build_status))
}

run_dir_value <- option_value("run-dir")
run_id <- if (is.null(run_dir_value)) {
  paste0(format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"), "-pid", Sys.getpid())
} else {
  basename(normalizePath(run_dir_value, mustWork = FALSE))
}
run_dir <- if (is.null(run_dir_value)) {
  file.path(root_dir, "results", "runs", run_id)
} else {
  normalizePath(run_dir_value, mustWork = FALSE)
}
if (dir.exists(run_dir) && length(list.files(run_dir, all.files = TRUE, no.. = TRUE)) > 0L) {
  stop(sprintf("run directory is not empty: %s", run_dir))
}
dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)

artifacts <- lapply(selected_runners, function(runner) {
  path <- direct_runner_artifact_path(root_dir, runner)
  if (!file.exists(path)) stop(sprintf("runner artifact is missing for %s: %s", runner, path))
  list(
    runner = runner,
    relative_path = if (startsWith(path, paste0(root_dir, .Platform$file.sep))) {
      substring(path, nchar(root_dir) + 2L)
    } else {
      path
    },
    md5 = unname(as.character(tools::md5sum(path))[[1L]])
  )
})
names(artifacts) <- selected_runners

timing_policy <- benchmark_timing_policy()
batch_repetitions <- direct_batch_repetition_map(selected_tasks)
timing_policy$batch_repetitions <- as.list(batch_repetitions)
metadata <- list(
  schema_version = 4L,
  artifact_layout = "direct-v1",
  run_id = run_id,
  status = "running",
  started_at = run_manifest_timestamp(),
  runners = as.list(selected_runners),
  tasks = as.list(selected_tasks),
  master_seed = master_seed,
  input_recipe_version = "revision-v1",
  input_seeds = setNames(lapply(selected_tasks, function(task) {
    task_input_seed(master_seed, task, "revision-v1")
  }), selected_tasks),
  rng_event_seed = task_input_seed(master_seed, "rng", "direct-timing-v1"),
  source_tree = source_tree_identity(normalizePath("..")),
  artifacts = artifacts,
  timing_policy = timing_policy,
  measurement_mode = if (correctness_only) "correctness_only" else "timed",
  command = as.list(commandArgs())
)
if (!is.null(memory_task)) {
  metadata$memory_task <- memory_task
  metadata$memory_policy <- direct_memory_policy()
}
run_direct_benchmark <- function() {
  write_run_manifest(run_dir, metadata)

  completed <- FALSE
  failure <- NULL
  on.exit(
    {
      if (!completed) {
        message <- if (is.null(failure)) geterrmessage() else failure
        try(write_incomplete_run_manifest(run_dir, metadata, message), silent = TRUE)
      }
    },
    add = TRUE
  )

  staging <- file.path(run_dir, ".staging")
  correctness_staging <- file.path(staging, "correctness")
  sizing_staging <- file.path(staging, "sizing")
  timing_staging <- file.path(staging, "timing")
  dir.create(correctness_staging, recursive = TRUE, showWarnings = FALSE)
  blas_environment <- c("OPENBLAS_NUM_THREADS=1")
  task_argument <- paste(selected_tasks, collapse = ",")

  cat("Correctness\n")
  for (runner in selected_runners) {
    status <- system2(
      "Rscript",
      c(
        "benchmark_worker.R", paste0("--runner=", runner), "--mode=correctness",
        paste0("--output-root=", correctness_staging), paste0("--tasks=", task_argument),
        paste0("--master-seed=", master_seed)
      ),
      env = blas_environment,
      stdout = "", stderr = "",
      timeout = as.integer(timing_policy$worker_timeout_seconds)
    )
    if (!identical(status, 0L)) {
      failure <- sprintf("correctness worker failed for %s with exit code %d", runner, status)
      stop(failure)
    }
  }
  correctness_files <- file.path(correctness_staging, paste0(selected_runners, ".csv"))
  combine_csv_files_once(correctness_files, file.path(run_dir, "correctness.csv"), "correctness")
  correctness <- read.csv(file.path(run_dir, "correctness.csv"), stringsAsFactors = FALSE)
  expected_correctness <- expand.grid(
    runner = selected_runners, task = selected_tasks,
    stringsAsFactors = FALSE
  )
  if (nrow(correctness) != nrow(expected_correctness) || any(correctness$status != "PASS") ||
    !setequal(
      paste(correctness$runner, correctness$task),
      paste(expected_correctness$runner, expected_correctness$task)
    )) {
    stop("correctness artifact does not cover every selected runner and task")
  }
  unlink(correctness_staging, recursive = TRUE)
  metadata$correctness_completed_at <- run_manifest_timestamp()
  write_run_manifest(run_dir, metadata)

  if (correctness_only) {
    unlink(staging, recursive = TRUE)
    validate_source_tree_identity(normalizePath(".."), metadata$source_tree)
    metadata$status <- "correctness_complete"
    metadata$finished_at <- run_manifest_timestamp()
    metadata$outputs <- list(correctness = list(
      relative_path = "correctness.csv",
      md5 = unname(as.character(tools::md5sum(file.path(run_dir, "correctness.csv")))[[1L]])
    ))
    write_run_manifest(run_dir, metadata)
    completed <- TRUE
    cat("Correctness-only run complete.\n")
    quit(save = "no", status = 0L, runLast = FALSE)
  }

  dir.create(sizing_staging, recursive = TRUE, showWarnings = FALSE)
  cat("Batch sizing\n")
  timed_started <- proc.time()[["elapsed"]]
  remaining_timed_seconds <- function() {
    remaining_direct_run_seconds(
      timed_started, proc.time()[["elapsed"]], timing_policy$total_run_timeout_seconds
    )
  }
  active_tasks <- selected_tasks
  blocked_tasks <- character()
  sizing_rows <- list()
  sizing_timer_floors <- NULL
  sizing_policy <- timing_policy$sizing_policy
  for (count in validate_direct_sizing_policy(sizing_policy)$ladder) {
    if (length(active_tasks) == 0L) break
    round_staging <- file.path(sizing_staging, as.character(count))
    dir.create(round_staging, recursive = TRUE, showWarnings = FALSE)
    round_tasks <- paste(active_tasks, collapse = ",")
    round_map <- setNames(rep.int(count, length(active_tasks)), active_tasks)
    for (runner in selected_runners) {
      remaining <- remaining_timed_seconds()
      if (remaining < 1L) stop("total direct sizing and timing timeout expired")
      status <- system2(
        "Rscript",
        c(
          "benchmark_worker.R", paste0("--runner=", runner), "--mode=sizing",
          paste0("--output-root=", round_staging), paste0("--tasks=", round_tasks),
          paste0("--batch-repetitions=", format_named_integer_map(round_map, "batch repetitions")),
          if (identical(count, 1L)) character() else "--skip-probes",
          paste0("--master-seed=", master_seed)
        ),
        env = blas_environment,
        stdout = "", stderr = "",
        timeout = as.integer(min(timing_policy$worker_timeout_seconds, remaining))
      )
      if (!identical(status, 0L)) {
        failure <- sprintf("sizing worker failed for %s with exit code %d", runner, status)
        stop(failure)
      }
    }
    round_files <- file.path(round_staging, paste0(selected_runners, "-sizing.csv"))
    round_rows <- do.call(rbind, lapply(round_files, read.csv, stringsAsFactors = FALSE))
    if (identical(count, 1L)) {
      summary_files <- file.path(round_staging, paste0(selected_runners, "-probe-summary.csv"))
      summaries <- do.call(rbind, lapply(summary_files, read.csv, stringsAsFactors = FALSE))
      if (!identical(as.character(summaries$runner), selected_runners)) {
        stop("batch sizing timer-floor coverage is invalid")
      }
      sizing_timer_floors <- setNames(as.numeric(summaries$timer_floor_ms), selected_runners)
    }
    sizing_rows[[length(sizing_rows) + 1L]] <- round_rows
    complete_tasks <- vapply(active_tasks, function(task) {
      direct_sizing_count_accepted(
        round_rows[round_rows$task == task, , drop = FALSE], sizing_timer_floors,
        selected_runners, sizing_policy
      )
    }, logical(1))
    progress <- advance_direct_sizing_tasks(active_tasks, complete_tasks, count, blocked_tasks, sizing_policy)
    active_tasks <- progress$active_tasks
    blocked_tasks <- progress$blocked_tasks
  }
  if (length(blocked_tasks) > 0L) {
    stop(sprintf(
      "batch sizing cannot meet the shared policy for: %s",
      paste(unique(blocked_tasks), collapse = ", ")
    ))
  }
  sizing <- do.call(rbind, sizing_rows)
  rownames(sizing) <- NULL
  write_csv_once(sizing, file.path(sizing_staging, "sizing.csv"), "batch sizing")
  sizing <- read.csv(file.path(sizing_staging, "sizing.csv"), stringsAsFactors = FALSE)
  batch_repetitions <- select_direct_batch_repetitions(
    sizing, sizing_timer_floors, selected_runners, selected_tasks, sizing_policy
  )
  timing_policy$batch_repetitions <- as.list(batch_repetitions)
  metadata$timing_policy <- timing_policy
  write_run_manifest(run_dir, metadata)
  unlink(sizing_staging, recursive = TRUE)

  dir.create(timing_staging, recursive = TRUE, showWarnings = FALSE)
  cat("Direct timing\n")
  for (runner in selected_runners) {
    remaining <- remaining_timed_seconds()
    if (remaining < 1L) stop("total direct sizing and timing timeout expired")
    status <- system2(
      "Rscript",
      c(
        "benchmark_worker.R", paste0("--runner=", runner), "--mode=timing",
        paste0("--output-root=", timing_staging), paste0("--tasks=", task_argument),
        paste0("--measurement-samples=", timing_policy$measurement_samples),
        paste0("--batch-repetitions=", format_named_integer_map(batch_repetitions, "batch repetitions")),
        paste0("--master-seed=", master_seed)
      ),
      env = blas_environment,
      stdout = "", stderr = "",
      timeout = as.integer(min(timing_policy$worker_timeout_seconds, remaining))
    )
    if (!identical(status, 0L)) {
      failure <- sprintf("timing worker failed for %s with exit code %d", runner, status)
      stop(failure)
    }
  }

  sample_files <- file.path(timing_staging, paste0(selected_runners, "-samples.csv"))
  first_files <- file.path(timing_staging, paste0(selected_runners, "-first-call.csv"))
  probe_files <- file.path(timing_staging, paste0(selected_runners, "-probes.csv"))
  probe_summary_files <- file.path(timing_staging, paste0(selected_runners, "-probe-summary.csv"))
  samples <- do.call(rbind, lapply(sample_files, read.csv, stringsAsFactors = FALSE))
  first_calls <- do.call(rbind, lapply(first_files, read.csv, stringsAsFactors = FALSE))
  probe_samples <- do.call(rbind, lapply(probe_files, read.csv, stringsAsFactors = FALSE))
  probe_summaries <- do.call(rbind, lapply(probe_summary_files, read.csv, stringsAsFactors = FALSE))
  rownames(samples) <- NULL
  rownames(first_calls) <- NULL
  validate_direct_timing_samples(
    samples, selected_runners, selected_tasks, timing_policy$measurement_samples
  )
  observed_repetitions <- vapply(selected_tasks, function(task) {
    values <- unique(samples$batch_repetitions[samples$task == task])
    if (length(values) != 1L) stop(sprintf("timing batch repetitions differ across workers for %s", task))
    as.integer(values)
  }, integer(1))
  if (!identical(unname(observed_repetitions), unname(batch_repetitions))) {
    stop("timing batch repetitions differ from the manifest sizing map")
  }

  expected_probe_columns <- c(
    "runner", "probe", "probe_sample", "batch_repetitions", "batch_elapsed_ms",
    "elapsed_per_event_ms", "gc_elapsed_ms"
  )
  expected_probe_summary_columns <- c(
    "runner", "timer_floor_ms", "nanotime_elapsed_ms", "independent_elapsed_ms"
  )
  expected_probe_runners <- rep(
    selected_runners,
    each = length(measurement_probe_names()) * timing_policy$measurement_probe_samples
  )
  if (!identical(names(probe_samples), expected_probe_columns) ||
    !identical(names(probe_summaries), expected_probe_summary_columns) ||
    !identical(as.character(probe_samples$runner), expected_probe_runners) ||
    !identical(as.character(probe_summaries$runner), selected_runners)) {
    stop("measurement probe staging order or coverage is invalid")
  }
  row_records <- function(rows) {
    lapply(seq_len(nrow(rows)), function(index) as.list(rows[index, , drop = FALSE]))
  }
  metadata$measurement_probes <- setNames(lapply(selected_runners, function(runner) {
    probe <- probe_summaries[probe_summaries$runner == runner, , drop = FALSE]
    raw <- probe_samples[
      probe_samples$runner == runner,
      setdiff(names(probe_samples), "runner"),
      drop = FALSE
    ]
    record <- list(
      timer_floor_ms = probe$timer_floor_ms[[1L]],
      nanotime_elapsed_ms = probe$nanotime_elapsed_ms[[1L]],
      independent_elapsed_ms = probe$independent_elapsed_ms[[1L]],
      samples = row_records(raw)
    )
    validate_measurement_probe_record(
      record, runner, timing_policy$measurement_probe_samples
    )
    record
  }), selected_runners)
  write_run_manifest(run_dir, metadata)
  timer_floors <- setNames(vapply(metadata$measurement_probes, function(probe) {
    as.numeric(probe$timer_floor_ms)
  }, numeric(1)), selected_runners)
  summary <- summarize_direct_timing(
    samples, first_calls, timer_floors, timing_policy$distribution_policy,
    timing_policy$allocation_policy
  )
  cost_account_tasks <- intersect(selected_tasks, direct_cost_account_task_ids())
  cost_account <- direct_task_cost_accounts(cost_account_tasks)
  validate_direct_task_cost_accounts(cost_account, cost_account_tasks)
  write_csv_once(cost_account, file.path(run_dir, "cost_account.csv"), "direct cost account")
  if (any(summary$distribution_status == "BLOCK")) {
    blocked <- summary[summary$distribution_status == "BLOCK", c(
      "runner", "task", "distribution_reason"
    ), drop = FALSE]
    message(paste("direct timing retained distribution blocks:", paste(
      paste(blocked$runner, blocked$task, blocked$distribution_reason, sep = "/"),
      collapse = "; "
    )))
  }
  if (!is.null(memory_task)) {
    memory_staging <- file.path(staging, "memory")
    dir.create(memory_staging, recursive = TRUE, showWarnings = FALSE)
    for (runner in selected_runners) {
      remaining <- remaining_timed_seconds()
      if (remaining < 1L) stop("total direct sizing, timing, and memory timeout expired")
      status <- system2(
        "Rscript",
        c(
          "benchmark_worker.R", paste0("--runner=", runner), "--mode=memory",
          paste0("--output-root=", memory_staging), paste0("--tasks=", memory_task),
          paste0("--master-seed=", master_seed)
        ),
        env = blas_environment,
        stdout = "", stderr = "",
        timeout = as.integer(min(timing_policy$worker_timeout_seconds, remaining))
      )
      if (!identical(status, 0L)) {
        failure <- sprintf("memory worker failed for %s with exit code %d", runner, status)
        stop(failure)
      }
    }
    memory_files <- file.path(memory_staging, paste0(selected_runners, "-memory.csv"))
    memory_summary <- do.call(rbind, lapply(memory_files, read.csv, stringsAsFactors = FALSE))
    rownames(memory_summary) <- NULL
    validate_direct_memory_summary(memory_summary, selected_runners, memory_task)
    if (any(memory_summary$memory_status == "BLOCK")) {
      stop("direct memory blocked because process swap or high-water growth exceeded policy")
    }
    write_csv_once(memory_summary, file.path(run_dir, "memory_summary.csv"), "memory summary")
    unlink(memory_staging, recursive = TRUE)
  }
  write_csv_once(samples, file.path(run_dir, "timing_samples.csv"), "timing samples")
  write_csv_once(summary, file.path(run_dir, "timing_summary.csv"), "timing summary")
  unlink(staging, recursive = TRUE)

  validate_source_tree_identity(normalizePath(".."), metadata$source_tree)
  current_artifacts <- lapply(artifacts, function(record) {
    path <- direct_runner_artifact_path(root_dir, record$runner)
    unname(as.character(tools::md5sum(path))[[1L]])
  })
  if (!identical(current_artifacts, lapply(artifacts, `[[`, "md5"))) {
    stop("runner artifacts changed during the run")
  }

  metadata$status <- "complete"
  metadata$finished_at <- run_manifest_timestamp()
  metadata$outputs <- lapply(
    c(
      "correctness.csv", "timing_samples.csv", "timing_summary.csv", "cost_account.csv",
      if (!is.null(memory_task)) "memory_summary.csv"
    ),
    function(name) {
      list(relative_path = name, md5 = unname(as.character(
        tools::md5sum(file.path(run_dir, name))
      )[[1L]]))
    }
  )
  names(metadata$outputs) <- c(
    "correctness", "timing_samples", "timing_summary", "cost_account",
    if (!is.null(memory_task)) "memory_summary"
  )
  write_run_manifest(run_dir, metadata)
  completed <- TRUE

  cat(sprintf(
    "Complete: %d runners, %d tasks, %d measurement samples.\n",
    length(selected_runners), length(selected_tasks), nrow(samples)
  ))
}

run_direct_benchmark()
