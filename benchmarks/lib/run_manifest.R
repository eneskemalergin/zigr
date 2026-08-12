run_manifest_path <- function(run_dir) file.path(run_dir, "run_manifest.json")

run_manifest_timestamp <- function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
}

run_manifest_values <- function(value) {
  if (is.null(value)) character(0) else as.character(unlist(value, use.names = FALSE))
}

# Worktree source identity used by the direct manifest.

source_tree_files <- function(root_dir) {
  git_files <- tryCatch(
    system2(
      "git",
      c("-C", root_dir, "ls-files", "--cached", "--others", "--exclude-standard", "--"),
      stdout = TRUE,
      stderr = TRUE
    ),
    error = function(error) stop(sprintf("could not enumerate source files with git: %s", conditionMessage(error)))
  )
  status <- attr(git_files, "status")
  if (!is.null(status) && status != 0L) stop("git source-file enumeration failed")
  relative <- gsub("\\\\", "/", git_files[nzchar(git_files)])
  if (length(relative) == 0L) stop("source tree contains no git worktree files")

  relevant <- grepl(
    "^(\\.gitignore|build\\.zig(?:\\.zon)?|\\.github/|src/|tests/|benchmarks/)",
    relative
  )
  generated <- grepl(
    "(^|/)(results|target|tmp|temp|zig-out|zig-cache|\\.zig-cache|\\.zig-global-cache)(/|$)|\\.(so|dll|dylib|o|a|d|obj|exe|stamp|tmp|log)$",
    relative
  )
  relative <- sort(relative[relevant & !generated])
  relative <- relative[file.exists(file.path(root_dir, relative))]
  if (length(relative) == 0L) stop("source tree has no identity files after exclusions")
  list(
    relative = relative,
    included_prefixes = c(".gitignore", "build.zig", "build.zig.zon", ".github/", "src/", "tests/", "benchmarks/"),
    excluded_patterns = c("git ignored files", "**/{results,target,tmp,temp,zig-out,zig-cache,.zig-cache,.zig-global-cache}/", "**/*.{so,dll,dylib,o,a,d,obj,exe,stamp,tmp,log}")
  )
}

source_tree_identity <- function(root_dir) {
  root_dir <- normalizePath(root_dir)
  selection <- source_tree_files(root_dir)
  files <- file.path(root_dir, selection$relative)
  file_md5 <- as.character(tools::md5sum(files))
  if (anyNA(file_md5)) stop("could not hash every source identity file")
  lines <- paste(selection$relative, file_md5, sep = "\t")
  temporary <- tempfile("source-tree-")
  on.exit(unlink(temporary), add = TRUE)
  writeLines(lines, temporary, useBytes = TRUE)
  digest <- as.character(tools::md5sum(temporary))
  list(
    method = "git-worktree-md5",
    digest = unname(digest[[1L]]),
    file_count = length(files),
    included_prefixes = selection$included_prefixes,
    excluded_patterns = selection$excluded_patterns
  )
}

validate_source_tree_identity <- function(root_dir, recorded) {
  actual <- source_tree_identity(root_dir)
  for (field in c("method", "digest", "file_count")) {
    if (!identical(as.character(actual[[field]]), as.character(recorded[[field]]))) {
      stop(sprintf("source tree identity field %s differs from the recorded run", field))
    }
  }
  invisible(actual)
}

write_run_manifest_json_atomic <- function(value, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  staged <- paste0(path, ".tmp-", Sys.getpid())
  on.exit(unlink(staged), add = TRUE)
  jsonlite::write_json(
    value, staged, auto_unbox = TRUE, pretty = TRUE, null = "null", digits = NA
  )
  if (!file.rename(staged, path)) stop(sprintf("cannot promote run manifest: %s", path))
  invisible(path)
}

manifest_scalar <- function(value, label) {
  value <- as.character(value)
  if (length(value) != 1L || is.na(value) || !nzchar(value)) {
    stop(sprintf("run manifest has no valid %s", label))
  }
  value
}

manifest_numeric_scalar <- function(value, label) {
  value <- unlist(value, use.names = FALSE)
  if (length(value) != 1L || !is.numeric(value) || is.na(value) || !is.finite(value)) {
    stop(sprintf("run manifest has no valid numeric %s", label))
  }
  as.numeric(value)
}

validate_measurement_probe_record <- function(probe, runner, sample_count) {
  required_probe <- c(
    "timer_floor_ms", "nanotime_elapsed_ms", "independent_elapsed_ms", "samples"
  )
  if (!is.list(probe) || !identical(names(probe), required_probe)) {
    stop(sprintf("measurement probe record is invalid for %s", runner))
  }
  timer_floor <- manifest_numeric_scalar(probe$timer_floor_ms, paste(runner, "timer floor"))
  nanotime_ms <- manifest_numeric_scalar(
    probe$nanotime_elapsed_ms, paste(runner, "nanotime elapsed")
  )
  independent_ms <- manifest_numeric_scalar(
    probe$independent_elapsed_ms, paste(runner, "independent elapsed")
  )
  if (timer_floor < 0 || nanotime_ms < 0 ||
      independent_ms < measurement_unit_minimum_ms() ||
      abs(nanotime_ms - independent_ms) > measurement_unit_tolerance_ms(independent_ms)) {
    stop(sprintf("measurement probe summary is invalid for %s", runner))
  }

  sample_rows <- probe$samples
  probe_names <- measurement_probe_names()
  expected_count <- length(probe_names) * sample_count
  if (!is.list(sample_rows) || length(sample_rows) != expected_count) {
    stop(sprintf("measurement probe raw sequence is incomplete for %s", runner))
  }
  required_sample <- c(
    "probe", "probe_sample", "batch_repetitions", "batch_elapsed_ms",
    "elapsed_per_event_ms", "gc_elapsed_ms"
  )
  if (any(!vapply(sample_rows, function(row) {
    is.list(row) && identical(names(row), required_sample)
  }, logical(1)))) {
    stop(sprintf("measurement probe raw columns are invalid for %s", runner))
  }
  identities <- vapply(sample_rows, function(row) {
    manifest_scalar(row$probe, paste(runner, "probe name"))
  }, character(1))
  sample_ids <- vapply(sample_rows, function(row) {
    manifest_numeric_scalar(row$probe_sample, paste(runner, "probe sample"))
  }, numeric(1))
  expected_identities <- rep(probe_names, each = sample_count)
  expected_samples <- rep(seq_len(sample_count), times = length(probe_names))
  if (!identical(identities, expected_identities) ||
      !identical(sample_ids, as.numeric(expected_samples))) {
    stop(sprintf("measurement probe raw order is invalid for %s", runner))
  }

  numeric_fields <- required_sample[-1L]
  numeric_rows <- lapply(numeric_fields, function(field) {
    vapply(sample_rows, function(row) {
      manifest_numeric_scalar(row[[field]], paste(runner, field))
    }, numeric(1))
  })
  names(numeric_rows) <- numeric_fields
  if (any(numeric_rows$batch_repetitions != 1) ||
      any(numeric_rows$batch_elapsed_ms < 0) ||
      any(numeric_rows$elapsed_per_event_ms < 0) ||
      any(numeric_rows$gc_elapsed_ms < 0) ||
      !isTRUE(all.equal(
        numeric_rows$elapsed_per_event_ms,
        numeric_rows$batch_elapsed_ms / numeric_rows$batch_repetitions,
        tolerance = 1e-9, check.attributes = FALSE
      ))) {
    stop(sprintf("measurement probe raw values are invalid for %s", runner))
  }
  raw <- data.frame(
    probe = identities,
    elapsed_per_event_ms = numeric_rows$elapsed_per_event_ms,
    stringsAsFactors = FALSE
  )
  expected_floor <- measurement_probe_timer_floor(raw)
  if (!isTRUE(all.equal(timer_floor, expected_floor, tolerance = 1e-12))) {
    stop(sprintf("measurement probe timer floor differs from raw samples for %s", runner))
  }
  invisible(probe)
}

validate_direct_run_manifest <- function(metadata) {
  allowed <- c(
    "schema_version", "artifact_layout", "run_id", "status", "started_at",
    "runners", "tasks", "master_seed", "input_recipe_version", "input_seeds",
    "rng_event_seed",
    "source_tree", "artifacts", "timing_policy", "measurement_mode", "command",
    "memory_task", "memory_policy",
    "correctness_completed_at", "measurement_probes", "finished_at", "outputs", "status_message"
  )
  extra <- setdiff(names(metadata), allowed)
  if (length(extra) > 0L) {
    stop(sprintf("run manifest has unsupported fields: %s", paste(extra, collapse = ", ")))
  }
  required <- c(
    "schema_version", "artifact_layout", "run_id", "status", "started_at",
    "runners", "tasks", "master_seed", "input_recipe_version", "input_seeds",
    "rng_event_seed", "source_tree",
    "artifacts", "timing_policy", "measurement_mode", "command"
  )
  missing <- setdiff(required, names(metadata))
  if (length(missing) > 0L) {
    stop(sprintf("run manifest is missing: %s", paste(missing, collapse = ", ")))
  }
  if (!identical(as.integer(metadata$schema_version), 4L) ||
      !identical(manifest_scalar(metadata$artifact_layout, "artifact layout"), "direct-v1")) {
    stop("run manifest is not the direct timing schema")
  }
  status <- manifest_scalar(metadata$status, "status")
  if (!(status %in% c("running", "incomplete", "correctness_complete", "complete"))) {
    stop("run manifest has an invalid status")
  }
  runners <- run_manifest_values(metadata$runners)
  tasks <- run_manifest_values(metadata$tasks)
  if (length(runners) == 0L || any(!nzchar(runners)) || anyDuplicated(runners) ||
      length(tasks) == 0L || any(!nzchar(tasks)) || anyDuplicated(tasks)) {
    stop("run manifest has invalid runner or task identities")
  }
  master_seed <- input_scalar_integer(metadata$master_seed, "manifest master seed")
  manifest_scalar(metadata$run_id, "run id")
  manifest_scalar(metadata$started_at, "start timestamp")
  recipe_version <- manifest_scalar(metadata$input_recipe_version, "input recipe version")
  if (!identical(recipe_version, "revision-v1")) stop("run manifest has an invalid input recipe version")
  if (!is.list(metadata$input_seeds) || !setequal(names(metadata$input_seeds), tasks)) {
    stop("run manifest input-seed coverage differs from its tasks")
  }
  for (task in tasks) {
    recorded_seed <- input_scalar_integer(metadata$input_seeds[[task]], paste(task, "input seed"))
    if (!identical(recorded_seed, task_input_seed(master_seed, task, recipe_version))) {
      stop(sprintf("run manifest input seed differs for %s", task))
    }
  }
  rng_event_seed <- input_scalar_integer(metadata$rng_event_seed, "RNG event seed")
  if (!identical(rng_event_seed, task_input_seed(master_seed, "rng", "direct-timing-v1"))) {
    stop("run manifest RNG event seed differs")
  }
  measurement_mode <- manifest_scalar(metadata$measurement_mode, "measurement mode")
  if (!(measurement_mode %in% c("timed", "correctness_only"))) {
    stop("run manifest has an invalid measurement mode")
  }
  if ((identical(status, "complete") && !identical(measurement_mode, "timed")) ||
      (identical(status, "correctness_complete") &&
       !identical(measurement_mode, "correctness_only"))) {
    stop("run manifest status disagrees with its measurement mode")
  }
  has_memory_task <- "memory_task" %in% names(metadata)
  has_memory_policy <- "memory_policy" %in% names(metadata)
  if (has_memory_task != has_memory_policy) {
    stop("run manifest memory selection is incomplete")
  }
  if (has_memory_task) {
    memory_task <- manifest_scalar(metadata$memory_task, "memory task")
    if (!(memory_task %in% tasks) ||
        !isTRUE(direct_task_suitability_row(memory_task)$large_output)) {
      stop("run manifest memory task is not a selected large-output task")
    }
    validate_direct_memory_policy(metadata$memory_policy)
  }
  if (identical(status, "incomplete")) {
    manifest_scalar(metadata$finished_at, "incomplete finish timestamp")
    manifest_scalar(metadata$status_message, "incomplete status message")
  }
  if (!is.list(metadata$source_tree) ||
      !all(c("method", "digest", "file_count") %in% names(metadata$source_tree))) {
    stop("run manifest has no source-tree identity")
  }
  artifacts <- metadata$artifacts
  if (!is.list(artifacts) || !setequal(names(artifacts), runners)) {
    stop("run manifest artifact coverage differs from its runners")
  }
  for (runner in runners) {
    record <- artifacts[[runner]]
    if (!is.list(record) || !all(c("runner", "relative_path", "md5") %in% names(record)) ||
        !identical(manifest_scalar(record$runner, "artifact runner"), runner)) {
      stop(sprintf("run manifest artifact identity differs for %s", runner))
    }
    manifest_scalar(record$relative_path, "artifact path")
    manifest_scalar(record$md5, "artifact digest")
  }
  policy <- metadata$timing_policy
  policy_fields <- c(
    "policy_version", "warmup_iterations", "local_calibration_batches",
    "measurement_samples", "measurement_probe_samples", "sizing_policy", "batch_repetitions", "worker_timeout_seconds",
    "total_run_timeout_seconds", "r_jit_policy", "distribution_policy", "comparison_policy",
    "allocation_policy", "gc_policy"
  )
  policy_version <- if (is.list(policy) && "policy_version" %in% names(policy)) {
    manifest_scalar(policy$policy_version, "timing policy")
  } else {
    ""
  }
  if (!is.list(policy) || !identical(names(policy), policy_fields) ||
      !policy_version %in% c("direct-batch-v13", "direct-batch-v14")) {
    stop("run manifest has an invalid direct timing policy")
  }
  for (field in setdiff(policy_fields[2:9], c("sizing_policy", "batch_repetitions"))) {
    input_scalar_integer(policy[[field]], field)
  }
  if (!identical(manifest_scalar(policy$r_jit_policy, "R JIT policy"), "disabled-before-runner-load")) {
    stop("run manifest has an invalid R JIT policy")
  }
  distribution_policy <- validate_direct_distribution_policy(policy$distribution_policy)
  if (identical(policy_version, "direct-batch-v13")) {
    validate_direct_comparison_policy(policy$comparison_policy)
  } else {
    validate_direct_paired_comparison_policy(policy$comparison_policy)
  }
  validate_direct_allocation_policy(policy$allocation_policy)
  if (!identical(as.integer(policy$measurement_samples), distribution_policy$measurement_samples)) {
    stop("run manifest measurement samples differ from its distribution policy")
  }
  validate_direct_sizing_policy(policy$sizing_policy)
  if (!is.list(policy$batch_repetitions) || !setequal(names(policy$batch_repetitions), tasks)) {
    stop("direct timing batch repetition coverage differs from tasks")
  }
  repetitions <- vapply(policy$batch_repetitions[tasks], input_scalar_integer, integer(1), label = "batch repetitions")
  sizing_policy <- validate_direct_sizing_policy(policy$sizing_policy)
  if (any(!repetitions %in% sizing_policy$ladder)) {
    stop("direct timing batch repetitions are outside the sizing ladder")
  }
  if (any(vapply(tasks, function(task) {
    identical(direct_task_batchability(task), "one") && repetitions[[task]] != 1L
  }, logical(1)))) {
    stop("direct timing batch repetitions violate a single-event task contract")
  }
  if (!identical(as.integer(policy$warmup_iterations), 1L) ||
      !identical(as.integer(policy$local_calibration_batches), 1L) ||
      !identical(as.integer(policy$measurement_probe_samples), 101L)) {
    stop("direct timing requires one warmup, one calibration batch, and 101 probe samples")
  }
  manifest_scalar(policy$gc_policy, "GC policy")
  if (identical(status, "complete")) {
    probes <- metadata$measurement_probes
    if (!is.list(probes) || !setequal(names(probes), runners)) {
      stop("completed run manifest has incomplete measurement-probe coverage")
    }
    for (runner in runners) {
      validate_measurement_probe_record(
        probes[[runner]], runner, as.integer(policy$measurement_probe_samples)
      )
    }
  }
  if (status %in% c("correctness_complete", "complete")) {
    expected <- if (identical(status, "complete")) {
      c("correctness", "timing_samples", "timing_summary", "cost_account",
        if (has_memory_task) "memory_summary")
    } else "correctness"
    if (!is.list(metadata$outputs) || !setequal(names(metadata$outputs), expected)) {
      stop("completed run manifest has the wrong output set")
    }
    for (name in expected) {
      record <- metadata$outputs[[name]]
      if (!is.list(record) || !all(c("relative_path", "md5") %in% names(record))) {
        stop(sprintf("run manifest output record is invalid for %s", name))
      }
      expected_path <- c(
        correctness = "correctness.csv",
        timing_samples = "timing_samples.csv",
        timing_summary = "timing_summary.csv",
        cost_account = "cost_account.csv",
        memory_summary = "memory_summary.csv"
      )[[name]]
      if (!identical(manifest_scalar(record$relative_path, "output path"), expected_path)) {
        stop(sprintf("run manifest output path differs for %s", name))
      }
      manifest_scalar(record$md5, "output digest")
    }
    manifest_scalar(metadata$finished_at, "finish timestamp")
  }
  invisible(metadata)
}

validate_direct_timing_summary <- function(summary, samples, metadata) {
  if (!is.data.frame(summary)) stop("completed timing summary is invalid")
  policy <- metadata$timing_policy
  runners <- run_manifest_values(metadata$runners)
  probes <- metadata$measurement_probes
  timer_floors <- setNames(vapply(probes[runners], function(probe) {
    as.numeric(probe$timer_floor_ms)
  }, numeric(1)), runners)
  first_calls <- summary[c("runner", "task", "first_call_ms")]
  expected <- summarize_direct_timing(
    samples, first_calls, timer_floors, policy$distribution_policy, policy$allocation_policy
  )
  if (!identical(names(summary), names(expected)) || nrow(summary) != nrow(expected) ||
      !isTRUE(all.equal(summary, expected, tolerance = 1e-12, check.attributes = FALSE))) {
    stop("completed timing summary differs from raw samples or distribution policy")
  }
  invisible(summary)
}

validate_direct_run_outputs <- function(run_dir, metadata) {
  if (!(as.character(metadata$status) %in% c("correctness_complete", "complete"))) {
    return(invisible(metadata))
  }
  for (record in metadata$outputs) {
    path <- file.path(run_dir, as.character(record$relative_path))
    if (!file.exists(path)) stop(sprintf("run output is missing: %s", path))
    digest <- unname(as.character(tools::md5sum(path))[[1L]])
    if (!identical(digest, as.character(record$md5))) {
      stop(sprintf("run output digest differs: %s", path))
    }
  }
  if (identical(as.character(metadata$status), "complete")) {
    samples <- read.csv(file.path(run_dir, "timing_samples.csv"), stringsAsFactors = FALSE)
    policy <- metadata$timing_policy
    runners <- run_manifest_values(metadata$runners)
    tasks <- run_manifest_values(metadata$tasks)
    validate_direct_timing_samples(samples, runners, tasks, policy$measurement_samples)
    observed <- vapply(tasks, function(task) {
      values <- unique(samples$batch_repetitions[samples$task == task])
      if (length(values) != 1L) stop(sprintf("completed timing repetitions differ across workers for %s", task))
      as.integer(values)
    }, integer(1))
    declared <- vapply(policy$batch_repetitions[tasks], input_scalar_integer, integer(1),
                       label = "batch repetitions")
    if (!identical(unname(observed), unname(declared))) {
      stop("completed timing repetitions differ from the manifest sizing map")
    }
    summary <- read.csv(file.path(run_dir, "timing_summary.csv"), stringsAsFactors = FALSE)
    validate_direct_timing_summary(summary, samples, metadata)
    cost_account <- read.csv(file.path(run_dir, "cost_account.csv"), stringsAsFactors = FALSE)
    cost_tasks <- intersect(tasks, direct_cost_account_task_ids())
    validate_direct_task_cost_accounts(cost_account, cost_tasks)
    if ("memory_task" %in% names(metadata)) {
      memory <- read.csv(file.path(run_dir, "memory_summary.csv"), stringsAsFactors = FALSE)
      validate_direct_memory_summary(memory, runners, as.character(metadata$memory_task))
    }
  }
  invisible(metadata)
}

write_run_manifest <- function(run_dir, metadata) {
  validate_direct_run_manifest(metadata)
  validate_direct_run_outputs(run_dir, metadata)
  write_run_manifest_json_atomic(metadata, run_manifest_path(run_dir))
}

write_incomplete_run_manifest <- function(run_dir, metadata, message) {
  metadata$status <- "incomplete"
  metadata$status_message <- manifest_scalar(message, "incomplete status message")
  metadata$finished_at <- run_manifest_timestamp()
  write_run_manifest(run_dir, metadata)
  metadata
}

read_run_manifest <- function(run_dir) {
  path <- run_manifest_path(run_dir)
  if (!file.exists(path)) stop(sprintf("run manifest not found: %s", path))
  metadata <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  validate_direct_run_manifest(metadata)
  validate_direct_run_outputs(run_dir, metadata)
  metadata
}
