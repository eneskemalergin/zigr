# Canonical task identity and comparison policy for the benchmark harness.
# Executable argument closures remain in runner_subprocess.R. The manifest
# identifies the task spec that owns each closure and owns all comparison policy.

load_task_manifest <- function(root_dir = normalizePath(".")) {
  path <- file.path(root_dir, "task_manifest.csv")
  if (!file.exists(path)) stop(sprintf("task manifest not found: %s", path))
  manifest <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  validate_task_manifest(manifest)
  manifest
}

validate_task_manifest <- function(manifest) {
  required <- c("task", "layer", "display_name", "category", "input_factory",
                "expected_return", "correctness_policy", "comparison_policy",
                "aggregate", "comparison_note")
  missing <- setdiff(required, names(manifest))
  if (length(missing) > 0L) {
    stop(sprintf("task manifest missing columns: %s", paste(missing, collapse = ", ")))
  }
  if (nrow(manifest) != 44L) stop(sprintf("task manifest must contain 44 rows, got %d", nrow(manifest)))
  if (anyDuplicated(manifest$task)) stop("task manifest contains duplicate task IDs")
  if (any(!nzchar(manifest$task)) || any(!nzchar(manifest$display_name))) stop("task manifest contains blank identity fields")
  if (any(!grepl("^([0-9]{2}_[a-z0-9_]+|07[ab]_[a-z0-9_]+)$", manifest$task))) stop("task manifest contains an invalid task ID")
  if (!all(manifest$input_factory == "task_spec.args")) stop("task manifest has an unsupported input factory")
  if (!all(manifest$layer %in% paste0("L", 1:6))) stop("task manifest has an invalid layer")
  if (!all(manifest$category %in% c("numeric_kernel", "api_overhead", "data_structure", "linear_algebra", "altrep", "integration"))) stop("task manifest has an invalid category")
  if (!all(manifest$correctness_policy %in% c("r_reference", "native_invariant", "nondeterministic"))) stop("task manifest has an invalid correctness policy")
  if (!all(manifest$comparison_policy %in% c("comparable", "non_comparable"))) stop("task manifest has an invalid comparison policy")
  if (!is.logical(manifest$aggregate)) stop("task manifest aggregate column must be logical")
  if (any(manifest$aggregate != (manifest$comparison_policy == "comparable"))) stop("aggregate policy must match comparison policy")
  if (any(manifest$comparison_policy == "comparable" & nzchar(manifest$comparison_note))) stop("comparable tasks must not have exclusion notes")
  if (any(manifest$comparison_policy == "non_comparable" & !nzchar(manifest$comparison_note))) stop("non-comparable tasks need exclusion notes")
  required_special <- c("07a_protect_shallow", "07b_protect_scaling", "42_external_ptr", "43_rng_stress")
  if (!all(required_special %in% manifest$task)) stop("task manifest is missing required special tasks")
  invisible(manifest)
}

validate_task_specs <- function(manifest, task_specs) {
  ids <- vapply(task_specs, function(task) task$id, character(1))
  if (anyDuplicated(ids)) stop("runner task specs contain duplicate task IDs")
  missing <- setdiff(manifest$task, ids)
  extra <- setdiff(ids, manifest$task)
  if (length(missing) > 0L || length(extra) > 0L) {
    stop(sprintf("runner task specs differ from manifest; missing: %s; extra: %s",
                 paste(missing, collapse = ", "), paste(extra, collapse = ", ")))
  }
  has_args <- vapply(task_specs, function(task) is.function(task$args), logical(1))
  if (any(!has_args)) stop(sprintf("task specs without args factories: %s", paste(ids[!has_args], collapse = ", ")))
  invisible(task_specs)
}

validate_runner_config <- function(manifest, cfg, runner_name) {
  exports <- if (is.null(cfg$exports)) character(0) else names(cfg$exports)
  missing <- setdiff(manifest$task, exports)
  extra <- setdiff(exports, manifest$task)
  if (length(missing) > 0L || length(extra) > 0L) {
    stop(sprintf("runner %s exports differ from manifest; missing: %s; extra: %s",
                 runner_name, paste(missing, collapse = ", "), paste(extra, collapse = ", ")))
  }
  invisible(cfg)
}

validate_r_reference_map <- function(manifest, r_ref) {
  required <- manifest$task[manifest$correctness_policy == "r_reference"]
  missing <- required[vapply(required, function(task) is.null(r_ref[[task]]) || !nzchar(r_ref[[task]]), logical(1))]
  if (length(missing) > 0L) stop(sprintf("R reference map missing tasks: %s", paste(missing, collapse = ", ")))
  invisible(r_ref)
}

order_task_specs <- function(manifest, task_specs) {
  ids <- vapply(task_specs, function(task) task$id, character(1))
  task_specs[match(manifest$task, ids)]
}
