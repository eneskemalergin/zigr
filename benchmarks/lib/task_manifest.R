load_task_manifest <- function(root_dir = normalizePath(".")) {
  path <- file.path(root_dir, "task_manifest.csv")
  if (!file.exists(path)) stop(sprintf("task manifest not found: %s", path))
  manifest <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  validate_task_manifest(manifest)
  manifest
}

task_report_category <- function(category) {
  mapping <- c(
    numeric_kernel = "kernels",
    api_overhead = "synthetic_api",
    data_structure = "boundary",
    linear_algebra = "kernels",
    altrep = "altrep",
    integration = "r_runtime"
  )
  values <- unname(mapping[as.character(category)])
  if (anyNA(values)) stop("task manifest has an unmapped report category")
  values
}

task_matrix_group <- function(task) {
  value <- as.character(task)
  ifelse(grepl("^[0-9]{2}_boundary_.*_(generated|handwritten)$", value),
         sub("^[0-9]{2}_boundary_(.*)_(generated|handwritten)$", "\\1", value), "")
}

task_matrix_variant <- function(task) {
  value <- as.character(task)
  ifelse(grepl("^[0-9]{2}_boundary_.*_(generated|handwritten)$", value),
         sub("^[0-9]{2}_boundary_.*_(generated|handwritten)$", "\\1", value), "")
}

boundary_budget_class <- function(group) {
  mapping <- c(
    zero = "safe_wrapper",
    external = "safe_wrapper",
    scalar = "scalar",
    optional_null = "scalar",
    optional_typed_na = "scalar",
    numeric_small = "borrowed_numeric",
    numeric_large = "borrowed_numeric",
    raw = "borrowed_numeric",
    complex = "borrowed_numeric",
    altrep_integer = "copied_altrep",
    string_view = "strings",
    schema = "structs",
    external_method = "methods"
  )
  result <- unname(mapping[as.character(group)])
  if (anyNA(result)) stop("boundary matrix has an unmapped budget class")
  result
}

boundary_budget_policy <- function() {
  data.frame(
    budget_class = c("safe_wrapper", "scalar", "borrowed_numeric", "copied_altrep", "strings", "structs", "methods"),
    max_generated_median_ms = c(0.01, 0.01, 0.12, 0.40, 0.01, 0.01, 0.01),
    max_eligible_overhead_ms = c(NA, NA, 0.01, NA, NA, NA, NA),
    max_eligible_ratio = c(NA, NA, 1.10, NA, NA, NA, NA),
    stringsAsFactors = FALSE
  )
}

boundary_budget_policy_version <- function() "2026-07-11-1"

representation_budget_policy <- function() {
  data.frame(
    task = c(
      "76_string_view_one", "77_string_cache_build", "78_string_cache_one",
      "79_string_headers_one", "80_string_view_repeated", "81_string_cache_repeated",
      "82_string_headers_repeated", "83_raw_view", "84_raw_copy",
      "85_complex_view", "86_complex_return"
    ),
    max_median_ms = c(0.50, 1.20, 1.25, 0.70, 1.90, 1.35, 0.75, 0.01, 0.20, 0.05, 0.06),
    stringsAsFactors = FALSE
  )
}

validate_task_manifest <- function(manifest) {
  required <- c("task", "workload_group", "display_name", "category", "input_factory", "input_arity",
                "expected_return", "correctness_policy", "comparison_policy",
                "aggregate", "comparison_note")
  missing <- setdiff(required, names(manifest))
  if (length(missing) > 0L) {
    stop(sprintf("task manifest missing columns: %s", paste(missing, collapse = ", ")))
  }
  if (nrow(manifest) != 83L) stop(sprintf("task manifest must contain 83 rows, got %d", nrow(manifest)))
  if (anyDuplicated(manifest$task)) stop("task manifest contains duplicate task IDs")
  if (any(!nzchar(manifest$task)) || any(!nzchar(manifest$display_name))) stop("task manifest contains blank identity fields")
  if (any(!grepl("^([0-9]{2}_[A-Za-z0-9_]+|07[ab]_[A-Za-z0-9_]+)$", manifest$task))) stop("task manifest contains an invalid task ID")
  if (!all(manifest$input_factory == "task_spec.args")) stop("task manifest has an unsupported input factory")
  if (!is.numeric(manifest$input_arity) || anyNA(manifest$input_arity) || any(manifest$input_arity < 0) || any(manifest$input_arity != as.integer(manifest$input_arity))) stop("task manifest has an invalid input arity")
  groups <- c("core_compute", "api_boundary", "objects_and_strings", "numerical", "altrep", "runtime_services")
  if (!all(manifest$workload_group %in% groups)) stop("task manifest has an invalid workload group")
  if (!all(manifest$category %in% c("numeric_kernel", "api_overhead", "data_structure", "linear_algebra", "altrep", "integration"))) stop("task manifest has an invalid category")
  task_report_category(manifest$category)
  if (!all(manifest$correctness_policy %in% c("r_reference", "native_invariant", "nondeterministic"))) stop("task manifest has an invalid correctness policy")
  if (!all(manifest$comparison_policy %in% c("comparable", "non_comparable"))) stop("task manifest has an invalid comparison policy")
  if (!is.logical(manifest$aggregate)) stop("task manifest aggregate column must be logical")
  if (any(manifest$aggregate != (manifest$comparison_policy == "comparable"))) stop("aggregate policy must match comparison policy")
  if (any(manifest$comparison_policy == "comparable" & nzchar(manifest$comparison_note))) stop("comparable tasks must not have exclusion notes")
  if (any(manifest$comparison_policy == "non_comparable" & !nzchar(manifest$comparison_note))) stop("non-comparable tasks need exclusion notes")
  required_special <- c("07a_protect_shallow", "07b_protect_scaling", "42_external_ptr", "43_rng_stress", "48_weakref_lifecycle", "49_owned_altrep_create")
  if (!all(required_special %in% manifest$task)) stop("task manifest is missing required special tasks")
  boundary_tasks <- manifest$task[grepl("^[0-9]{2}_boundary_.*_(generated|handwritten)$", manifest$task)]
  boundary_classes <- boundary_budget_class(task_matrix_group(boundary_tasks))
  if (!setequal(unique(boundary_classes), boundary_budget_policy()$budget_class)) stop("boundary budget policy does not cover every boundary class")
  representation_tasks <- manifest$task[grepl("^(76|77|78|79|80|81|82|83|84|85|86)_", manifest$task)]
  if (!setequal(representation_tasks, representation_budget_policy()$task)) stop("representation budget policy does not cover every representation task")
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

validate_task_arguments <- function(manifest, task_specs) {
  ids <- vapply(task_specs, function(task) task$id, character(1))
  rows <- match(ids, manifest$task)
  if (anyNA(rows)) stop("task argument validation found an unmanifested task")
  for (i in seq_along(task_specs)) {
    task_id <- ids[[i]]
    values <- tryCatch(task_specs[[i]]$args(), error = function(e) {
      stop(sprintf("input factory failed for %s: %s", task_id, conditionMessage(e)))
    })
    if (!is.list(values)) stop(sprintf("input factory for %s did not return a list", task_id))
    expected <- manifest$input_arity[[rows[[i]]]]
    if (length(values) != expected) stop(sprintf("input factory for %s returned %d arguments; expected %d", task_id, length(values), expected))
    rm(values)
    gc(verbose = FALSE)
  }
  invisible(task_specs)
}

validate_runner_config <- function(manifest, cfg, runner_name) {
  exports <- if (is.null(cfg$exports)) character(0) else names(cfg$exports)
  optional_tasks <- if (is.null(cfg$optional_tasks)) character(0) else as.character(unlist(cfg$optional_tasks, use.names = FALSE))
  unknown_optional <- setdiff(optional_tasks, manifest$task)
  if (length(unknown_optional) > 0L) {
    stop(sprintf("runner %s declares optional tasks absent from the manifest: %s", runner_name, paste(unknown_optional, collapse = ", ")))
  }
  missing <- setdiff(manifest$task, exports)
  extra <- setdiff(exports, manifest$task)
  unallowed_missing <- setdiff(missing, optional_tasks)
  if (length(unallowed_missing) > 0L || length(extra) > 0L) {
    stop(sprintf("runner %s exports differ from manifest; missing: %s; extra: %s",
                 runner_name, paste(unallowed_missing, collapse = ", "), paste(extra, collapse = ", ")))
  }
  invisible(cfg)
}

validate_r_reference_map <- function(manifest, r_ref) {
  required <- manifest$task[manifest$correctness_policy == "r_reference"]
  missing <- required[vapply(required, function(task) is.null(r_ref[[task]]) || !nzchar(r_ref[[task]]), logical(1))]
  if (length(missing) > 0L) stop(sprintf("R reference map missing tasks: %s", paste(missing, collapse = ", ")))
  invisible(r_ref)
}

validate_result_contract <- function(value, contract) {
  ok <- switch(contract,
    real_scalar = is.double(value) && is.null(dim(value)) && length(value) == 1L,
    integer_scalar = is.integer(value) && is.null(dim(value)) && length(value) == 1L,
    real_vector = is.double(value) && is.null(dim(value)),
    complex_vector = is.complex(value) && is.null(dim(value)),
    named_real_vector = is.double(value) && is.null(dim(value)) && !is.null(names(value)),
    real_matrix = is.double(value) && !is.null(dim(value)) && length(dim(value)) == 2L,
    real_list = is.list(value),
    data_frame_list = is.data.frame(value),
    named_list = is.list(value) && !is.null(names(value)),
    r_object = !is.null(value),
    external_pointer = identical(typeof(value), "externalptr"),
    nondeterministic_vector = is.double(value) && is.null(dim(value)) && length(value) > 0L,
    FALSE
  )
  list(ok = ok, message = if (ok) "" else sprintf("expected result contract %s", contract))
}

order_task_specs <- function(manifest, task_specs) {
  ids <- vapply(task_specs, function(task) task$id, character(1))
  task_specs[match(manifest$task, ids)]
}
