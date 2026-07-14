r_provenance_schema_version <- function() "p4.1-r-provenance-v1"

r_body_digest <- function(fn) {
  if (!is.function(fn)) stop("R provenance source is not a function")
  serialized_md5(list(
    formals = formals(fn),
    body = body(fn)
  ))
}

r_function_call_names <- function(fn) {
  if (!is.function(fn)) stop("R AST source is not a function")
  if (!requireNamespace("codetools", quietly = TRUE)) stop("codetools is required for R provenance validation")
  sort(unique(codetools::findGlobals(fn, merge = FALSE)$functions))
}

r_pure_forbidden_calls <- function() {
  c(
    ".Call", ".External", ".C", ".Fortran", "::", ":::",
    "dyn.load", "dyn.unload", "getNativeSymbolInfo",
    "get", "mget", "do.call", "match.fun", "eval", "evalq", "parse", "source", "sys.source",
    "getExportedValue", "library", "require",
    "sum", "mean", "sort", "cumsum", "crossprod", "chol", "lm.fit", "%*%", "t",
    "rowSums", "rowMeans", "colSums", "colMeans", "aggregate", "factor", "which",
    "rnorm", "runif", "sample", "serialize", "unserialize", "nchar", "paste", "paste0",
    "vapply", "sapply", "lapply", "apply", "startsWith", "substring", "toupper",
    "abs", "log", "exp", "sqrt"
  )
}

r_pure_contract_policy <- function(task_id) {
  policy_by_task <- c(
    "05_fib_recursive" = "fibonacci",
    "24_long_vector_idx" = "sampled_index_loop",
    "25_l1_arithmetic" = "l1_scalar_loops",
    "32_altrep_elt_walk" = "altrep_index_loop",
    "33_altrep_region_read" = "altrep_value_loop",
    "35_altrep_sum_native" = "altrep_index_loop",
    "50_boundary_zero_generated" = "zero_boundary",
    "51_boundary_zero_handwritten" = "zero_boundary",
    "52_boundary_scalar_generated" = "scalar_boundary",
    "53_boundary_scalar_handwritten" = "scalar_boundary",
    "54_boundary_optional_null_generated" = "optional_boundary",
    "55_boundary_optional_null_handwritten" = "optional_boundary",
    "56_boundary_optional_typed_na_generated" = "optional_boundary",
    "57_boundary_optional_typed_na_handwritten" = "optional_boundary",
    "70_boundary_schema_generated" = "fixed_schema_boundary",
    "71_boundary_schema_handwritten" = "fixed_schema_boundary",
    "72_boundary_external_method_generated" = "external_method_value_invariant",
    "73_boundary_external_method_handwritten" = "external_method_value_invariant"
  )
  policy_name <- unname(policy_by_task[as.character(task_id)])
  if (length(policy_name) != 1L || is.na(policy_name)) {
    stop(sprintf("pure-R contract has no authored AST allowlist for %s", task_id))
  }
  calls <- switch(policy_name,
    fibonacci = c("-", "{", "+", "<=", "as.numeric", "if", "Recall", "return"),
    sampled_index_loop = c("[", "{", "+", "<-", "for", "length", "seq"),
    l1_scalar_loops = c(":", "{", "*", "+", "<-", "for"),
    altrep_index_loop = c("[", "{", "+", "<-", "for", "seq_len"),
    altrep_value_loop = c("{", "+", "<-", "for", "seq_len"),
    zero_boundary = character(0),
    scalar_boundary = c("!", "!=", "{", "&&", "||", "if", "is.na", "is.nan", "length", "stop", "typeof"),
    optional_boundary = c(
      "!", "!=", "{", "&&", "||", "if", "is.na", "is.nan", "is.null", "length", "return", "stop", "typeof"
    ),
    fixed_schema_boundary = c(
      "!", "!=", "(", "[[", "{", "&&", "<-", "||", "attributes", "c", "identical", "if",
      "is.double", "is.integer", "is.logical", "is.na", "is.nan", "length", "list", "stop", "typeof"
    ),
    external_method_value_invariant = "as.integer"
  )
  list(
    id = paste0("pure-r-contract-", policy_name, "-v1"),
    allowed_calls = sort(unique(calls))
  )
}

validate_r_provenance_record <- function(record, fn = NULL) {
  required <- c(
    "schema_version", "task", "function_name", "implementation_class", "source_digest",
    "ast_allowlist_id", "forbidden_call_result", "compiled_backend"
  )
  missing <- required[vapply(required, function(field) is.null(record[[field]]), logical(1))]
  if (length(missing) > 0L) stop(sprintf("R provenance record missing fields: %s", paste(missing, collapse = ", ")))
  if (!identical(as.character(record$schema_version), r_provenance_schema_version())) {
    stop(sprintf("unsupported R provenance schema for %s", record$task))
  }
  implementation_class <- as.character(record$implementation_class)
  if (!(implementation_class %in% c("pure_r", "optimized_base_r", "pure_r_unrepresentable"))) {
    stop(sprintf("invalid R implementation class for %s", record$task))
  }
  if (length(record$source_digest) != 1L || is.na(record$source_digest) || !nzchar(as.character(record$source_digest))) {
    stop(sprintf("R provenance source digest is missing for %s", record$task))
  }
  if (identical(implementation_class, "pure_r_unrepresentable")) {
    if (is.null(record$reason) || !nzchar(as.character(record$reason))) {
      stop(sprintf("unrepresentable R provenance lacks a reason for %s", record$task))
    }
    return(invisible(record))
  }
  if (is.null(fn) || !is.function(fn)) stop(sprintf("R provenance function is missing for %s", record$task))
  actual_digest <- r_body_digest(fn)
  if (!identical(actual_digest, as.character(record$source_digest))) {
    stop(sprintf("R provenance source digest differs for %s", record$task))
  }
  calls <- r_function_call_names(fn)
  forbidden <- intersect(calls, r_pure_forbidden_calls())
  if (identical(implementation_class, "pure_r")) {
    policy <- r_pure_contract_policy(record$task)
    if (!identical(as.character(record$ast_allowlist_id), policy$id)) {
      stop(sprintf("pure-R AST allowlist identity differs for %s", record$task))
    }
    if (length(forbidden) > 0L) {
      stop(sprintf("pure-R provenance contains forbidden calls for %s: %s", record$task, paste(forbidden, collapse = ", ")))
    }
    outside_allowlist <- setdiff(calls, policy$allowed_calls)
    if (length(outside_allowlist) > 0L) {
      stop(sprintf("pure-R provenance contains calls outside the AST allowlist for %s: %s", record$task, paste(outside_allowlist, collapse = ", ")))
    }
    if (!identical(as.character(record$compiled_backend), "none")) {
      stop(sprintf("optimized backend row claims pure R for %s", record$task))
    }
  }
  expected_result <- if (length(forbidden) == 0L) "pass" else paste0("declared:", paste(sort(forbidden), collapse = "+"))
  if (!identical(as.character(record$forbidden_call_result), expected_result)) {
    stop(sprintf("R forbidden-call result differs for %s", record$task))
  }
  if (identical(implementation_class, "optimized_base_r") &&
      (is.null(record$compiled_backend) || !nzchar(as.character(record$compiled_backend)) || identical(record$compiled_backend, "none"))) {
    stop(sprintf("optimized base-R provenance lacks a compiled backend for %s", record$task))
  }
  invisible(record)
}

build_r_provenance <- function(task_id, function_name, evidence_row) {
  if (length(function_name) != 1L || !nzchar(as.character(function_name)) ||
      !exists(function_name, mode = "function", inherits = TRUE)) {
    stop(sprintf("R implementation function is missing for %s", task_id))
  }
  fn <- get(function_name, mode = "function", inherits = TRUE)
  implementation_class <- if (identical(as.character(evidence_row$implementation_role), "pure_r")) {
    "pure_r"
  } else if (identical(as.character(evidence_row$implementation_role), "optimized_base_r")) {
    "optimized_base_r"
  } else {
    stop(sprintf("executable R row has no pure or optimized classification for %s", task_id))
  }
  calls <- r_function_call_names(fn)
  forbidden <- intersect(calls, r_pure_forbidden_calls())
  pure_policy <- if (identical(implementation_class, "pure_r")) r_pure_contract_policy(task_id) else NULL
  record <- list(
    schema_version = r_provenance_schema_version(),
    task = task_id,
    function_name = as.character(function_name),
    implementation_class = implementation_class,
    source_digest = r_body_digest(fn),
    ast_allowlist_id = if (identical(implementation_class, "pure_r")) pure_policy$id else "optimized-base-r-declared-v1",
    forbidden_call_result = if (length(forbidden) == 0L) "pass" else paste0("declared:", paste(sort(forbidden), collapse = "+")),
    compiled_backend = if (identical(implementation_class, "pure_r")) "none" else "R compiled primitive or declared runtime service"
  )
  validate_r_provenance_record(record, fn)
  record
}

r_unrepresentable_provenance <- function(task_id, reason) {
  record <- list(
    schema_version = r_provenance_schema_version(),
    task = task_id,
    function_name = "",
    implementation_class = "pure_r_unrepresentable",
    source_digest = "not_applicable",
    ast_allowlist_id = "not_applicable",
    forbidden_call_result = "not_applicable",
    compiled_backend = "not_applicable",
    reason = as.character(reason)
  )
  validate_r_provenance_record(record)
  record
}

build_run_r_provenance <- function(task_ids, reference_map, task_manifest, evidence_rows) {
  runner_records <- lapply(task_ids, function(task_id) {
    row <- evidence_rows[evidence_rows$task == task_id, , drop = FALSE]
    if (nrow(row) != 1L) stop(sprintf("normalized R evidence is missing for %s", task_id))
    if (isTRUE(row$executable)) {
      build_r_provenance(task_id, reference_map[[task_id]], row)
    } else {
      r_unrepresentable_provenance(task_id, as.character(row$reason))
    }
  })
  names(runner_records) <- task_ids

  reference_ids <- task_manifest$task[
    task_manifest$task %in% task_ids & task_manifest$correctness_policy == "r_reference"
  ]
  reference_records <- lapply(reference_ids, function(task_id) {
    row <- evidence_rows[evidence_rows$task == task_id, , drop = FALSE]
    if (!isTRUE(row$executable)) {
      if (!(task_id %in% c("72_boundary_external_method_generated", "73_boundary_external_method_handwritten"))) {
        stop(sprintf("R reference is unrepresentable without a declared native invariant for %s", task_id))
      }
      row$implementation_role <- "pure_r"
    }
    build_r_provenance(task_id, reference_map[[task_id]], row)
  })
  names(reference_records) <- reference_ids
  list(
    schema_version = r_provenance_schema_version(),
    runner_rows = unname(runner_records),
    reference_rows = unname(reference_records)
  )
}

named_r_provenance_records <- function(provenance, field) {
  records <- provenance[[field]]
  if (is.null(records)) return(list())
  task_ids <- vapply(records, function(record) as.character(record$task), character(1))
  if (anyDuplicated(task_ids)) stop(sprintf("R provenance %s contains duplicate tasks", field))
  names(records) <- task_ids
  records
}

compare_r_provenance_records <- function(expected, actual, label) {
  fields <- c(
    "schema_version", "task", "function_name", "implementation_class", "source_digest",
    "ast_allowlist_id", "forbidden_call_result", "compiled_backend", "reason"
  )
  for (field in fields) {
    expected_value <- if (is.null(expected[[field]])) "" else as.character(expected[[field]])
    actual_value <- if (is.null(actual[[field]])) "" else as.character(actual[[field]])
    if (!identical(expected_value, actual_value)) stop(sprintf("R provenance field %s differs for %s", field, label))
  }
  invisible(actual)
}
