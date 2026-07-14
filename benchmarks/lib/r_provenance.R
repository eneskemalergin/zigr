r_provenance_schema_version <- function() "p4.2-r-provenance-v2"

r_body_digest <- function(fn) {
  if (!is.function(fn)) stop("R provenance source is not a function")
  serialized_md5(list(
    formals = formals(fn),
    body = body(fn)
  ))
}

r_source_body <- function(fn) {
  if (!is.function(fn)) stop("R provenance source is not a function")
  paste(deparse(fn, width.cutoff = 500L, control = c("keepNA", "keepInteger")), collapse = "\n")
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
    "73_boundary_external_method_handwritten" = "external_method_value_invariant",
    "F01" = "fixture_zero",
    "F02" = "fixture_scalar",
    "F03" = "fixture_numeric",
    "F04" = "fixture_altrep_integer",
    "F05" = "fixture_strings",
    "F06" = "fixture_raw",
    "F07" = "fixture_complex",
    "F08" = "fixture_logical_counts",
    "F09" = "fixture_schema",
    "F11" = "fixture_error",
    "F12" = "fixture_outputs"
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
    external_method_value_invariant = "as.integer",
    fixture_zero = c("[[<-", "{", "<-", "integer"),
    fixture_scalar = c(
      "!", "!=", "[[", "[[<-", "{", "<-", "&&", "||", "if", "is.na", "is.nan",
      "length", "numeric", "stop", "typeof"
    ),
    fixture_numeric = c("*", "!=", "[[", "[[<-", "{", "<-", "for", "if", "length", "numeric", "seq_len", "stop", "typeof"),
    fixture_altrep_integer = c("+", "!=", "[[", "{", "<-", "for", "if", "length", "seq_len", "stop", "typeof"),
    fixture_strings = c("!", "+", "!=", "[[", "{", "<-", "for", "if", "is.na", "length", "seq_len", "stop", "typeof"),
    fixture_raw = c("!=", "[[", "[[<-", "{", "<-", "for", "if", "length", "raw", "seq_len", "stop", "typeof"),
    fixture_complex = c("!=", "[[", "[[<-", "{", "<-", "complex", "for", "if", "length", "seq_len", "stop", "typeof"),
    fixture_logical_counts = c(
      "+", "!=", "[[", "[[<-", "{", "<-", "c", "for", "if", "integer", "is.na",
      "length", "names<-", "seq_len", "stop", "typeof"
    ),
    fixture_schema = c(
      "!", "!=", "(", "[[", "{", "&&", "<-", "||", "attributes", "c", "identical", "if",
      "is.double", "is.integer", "is.logical", "is.na", "is.nan", "length", "list", "stop", "typeof"
    ),
    fixture_error = c("!=", "{", "||", "if", "length", "stop", "typeof"),
    fixture_outputs = c(
      "+", "[[<-", "{", "<-", "as.raw", "c", "character", "complex", "logical", "names<-",
      "numeric", "raw", "vector"
    )
  )
  list(
    id = paste0("pure-r-contract-", policy_name, "-v1"),
    allowed_calls = sort(unique(calls))
  )
}

r_backend_policy <- function(task_id, function_name, calls, implementation_class) {
  if (identical(implementation_class, "pure_r")) {
    return(list(calls = character(0), classes = "none", identity_keys = "none", description = "none"))
  }
  language_calls <- c(
    "(", ":", "[", "[[", "[<-", "$", "$<-", "{", "!", "!=", "&", "&&", "*", "+", "+<-", "-", "/",
    "<", "<-", "<<-", "<=", "==", ">", ">=", "||", "~", "for", "if", "return", "switch"
  )
  backend_calls <- sort(unique(setdiff(calls, language_calls)))
  if (length(backend_calls) == 0L && nzchar(function_name)) backend_calls <- function_name
  classes <- "r_compiled_primitive_or_runtime_service"
  identity_keys <- "r_runtime"
  if (task_id %in% c("26_matmul", "27_crossprod")) {
    classes <- "blas"
    identity_keys <- c("r_runtime", "blas")
  } else if (identical(task_id, "28_cholesky")) {
    classes <- "lapack"
    identity_keys <- c("r_runtime", "lapack")
  } else if (identical(task_id, "29_lm_fit")) {
    classes <- "stats_native_fortran_qr"
    identity_keys <- c("r_runtime", "stats_package", "fortran_runtime")
  } else if (any(backend_calls %in% c("serialize", "unserialize"))) {
    classes <- "r_serialization_runtime"
  } else if (any(backend_calls %in% c("rnorm", "runif", "sample"))) {
    classes <- "r_rng_runtime"
  } else if (identical(task_id, "22_s4_slot_access")) {
    classes <- "methods_runtime"
    identity_keys <- c("r_runtime", "methods_package")
  } else if (any(backend_calls %in% c("aggregate", "data.frame", "factor", "lm.fit"))) {
    classes <- "base_or_recommended_package_runtime"
    if (any(backend_calls %in% c("aggregate", "lm.fit"))) {
      identity_keys <- c("r_runtime", "stats_package")
    }
  }
  list(
    calls = backend_calls,
    classes = classes,
    identity_keys = identity_keys,
    description = sprintf("%s via %s", classes, paste(backend_calls, collapse = "+"))
  )
}

validate_r_provenance_record <- function(record, fn = NULL) {
  required <- c(
    "schema_version", "task", "function_name", "implementation_class", "source_digest",
    "source_body", "ast_calls", "ast_allowlist_id", "ast_allowlist", "forbidden_call_result",
    "compiled_backend", "backend_calls", "backend_classes", "backend_identity_keys"
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
  if (!identical(as.character(record$source_body), r_source_body(fn))) {
    stop(sprintf("R provenance source body differs for %s", record$task))
  }
  calls <- r_function_call_names(fn)
  if (!identical(sort(as.character(unlist(record$ast_calls, use.names = FALSE))), calls)) {
    stop(sprintf("R provenance AST calls differ for %s", record$task))
  }
  forbidden <- intersect(calls, r_pure_forbidden_calls())
  if (identical(implementation_class, "pure_r")) {
    policy <- r_pure_contract_policy(record$task)
    if (!identical(as.character(record$ast_allowlist_id), policy$id)) {
      stop(sprintf("pure-R AST allowlist identity differs for %s", record$task))
    }
    if (!identical(sort(as.character(unlist(record$ast_allowlist, use.names = FALSE))), policy$allowed_calls)) {
      stop(sprintf("pure-R AST allowlist differs for %s", record$task))
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
  backend <- r_backend_policy(record$task, record$function_name, calls, implementation_class)
  for (field in c("backend_calls", "backend_classes", "backend_identity_keys")) {
    expected <- sort(as.character(unlist(backend[[sub("^backend_", "", field)]], use.names = FALSE)))
    actual <- sort(as.character(unlist(record[[field]], use.names = FALSE)))
    if (!identical(actual, expected)) stop(sprintf("R provenance %s differs for %s", field, record$task))
  }
  if (!identical(as.character(record$compiled_backend), backend$description)) {
    stop(sprintf("R compiled backend description differs for %s", record$task))
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
  backend <- r_backend_policy(task_id, as.character(function_name), calls, implementation_class)
  record <- list(
    schema_version = r_provenance_schema_version(),
    task = task_id,
    function_name = as.character(function_name),
    implementation_class = implementation_class,
    source_digest = r_body_digest(fn),
    source_body = r_source_body(fn),
    ast_calls = as.list(calls),
    ast_allowlist_id = if (identical(implementation_class, "pure_r")) pure_policy$id else "optimized-base-r-declared-v1",
    ast_allowlist = if (identical(implementation_class, "pure_r")) as.list(pure_policy$allowed_calls) else list(),
    forbidden_call_result = if (length(forbidden) == 0L) "pass" else paste0("declared:", paste(sort(forbidden), collapse = "+")),
    compiled_backend = backend$description,
    backend_calls = as.list(backend$calls),
    backend_classes = as.list(backend$classes),
    backend_identity_keys = as.list(backend$identity_keys)
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
    source_body = "not_applicable",
    ast_calls = list(),
    ast_allowlist_id = "not_applicable",
    ast_allowlist = list(),
    forbidden_call_result = "not_applicable",
    compiled_backend = "not_applicable",
    backend_calls = list(),
    backend_classes = list("not_applicable"),
    backend_identity_keys = list("not_applicable"),
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
    "source_body", "ast_calls", "ast_allowlist_id", "ast_allowlist", "forbidden_call_result",
    "compiled_backend", "backend_calls", "backend_classes", "backend_identity_keys", "reason"
  )
  for (field in fields) {
    expected_value <- if (is.null(expected[[field]])) "" else as.character(unlist(expected[[field]], use.names = FALSE))
    actual_value <- if (is.null(actual[[field]])) "" else as.character(unlist(actual[[field]], use.names = FALSE))
    if (!identical(expected_value, actual_value)) stop(sprintf("R provenance field %s differs for %s", field, label))
  }
  invisible(actual)
}
