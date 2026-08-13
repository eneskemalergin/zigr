# Deterministic input construction and mutation contracts.

benchmark_input_schema_version <- function() "benchmark-input-v2"

benchmark_master_seed <- function() 20260713L

# This is the frozen comparison vocabulary for the current direct harness.
# Pure R and optimized base R are task roles, not additional live runners.
direct_comparison_scope <- function() {
  products <- c("zigr", "rcpp", "cpp11", "extendr", "savvy")
  list(
    product_runners = products,
    baseline_roles = c("pure_r", "optimized_base_r"),
    control_runners = "c_call",
    direct_runners = c("r", "c_call", products),
    pure_r_runner = NA_character_,
    optimized_base_r_runner = "r"
  )
}

# The live runner contract is defined once here.  Callers derive names,
# artifacts, package loading, and invocation dispatch from these records.
direct_runner_registry <- function(root_dir) {
  scope <- direct_comparison_scope()
  fixture_library <- file.path(root_dir, "tmp", "fixture-library")
  package_runner <- function(name, package, library, dll) {
    list(
      name = name,
      label = name,
      invocation = "package_function",
      artifact_path = file.path(library, package, "libs", paste0(dll, .Platform$dynlib.ext)),
      package = list(package = package, library = library, dll = dll)
    )
  }
  products <- list(
    zigr = package_runner("zigr", "zigrFixture", fixture_library, "zigrFixture"),
    rcpp = package_runner("rcpp", "zigrRcpp", fixture_library, "zigrRcpp"),
    cpp11 = package_runner(
      "cpp11", "zigrCpp11", file.path(root_dir, "tmp", "cpp11-library"), "zigrCpp11"
    ),
    extendr = package_runner("extendr", "zigrExtendr", fixture_library, "zigrExtendr"),
    savvy = package_runner("savvy", "zigrSavvy", fixture_library, "zigrSavvy")
  )
  products <- products[scope$product_runners]
  c(list(
    r = list(
      name = "r", label = "R", invocation = "r_function",
      artifact_path = file.path(root_dir, "src", "r", "run_all.R"), package = NULL
    ),
    c_call = list(
      name = "c_call", label = "c_call (C)", invocation = "registered_native",
      artifact_path = file.path(root_dir, "src", "c_call", "bench.so"), package = NULL
    )
  ), products)
}

direct_runner_names <- function(root_dir) {
  names <- names(direct_runner_registry(root_dir))
  expected <- direct_comparison_scope()$direct_runners
  if (!identical(names, expected)) {
    stop("live runner registry differs from frozen comparison scope")
  }
  names
}

direct_runner_spec <- function(root_dir, runner) {
  if (length(runner) != 1L || is.na(runner) || !nzchar(runner)) {
    stop("direct runner requires one runner")
  }
  spec <- direct_runner_registry(root_dir)[[runner]]
  if (is.null(spec)) stop(sprintf("unknown direct runner: %s", runner))
  spec
}

direct_runner_package <- function(root_dir, runner) {
  package <- direct_runner_spec(root_dir, runner)$package
  if (is.null(package)) stop(sprintf("no direct runner package is declared for %s", runner))
  package
}

direct_runner_artifact_path <- function(root_dir, runner) {
  direct_runner_spec(root_dir, runner)$artifact_path
}

direct_runner_environment <- function(root_dir, runner, spec = NULL) {
  if (is.null(spec)) spec <- direct_runner_spec(root_dir, runner)
  if (spec$invocation %in% c("r_function", "registered_native")) return(.GlobalEnv)
  package <- spec$package
  loadNamespace(package$package, lib.loc = package$library)
}

validate_cli_arguments <- function(args, value_options = character(0), flag_options = character(0), label = "command") {
  args <- as.character(args)
  value_options <- as.character(value_options)
  flag_options <- as.character(flag_options)
  known <- c(value_options, flag_options)
  if (anyDuplicated(known) || any(!nzchar(known))) stop("CLI option declarations must be unique and non-empty")
  keys <- character(length(args))
  for (index in seq_along(args)) {
    argument <- args[[index]]
    value_match <- value_options[startsWith(argument, paste0("--", value_options, "="))]
    flag_match <- flag_options[argument == paste0("--", flag_options)]
    matches <- c(value_match, flag_match)
    if (length(matches) != 1L) stop(sprintf("unknown %s argument: %s", label, argument))
    if (matches[[1L]] %in% value_options && !nzchar(sub("^[^=]*=", "", argument))) {
      stop(sprintf("%s argument --%s requires a value", label, matches[[1L]]))
    }
    keys[[index]] <- matches[[1L]]
  }
  duplicates <- unique(keys[duplicated(keys)])
  if (length(duplicates) > 0L) stop(sprintf("repeated %s argument: --%s", label, duplicates[[1L]]))
  invisible(args)
}

benchmark_encoded_strings <- function() {
  byte_marked <- rawToChar(as.raw(c(0x66, 0x61, 0xe7, 0x61, 0x64, 0x65)))
  values <- c(
    enc2utf8("façade"),
    iconv("façade", from = "UTF-8", to = "latin1"),
    byte_marked,
    "",
    NA_character_
  )
  Encoding(values[[1L]]) <- "UTF-8"
  Encoding(values[[2L]]) <- "latin1"
  Encoding(values[[3L]]) <- "bytes"
  values
}

benchmark_string_input <- function(task_id) {
  cases <- benchmark_encoded_strings()
  if (identical(task_id, "17_string_concat")) {
    return(rep(cases, length.out = 10000L))
  }
  if (identical(task_id, "18_string_nchar")) {
    values <- rep(cases[seq_len(4L)], length.out = 10000L)
    values[seq.int(20L, 10000L, by = 20L)] <- NA_character_
    return(values)
  }
  if (identical(task_id, "19_string_encoding")) {
    return(rep(cases, length.out = 10000L))
  }
  stop(sprintf("no benchmark string input is declared for %s", task_id))
}

benchmark_factor_input <- function() {
  vocabulary <- sprintf("level_%03d", seq_len(100L))
  values <- rep(vocabulary, length.out = 10000L)
  values[[10000L]] <- NA_character_
  values
}

# Each factory builds one task input on demand so correctness checks keep only
# the current event's input live.
benchmark_revision_task_specs <- function() {
  list(
    list(id = "vector_sum", function_name = "bench_vector_sum", arguments = function() list(runif(1000000L)), tolerance = TRUE),
    list(id = "numeric_transform", function_name = "bench_numeric_transform", arguments = function() list(c(seq_len(99998L), NA_real_, NaN))),
    list(id = "broadcast", function_name = "bench_broadcast", arguments = function() list(runif(1000000L), 3.14), tolerance = TRUE),
    list(id = "sort", function_name = "bench_sort", arguments = function() list(runif(100000L))),
    list(id = "missing_mean", function_name = "bench_missing_mean", arguments = function() {
      x <- runif(1000000L)
      positions <- sample.int(length(x), 50000L)
      x[positions[seq_len(25000L)]] <- NA_real_
      x[positions[25000L + seq_len(25000L)]] <- NaN
      list(x)
    }, tolerance = TRUE),
    list(id = "transpose", function_name = "bench_transpose", arguments = function() list(matrix(runif(512L * 512L), 512L, 512L))),
    list(id = "rowcol", function_name = "bench_rowcol", arguments = function() list(matrix(runif(500L * 1000L), 500L, 1000L)), tolerance = TRUE),
    list(id = "matmul", function_name = "bench_matmul", arguments = function() list(matrix(runif(256L * 256L), 256L), matrix(runif(256L * 256L), 256L)), tolerance = TRUE),
    list(id = "dataframe", function_name = "bench_dataframe", arguments = function() {
      x <- rnorm(100000L)
      y <- abs(rnorm(100000L)) + 0.1
      missing <- sample.int(length(x), 5000L)
      x[missing] <- NA_real_
      groups <- sprintf("grp_%02d", 1:10)
      list(data.frame(x = x, y = y, grp = factor(rep(groups, length.out = length(x)), levels = groups)))
    }, tolerance = TRUE),
    list(id = "list_sum", function_name = "bench_list_sum", arguments = function() list(replicate(1000L, runif(100L), simplify = FALSE)), tolerance = TRUE),
    list(id = "string_concat", function_name = "bench_string_concat", arguments = function() list(rep(benchmark_encoded_strings(), 2000L))),
    list(id = "string_metadata", function_name = "bench_string_metadata", arguments = function() list(rep(benchmark_encoded_strings(), 2000L))),
    list(id = "factor", function_name = "bench_factor", arguments = function() list(benchmark_factor_input())),
    list(id = "attributes", function_name = "bench_attributes", arguments = function() list(runif(100000L))),
    list(id = "s4", function_name = "bench_s4", arguments = function() list(runif(100000L))),
    list(id = "logical_counts", function_name = "bench_logical_counts", arguments = function() list(rep(c(FALSE, TRUE, NA), length.out = 100000L))),
    list(id = "raw_copy", function_name = "bench_raw_copy", arguments = function() list(as.raw((0:262143) %% 251L))),
    list(id = "complex_conjugate", function_name = "bench_complex_conjugate", arguments = function() {
      x <- complex(real = seq_len(32768L), imaginary = seq_len(32768L) / 2)
      x[[1L]] <- NA_complex_
      x[[2L]] <- complex(real = NaN, imaginary = 3)
      list(x)
    }),
    list(id = "schema", function_name = "bench_schema", arguments = function() list(list(id = 1L, count = 2L, ratio = 0.5, enabled = TRUE))),
    list(id = "altrep_sum", function_name = "bench_altrep_sum", arguments = function() list(seq_len(1000000L)), altrep = TRUE, altrep_input_postcondition = "preserve"),
    list(id = "altrep_index", function_name = "bench_altrep_index", arguments = function() list(seq_len(10000000L)), altrep = TRUE, altrep_input_postcondition = "preserve"),
    list(id = "altrep_materialize", function_name = "bench_altrep_materialize", arguments = function() list(seq_len(5000000L)), altrep = TRUE, altrep_input_postcondition = "allow_change"),
    list(id = "external_state", function_name = "bench_external_state", arguments = function() list()),
    list(id = "eval", function_name = "bench_eval", arguments = function() list(runif(100000L)), tolerance = TRUE),
    list(id = "serialize", function_name = "bench_serialize", arguments = function() list(runif(100000L))),
    list(id = "rng", function_name = "bench_rng", arguments = function() list(100000L), rng = TRUE),
    list(id = "outputs", function_name = "bench_outputs", arguments = function() list())
  )
}

direct_task_suitability <- function() {
  data.frame(
    task = vapply(benchmark_revision_task_specs(), `[[`, character(1), "id"),
    immutable_input = TRUE,
    input_mutating = FALSE,
    stateful = c(rep(FALSE, 25L), TRUE, FALSE),
    representation_changing = c(rep(FALSE, 19L), TRUE, TRUE, TRUE, rep(FALSE, 5L)),
    batch_equivalent = c(rep(TRUE, 19L), FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE),
    large_output = c(
      FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE,
      FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE,
      FALSE, FALSE, TRUE, TRUE, FALSE
    ),
    small_output = c(
      FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE,
      TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
      FALSE, FALSE, FALSE, FALSE, TRUE
    ),
    gc_relevant = c(
      FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE,
      FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE,
      FALSE, FALSE, TRUE, TRUE, FALSE
    ),
    matrix_task = c(
      FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, rep(FALSE, 19L)
    ),
    stringsAsFactors = FALSE
  )
}

validate_direct_task_suitability <- function(rows = direct_task_suitability()) {
  required <- c(
    "task", "immutable_input", "input_mutating", "stateful", "representation_changing",
    "batch_equivalent", "large_output", "small_output", "gc_relevant", "matrix_task"
  )
  expected_tasks <- vapply(benchmark_revision_task_specs(), `[[`, character(1), "id")
  if (!is.data.frame(rows) || !identical(names(rows), required) ||
      !identical(as.character(rows$task), expected_tasks) || anyDuplicated(rows$task) ||
      any(!vapply(rows[-1L], is.logical, logical(1))) ||
      any(rows$immutable_input == rows$input_mutating) ||
      any((rows$stateful | rows$representation_changing) & rows$batch_equivalent) ||
      any(rows$large_output & rows$small_output) ||
      any(rows$gc_relevant & !rows$large_output)) {
    stop("direct task suitability is invalid")
  }
  rows
}

direct_task_suitability_row <- function(task_id, rows = direct_task_suitability()) {
  rows <- validate_direct_task_suitability(rows)
  index <- match(as.character(task_id), rows$task)
  if (is.na(index)) stop(sprintf("no direct task suitability is declared for %s", task_id))
  rows[index, , drop = FALSE]
}

# One event is one invocation with freshly prepared deterministic arguments.
# Tasks may repeat one prepared call only when doing so preserves the event's
# state, representation, allocation, and result-lifetime shape.
direct_task_batchability <- function(task_id) {
  suitability <- direct_task_suitability_row(task_id)
  if (isTRUE(suitability$batch_equivalent)) "repeat" else "one"
}

direct_task_repeats_for_correctness <- function(task_id) {
  suitability <- direct_task_suitability_row(task_id)
  !isTRUE(suitability$stateful) && !isTRUE(suitability$representation_changing)
}

direct_task_is_altrep <- function(task_id) {
  isTRUE(direct_task_suitability_row(task_id)$representation_changing)
}

direct_task_requires_measurement_gc <- function(task_id) {
  isTRUE(direct_task_suitability_row(task_id)$gc_relevant)
}

direct_task_altrep_input_postcondition <- function(spec) {
  if (!is.list(spec) || length(spec$id) != 1L || is.na(spec$id) || !nzchar(spec$id)) {
    stop("ALTREP input postcondition requires one task specification")
  }
  if (!isTRUE(spec$altrep)) {
    if (!is.null(spec$altrep_input_postcondition)) {
      stop(sprintf("non-ALTREP task %s declares an ALTREP input postcondition", spec$id))
    }
    return("ordinary")
  }
  postcondition <- spec$altrep_input_postcondition
  if (length(postcondition) != 1L || is.na(postcondition) ||
      !postcondition %in% c("preserve", "allow_change")) {
    stop(sprintf("ALTREP task %s has an invalid input postcondition", spec$id))
  }
  postcondition
}

assert_direct_task_altrep_input <- function(spec, unmaterialized, label = NULL) {
  postcondition <- direct_task_altrep_input_postcondition(spec)
  if (identical(postcondition, "preserve") && !isTRUE(unmaterialized)) {
    if (is.null(label)) label <- spec$id
    stop(sprintf("%s materialized compact ALTREP inside the timed call", label))
  }
  postcondition
}

direct_allocation_policy <- function() {
  list(
    policy_version = "large-output-observation-v2",
    large_output_vcells = list(complex_conjugate = 65536L),
    gc_requirement = "r-owned-output-gc-is-diagnostic-only-v1"
  )
}

validate_direct_allocation_policy <- function(policy) {
  fields <- c("policy_version", "large_output_vcells", "gc_requirement")
  if (!is.list(policy) || !identical(names(policy), fields) ||
      !identical(as.character(policy$policy_version), "large-output-observation-v2") ||
      !identical(as.character(policy$gc_requirement),
                 "r-owned-output-gc-is-diagnostic-only-v1") ||
      !is.list(policy$large_output_vcells) ||
      !identical(names(policy$large_output_vcells), "complex_conjugate")) {
    stop("direct allocation policy is invalid")
  }
  output_vcells <- input_scalar_integer(
    policy$large_output_vcells$complex_conjugate, "complex conjugate output vcells"
  )
  if (!identical(output_vcells, 65536L)) {
    stop("direct allocation policy has an invalid complex output size")
  }
  list(
    policy_version = "large-output-observation-v2",
    large_output_vcells = list(complex_conjugate = output_vcells),
    gc_requirement = "r-owned-output-gc-is-diagnostic-only-v1"
  )
}

direct_task_allocation_class <- function(task_id, policy = direct_allocation_policy()) {
  policy <- validate_direct_allocation_policy(policy)
  if (task_id %in% names(policy$large_output_vcells)) "large_output" else "ordinary_result"
}

direct_task_output_vcells <- function(task_id, policy = direct_allocation_policy()) {
  policy <- validate_direct_allocation_policy(policy)
  if (task_id %in% names(policy$large_output_vcells)) {
    return(as.integer(policy$large_output_vcells[[task_id]]))
  }
  0L
}

# These are the fixed revision inputs whose public paths are retained in the
# performance ledger.  The account describes only work attributable to the
# generated fixture source; R headers, allocator rounding, and opaque R work
# are kept explicit below.
direct_cost_account_task_ids <- function() {
  c(
    "vector_sum", "broadcast", "missing_mean", "list_sum", "sort", "serialize",
    "transpose", "complex_conjugate", "rng", "attributes", "altrep_materialize",
    "factor", "eval", "string_metadata", "altrep_index"
  )
}

direct_task_cost_accounts <- function(tasks = direct_cost_account_task_ids()) {
  tasks <- as.character(tasks)
  expected <- direct_cost_account_task_ids()
  if (!identical(as.character(R.version$arch), "x86_64") ||
      !identical(as.integer(.Machine$sizeof.pointer), 8L)) {
    stop("cost accounts require the x86_64 64-bit fixture ABI")
  }
  if (anyNA(tasks) || any(!nzchar(tasks)) || anyDuplicated(tasks) ||
      any(!tasks %in% expected)) {
    stop("cost account tasks are invalid")
  }
  rows <- data.frame(
    task = expected,
    input_elements = c(1000000L, 1000000L, 1000000L, 100000L, 100000L,
                       100000L, 262144L, 32768L, 1L, 100000L, 5000000L,
                       10000L, 100000L, 10000L, 10000000L),
    input_passes = c(1L, 1L, 2L, 1L, 1L, 0L, 1L, 1L, 0L, 0L, 1L,
                     0L, 0L, 1L, 0L),
    borrowed_bytes = c(8000000, 8000000, 8000000, 800000, 800000,
                       0, 2097152, 524288, 0, 0, 0, 0, 0, 80000, 0),
    materialized_bytes = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1024, 0, 0, 0, 0),
    native_allocation_events = c(0L, 0L, 0L, 0L, 2L, 0L, 0L, 0L, 0L, 0L, 1L,
                                 0L, 0L, 0L, 0L),
    native_requested_bytes = c(0, 0, 0, 0, 1324288, 0, 0, 0, 0, 0, 1024,
                               0, 0, 0, 0),
    r_payload_allocations = c(1L, 1L, 1L, 1L, 1L, 2L, 1L, 1L, 1L, 1L, 1L,
                              1L, 0L, 2L, 1L),
    r_payload_bytes = c(8, 8, 8, 8, 800000, 1600031, 2097152,
                        524288, 800000, 800000, 20000000, 800, 0, 60, 8),
    copied_bytes = c(0, 0, 0, 0, 800000, 0, 0, 0, 0, 800000, 20000000,
                     0, 0, 20, 0),
    written_bytes = c(8, 8, 8, 8, 800000, 0, 2097152,
                      524288, 800000, 800000, 20000000, 800, 0, 60, 8),
    opaque_r_operations = c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE,
                            FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
    stringsAsFactors = FALSE
  )
  rows$list_slot_reads <- 0L
  rows$list_slot_reads[rows$task == "list_sum"] <- 1000L
  rows$indexed_element_reads <- 0L
  rows$indexed_element_reads[rows$task == "altrep_index"] <- 312500L
  rows$account_scope <- "exact-attributable-fixed-input-x86_64-v3"
  rows <- rows[match(tasks, rows$task), , drop = FALSE]
  rownames(rows) <- NULL
  rows
}

validate_direct_task_cost_accounts <- function(accounts, tasks = direct_cost_account_task_ids()) {
  tasks <- as.character(tasks)
  required <- c(
    "task", "input_elements", "input_passes", "borrowed_bytes", "materialized_bytes",
    "native_allocation_events", "native_requested_bytes", "r_payload_allocations",
    "r_payload_bytes", "copied_bytes", "written_bytes", "opaque_r_operations",
    "list_slot_reads", "indexed_element_reads", "account_scope"
  )
  if (!is.data.frame(accounts) || !identical(names(accounts), required) ||
      !identical(as.character(accounts$task), tasks) || anyDuplicated(accounts$task)) {
    stop("direct task cost accounts are invalid")
  }
  if (nrow(accounts) == 0L) return(accounts)
  numeric_fields <- setdiff(required, c("task", "opaque_r_operations", "account_scope"))
  numeric_values <- unlist(accounts[numeric_fields], use.names = FALSE)
  if (
      any(!vapply(accounts[numeric_fields], is.numeric, logical(1))) ||
      any(!vapply(accounts["opaque_r_operations"], is.logical, logical(1))) ||
      any(!vapply(accounts["account_scope"], is.character, logical(1))) ||
      anyNA(numeric_values) || any(!is.finite(numeric_values)) || any(numeric_values < 0) ||
      anyNA(accounts$opaque_r_operations) || anyNA(accounts$account_scope) ||
      any(accounts$account_scope != "exact-attributable-fixed-input-x86_64-v3")) {
    stop("direct task cost accounts are invalid")
  }
  expected <- direct_task_cost_accounts(tasks)
  if (!isTRUE(all.equal(accounts, expected, check.attributes = FALSE))) {
    stop("direct task cost accounts are invalid")
  }
  accounts
}

direct_batch_repetition_map <- function(tasks, repetitions = 1L) {
  tasks <- as.character(tasks)
  if (length(tasks) == 0L || any(!nzchar(tasks)) || anyDuplicated(tasks)) {
    stop("batch repetition tasks are invalid")
  }
  repetitions <- input_scalar_integer(repetitions, "default batch repetitions")
  result <- setNames(rep.int(repetitions, length(tasks)), tasks)
  result[vapply(tasks, direct_task_batchability, character(1)) == "one"] <- 1L
  result
}

format_named_integer_map <- function(values, label) {
  if (!is.numeric(values) || is.null(names(values)) || any(!nzchar(names(values))) ||
      anyDuplicated(names(values))) {
    stop(sprintf("%s must be a named integer map", label))
  }
  checked <- vapply(values, input_scalar_integer, integer(1), label = label)
  paste(paste(names(checked), checked, sep = ":"), collapse = ",")
}

parse_named_integer_map <- function(value, expected_names, label) {
  entries <- strsplit(as.character(value), ",", fixed = TRUE)[[1L]]
  fields <- strsplit(entries, ":", fixed = TRUE)
  if (length(entries) == 0L || any(lengths(fields) != 2L)) {
    stop(sprintf("%s must be task:integer entries", label))
  }
  names <- vapply(fields, `[[`, character(1), 1L)
  values <- vapply(fields, `[[`, character(1), 2L)
  if (any(!nzchar(names)) || anyDuplicated(names) || !setequal(names, expected_names)) {
    stop(sprintf("%s task coverage differs", label))
  }
  parsed <- setNames(vapply(values, input_scalar_integer, integer(1), label = label), names)
  parsed[expected_names]
}

direct_sizing_policy <- function() {
  list(
    policy_version = "shared-ladder-v4",
    ladder = as.list(c(1L, 8L, 64L, 512L, 4096L, 8192L)),
    minimum_batch_ms = 1,
    timer_floor_multiplier = 20L,
    target_batch_ms = 1,
    maximum_batch_ms = 500
  )
}

validate_direct_sizing_policy <- function(policy) {
  fields <- c(
    "policy_version", "ladder", "minimum_batch_ms", "timer_floor_multiplier",
    "target_batch_ms", "maximum_batch_ms"
  )
  if (!is.list(policy) || !identical(names(policy), fields) ||
      !identical(as.character(policy$policy_version), "shared-ladder-v4") ||
      !is.list(policy$ladder) || !is.null(names(policy$ladder)) ||
      length(policy$ladder) != 6L || any(lengths(policy$ladder) != 1L)) {
    stop("direct sizing policy is invalid")
  }
  ladder <- unlist(policy$ladder, use.names = FALSE)
  if (!is.numeric(ladder) || anyNA(ladder) || any(!is.finite(ladder)) ||
      !identical(as.numeric(ladder), c(1, 8, 64, 512, 4096, 8192))) {
    stop("direct sizing policy is invalid")
  }
  multiplier <- input_scalar_integer(policy$timer_floor_multiplier, "timer floor multiplier")
  numeric <- unlist(policy[c("minimum_batch_ms", "target_batch_ms", "maximum_batch_ms")], use.names = FALSE)
  if (length(numeric) != 3L || any(!is.finite(numeric)) || any(numeric <= 0) ||
      numeric[[1L]] > numeric[[2L]] || numeric[[2L]] > numeric[[3L]] ||
      !identical(multiplier, 20L) || !identical(as.numeric(numeric), c(1, 1, 500))) {
    stop("direct sizing policy has invalid batch bounds")
  }
  list(
    ladder = as.integer(ladder),
    minimum_batch_ms = as.numeric(numeric[[1L]]),
    timer_floor_multiplier = multiplier,
    target_batch_ms = as.numeric(numeric[[2L]]),
    maximum_batch_ms = as.numeric(numeric[[3L]])
  )
}

direct_sizing_target_ms <- function(timer_floors, policy = direct_sizing_policy()) {
  policy <- validate_direct_sizing_policy(policy)
  if (length(timer_floors) == 0L || any(!is.finite(timer_floors)) || any(timer_floors < 0)) {
    stop("batch sizing timer floors are invalid")
  }
  max(policy$minimum_batch_ms, policy$target_batch_ms,
      max(as.numeric(timer_floors)) * policy$timer_floor_multiplier)
}

remaining_direct_run_seconds <- function(started_elapsed, current_elapsed, timeout_seconds) {
  if (length(started_elapsed) != 1L || length(current_elapsed) != 1L ||
      !is.finite(started_elapsed) || !is.finite(current_elapsed) ||
      current_elapsed < started_elapsed) {
    stop("direct run elapsed times are invalid")
  }
  timeout_seconds <- input_scalar_integer(timeout_seconds, "total run timeout")
  floor(timeout_seconds - (current_elapsed - started_elapsed))
}

direct_sizing_count_accepted <- function(rows, timer_floors, runners,
                                         policy = direct_sizing_policy()) {
  required <- c("runner", "task", "batch_repetitions", "batch_elapsed_ms", "gc_elapsed_ms")
  sizing_policy <- validate_direct_sizing_policy(policy)
  if (!is.data.frame(rows) || !identical(names(rows), required) ||
      nrow(rows) != length(runners) || !identical(as.character(rows$runner), runners) ||
      anyNA(rows) || any(!vapply(rows[c("batch_repetitions", "batch_elapsed_ms", "gc_elapsed_ms")], is.numeric, logical(1))) ||
      any(!is.finite(as.matrix(rows[c("batch_repetitions", "batch_elapsed_ms", "gc_elapsed_ms")]))) ||
      any(rows$batch_repetitions < 1L) || any(rows$batch_repetitions != as.integer(rows$batch_repetitions)) ||
      any(!rows$batch_repetitions %in% sizing_policy$ladder) ||
      any(rows$batch_elapsed_ms < 0) || any(rows$gc_elapsed_ms < 0) ||
      any(rows$gc_elapsed_ms > rows$batch_elapsed_ms) ||
      any(rows$batch_elapsed_ms > sizing_policy$maximum_batch_ms)) {
    return(FALSE)
  }
  target_ms <- max(
    sizing_policy$minimum_batch_ms, sizing_policy$target_batch_ms,
    max(as.numeric(timer_floors)) * sizing_policy$timer_floor_multiplier
  )
  all(rows$batch_elapsed_ms > target_ms)
}

advance_direct_sizing_tasks <- function(active_tasks, complete_tasks, count,
                                        blocked_tasks = character(),
                                        policy = direct_sizing_policy(),
                                        cap_exceeded_tasks = character()) {
  policy <- validate_direct_sizing_policy(policy)
  active_tasks <- as.character(active_tasks)
  blocked_tasks <- as.character(blocked_tasks)
  cap_exceeded_tasks <- as.character(cap_exceeded_tasks)
  if (length(active_tasks) == 0L || length(complete_tasks) != length(active_tasks) ||
      anyNA(active_tasks) || any(!nzchar(active_tasks)) || anyNA(complete_tasks) ||
      !is.logical(complete_tasks) || anyDuplicated(active_tasks) ||
      any(!active_tasks %in% vapply(benchmark_revision_task_specs(), `[[`, character(1), "id")) ||
      any(!blocked_tasks %in% vapply(benchmark_revision_task_specs(), `[[`, character(1), "id")) ||
      anyDuplicated(blocked_tasks) || any(!cap_exceeded_tasks %in% active_tasks) ||
      anyDuplicated(cap_exceeded_tasks) || any(cap_exceeded_tasks %in% active_tasks[complete_tasks])) {
    stop("direct sizing task progress is invalid")
  }
  count <- input_scalar_integer(count, "sizing count")
  if (!count %in% policy$ladder) stop("direct sizing count is not on the declared ladder")
  failed_tasks <- active_tasks[!complete_tasks]
  one_event_failures <- failed_tasks[vapply(failed_tasks, direct_task_batchability, character(1)) == "one"]
  repeat_failures <- setdiff(failed_tasks, c(one_event_failures, cap_exceeded_tasks))
  if (identical(count, max(policy$ladder)) && length(repeat_failures) > 0L) {
    stop(sprintf(
      "batch sizing cannot meet the shared policy for: %s",
      paste(c(blocked_tasks, cap_exceeded_tasks, repeat_failures), collapse = ", ")
    ))
  }
  list(
    active_tasks = repeat_failures,
    blocked_tasks = unique(c(blocked_tasks, one_event_failures, cap_exceeded_tasks))
  )
}

select_direct_batch_repetitions <- function(sizing, timer_floors, runners, tasks,
                                            policy = direct_sizing_policy()) {
  required <- c("runner", "task", "batch_repetitions", "batch_elapsed_ms", "gc_elapsed_ms")
  if (!is.data.frame(sizing) || !identical(names(sizing), required) ||
      anyNA(sizing) || any(!is.finite(as.matrix(sizing[c("batch_repetitions", "batch_elapsed_ms", "gc_elapsed_ms")]))) ||
      any(sizing$batch_repetitions < 1L) || any(sizing$batch_elapsed_ms < 0) ||
      any(sizing$gc_elapsed_ms < 0) || any(sizing$gc_elapsed_ms > sizing$batch_elapsed_ms)) {
    stop("batch sizing rows are invalid")
  }
  if (!identical(names(timer_floors), runners) || any(!is.finite(timer_floors)) || any(timer_floors < 0)) {
    stop("batch sizing timer floors are invalid")
  }
  target_ms <- direct_sizing_target_ms(timer_floors, policy)
  policy <- validate_direct_sizing_policy(policy)
  if (any(sizing$batch_repetitions != as.integer(sizing$batch_repetitions)) ||
      any(!sizing$batch_repetitions %in% policy$ladder) ||
      !setequal(as.character(sizing$runner), runners) || !setequal(as.character(sizing$task), tasks)) {
    stop("batch sizing rows have invalid coverage or repetitions")
  }
  selected <- integer(length(tasks))
  names(selected) <- tasks
  for (task in tasks) {
    candidates <- if (identical(direct_task_batchability(task), "one")) 1L else policy$ladder
    rows <- sizing[sizing$task == task, , drop = FALSE]
    if (anyDuplicated(paste(rows$runner, rows$batch_repetitions, sep = "\r"))) {
      stop(sprintf("%s has duplicate batch sizing observations", task))
    }
    observed <- lapply(runners, function(runner) {
      as.integer(rows$batch_repetitions[rows$runner == runner])
    })
    if (any(vapply(observed, length, integer(1)) == 0L) ||
        any(!vapply(observed, identical, logical(1), observed[[1L]]))) {
      stop(sprintf("%s batch sizing coverage differs across runners", task))
    }
    chosen <- NA_integer_
    for (count in candidates) {
      candidate <- rows[rows$batch_repetitions == count, , drop = FALSE]
      if (nrow(candidate) == length(runners) && identical(as.character(candidate$runner), runners) &&
          all(candidate$batch_elapsed_ms > target_ms) &&
          all(candidate$batch_elapsed_ms <= policy$maximum_batch_ms)) {
        chosen <- count
        break
      }
    }
    if (is.na(chosen)) {
      stop(sprintf("%s cannot meet the shared sizing target within the batch cap", task))
    }
    expected <- candidates[seq_len(match(chosen, candidates))]
    if (!identical(observed[[1L]], expected)) {
      stop(sprintf("%s batch sizing ladder contains an unnecessary or undeclared step", task))
    }
    selected[[task]] <- chosen
  }
  selected
}

benchmark_revision_arguments <- function(spec, master_seed = benchmark_master_seed()) {
  seed <- task_input_seed(master_seed, spec$id, "revision-v1")
  arguments <- with_preserved_rng(seed, spec$arguments())
  if (!is.list(arguments)) stop(sprintf("input factory for %s did not return a list", spec$id))
  arguments
}

validate_special_task_input <- function(task_id, arguments) {
  if (task_id %in% c("17_string_concat", "18_string_nchar", "19_string_encoding")) {
    expected <- benchmark_string_input(task_id)
    if (length(arguments) != 1L || !identical(arguments[[1L]], expected) ||
        !identical(Encoding(arguments[[1L]]), Encoding(expected))) {
      stop(sprintf("string contract input differs for %s", task_id))
    }
  }
  if (identical(task_id, "20_factor_ops")) {
    if (length(arguments) != 1L) {
      stop("factor contract requires 100 deterministic levels and one explicit trailing missing value")
    }
    values <- arguments[[1L]]
    vocabulary <- sprintf("level_%03d", seq_len(100L))
    if (!is.character(values) || length(values) != 10000L ||
        !identical(sort(unique(values[!is.na(values)])), vocabulary) ||
        sum(is.na(values)) != 1L || !is.na(values[[10000L]])) {
      stop("factor contract requires 100 deterministic levels and one explicit trailing missing value")
    }
  }
  if (identical(task_id, "24_long_vector_idx")) {
    values <- arguments[[1L]]
    if (length(arguments) != 1L || typeof(values) != "integer" || length(values) != 10000000L ||
        values[[1L]] != 1L || values[[length(values)]] != 10000000L) {
      stop("compact ALTREP indexing contract input differs")
    }
  }
  if (identical(task_id, "43_rng_stress")) {
    if (length(arguments) != 1L || !identical(arguments[[1L]], 1000000L)) {
      stop("RNG contract requires exactly one million normal draws")
    }
  }
  invisible(arguments)
}

input_scalar_integer <- function(value, label) {
  numeric_value <- suppressWarnings(as.numeric(value))
  if (length(numeric_value) != 1L || is.na(numeric_value) || !is.finite(numeric_value) ||
      numeric_value < 1 || numeric_value > .Machine$integer.max || numeric_value != as.integer(numeric_value)) {
    stop(sprintf("%s must be one positive integer", label))
  }
  as.integer(numeric_value)
}

task_input_seed <- function(master_seed, task_id, fixture_version) {
  master_seed <- input_scalar_integer(master_seed, "master seed")
  identity <- paste(task_id, fixture_version, sep = "\r")
  bytes <- utf8ToInt(enc2utf8(identity))
  hash <- as.double(master_seed)
  if (length(bytes) > 0L) {
    for (index in seq_along(bytes)) {
      hash <- (hash * 131 + bytes[[index]] + index) %% 2147483646
    }
  }
  as.integer(hash + 1)
}

task_altrep_intent <- function(task_id) {
  if (task_id %in% c("24_long_vector_idx", "62_boundary_altrep_integer_generated", "63_boundary_altrep_integer_handwritten")) {
    return("compact_integer_input")
  }
  if (task_id %in% sprintf("%02d_%s", 30:37, c(
    "altrep_create", "altrep_materialize", "altrep_elt_walk", "altrep_region_read",
    "altrep_sum_via_R", "altrep_sum_native", "altrep_min_max", "altrep_no_na_query"
  ))) {
    return("runtime_compact_integer_recipe")
  }
  "ordinary_r_object"
}

with_preserved_rng <- function(seed, expression) {
  existed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  previous <- if (existed) get(".Random.seed", envir = .GlobalEnv, inherits = FALSE) else NULL
  on.exit({
    if (existed) {
      assign(".Random.seed", previous, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(input_scalar_integer(seed, "task seed"), kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
  force(expression)
}

materialize_task_input <- function(task, seed) {
  with_preserved_rng(seed, {
    arguments <- task$args()
    if (!is.list(arguments)) stop(sprintf("input factory for %s did not return a list", task$id))
    validate_special_task_input(task$id, arguments)
    list(arguments = arguments, post_factory_rng_state = get(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
  })
}

rng_state_snapshot <- function() {
  if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    stop("RNG contract expected .Random.seed to exist")
  }
  as.integer(get(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
}

assert_rng_state_equivalent <- function(expected, actual, task_id = "43_rng_stress") {
  if (!identical(as.integer(expected), as.integer(actual))) {
    stop(sprintf("post-call RNG state differs for %s", task_id))
  }
  invisible(actual)
}

canonical_input_value <- function(value) {
  if (identical(typeof(value), "externalptr") || inherits(value, "benchmark_external_state_recipe")) {
    return(list(type = "externalptr", recipe = "runner_registered_fixture_state"))
  }
  if (is.environment(value) || is.function(value) || identical(typeof(value), "weakref")) {
    stop(sprintf("input fingerprint does not support values of type %s", typeof(value)))
  }
  attributes_value <- attributes(value)
  if (is.list(value)) {
    payload <- lapply(value, canonical_input_value)
  } else {
    payload <- value
  }
  list(
    type = typeof(value),
    length = length(value),
    payload = payload,
    encodings = if (is.character(value)) Encoding(value) else NULL,
    attributes = if (is.null(attributes_value)) NULL else list(
      names = names(attributes_value),
      values = lapply(unname(attributes_value), canonical_input_value)
    )
  )
}

serialized_md5 <- function(value) {
  path <- tempfile("zigr-fingerprint-")
  on.exit(unlink(path), add = TRUE)
  saveRDS(value, path, version = 3, compress = FALSE)
  unname(as.character(tools::md5sum(path))[[1L]])
}

task_input_fingerprint <- function(task_id, arguments, post_factory_rng_state, altrep_intent) {
  serialized_md5(list(
    schema_version = benchmark_input_schema_version(),
    task = as.character(task_id),
    arguments = canonical_input_value(arguments),
    altrep_intent = as.character(altrep_intent),
    post_factory_rng_state = as.integer(post_factory_rng_state)
  ))
}

task_arguments_fingerprint <- function(task_id, arguments, altrep_intent) {
  serialized_md5(list(
    schema_version = benchmark_input_schema_version(),
    task = as.character(task_id),
    arguments = canonical_input_value(arguments),
    altrep_intent = as.character(altrep_intent)
  ))
}

materialize_runtime_task_arguments <- function(arguments, mutation_policy, method_receiver) {
  if (!identical(mutation_policy, "stateful_reset_required")) return(arguments)
  if (length(arguments) < 1L || !inherits(arguments[[1L]], "benchmark_external_state_recipe")) {
    stop("stateful task input is missing its external-state recipe")
  }
  if (!is.function(method_receiver)) stop("stateful task has no method receiver factory")
  arguments[[1L]] <- method_receiver()
  arguments
}

build_input_recipe_record <- function(task, master_seed, fixture_version, contract_version) {
  seed <- task_input_seed(master_seed, task$id, fixture_version)
  materialized <- materialize_task_input(task, seed)
  intent <- task_altrep_intent(task$id)
  list(
    task = task$id,
    master_seed = input_scalar_integer(master_seed, "master seed"),
    task_seed = seed,
    fixture_version = as.character(fixture_version),
    contract_version = as.character(contract_version),
    mutation_policy = task_mutation_policy(task$id),
    altrep_intent = intent,
    fingerprint = task_input_fingerprint(
      task$id,
      materialized$arguments,
      materialized$post_factory_rng_state,
      intent
    )
  )
}

write_input_recipe_manifest <- function(path, tasks, task_manifest, evidence, master_seed) {
  selected_ids <- vapply(tasks, function(task) task$id, character(1))
  task_rows <- match(selected_ids, task_manifest$task)
  if (anyNA(task_rows)) stop("input recipes contain a task absent from the task manifest")
  r_rows <- evidence$tasks[evidence$tasks$runner == "r", , drop = FALSE]
  evidence_indices <- match(selected_ids, r_rows$task)
  if (anyNA(evidence_indices)) stop("input recipes contain a task absent from normalized evidence")
  records <- vector("list", length(tasks))
  for (index in seq_along(tasks)) {
    records[[index]] <- build_input_recipe_record(
      tasks[[index]],
      master_seed,
      r_rows$fixture_version[[evidence_indices[[index]]]],
      r_rows$contract_version[[evidence_indices[[index]]]]
    )
    gc(verbose = FALSE)
  }
  payload <- list(
    schema_version = benchmark_input_schema_version(),
    master_seed = input_scalar_integer(master_seed, "master seed"),
    fingerprint_algorithm = "R serialization v3 structural MD5",
    tasks = records
  )
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(payload, path, auto_unbox = TRUE, pretty = TRUE)
  invisible(payload)
}

read_input_recipe_manifest <- function(path) {
  if (!file.exists(path)) stop(sprintf("canonical input manifest not found: %s", path))
  payload <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  if (!identical(as.character(payload$schema_version), benchmark_input_schema_version())) {
    stop("unsupported canonical input schema version")
  }
  records <- payload$tasks
  if (is.null(records) || length(records) == 0L) stop("canonical input manifest has no task recipes")
  ids <- vapply(records, function(record) as.character(record$task), character(1))
  if (anyDuplicated(ids)) stop("canonical input manifest has duplicate task recipes")
  names(records) <- ids
  payload$tasks <- records
  payload
}

validate_input_manifest_digest <- function(path, expected_digest) {
  if (length(expected_digest) != 1L || is.na(expected_digest) || !nzchar(expected_digest)) {
    stop("expected canonical input manifest digest is missing")
  }
  actual <- unname(as.character(tools::md5sum(path))[[1L]])
  if (!identical(actual, as.character(expected_digest))) {
    stop(sprintf("canonical input manifest digest differs: expected %s, got %s", expected_digest, actual))
  }
  invisible(actual)
}

validate_materialized_task_input <- function(task, record, master_seed, materialized) {
  if (is.null(record)) stop(sprintf("canonical input recipe is missing for %s", task$id))
  expected_master <- input_scalar_integer(master_seed, "master seed")
  recorded_master <- input_scalar_integer(record$master_seed, sprintf("recorded master seed for %s", task$id))
  if (!identical(expected_master, recorded_master)) stop(sprintf("master seed mismatch for %s", task$id))
  expected_seed <- task_input_seed(expected_master, task$id, as.character(record$fixture_version))
  recorded_seed <- input_scalar_integer(record$task_seed, sprintf("recorded task seed for %s", task$id))
  if (!identical(expected_seed, recorded_seed)) stop(sprintf("task seed mismatch for %s", task$id))
  expected_policy <- task_mutation_policy(task$id)
  if (!identical(as.character(record$mutation_policy), expected_policy)) {
    stop(sprintf("mutation policy mismatch for %s", task$id))
  }
  expected_intent <- task_altrep_intent(task$id)
  if (!identical(as.character(record$altrep_intent), expected_intent)) {
    stop(sprintf("ALTREP intent mismatch for %s", task$id))
  }
  actual <- task_input_fingerprint(
    task$id,
    materialized$arguments,
    materialized$post_factory_rng_state,
    expected_intent
  )
  if (!identical(actual, as.character(record$fingerprint))) {
    stop(sprintf("canonical input fingerprint mismatch for %s: expected %s, got %s", task$id, record$fingerprint, actual))
  }
  invisible(materialized)
}

assert_immutable_input <- function(task_id, arguments, before_fingerprint, altrep_intent) {
  after <- task_arguments_fingerprint(task_id, arguments, altrep_intent)
  if (!identical(after, before_fingerprint)) stop(sprintf("immutable input mutated for %s", task_id))
  invisible(after)
}

# One direct timing path: calls are prepared before the clock, and only the
# benchmark event is evaluated between the two timer reads.

if (!requireNamespace("microbenchmark", quietly = TRUE)) {
  stop("direct timing requires the microbenchmark package")
}

direct_function_call <- function(function_object, arguments = list()) {
  if (!is.function(function_object)) stop("direct R call requires a function")
  if (!is.list(arguments)) stop("direct call arguments must be a list")
  as.call(c(list(function_object), arguments))
}

direct_native_call <- function(interface, symbol, arguments = list()) {
  if (!(interface %in% c(".Call", ".External"))) {
    stop("direct native interface must be .Call or .External")
  }
  if (!inherits(symbol, "NativeSymbolInfo") && !inherits(symbol, "NativeSymbol")) {
    stop("direct native call requires a resolved symbol")
  }
  if (!is.list(arguments)) stop("direct call arguments must be a list")
  as.call(c(list(as.name(interface), symbol), arguments))
}

direct_batch_expression <- function(call, repetitions) {
  repetitions <- input_scalar_integer(repetitions, "batch repetitions")
  if (repetitions < 1L) stop("batch repetitions must be positive")
  if (!is.call(call)) stop("direct batch requires one prepared call expression")
  if (repetitions == 1L) return(call)
  as.call(c(list(as.name("{")), rep(list(call), repetitions)))
}

measure_direct_batch <- function(
    call, environment, repetitions,
    clock = function() microbenchmark::get_nanotime() / 1e9,
    gc_clock = function() gc.time()[[3L]]) {
  batch <- direct_batch_expression(call, repetitions)
  read_elapsed <- function() {
    value <- clock()
    if (length(value) != 1L) value <- value[["elapsed"]]
    value <- as.numeric(value)
    if (length(value) != 1L || !is.finite(value)) stop("timer returned an invalid elapsed value")
    value
  }
  gc_started <- as.numeric(gc_clock())
  started <- read_elapsed()
  result <- eval(batch, envir = environment)
  finished <- read_elapsed()
  gc_finished <- as.numeric(gc_clock())
  elapsed_ms <- (finished - started) * 1000
  gc_elapsed_ms <- (gc_finished - gc_started) * 1000
  if (!is.finite(elapsed_ms) || elapsed_ms < 0 || !is.finite(gc_elapsed_ms) ||
      gc_elapsed_ms < 0) {
    stop("timer moved backwards")
  }
  list(
    result = result,
    batch_elapsed_ms = elapsed_ms,
    batch_repetitions = as.integer(repetitions),
    elapsed_per_event_ms = elapsed_ms / repetitions,
    gc_elapsed_ms = gc_elapsed_ms
  )
}

direct_vector_heap_trigger_vcells <- function(gc_state) {
  if (!is.matrix(gc_state) || !identical(rownames(gc_state), c("Ncells", "Vcells")) ||
      !("gc trigger" %in% colnames(gc_state))) {
    stop("R GC state has no vector-heap trigger")
  }
  trigger <- as.numeric(gc_state["Vcells", "gc trigger"])
  if (length(trigger) != 1L || !is.finite(trigger) || trigger <= 0 ||
      trigger != floor(trigger)) {
    stop("R GC state has an invalid vector-heap trigger")
  }
  trigger
}

validate_direct_timing_samples <- function(
    samples, runners = NULL, tasks = NULL, measurement_samples = NULL) {
  required <- c(
    "runner", "task", "phase", "measurement_sample", "batch_repetitions",
    "batch_elapsed_ms", "elapsed_per_event_ms", "gc_elapsed_ms", "vector_heap_trigger_vcells"
  )
  if (!is.data.frame(samples) || !identical(names(samples), required)) {
    stop("direct timing samples have the wrong columns")
  }
  if (nrow(samples) == 0L) stop("direct timing samples are empty")
  identity_fields <- c("runner", "task", "phase")
  if (anyNA(samples[identity_fields]) ||
      any(!vapply(samples[identity_fields], is.character, logical(1))) ||
      any(!nzchar(samples$runner)) || any(!nzchar(samples$task)) ||
      any(samples$phase != "measurement")) {
    stop("only measurement rows may enter timing samples")
  }
  numeric_fields <- c(
    "measurement_sample", "batch_repetitions", "batch_elapsed_ms", "elapsed_per_event_ms",
    "gc_elapsed_ms", "vector_heap_trigger_vcells"
  )
  if (any(!vapply(samples[numeric_fields], is.numeric, logical(1))) ||
      anyNA(samples[numeric_fields]) ||
      any(!is.finite(as.matrix(samples[numeric_fields]))) ||
      any(samples$measurement_sample < 1L) ||
      any(samples$measurement_sample != as.integer(samples$measurement_sample)) ||
      any(samples$batch_repetitions < 1L) ||
      any(samples$batch_repetitions != as.integer(samples$batch_repetitions)) ||
      any(samples$batch_elapsed_ms < 0) || any(samples$elapsed_per_event_ms < 0) ||
      any(samples$gc_elapsed_ms < 0) || any(samples$gc_elapsed_ms > samples$batch_elapsed_ms) ||
      any(samples$vector_heap_trigger_vcells < 1L) ||
      any(samples$vector_heap_trigger_vcells != floor(samples$vector_heap_trigger_vcells))) {
    stop("direct timing samples contain invalid measurements")
  }
  expected_elapsed <- samples$batch_elapsed_ms / samples$batch_repetitions
  if (!isTRUE(all.equal(
    samples$elapsed_per_event_ms, expected_elapsed,
    tolerance = 1e-12, check.attributes = FALSE
  ))) {
    stop("direct timing elapsed-per-event values differ from their batches")
  }
  keys <- paste(samples$runner, samples$task, samples$measurement_sample, sep = "\r")
  if (anyDuplicated(keys)) stop("direct timing samples contain duplicate identities")
  repetitions <- split(samples$batch_repetitions, samples$task)
  if (any(vapply(repetitions, function(value) length(unique(value)) != 1L, logical(1)))) {
    stop("a task does not use one shared batch repetition count")
  }
  heap_triggers <- split(samples$vector_heap_trigger_vcells, paste(samples$runner, samples$task, sep = "\r"))
  if (any(vapply(heap_triggers, function(value) length(unique(value)) != 1L, logical(1)))) {
    stop("a runner-task does not use one vector-heap trigger")
  }
  if (!is.null(runners)) {
    runners <- as.character(runners)
    coverage <- split(as.character(samples$runner), samples$task)
    if (any(vapply(coverage, function(value) !setequal(unique(value), runners), logical(1)))) {
      stop("direct timing runner coverage is incomplete")
    }
  }
  if (!is.null(tasks)) {
    tasks <- as.character(tasks)
    if (!setequal(unique(samples$task), tasks)) stop("direct timing task coverage is incomplete")
  }
  if (!is.null(measurement_samples)) {
    measurement_samples <- input_scalar_integer(measurement_samples, "measurement samples")
    groups <- split(samples$measurement_sample, paste(samples$runner, samples$task, sep = "\r"))
    expected <- seq_len(measurement_samples)
    if (any(vapply(groups, function(value) !identical(sort(as.integer(value)), expected), logical(1)))) {
      stop("direct timing measurement-sample coverage is incomplete")
    }
    if (!is.null(runners) && !is.null(tasks)) {
      expected_groups <- as.vector(outer(runners, tasks, paste, sep = "\r"))
      if (!setequal(names(groups), expected_groups)) {
        stop("direct timing runner-task coverage is incomplete")
      }
      expected_order <- unlist(lapply(runners, function(runner) {
        unlist(lapply(tasks, function(task) {
          paste(runner, task, seq_len(measurement_samples), sep = "\r")
        }), use.names = FALSE)
      }), use.names = FALSE)
      observed_order <- paste(samples$runner, samples$task, samples$measurement_sample, sep = "\r")
      if (!identical(observed_order, expected_order)) {
        stop("direct timing raw sample order is invalid")
      }
    }
  }
  invisible(samples)
}

direct_distribution_policy <- function() {
  list(
    policy_version = "ordered-distribution-v2",
    timer_floor_method = "max-noop-p99-v1",
    quantile_type = 7L,
    measurement_samples = 11L,
    max_over_median_limit = 20,
    p99_over_median_limit = 5,
    cv_pct_limit = 50,
    regime_median_ratio_limit = 3,
    regime_detector = "ordered-split-and-separated-modes-v1",
    gc_explanation_rule = "remove-only-measured-gc-samples-then-recheck-v1"
  )
}

validate_direct_distribution_policy <- function(policy) {
  fields <- c(
    "policy_version", "timer_floor_method", "quantile_type", "measurement_samples",
    "max_over_median_limit", "p99_over_median_limit", "cv_pct_limit",
    "regime_median_ratio_limit", "regime_detector", "gc_explanation_rule"
  )
  if (!is.list(policy) || !identical(names(policy), fields) ||
      !identical(as.character(policy$policy_version), "ordered-distribution-v2") ||
      !identical(as.character(policy$timer_floor_method), "max-noop-p99-v1") ||
      !identical(as.character(policy$regime_detector), "ordered-split-and-separated-modes-v1") ||
      !identical(as.character(policy$gc_explanation_rule), "remove-only-measured-gc-samples-then-recheck-v1")) {
    stop("direct distribution policy is invalid")
  }
  integers <- c(quantile_type = 7L, measurement_samples = 11L)
  for (field in names(integers)) {
    if (!identical(input_scalar_integer(policy[[field]], field), integers[[field]])) {
      stop("direct distribution policy has invalid fixed values")
    }
  }
  limits <- unlist(policy[c(
    "max_over_median_limit", "p99_over_median_limit", "cv_pct_limit", "regime_median_ratio_limit"
  )], use.names = FALSE)
  if (length(limits) != 4L || any(!is.finite(limits)) || any(limits <= 0)) {
    stop("direct distribution policy has invalid limits")
  }
  list(
    policy_version = "ordered-distribution-v2",
    timer_floor_method = "max-noop-p99-v1",
    quantile_type = 7L,
    measurement_samples = 11L,
    max_over_median_limit = as.numeric(limits[[1L]]),
    p99_over_median_limit = as.numeric(limits[[2L]]),
    cv_pct_limit = as.numeric(limits[[3L]]),
    regime_median_ratio_limit = as.numeric(limits[[4L]]),
    regime_detector = "ordered-split-and-separated-modes-v1",
    gc_explanation_rule = "remove-only-measured-gc-samples-then-recheck-v1"
  )
}

direct_distribution_policy_digest <- function(policy = direct_distribution_policy()) {
  serialized_md5(validate_direct_distribution_policy(policy))
}

direct_comparison_policy <- function() {
  list(
    policy_version = "fixed-equivalence-v1",
    evidence_unit = "source-identical-opposite-order-worker-pair-v1",
    interval_method = "paired-student-t-log-ratio-v1",
    confidence_level = 0.90,
    required_order_pairs = 4L,
    equivalence_margin_pct = 2,
    decision_rule = "fixed-margin-equivalence-with-order-check-v1"
  )
}

direct_paired_comparison_policy_v1 <- function() {
  list(
    policy_version = "alternating-paired-equivalence-v1",
    evidence_unit = "fresh-process-single-task-alternating-runner-pair-v1",
    interval_method = "worker-mean-paired-student-t-log-ratio-v1",
    confidence_level = 0.90,
    required_workers = 4L,
    measurement_rounds = 96L,
    extended_measurement_rounds = 192L,
    extended_round_tasks = c(
      "schema", "altrep_sum", "altrep_index", "altrep_materialize", "serialize", "rng",
      "outputs"
    ),
    repeat_batch_ladder_steps = 1L,
    equivalence_margin_pct = 2,
    decision_rule = "fixed-margin-equivalence-with-balanced-position-v1"
  )
}

validate_direct_paired_comparison_policy_v1 <- function(policy) {
  expected <- direct_paired_comparison_policy_v1()
  if (!is.list(policy) || !identical(names(policy), names(expected)) ||
      !identical(as.character(policy$policy_version), expected$policy_version) ||
      !identical(as.character(policy$evidence_unit), expected$evidence_unit) ||
      !identical(as.character(policy$interval_method), expected$interval_method) ||
      !identical(as.character(policy$decision_rule), expected$decision_rule)) {
    stop("legacy direct paired comparison policy is invalid")
  }
  numeric_fields <- c(
    "confidence_level", "required_workers", "measurement_rounds",
    "extended_measurement_rounds", "repeat_batch_ladder_steps",
    "equivalence_margin_pct"
  )
  if (any(vapply(numeric_fields, function(field) {
    !identical(as.numeric(policy[[field]]), as.numeric(expected[[field]]))
  }, logical(1))) ||
      !identical(as.character(policy$extended_round_tasks), expected$extended_round_tasks)) {
    stop("legacy direct paired comparison policy has invalid limits")
  }
  invisible(policy)
}

direct_paired_comparison_policy <- function() {
  list(
    policy_version = "alternating-paired-equivalence-v2",
    evidence_unit = "fresh-process-single-task-four-round-balanced-pair-v2",
    interval_method = "worker-mean-paired-student-t-log-ratio-v2",
    confidence_level = 0.90,
    required_workers = 4L,
    measurement_rounds = 96L,
    extended_measurement_rounds = 192L,
    extended_round_tasks = c(
      "schema", "altrep_sum", "altrep_index", "altrep_materialize", "serialize", "rng",
      "outputs"
    ),
    repeat_batch_ladder_steps = 1L,
    equivalence_margin_pct = 2,
    decision_rule = "fixed-margin-equivalence-with-four-round-balance-v2"
  )
}

validate_direct_paired_comparison_policy <- function(policy) {
  fields <- c(
    "policy_version", "evidence_unit", "interval_method", "confidence_level",
    "required_workers", "measurement_rounds", "extended_measurement_rounds",
    "extended_round_tasks", "repeat_batch_ladder_steps",
    "equivalence_margin_pct", "decision_rule"
  )
  if (!is.list(policy) || !identical(names(policy), fields) ||
      !identical(as.character(policy$policy_version),
                 "alternating-paired-equivalence-v2") ||
      !identical(as.character(policy$evidence_unit),
                 "fresh-process-single-task-four-round-balanced-pair-v2") ||
      !identical(as.character(policy$interval_method),
                 "worker-mean-paired-student-t-log-ratio-v2") ||
      !identical(as.character(policy$decision_rule),
                 "fixed-margin-equivalence-with-four-round-balance-v2")) {
    stop("direct paired comparison policy is invalid")
  }
  confidence_level <- as.numeric(policy$confidence_level)
  required_workers <- input_scalar_integer(
    policy$required_workers, "required paired workers"
  )
  measurement_rounds <- input_scalar_integer(
    policy$measurement_rounds, "paired measurement rounds"
  )
  extended_measurement_rounds <- input_scalar_integer(
    policy$extended_measurement_rounds, "extended paired measurement rounds"
  )
  extended_round_tasks <- as.character(policy$extended_round_tasks)
  repeat_batch_ladder_steps <- input_scalar_integer(
    policy$repeat_batch_ladder_steps, "paired batch ladder steps"
  )
  equivalence_margin_pct <- as.numeric(policy$equivalence_margin_pct)
  if (length(confidence_level) != 1L || !is.finite(confidence_level) ||
      !identical(confidence_level, 0.90) || required_workers != 4L ||
      measurement_rounds != 96L || measurement_rounds %% 4L != 0L ||
      extended_measurement_rounds != 192L ||
      extended_measurement_rounds %% 4L != 0L ||
      !identical(extended_round_tasks, c(
        "schema", "altrep_sum", "altrep_index", "altrep_materialize", "serialize", "rng",
        "outputs"
      )) || repeat_batch_ladder_steps != 1L ||
      length(equivalence_margin_pct) != 1L || !is.finite(equivalence_margin_pct) ||
      !identical(equivalence_margin_pct, 2)) {
    stop("direct paired comparison policy has invalid limits")
  }
  list(
    confidence_level = confidence_level,
    required_workers = required_workers,
    measurement_rounds = measurement_rounds,
    extended_measurement_rounds = extended_measurement_rounds,
    extended_round_tasks = extended_round_tasks,
    repeat_batch_ladder_steps = repeat_batch_ladder_steps,
    equivalence_margin_pct = equivalence_margin_pct
  )
}

direct_paired_measurement_rounds <- function(
    task, policy = direct_paired_comparison_policy()) {
  limits <- validate_direct_paired_comparison_policy(policy)
  task <- as.character(task)
  if (length(task) != 1L || is.na(task) || !nzchar(task)) {
    stop("paired task identity is invalid")
  }
  if (task %in% limits$extended_round_tasks) {
    limits$extended_measurement_rounds
  } else {
    limits$measurement_rounds
  }
}

direct_paired_batch_repetitions <- function(
    task, base_repetitions, policy = direct_paired_comparison_policy(),
    sizing_policy = direct_sizing_policy()) {
  limits <- validate_direct_paired_comparison_policy(policy)
  sizing <- validate_direct_sizing_policy(sizing_policy)
  base_repetitions <- input_scalar_integer(
    base_repetitions, "base paired batch repetitions"
  )
  position <- match(base_repetitions, sizing$ladder)
  if (is.na(position)) stop("base paired batch repetitions are outside the sizing ladder")
  if (identical(direct_task_batchability(task), "one")) return(base_repetitions)
  target <- min(position + limits$repeat_batch_ladder_steps, length(sizing$ladder))
  sizing$ladder[[target]]
}

validate_direct_comparison_policy <- function(policy) {
  fields <- c(
    "policy_version", "evidence_unit", "interval_method", "confidence_level",
    "required_order_pairs", "equivalence_margin_pct", "decision_rule"
  )
  if (!is.list(policy) || !identical(names(policy), fields) ||
      !identical(as.character(policy$policy_version), "fixed-equivalence-v1") ||
      !identical(as.character(policy$evidence_unit),
                 "source-identical-opposite-order-worker-pair-v1") ||
      !identical(as.character(policy$interval_method), "paired-student-t-log-ratio-v1") ||
      !identical(as.character(policy$decision_rule),
                 "fixed-margin-equivalence-with-order-check-v1")) {
    stop("direct comparison policy is invalid")
  }
  confidence_level <- as.numeric(policy$confidence_level)
  required_order_pairs <- input_scalar_integer(
    policy$required_order_pairs, "required comparison order pairs"
  )
  equivalence_margin_pct <- as.numeric(policy$equivalence_margin_pct)
  if (length(confidence_level) != 1L || !is.finite(confidence_level) ||
      !identical(confidence_level, 0.90) || required_order_pairs != 4L ||
      length(equivalence_margin_pct) != 1L || !is.finite(equivalence_margin_pct) ||
      !identical(equivalence_margin_pct, 2)) {
    stop("direct comparison policy has invalid limits")
  }
  list(
    confidence_level = confidence_level,
    required_order_pairs = required_order_pairs,
    equivalence_margin_pct = equivalence_margin_pct
  )
}

direct_log_ratio_interval <- function(values, confidence_level) {
  center <- mean(values)
  half_width <- if (length(values) > 1L && stats::sd(values) > 0) {
    stats::qt((1 + confidence_level) / 2, df = length(values) - 1L) *
      stats::sd(values) / sqrt(length(values))
  } else {
    0
  }
  c(lower = center - half_width, estimate = center, upper = center + half_width)
}

classify_direct_comparison <- function(candidate_forward_ms, candidate_reverse_ms,
                                       comparator_forward_ms, comparator_reverse_ms,
                                       policy = direct_comparison_policy()) {
  limits <- validate_direct_comparison_policy(policy)
  values <- lapply(list(
    candidate_forward_ms, candidate_reverse_ms, comparator_forward_ms,
    comparator_reverse_ms
  ), as.numeric)
  lengths <- vapply(values, length, integer(1))
  if (length(unique(lengths)) != 1L || lengths[[1L]] != limits$required_order_pairs ||
      any(vapply(values, function(value) {
        anyNA(value) || any(!is.finite(value)) || any(value <= 0)
      }, logical(1)))) {
    stop("direct comparison evidence is invalid")
  }
  candidate_forward_ms <- values[[1L]]
  candidate_reverse_ms <- values[[2L]]
  comparator_forward_ms <- values[[3L]]
  comparator_reverse_ms <- values[[4L]]

  candidate_order_interval <- direct_log_ratio_interval(
    log(candidate_forward_ms / candidate_reverse_ms), limits$confidence_level
  )
  comparator_order_interval <- direct_log_ratio_interval(
    log(comparator_forward_ms / comparator_reverse_ms), limits$confidence_level
  )
  equivalence_limit <- log1p(limits$equivalence_margin_pct / 100)
  comparison_interval <- direct_log_ratio_interval(
    (log(candidate_forward_ms / comparator_forward_ms) +
       log(candidate_reverse_ms / comparator_reverse_ms)) / 2,
    limits$confidence_level
  )
  as_pct <- function(value) 100 * expm1(value)
  result <- list(
    status = "INDETERMINATE",
    reason = "comparison interval crosses the fixed equivalence boundary",
    estimate_pct = unname(as_pct(comparison_interval[["estimate"]])),
    lower_pct = unname(as_pct(comparison_interval[["lower"]])),
    upper_pct = unname(as_pct(comparison_interval[["upper"]])),
    equivalence_margin_pct = limits$equivalence_margin_pct,
    candidate_order_lower_pct = unname(as_pct(candidate_order_interval[["lower"]])),
    candidate_order_upper_pct = unname(as_pct(candidate_order_interval[["upper"]])),
    comparator_order_lower_pct = unname(as_pct(comparator_order_interval[["lower"]])),
    comparator_order_upper_pct = unname(as_pct(comparator_order_interval[["upper"]]))
  )
  interval_within_margin <- function(interval) {
    interval[["lower"]] >= -equivalence_limit && interval[["upper"]] <= equivalence_limit
  }
  if (!interval_within_margin(candidate_order_interval)) {
    result$status <- "BLOCK"
    result$reason <- "candidate order effect is not equivalent within the fixed margin"
  } else if (!interval_within_margin(comparator_order_interval)) {
    result$status <- "BLOCK"
    result$reason <- "comparator order effect is not equivalent within the fixed margin"
  } else if (interval_within_margin(comparison_interval)) {
    result$status <- "TIE"
    result$reason <- "comparison interval is contained by the fixed equivalence margin"
  } else if (comparison_interval[["upper"]] < -equivalence_limit) {
    result$status <- "LEAD"
    result$reason <- "comparison interval is faster than the fixed equivalence margin"
  } else if (comparison_interval[["upper"]] <= equivalence_limit &&
             comparison_interval[["lower"]] < -equivalence_limit) {
    result$status <- "LEAD_OR_TIE"
    result$reason <- "comparison interval contains only a lead or fixed-margin equivalence"
  } else if (comparison_interval[["lower"]] > equivalence_limit) {
    result$status <- "LOSS"
    result$reason <- "comparison interval is slower than the fixed equivalence margin"
  }
  result
}

direct_paired_round_order <- function(worker, round,
                                      policy = direct_paired_comparison_policy()) {
  limits <- validate_direct_paired_comparison_policy(policy)
  worker <- input_scalar_integer(worker, "paired worker")
  round <- input_scalar_integer(round, "paired round")
  if (worker < 1L || worker > limits$required_workers ||
      round < 1L || round > limits$extended_measurement_rounds) {
    stop("paired worker or round is outside the declared policy")
  }
  candidate_first <- (round - 1L) %% 4L %in% c(0L, 3L)
  if (worker %% 2L == 0L) candidate_first <- !candidate_first
  if (candidate_first) {
    c("candidate", "comparator")
  } else {
    c("comparator", "candidate")
  }
}

validate_direct_paired_worker_samples <- function(
    samples, worker, candidate_runner, comparator_runner, task,
    policy = direct_paired_comparison_policy()) {
  limits <- validate_direct_paired_comparison_policy(policy)
  measurement_rounds <- direct_paired_measurement_rounds(task, policy)
  required <- c(
    "worker", "task", "round", "position", "order", "side", "runner",
    "phase", "batch_repetitions", "batch_elapsed_ms", "elapsed_per_event_ms",
    "gc_elapsed_ms", "vector_heap_trigger_vcells"
  )
  if (!is.data.frame(samples) || !identical(names(samples), required) ||
      nrow(samples) == 0L || anyNA(samples)) {
    stop("direct paired samples have the wrong shape")
  }
  candidate_runner <- as.character(candidate_runner)
  comparator_runner <- as.character(comparator_runner)
  task <- as.character(task)
  worker <- input_scalar_integer(worker, "paired worker")
  if (worker < 1L || worker > limits$required_workers ||
      length(candidate_runner) != 1L || !nzchar(candidate_runner) ||
      length(comparator_runner) != 1L || !nzchar(comparator_runner) ||
      length(task) != 1L || !nzchar(task)) {
    stop("direct paired sample identities are invalid")
  }
  numeric_columns <- c(
    "worker", "round", "position", "batch_repetitions", "batch_elapsed_ms",
    "elapsed_per_event_ms", "gc_elapsed_ms", "vector_heap_trigger_vcells"
  )
  if (any(!vapply(samples[numeric_columns], is.numeric, logical(1))) ||
      any(!is.finite(as.matrix(samples[numeric_columns]))) ||
      any(samples$batch_repetitions < 1L) ||
      any(samples$batch_repetitions != as.integer(samples$batch_repetitions)) ||
      any(samples$batch_elapsed_ms <= 0) || any(samples$elapsed_per_event_ms <= 0) ||
      any(samples$gc_elapsed_ms < 0) ||
      any(samples$vector_heap_trigger_vcells <= 0) ||
      !isTRUE(all.equal(
        samples$elapsed_per_event_ms,
        samples$batch_elapsed_ms / samples$batch_repetitions,
        tolerance = 1e-9,
        check.attributes = FALSE
      ))) {
    stop("direct paired samples contain invalid measurements")
  }
  expected_rows <- measurement_rounds * 2L
  if (nrow(samples) != expected_rows ||
      any(samples$worker != worker) ||
      !identical(unique(as.character(samples$task)), task) ||
      any(as.character(samples$phase) != "measurement") ||
      length(unique(samples$batch_repetitions)) != 1L) {
    stop("direct paired sample coverage is incomplete")
  }
  expected <- do.call(rbind, lapply(seq_len(measurement_rounds), function(round) {
    sides <- direct_paired_round_order(worker, round, policy)
    data.frame(
      worker = worker,
      round = round,
      position = seq_len(2L),
      order = if (identical(sides[[1L]], "candidate")) {
        "candidate_first"
      } else {
        "comparator_first"
      },
      side = sides,
      runner = c(candidate_runner, comparator_runner)[match(
        sides, c("candidate", "comparator")
      )],
      stringsAsFactors = FALSE
    )
  }))
  observed <- samples[c("worker", "round", "position", "order", "side", "runner")]
  rownames(observed) <- NULL
  rownames(expected) <- NULL
  if (!identical(observed, expected)) {
    stop("direct paired raw sample order is invalid")
  }
  invisible(samples)
}

validate_direct_paired_samples <- function(
    samples, candidate_runner, comparator_runner, task,
    policy = direct_paired_comparison_policy()) {
  limits <- validate_direct_paired_comparison_policy(policy)
  measurement_rounds <- direct_paired_measurement_rounds(task, policy)
  expected_rows <- limits$required_workers * measurement_rounds * 2L
  if (!is.data.frame(samples) || nrow(samples) != expected_rows ||
      !identical(unique(samples$worker), seq_len(limits$required_workers)) ||
      length(unique(samples$batch_repetitions)) != 1L) {
    stop("direct paired sample coverage is incomplete")
  }
  for (worker in seq_len(limits$required_workers)) {
    validate_direct_paired_worker_samples(
      samples[samples$worker == worker, , drop = FALSE], worker,
      candidate_runner, comparator_runner, task, policy
    )
  }
  invisible(samples)
}

classify_direct_paired_comparison <- function(
    samples, timer_floors_ms, candidate_runner, comparator_runner, task,
    policy = direct_paired_comparison_policy(),
    distribution_policy = direct_distribution_policy()) {
  limits <- validate_direct_paired_comparison_policy(policy)
  measurement_rounds <- direct_paired_measurement_rounds(task, policy)
  validate_direct_distribution_policy(distribution_policy)
  validate_direct_paired_samples(
    samples, candidate_runner, comparator_runner, task, policy
  )
  timer_floors_ms <- as.numeric(timer_floors_ms)
  if (length(timer_floors_ms) != limits$required_workers ||
      anyNA(timer_floors_ms) || any(!is.finite(timer_floors_ms)) ||
      any(timer_floors_ms < 0)) {
    stop("direct paired timer-floor evidence is invalid")
  }

  distributions <- do.call(rbind, lapply(seq_len(limits$required_workers), function(worker) {
    do.call(rbind, lapply(c("candidate", "comparator"), function(side) {
      rows <- samples[samples$worker == worker & samples$side == side, , drop = FALSE]
      classification <- classify_direct_distribution(
        rows$elapsed_per_event_ms,
        rows$batch_elapsed_ms,
        rows$gc_elapsed_ms,
        timer_floors_ms[[worker]],
        distribution_policy
      )
      data.frame(
        worker = worker,
        side = side,
        status = classification$status,
        reason = classification$reason,
        stringsAsFactors = FALSE
      )
    }))
  }))

  worker_effects <- do.call(rbind, lapply(seq_len(limits$required_workers), function(worker) {
    rows <- samples[samples$worker == worker, , drop = FALSE]
    ratios <- vapply(seq_len(measurement_rounds), function(round) {
      pair <- rows[rows$round == round, , drop = FALSE]
      candidate <- pair$elapsed_per_event_ms[pair$side == "candidate"]
      comparator <- pair$elapsed_per_event_ms[pair$side == "comparator"]
      log(candidate[[1L]] / comparator[[1L]])
    }, numeric(1))
    candidate_first <- rows$order[match(
      seq_len(measurement_rounds), rows$round
    )] == "candidate_first"
    data.frame(
      worker = worker,
      comparison_log_ratio = mean(ratios),
      position_log_ratio = (
        mean(ratios[candidate_first]) - mean(ratios[!candidate_first])
      ) / 2,
      stringsAsFactors = FALSE
    )
  }))
  comparison_interval <- direct_log_ratio_interval(
    worker_effects$comparison_log_ratio, limits$confidence_level
  )
  position_interval <- direct_log_ratio_interval(
    worker_effects$position_log_ratio, limits$confidence_level
  )
  equivalence_limit <- log1p(limits$equivalence_margin_pct / 100)
  interval_within_margin <- function(interval) {
    interval[["lower"]] >= -equivalence_limit &&
      interval[["upper"]] <= equivalence_limit
  }
  as_pct <- function(value) 100 * expm1(value)
  result <- list(
    status = "INDETERMINATE",
    reason = "comparison interval crosses the fixed equivalence boundary",
    estimate_pct = unname(as_pct(comparison_interval[["estimate"]])),
    lower_pct = unname(as_pct(comparison_interval[["lower"]])),
    upper_pct = unname(as_pct(comparison_interval[["upper"]])),
    equivalence_margin_pct = limits$equivalence_margin_pct,
    position_lower_pct = unname(as_pct(position_interval[["lower"]])),
    position_upper_pct = unname(as_pct(position_interval[["upper"]])),
    distributions = distributions,
    worker_effects = worker_effects
  )
  if (interval_within_margin(comparison_interval)) {
    result$status <- "TIE"
    result$reason <- "paired comparison interval is contained by the fixed equivalence margin"
  } else if (comparison_interval[["upper"]] < -equivalence_limit) {
    result$status <- "LEAD"
    result$reason <- "paired comparison interval is faster than the fixed equivalence margin"
  } else if (comparison_interval[["upper"]] <= equivalence_limit &&
             comparison_interval[["lower"]] < -equivalence_limit) {
    result$status <- "LEAD_OR_TIE"
    result$reason <- "paired comparison interval contains only a lead or equivalence"
  } else if (comparison_interval[["lower"]] > equivalence_limit) {
    result$status <- "LOSS"
    result$reason <- "paired comparison interval is slower than the fixed equivalence margin"
  }
  result
}

direct_distribution_metrics <- function(values, policy = direct_distribution_policy()) {
  policy <- validate_direct_distribution_policy(policy)
  values <- as.numeric(values)
  if (length(values) < 1L || anyNA(values) || any(!is.finite(values)) || any(values < 0)) {
    stop("distribution values are invalid")
  }
  median_ms <- stats::median(values)
  mean_ms <- mean(values)
  ordered_regime_ratio <- NA_real_
  if (length(values) >= 6L) {
    for (split in 3L:(length(values) - 3L)) {
      left_values <- values[seq_len(split)]
      right_values <- values[(split + 1L):length(values)]
      if (!(max(left_values) < min(right_values) || max(right_values) < min(left_values))) next
      left <- stats::median(left_values)
      right <- stats::median(right_values)
      ratio <- if (min(left, right) > 0) max(left, right) / min(left, right) else Inf
      ordered_regime_ratio <- max(ordered_regime_ratio, ratio, na.rm = TRUE)
    }
  }
  separated_mode_ratio <- NA_real_
  alternating_switches <- 0L
  if (length(values) >= 6L) {
    sorted <- sort(values)
    for (split in 3L:(length(values) - 3L)) {
      ratio <- if (sorted[[split]] > 0) sorted[[split + 1L]] / sorted[[split]] else Inf
      if (is.na(separated_mode_ratio) || ratio > separated_mode_ratio) {
        separated_mode_ratio <- ratio
        threshold <- (sorted[[split]] + sorted[[split + 1L]]) / 2
        labels <- values > threshold
        alternating_switches <- sum(labels[-1L] != labels[-length(labels)])
      }
    }
  }
  alternating_regime <- is.finite(separated_mode_ratio) &&
    separated_mode_ratio > policy$regime_median_ratio_limit &&
    alternating_switches >= floor(length(values) / 2)
  if (!is.finite(separated_mode_ratio) || separated_mode_ratio <= policy$regime_median_ratio_limit) {
    alternating_switches <- 0L
  }
  q1 <- unname(stats::quantile(values, 0.25, names = FALSE, type = policy$quantile_type))
  q3 <- unname(stats::quantile(values, 0.75, names = FALSE, type = policy$quantile_type))
  p95 <- unname(stats::quantile(values, 0.95, names = FALSE, type = policy$quantile_type))
  p99 <- unname(stats::quantile(values, 0.99, names = FALSE, type = policy$quantile_type))
  denominator <- if (median_ms > 0) median_ms else NA_real_
  list(
    median_ms = median_ms,
    mean_ms = mean_ms,
    q1_ms = q1, q3_ms = q3, p95_ms = p95, p99_ms = p99,
    min_ms = min(values),
    max_ms = max(values),
    sd_ms = if (length(values) > 1L) stats::sd(values) else 0,
    cv_pct = if (mean_ms > 0 && length(values) > 1L) stats::sd(values) / mean_ms * 100 else NA_real_,
    max_over_median = max(values) / denominator,
    p99_over_median = p99 / denominator,
    ordered_regime_ratio = ordered_regime_ratio,
    separated_mode_ratio = separated_mode_ratio,
    alternating_switches = as.integer(alternating_switches),
    alternating_regime = alternating_regime,
    regime_change = (is.finite(ordered_regime_ratio) && ordered_regime_ratio > policy$regime_median_ratio_limit) ||
      (is.finite(separated_mode_ratio) && separated_mode_ratio > policy$regime_median_ratio_limit)
  )
}

direct_distribution_triggers <- function(metrics, policy = direct_distribution_policy()) {
  policy <- validate_direct_distribution_policy(policy)
  if (!is.finite(metrics$median_ms) || metrics$median_ms <= 0 ||
      any(!is.finite(unlist(metrics[c("max_over_median", "p99_over_median", "cv_pct")])))) {
    return("per-event distribution has an undefined ratio because its median or mean is zero")
  }
  if (is.infinite(metrics$ordered_regime_ratio) || is.infinite(metrics$separated_mode_ratio)) {
    return("distribution detector has an undefined separation ratio")
  }
  triggers <- character()
  add <- function(condition, text) if (isTRUE(condition)) triggers <<- c(triggers, text)
  add(metrics$max_over_median > policy$max_over_median_limit,
      sprintf("max/median %.6g exceeds %.6g", metrics$max_over_median, policy$max_over_median_limit))
  add(metrics$p99_over_median > policy$p99_over_median_limit,
      sprintf("p99/median %.6g exceeds %.6g", metrics$p99_over_median, policy$p99_over_median_limit))
  add(metrics$cv_pct > policy$cv_pct_limit,
      sprintf("CV %.6g%% exceeds %.6g%%", metrics$cv_pct, policy$cv_pct_limit))
  add(is.finite(metrics$ordered_regime_ratio) && metrics$ordered_regime_ratio > policy$regime_median_ratio_limit,
      sprintf("ordered split median ratio %.6g exceeds %.6g", metrics$ordered_regime_ratio, policy$regime_median_ratio_limit))
  add(is.finite(metrics$separated_mode_ratio) && metrics$separated_mode_ratio > policy$regime_median_ratio_limit,
      sprintf("separated-mode ratio %.6g exceeds %.6g", metrics$separated_mode_ratio, policy$regime_median_ratio_limit))
  add(isTRUE(metrics$alternating_regime),
      sprintf("alternating mode sequence has %d switches", metrics$alternating_switches))
  triggers
}

classify_direct_distribution <- function(values, batch_elapsed_ms, gc_elapsed_ms, timer_floor_ms,
                                         policy = direct_distribution_policy()) {
  policy <- validate_direct_distribution_policy(policy)
  metrics <- direct_distribution_metrics(values, policy)
  batch_metrics <- direct_distribution_metrics(batch_elapsed_ms, policy)
  triggers <- direct_distribution_triggers(metrics, policy)
  if (length(triggers) == 1L && startsWith(triggers, "per-event distribution has an undefined ratio")) {
    return(list(status = "BLOCK", reason = triggers, metrics = metrics))
  }
  if (batch_metrics$median_ms <= timer_floor_ms) {
    return(list(status = "BLOCK", reason = "median is at or below the measured timer floor", metrics = metrics))
  }
  if (length(triggers) == 0L) {
    return(list(status = "PASS", reason = "ordered samples pass the distribution gates", metrics = metrics))
  }
  gc_samples <- which(as.numeric(gc_elapsed_ms) > 0)
  if (length(gc_samples) > 0L && length(gc_samples) <= length(values) - 3L) {
    without_gc <- direct_distribution_metrics(values[-gc_samples], policy)
    residual_triggers <- direct_distribution_triggers(without_gc, policy)
    if (length(residual_triggers) == 0L) {
      return(list(
        status = "PASS_GC", reason = paste("measured R GC explains", paste(triggers, collapse = "; ")),
        metrics = metrics
      ))
    }
    triggers <- c(
      triggers,
      paste("measured R GC did not explain", paste(residual_triggers, collapse = "; "))
    )
  }
  list(status = "BLOCK", reason = paste(triggers, collapse = "; "), metrics = metrics)
}

classify_direct_allocation_gc <- function(task, batch_repetitions, measurement_samples,
                                          vector_heap_trigger_vcells, gc_elapsed_ms,
                                          policy = direct_allocation_policy()) {
  policy <- validate_direct_allocation_policy(policy)
  output_vcells <- direct_task_output_vcells(task, policy)
  allocation_class <- direct_task_allocation_class(task, policy)
  fixed_sequence_output_vcells <- as.double(output_vcells) * batch_repetitions * measurement_samples
  if (allocation_class != "large_output" || fixed_sequence_output_vcells < vector_heap_trigger_vcells) {
    return(list(
      allocation_class = allocation_class,
      fixed_sequence_output_vcells = fixed_sequence_output_vcells,
      vector_heap_trigger_vcells = vector_heap_trigger_vcells,
      status = "NOT_REQUIRED", reason = "GC observation is not required for this fixed sequence"
    ))
  }
  if (any(gc_elapsed_ms > 0)) {
    return(list(
      allocation_class = allocation_class,
      fixed_sequence_output_vcells = fixed_sequence_output_vcells,
      vector_heap_trigger_vcells = vector_heap_trigger_vcells,
      status = "GC_OBSERVED", reason = "measured R GC occurred during the fixed sequence"
    ))
  }
  if (identical(policy$gc_requirement, "r-owned-output-gc-is-diagnostic-only-v1")) {
    return(list(
      allocation_class = allocation_class,
      fixed_sequence_output_vcells = fixed_sequence_output_vcells,
      vector_heap_trigger_vcells = vector_heap_trigger_vcells,
      status = "NOT_REQUIRED",
      reason = "R-owned output GC remains diagnostic; resource evidence owns allocation safety"
    ))
  }
  list(
    allocation_class = allocation_class,
    fixed_sequence_output_vcells = fixed_sequence_output_vcells,
    vector_heap_trigger_vcells = vector_heap_trigger_vcells,
    status = "GC_NOT_OBSERVED",
    reason = sprintf(
      "GC not observed although %.0f declared output Vcells across the fixed sequence reaches the %.0f Vcell trigger",
      fixed_sequence_output_vcells, vector_heap_trigger_vcells
    )
  )
}

summarize_direct_timing <- function(samples, first_calls, timer_floors = NULL,
                                    distribution_policy = direct_distribution_policy(),
                                    allocation_policy = direct_allocation_policy()) {
  distribution_policy <- validate_direct_distribution_policy(distribution_policy)
  allocation_policy <- validate_direct_allocation_policy(allocation_policy)
  validate_direct_timing_samples(samples)
  first_required <- c("runner", "task", "first_call_ms")
  if (!is.data.frame(first_calls) || !identical(names(first_calls), first_required) ||
      anyNA(first_calls) || !is.numeric(first_calls$first_call_ms) ||
      any(!is.finite(first_calls$first_call_ms)) || any(first_calls$first_call_ms < 0) ||
      any(!nzchar(first_calls$runner)) || any(!nzchar(first_calls$task))) {
    stop("direct first-call rows are invalid")
  }
  first_keys <- paste(first_calls$runner, first_calls$task, sep = "\r")
  if (anyDuplicated(first_keys)) stop("direct first-call rows contain duplicate identities")
  groups <- split(seq_len(nrow(samples)), paste(samples$runner, samples$task, sep = "\r"))
  if (!setequal(first_keys, names(groups))) stop("direct first-call coverage differs from timing samples")
  rows <- lapply(groups, function(index) {
    values <- samples$elapsed_per_event_ms[index]
    key <- paste(samples$runner[index[[1L]]], samples$task[index[[1L]]], sep = "\r")
    first_index <- match(key, first_keys)
    if (is.na(first_index)) stop("direct timing summary is missing first-call evidence")
    timer_floor <- if (is.null(timer_floors)) 0 else as.numeric(timer_floors[[samples$runner[index[[1L]]]]])
    if (length(timer_floor) != 1L || !is.finite(timer_floor) || timer_floor < 0) {
      stop("direct timing summary is missing a valid runner timer floor")
    }
    classification <- classify_direct_distribution(
      values, samples$batch_elapsed_ms[index], samples$gc_elapsed_ms[index], timer_floor,
      distribution_policy
    )
    allocation <- classify_direct_allocation_gc(
      samples$task[index[[1L]]], samples$batch_repetitions[index[[1L]]], length(values),
      samples$vector_heap_trigger_vcells[index[[1L]]], samples$gc_elapsed_ms[index], allocation_policy
    )
    if (identical(allocation$status, "GC_NOT_OBSERVED")) {
      classification$status <- "BLOCK"
      classification$reason <- paste(classification$reason, allocation$reason, sep = "; ")
    }
    metrics <- classification$metrics
    data.frame(
      runner = samples$runner[index[[1L]]], task = samples$task[index[[1L]]],
      distribution_policy_digest = direct_distribution_policy_digest(distribution_policy),
      measurement_samples = length(values),
      batch_repetitions = samples$batch_repetitions[index[[1L]]],
      median_ms = metrics$median_ms, mean_ms = metrics$mean_ms,
      q1_ms = metrics$q1_ms, q3_ms = metrics$q3_ms, p95_ms = metrics$p95_ms,
      p99_ms = metrics$p99_ms, min_ms = metrics$min_ms, max_ms = metrics$max_ms,
      sd_ms = metrics$sd_ms, cv_pct = metrics$cv_pct,
      max_over_median = metrics$max_over_median,
      p99_over_median = metrics$p99_over_median,
      allocation_class = allocation$allocation_class,
      fixed_sequence_output_vcells = allocation$fixed_sequence_output_vcells,
      vector_heap_trigger_vcells = allocation$vector_heap_trigger_vcells,
      allocation_gc_status = allocation$status,
      regime_change = is.finite(metrics$ordered_regime_ratio) &&
        metrics$ordered_regime_ratio > distribution_policy$regime_median_ratio_limit,
      separated_modes = is.finite(metrics$separated_mode_ratio) &&
        metrics$separated_mode_ratio > distribution_policy$regime_median_ratio_limit,
      alternating_switches = metrics$alternating_switches,
      alternating_regime = metrics$alternating_regime,
      timer_floor_ms = timer_floor,
      distribution_status = classification$status,
      distribution_reason = classification$reason,
      first_call_ms = first_calls$first_call_ms[first_index],
      stringsAsFactors = FALSE
    )
  })
  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  result[order(result$task, result$runner), , drop = FALSE]
}

measurement_probe_names <- function() c("noop_native", "noop_r", "cpu", "allocate")

measurement_probe_timer_floor <- function(samples) {
  policy <- validate_direct_distribution_policy(direct_distribution_policy())
  values <- lapply(c("noop_native", "noop_r"), function(name) {
    samples$elapsed_per_event_ms[samples$probe == name]
  })
  noops <- unlist(values, use.names = FALSE)
  if (any(lengths(values) == 0L) || anyNA(noops) ||
      any(!is.finite(noops)) || any(noops < 0)) {
    stop("no-op probe samples are invalid")
  }
  max(vapply(values, stats::quantile, numeric(1), probs = 0.99, names = FALSE,
             type = policy$quantile_type))
}

measurement_unit_minimum_ms <- function() 10

measurement_unit_tolerance_ms <- function(independent_elapsed_ms) {
  max(2, as.numeric(independent_elapsed_ms) * 0.20)
}

run_direct_measurement_probes <- function(c_dll, samples = 101L) {
  samples <- input_scalar_integer(samples, "probe samples")
  symbol <- function(name) getNativeSymbolInfo(name, c_dll)
  same_sexp <- function(left, right) {
    isTRUE(eval(direct_native_call(
      ".Call", symbol("c_benchmark_same_sexp"), list(left, right)
    ), envir = environment()))
  }
  cpu_input <- seq(0, 1, length.out = 200000L)
  allocating_expected <- as.double(seq.int(0L, 32767L))
  calls <- list(
    noop_native = direct_native_call(".Call", symbol("c_measurement_probe_noop")),
    noop_r = direct_function_call(compiler::cmpfun(function() NULL)),
    cpu = direct_native_call(
      ".Call", symbol("c_measurement_probe_cpu"), list(cpu_input)
    ),
    allocate = direct_native_call(
      ".Call", symbol("c_measurement_probe_allocate"), list(32768L)
    )
  )
  gc(full = TRUE)
  gc.time(TRUE)
  rows <- list()
  for (name in names(calls)) {
    for (sample in seq_len(samples)) {
      measured <- measure_direct_batch(calls[[name]], environment(), 1L)
      valid <- switch(name,
        noop_native = is.null(measured$result),
        noop_r = is.null(measured$result),
        cpu = same_sexp(cpu_input, measured$result),
        allocate = identical(measured$result, allocating_expected)
      )
      if (!isTRUE(valid)) stop(sprintf("%s measurement probe result was not evaluated", name))
      rows[[length(rows) + 1L]] <- data.frame(
        probe = name, probe_sample = sample, batch_repetitions = 1L,
        batch_elapsed_ms = measured$batch_elapsed_ms,
        elapsed_per_event_ms = measured$elapsed_per_event_ms,
        gc_elapsed_ms = measured$gc_elapsed_ms,
        stringsAsFactors = FALSE
      )
    }
  }
  rows <- do.call(rbind, rows)
  rownames(rows) <- NULL

  first_allocated <- eval(calls$allocate, envir = environment())
  second_allocated <- eval(calls$allocate, envir = environment())
  if (same_sexp(first_allocated, second_allocated)) {
    stop("allocation probe returned the same R object twice")
  }
  first_allocated[[1L]] <- -1
  if (!identical(second_allocated[[1L]], 0)) {
    stop("allocation probe results alias each other")
  }

  unit_batch <- direct_batch_expression(calls$cpu, 256L)
  proc_started <- proc.time()[["elapsed"]]
  nano_started <- microbenchmark::get_nanotime() / 1e9
  unit_result <- eval(unit_batch, envir = environment())
  nano_finished <- microbenchmark::get_nanotime() / 1e9
  proc_finished <- proc.time()[["elapsed"]]
  nano_ms <- (nano_finished - nano_started) * 1000
  proc_ms <- (proc_finished - proc_started) * 1000
  tolerance_ms <- measurement_unit_tolerance_ms(proc_ms)
  if (!same_sexp(cpu_input, unit_result) ||
      proc_ms < measurement_unit_minimum_ms() ||
      abs(nano_ms - proc_ms) > tolerance_ms) {
    stop("nanotime units disagree with the independent elapsed-time check")
  }
  list(
    samples = rows,
    timer_floor_ms = unname(measurement_probe_timer_floor(rows)),
    nanotime_elapsed_ms = nano_ms,
    independent_elapsed_ms = proc_ms
  )
}
benchmark_timing_policy <- function() {
  tasks <- vapply(benchmark_revision_task_specs(), `[[`, character(1), "id")
  list(
    policy_version = "direct-batch-v15",
    warmup_iterations = 1L,
    local_calibration_batches = 1L,
    measurement_samples = 11L,
    measurement_probe_samples = 101L,
    sizing_policy = direct_sizing_policy(),
    batch_repetitions = as.list(direct_batch_repetition_map(tasks)),
    worker_timeout_seconds = 600L,
    total_run_timeout_seconds = 5400L,
    r_jit_policy = "disabled-before-runner-load",
    distribution_policy = direct_distribution_policy(),
    comparison_policy = direct_paired_comparison_policy(),
    allocation_policy = direct_allocation_policy(),
    gc_policy = paste(
      "full before first call, warmup, calibration, and each large-output measurement sample;",
      "completed task state released before the next task"
    )
  )
}

direct_memory_policy <- function() {
  list(
    policy_version = "linux-proc-status-v1",
    event_repetitions = 1L,
    rss_metric = "VmRSS-and-VmHWM-kB",
    swap_metric = "VmSwap-kB",
    maximum_high_water_growth_kb = 65536L
  )
}

validate_direct_memory_policy <- function(policy = direct_memory_policy()) {
  fields <- c(
    "policy_version", "event_repetitions", "rss_metric", "swap_metric",
    "maximum_high_water_growth_kb"
  )
  if (!is.list(policy) || !identical(names(policy), fields) ||
      !identical(as.character(policy$policy_version), "linux-proc-status-v1") ||
      !identical(input_scalar_integer(policy$event_repetitions, "memory event repetitions"), 1L) ||
      !identical(as.character(policy$rss_metric), "VmRSS-and-VmHWM-kB") ||
      !identical(as.character(policy$swap_metric), "VmSwap-kB") ||
      !identical(input_scalar_integer(
        policy$maximum_high_water_growth_kb, "maximum memory high-water growth"
      ), 65536L)) {
    stop("direct memory policy is invalid")
  }
  policy
}

direct_proc_status_value_kb <- function(lines, field) {
  match <- grep(paste0("^", field, ":[[:space:]]+"), lines, value = TRUE)
  if (length(match) != 1L) return(NA_real_)
  value <- sub(paste0("^", field, ":[[:space:]]+([0-9]+)[[:space:]]+kB$"), "\\1", match)
  if (identical(value, match) || !grepl("^[0-9]+$", value)) return(NA_real_)
  as.numeric(value)
}

direct_process_memory_snapshot <- function(status_path = "/proc/self/status") {
  if (!identical(.Platform$OS.type, "unix") || !file.exists(status_path)) return(NULL)
  lines <- readLines(status_path, warn = FALSE)
  values <- vapply(c("VmRSS", "VmHWM", "VmSwap"), function(field) {
    direct_proc_status_value_kb(lines, field)
  }, numeric(1))
  if (any(!is.finite(values))) return(NULL)
  list(rss_kb = values[["VmRSS"]], hwm_kb = values[["VmHWM"]], swap_kb = values[["VmSwap"]])
}

direct_memory_event_status <- function(baseline, after, policy = direct_memory_policy()) {
  policy <- validate_direct_memory_policy(policy)
  if (is.null(baseline) || is.null(after)) {
    return(list(status = "UNSUPPORTED", reason = "Linux /proc/self/status is unavailable"))
  }
  fields <- c("rss_kb", "hwm_kb", "swap_kb")
  valid_snapshot <- function(snapshot) {
    is.list(snapshot) && identical(names(snapshot), fields) &&
      all(is.finite(unlist(snapshot, use.names = FALSE))) &&
      all(unlist(snapshot, use.names = FALSE) >= 0) && snapshot$hwm_kb >= snapshot$rss_kb
  }
  if (!valid_snapshot(baseline) || !valid_snapshot(after) || after$hwm_kb < baseline$hwm_kb) {
    stop("direct memory snapshots are invalid")
  }
  if (baseline$swap_kb > 0 || after$swap_kb > 0) {
    return(list(status = "BLOCK", reason = "process swap is nonzero during the memory event"))
  }
  high_water_growth <- after$hwm_kb - baseline$hwm_kb
  if (high_water_growth > policy$maximum_high_water_growth_kb) {
    return(list(
      status = "BLOCK",
      reason = sprintf(
        "process high-water RSS growth %.0f kB exceeds the %d kB cap",
        high_water_growth, policy$maximum_high_water_growth_kb
      )
    ))
  }
  list(status = "PASS", reason = "process high-water RSS recorded")
}

direct_memory_summary_schema <- function() c(
  "runner", "task", "memory_status", "rss_metric", "loaded_process_rss_kb",
  "initial_process_high_water_rss_kb", "process_high_water_rss_kb",
  "swap_before_kb", "swap_after_kb", "reason"
)

validate_direct_memory_summary <- function(rows, runners, task) {
  required <- direct_memory_summary_schema()
  if (!is.data.frame(rows) || !identical(names(rows), required) ||
      nrow(rows) != length(runners) || !identical(as.character(rows$runner), runners) ||
      !identical(as.character(rows$task), rep(task, length(runners))) ||
      any(!rows$memory_status %in% c("PASS", "UNSUPPORTED", "BLOCK")) ||
      any(!nzchar(rows$rss_metric)) || any(!nzchar(rows$reason))) {
    stop("direct memory summary is invalid")
  }
  numeric <- c(
    "loaded_process_rss_kb", "initial_process_high_water_rss_kb",
    "process_high_water_rss_kb", "swap_before_kb", "swap_after_kb"
  )
  supported <- rows$memory_status != "UNSUPPORTED"
  if (any(vapply(rows[numeric], function(value) !is.numeric(value), logical(1))) ||
      anyNA(as.matrix(rows[supported, numeric, drop = FALSE])) ||
      any(as.matrix(rows[supported, numeric, drop = FALSE]) < 0) ||
      any(rows$initial_process_high_water_rss_kb[supported] < rows$loaded_process_rss_kb[supported]) ||
      any(rows$process_high_water_rss_kb[supported] < rows$initial_process_high_water_rss_kb[supported]) ||
      any(!is.na(as.matrix(rows[!supported, numeric, drop = FALSE])))) {
    stop("direct memory summary has invalid measurements")
  }
  invisible(rows)
}

write_csv <- function(df, path, append = FALSE) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  write.table(df, path, sep = ",", row.names = FALSE, quote = TRUE, na = "",
              append = append, col.names = !append)
}

write_csv_once <- function(df, path, label = "CSV output") {
  path <- normalizePath(path, mustWork = FALSE)
  if (file.exists(path)) stop(sprintf("%s already exists: %s", label, path))
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  staged <- paste0(path, ".tmp-", Sys.getpid())
  on.exit(unlink(staged), add = TRUE)
  write_csv(df, staged)
  if (!file.rename(staged, path)) stop(sprintf("cannot promote %s: %s", label, path))
  invisible(path)
}

combine_csv_files_once <- function(paths, output, label) {
  paths <- as.character(paths)
  if (length(paths) == 0L || any(!file.exists(paths))) {
    stop(sprintf("cannot combine incomplete %s", label))
  }
  tables <- lapply(paths, read.csv, stringsAsFactors = FALSE)
  columns <- lapply(tables, names)
  if (!all(vapply(columns, identical, logical(1), columns[[1L]]))) {
    stop(sprintf("cannot combine %s with different columns", label))
  }
  write_csv_once(do.call(rbind, tables), output, label)
  unlink(paths)
  invisible(output)
}
