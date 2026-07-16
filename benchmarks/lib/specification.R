# Task catalog, policy, evidence expansion, and report contracts.

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

boundary_budget_policy_version <- function() "2026-07-15-1"

representation_budget_policy <- function() {
  data.frame(
    task = c(
      "76_string_view_one", "77_string_cache_build", "78_string_cache_one",
      "79_string_headers_one", "80_string_view_repeated", "81_string_cache_repeated",
      "82_string_headers_repeated", "83_raw_view", "84_raw_copy",
      "85_complex_view", "86_complex_return"
    ),
    max_median_ms = c(0.50, 1.20, 1.25, 0.70, 1.90, 1.35, 0.75, 0.02, 0.20, 0.05, 0.06),
    stringsAsFactors = FALSE
  )
}

validate_task_manifest <- function(manifest) {
  required <- c("task", "workload_group", "display_name", "category", "input_arity",
                "expected_return", "correctness_policy", "comparison_policy",
                "comparison_note")
  missing <- setdiff(required, names(manifest))
  if (length(missing) > 0L) {
    stop(sprintf("task manifest missing columns: %s", paste(missing, collapse = ", ")))
  }
  unsupported <- setdiff(names(manifest), required)
  if (length(unsupported) > 0L) {
    stop(sprintf("task manifest contains unsupported columns: %s", paste(unsupported, collapse = ", ")))
  }
  if (nrow(manifest) != 83L) stop(sprintf("task manifest must contain 83 rows, got %d", nrow(manifest)))
  if (anyDuplicated(manifest$task)) stop("task manifest contains duplicate task IDs")
  if (any(!nzchar(manifest$task)) || any(!nzchar(manifest$display_name))) stop("task manifest contains blank identity fields")
  if (any(!grepl("^([0-9]{2}_[A-Za-z0-9_]+|07[ab]_[A-Za-z0-9_]+)$", manifest$task))) stop("task manifest contains an invalid task ID")
  if (!is.numeric(manifest$input_arity) || anyNA(manifest$input_arity) || any(manifest$input_arity < 0) || any(manifest$input_arity != as.integer(manifest$input_arity))) stop("task manifest has an invalid input arity")
  groups <- c("core_compute", "api_boundary", "objects_and_strings", "numerical", "altrep", "runtime_services")
  if (!all(manifest$workload_group %in% groups)) stop("task manifest has an invalid workload group")
  if (!all(manifest$category %in% c("numeric_kernel", "api_overhead", "data_structure", "linear_algebra", "altrep", "integration"))) stop("task manifest has an invalid category")
  task_report_category(manifest$category)
  if (!all(manifest$correctness_policy %in% c("r_reference", "native_invariant", "nondeterministic"))) stop("task manifest has an invalid correctness policy")
  if (!all(manifest$comparison_policy %in% c("comparable", "non_comparable"))) stop("task manifest has an invalid comparison policy")
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
  unsupported <- setdiff(unique(unlist(lapply(task_specs, names), use.names = FALSE)), c("id", "args", "call_type"))
  if (length(unsupported) > 0L) {
    stop(sprintf("runner task specs contain unsupported fields: %s", paste(unsupported, collapse = ", ")))
  }
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

benchmark_task_specs <- function() {
  # The parent input-recipe process cannot construct registered native state.
  # The fingerprint normalizes this sentinel and real external pointers to the
  # same declared recipe identity.
  method_receiver <- function() structure("runner_registered_fixture_state", class = "benchmark_external_state_recipe")
  
  string_input <- function() {
    rep(c("zigr", "boundary", NA_character_, ""), length.out = 32768L)
  }
  
  raw_input <- function() {
    as.raw((seq_len(262144L) - 1L) %% 251L)
  }
  
  complex_input <- function() {
    complex(real = as.double(seq_len(32768L)), imaginary = as.double(seq_len(32768L)) * 0.5)
  }
  
  tasks <- list(
    list(id = "01_vectorsum",
         args = function() list(runif(1e7))),
    list(id = "02_elem_ops",
         args = function() list(runif(1e6, -5, 5))),
    list(id = "03_memcpy_bandwidth",
         args = function() list(rep.int(as.double(1:8), 131072L))),
    list(id = "04_sort",
         args = function() list(runif(1e6))),
    list(id = "05_fib_recursive",
         args = function() list(25L)),
    list(id = "06_broadcast",
         args = function() list(runif(1e7), 3.14)),
    list(id = "07a_protect_shallow",
         args = function() list(runif(10))),
    list(id = "07b_protect_scaling",
         args = function() list(runif(100000))),
    list(id = "08_type_dispatch",
         args = function() list(list(runif(2048), 1:2048, rep("x", 2048)))),
    list(id = "09_longjmp_safety",
         args = function() list(runif(64))),
    list(id = "10_sexp_create",
         args = function() list(100000L)),
    list(id = "11_sexp_inspect",
         args = function() list(list(runif(1), 1L, "x", list(), NULL))),
    list(id = "12_matrix_transpose",
         args = function() list(matrix(runif(500*500), 500, 500))),
    list(id = "13_matrix_rowsums",
         args = function() list(matrix(runif(500000), 1000, 500))),
    list(id = "14_matrix_rowcol_means",
         args = function() list(matrix(runif(500000), 500, 1000))),
    list(id = "15_dataframe_filter",
         args = function() list(data.frame(x = rnorm(1e5), y = abs(rnorm(1e5)) + 0.1,
                                           grp = factor(sample(letters[1:10], 1e5, T)),
                                           stringsAsFactors = FALSE))),
    list(id = "16_list_access",
         args = function() list(replicate(1000, runif(100), simplify = FALSE))),
    list(id = "17_string_concat",
         args = function() list(benchmark_string_input("17_string_concat"))),
    list(id = "18_string_nchar",
         args = function() list(benchmark_string_input("18_string_nchar"))),
    list(id = "19_string_encoding",
         args = function() list(benchmark_string_input("19_string_encoding"))),
    list(id = "20_factor_ops",
         args = function() list(benchmark_factor_input())),
    list(id = "21_attrib_ops",
         args = function() list(runif(1e6))),
    list(id = "22_s4_slot_access",
         args = function() list(1.0)),
    list(id = "23_na_propagation",
         args = function() { x <- runif(1e6); x[sample(1e6, 5e4)] <- NA; list(x) }),
    list(id = "24_long_vector_idx",
         args = function() list(1:1e7)),
    list(id = "25_l1_arithmetic",
         args = function() list(runif(4000))),
    list(id = "26_matmul",
         args = function() { n <- 256L; list(matrix(runif(n * n), n, n),
                                             matrix(runif(n * n), n, n)) }),
    list(id = "27_crossprod",
         args = function() list(matrix(runif(25000), 500, 50))),
    list(id = "28_cholesky",
         args = function() { n <- 200L; A <- crossprod(matrix(runif(n * n), n, n)) + diag(n); list(A) }),
    list(id = "29_lm_fit",
         args = function() { n <- 5000L; p <- 20L; X <- matrix(runif(n * p), n, p); y <- runif(n); list(X, y) }),
    list(id = "30_altrep_create",
         args = function() list(1000000L)),
    list(id = "31_altrep_materialize",
         args = function() list(1000000L)),
    list(id = "32_altrep_elt_walk",
         args = function() list(1000000L)),
    list(id = "33_altrep_region_read",
         args = function() list(1000000L)),
    list(id = "34_altrep_sum_via_R",
         args = function() list(1000000L)),
    list(id = "35_altrep_sum_native",
         args = function() list(1000000L)),
    list(id = "36_altrep_min_max",
         args = function() list(1000000L)),
    list(id = "37_altrep_no_na_query",
         args = function() list(1000000L)),
    list(id = "38_struct_convert",
         args = function() list(list(id = 42L, count = 7L, level = 3L, flag = TRUE,
                                     enabled = FALSE, ratio = 1.25, offset = -3.5, scale = 9.75,
                                     weights = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5),
                                     indices = c(3L, 1L, 4L, 1L, 5L, 9L, 2L, 6L)))),
    list(id = "39_r_eval",
         args = function() list(runif(1e6))),
    list(id = "40_r_tryeval",
         args = function() list(runif(1))),
    list(id = "41_serialize_roundtrip",
         args = function() list(runif(1e6))),
    list(id = "42_external_ptr",
         args = function() list(1L)),
    list(id = "43_rng_stress",
         args = function() list(1000000L)),
    list(id = "48_weakref_lifecycle",
         args = function() list(4096L)),
    list(id = "49_owned_altrep_create",
         args = function() list(1000000L)),
    list(id = "50_boundary_zero_generated",
         args = function() list()),
    list(id = "51_boundary_zero_handwritten",
         args = function() list()),
    list(id = "52_boundary_scalar_generated",
         args = function() list(3.5)),
    list(id = "53_boundary_scalar_handwritten",
         args = function() list(3.5)),
    list(id = "54_boundary_optional_null_generated",
         args = function() list(NULL)),
    list(id = "55_boundary_optional_null_handwritten",
         args = function() list(NULL)),
    list(id = "56_boundary_optional_typed_na_generated",
         args = function() list(NA_real_)),
    list(id = "57_boundary_optional_typed_na_handwritten",
         args = function() list(NA_real_)),
    # as.double(seq_len()) is compact ALTREP in current R; arithmetic makes the
    # ordinary REALSXP required by the materialized numeric pair.
    list(id = "58_boundary_numeric_small_generated",
         args = function() list(as.double(seq_len(16L)) + 0.0)),
    list(id = "59_boundary_numeric_small_handwritten",
         args = function() list(as.double(seq_len(16L)) + 0.0)),
    list(id = "60_boundary_numeric_large_generated",
         args = function() list(as.double(seq_len(100000L)) + 0.0)),
    list(id = "61_boundary_numeric_large_handwritten",
         args = function() list(as.double(seq_len(100000L)) + 0.0)),
    list(id = "62_boundary_altrep_integer_generated",
         args = function() list(seq_len(100000L))),
    list(id = "63_boundary_altrep_integer_handwritten",
         args = function() list(seq_len(100000L))),
    list(id = "64_boundary_string_view_generated",
         args = function() list(rep(c("zigr", "boundary", NA_character_), length.out = 32L))),
    list(id = "65_boundary_string_view_handwritten",
         args = function() list(rep(c("zigr", "boundary", NA_character_), length.out = 32L))),
    list(id = "66_boundary_raw_generated",
         args = function() list(as.raw(seq_len(64L) %% 251L))),
    list(id = "67_boundary_raw_handwritten",
         args = function() list(as.raw(seq_len(64L) %% 251L))),
    list(id = "68_boundary_complex_generated",
         args = function() list(complex(real = as.double(seq_len(32L)), imaginary = as.double(seq_len(32L)) * 0.5))),
    list(id = "69_boundary_complex_handwritten",
         args = function() list(complex(real = as.double(seq_len(32L)), imaginary = as.double(seq_len(32L)) * 0.5))),
    list(id = "70_boundary_schema_generated",
         args = function() list(list(id = 42L, count = 7L, ratio = 1.25, enabled = TRUE))),
    list(id = "71_boundary_schema_handwritten",
         args = function() list(list(id = 42L, count = 7L, ratio = 1.25, enabled = TRUE))),
    list(id = "72_boundary_external_method_generated",
         args = function() list(method_receiver(), 7L)),
    list(id = "73_boundary_external_method_handwritten",
         args = function() list(method_receiver(), 7L)),
    list(id = "74_boundary_external_generated",
         call_type = ".External",
         args = function() list(4.0)),
    list(id = "75_boundary_external_handwritten",
         call_type = ".External",
         args = function() list(4.0)),
    list(id = "76_string_view_one",
         args = function() list(string_input())),
    list(id = "77_string_cache_build",
         args = function() list(string_input())),
    list(id = "78_string_cache_one",
         args = function() list(string_input())),
    list(id = "79_string_headers_one",
         args = function() list(string_input())),
    list(id = "80_string_view_repeated",
         args = function() list(string_input())),
    list(id = "81_string_cache_repeated",
         args = function() list(string_input())),
    list(id = "82_string_headers_repeated",
         args = function() list(string_input())),
    list(id = "83_raw_view",
         args = function() list(raw_input())),
    list(id = "84_raw_copy",
         args = function() list(raw_input())),
    list(id = "85_complex_view",
         args = function() list(complex_input())),
    list(id = "86_complex_return",
         args = function() list(complex_input()))
  )
  
  tasks
}

# Evidence matrix and report contracts.

evidence_schema_vocabulary <- function() {
  list(
    version = "evidence-v1",
    runners = c("c_call", "cpp11", "extendr", "r", "rcpp", "savvy", "zigr"),
    fixtures = sprintf("F%02d", seq_len(12L)),
    dispositions = c(
      "applicable", "control_only", "product_gap", "not_meaningful_for_product",
      "fixture_not_implemented", "fixture_invalid", "supported_and_executable"
    ),
    implementation_roles = c(
      "pure_r", "optimized_base_r", "c_control", "product_public_path",
      "language_control", "capability_gap"
    ),
    evidence_uses = c(
      "semantic_oracle", "timed_baseline", "product_comparison", "kernel_comparison",
      "strategy_comparison", "diagnostic_control", "gap"
    ),
    path_kinds = c(
      "pure_r", "optimized_base_r", "registered_c", "generated_typed", "generated_public_adapter",
      "handwritten_typed", "direct_export", "raw_ffi", "mixed", "unclassified", "none"
    ),
    representation_strategies = c(
      "borrowed_direct", "element_access", "region_access", "copied_contiguous",
      "materialized_r_vector", "owned_output", "cache_construction", "external_state",
      "kernel_specific", "runtime_service", "mixed", "not_applicable", "unclassified"
    ),
    comparison_tiers = c("tier_a", "tier_b", "tier_c", "tier_d", "gap"),
    mutation_policies = c(
      "immutable", "fresh_input_required", "stateful_reset_required", "rng_reset_required",
      "not_applicable", "unclassified"
    ),
    setup_policies = c("setup_outside_timer", "legacy_shared_instance", "not_timed", "unclassified")
  )
}

load_runner_configs <- function(root_dir = normalizePath(".")) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("jsonlite is required to load runner configs")
  path <- file.path(root_dir, "runners.json")
  if (!file.exists(path)) stop(sprintf("runner config file is missing: %s", path))
  document <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  if (!is.list(document) || is.null(names(document)) || anyDuplicated(names(document)) ||
      !setequal(names(document), c("schema_version", "runners"))) {
    stop("runner config document must contain only schema_version and runners")
  }
  if (!identical(as.integer(document$schema_version), 1L)) stop("unsupported runner config schema version")
  configs <- document$runners
  if (!is.list(configs) || length(configs) == 0L || is.null(names(configs)) ||
      anyDuplicated(names(configs)) || any(!nzchar(names(configs)))) {
    stop("runner configs must be a non-empty uniquely named object")
  }
  expected <- evidence_schema_vocabulary()$runners
  if (!setequal(names(configs), expected)) {
    stop("runner config coverage differs from the evidence vocabulary")
  }
  for (runner_name in names(configs)) {
    config <- configs[[runner_name]]
    if (!is.list(config) || is.null(names(config)) || anyDuplicated(names(config))) {
      stop(sprintf("runner config %s must be a uniquely named object", runner_name))
    }
    if (!identical(as.character(config$name), runner_name)) {
      stop(sprintf("runner config key/name mismatch for %s", runner_name))
    }
  }
  configs[expected]
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

parse_csv_option <- function(value, label) {
  if (length(value) != 1L || is.na(value) || !nzchar(value)) stop(sprintf("%s must be one non-empty comma-separated value", label))
  values <- strsplit(value, ",", fixed = TRUE)[[1L]]
  if (any(!nzchar(values)) || any(values != trimws(values))) stop(sprintf("%s contains an empty or padded item", label))
  if (anyDuplicated(values)) stop(sprintf("%s contains duplicate items", label))
  values
}

parse_task_filter <- function(value) {
  values <- parse_csv_option(value, "task filter")
  if (any(!grepl("^[0-9]+$", values))) stop("task filter must contain only positive integers")
  tasks <- as.integer(values)
  if (any(is.na(tasks)) || any(tasks <= 0L)) stop("task filter must contain only positive integers")
  if (anyDuplicated(tasks)) stop("task filter contains duplicate task numbers")
  tasks
}

select_task_ids <- function(task_ids, task_numbers) {
  available_numbers <- as.integer(sub("([0-9]+).*", "\\1", as.character(task_ids)))
  missing <- setdiff(as.integer(task_numbers), available_numbers)
  if (length(missing) > 0L) stop("task filter differs from available task numbers")
  as.character(task_ids)[available_numbers %in% as.integer(task_numbers)]
}

parse_benchmark_suite <- function(value) {
  value <- as.character(value)
  if (length(value) != 1L || is.na(value) || !(value %in% c("tasks", "fixtures", "all"))) {
    stop("benchmark suite must be tasks, fixtures, or all")
  }
  value
}

benchmark_run_suite <- function(metadata) {
  if (is.null(metadata$suite)) return("all")
  parse_benchmark_suite(metadata$suite)
}

benchmark_run_includes <- function(metadata, universe) {
  if (!(universe %in% c("task", "fixture"))) stop("benchmark universe must be task or fixture")
  suite <- benchmark_run_suite(metadata)
  identical(suite, "all") || identical(suite, paste0(universe, "s"))
}

benchmark_run_promotion_eligible <- function(metadata) {
  isTRUE(metadata$full_matrix) &&
    identical(benchmark_run_suite(metadata), "all") &&
    identical(as.character(metadata$measurement_mode), "timed")
}

evidence_task_contract_version <- function(task_id) {
  revision <- if (task_id %in% c(
    "17_string_concat", "18_string_nchar", "19_string_encoding", "20_factor_ops",
    "41_serialize_roundtrip", "42_external_ptr", "43_rng_stress", "72_boundary_external_method_generated",
    "73_boundary_external_method_handwritten"
  )) "v2" else "v1"
  paste0("task-contract:", task_id, ":", revision)
}

task_mutation_policy <- function(task_id) {
  if (task_id %in% c("04_sort", "21_attrib_ops")) return("fresh_input_required")
  if (task_id %in% c("72_boundary_external_method_generated", "73_boundary_external_method_handwritten")) {
    return("stateful_reset_required")
  }
  if (identical(task_id, "43_rng_stress")) return("rng_reset_required")
  "immutable"
}

evidence_boundary_group <- function(task_id) {
  if (!grepl("^[0-9]{2}_boundary_.*_(generated|handwritten)$", task_id)) return("")
  sub("^[0-9]{2}_boundary_(.*)_(generated|handwritten)$", "\\1", task_id)
}

evidence_boundary_kernel <- function(boundary) {
  kernels <- c(
    zero = "real-scalar-one-construction-v1",
    scalar = "nonmissing-real-scalar-roundtrip-v1",
    optional_null = "null-or-missing-real-presence-indicator-v1",
    optional_typed_na = "null-or-missing-real-presence-indicator-v1",
    numeric_small = "f64-scalar-left-fold-sum-v1",
    numeric_large = "f64-scalar-left-fold-sum-v1",
    altrep_integer = "i32-to-f64-scalar-left-fold-sum-v1",
    string_view = "nonmissing-string-element-count-v1",
    raw = "raw-byte-integer-left-fold-sum-v1",
    complex = "complex-real-component-left-fold-sum-v1",
    schema = "four-field-named-list-validation-and-identity-v1",
    external_method = "typed-state-integer-add-and-return-v1",
    external = "real-scalar-add-one-v1"
  )
  value <- unname(kernels[[boundary]])
  if (is.null(value)) stop(sprintf("boundary kernel catalog is missing %s", boundary))
  value
}

evidence_task_kernel_base <- function(task_id) {
  boundary <- evidence_boundary_group(task_id)
  if (nzchar(boundary)) return(evidence_boundary_kernel(boundary))
  representation <- c(
    "76_string_view_one" = "string-byte-count-one-pass-v1",
    "77_string_cache_build" = "string-metadata-cache-construction-v1",
    "78_string_cache_one" = "string-cache-build-plus-one-byte-count-pass-v1",
    "79_string_headers_one" = "string-header-copy-plus-one-byte-count-pass-v1",
    "80_string_view_repeated" = "string-byte-count-four-element-passes-v1",
    "81_string_cache_repeated" = "string-cache-build-plus-four-byte-count-passes-v1",
    "82_string_headers_repeated" = "string-header-copy-plus-four-byte-count-passes-v1",
    "83_raw_view" = "raw-byte-sum-borrowed-v1",
    "84_raw_copy" = "raw-byte-copy-plus-sum-v1",
    "85_complex_view" = "complex-real-plus-imaginary-sum-v1",
    "86_complex_return" = "complex-output-copy-v1"
  )
  if (task_id %in% names(representation)) return(unname(representation[[task_id]]))
  kernels <- c(
    "01_vectorsum" = "f64-scalar-left-fold-sum-v1",
    "02_elem_ops" = "f64-abs-log-exp-sqrt-four-column-loop-v1",
    "03_memcpy_bandwidth" = "copy-temp-copy-output-fill-output-two-pass-compound-v1",
    "04_sort" = "f64-eight-pass-lsd-radix-copy-sort-v1",
    "05_fib_recursive" = "recursive-fibonacci-i64-v1",
    "06_broadcast" = "f64-scalar-left-fold-vector-plus-scalar-v1",
    "07a_protect_shallow" = "protect-unprotect-100x10-v1",
    "07b_protect_scaling" = "protect-unprotect-100x10x10000-v1",
    "08_type_dispatch" = "three-type-tag-dispatch-2048-passes-v1",
    "09_longjmp_safety" = "direct-adjusted-sum-plus-try-sum-error-unwind-four-strategy-512-passes-v1",
    "10_sexp_create" = "real-scalar-allocation-10x10000-v1",
    "11_sexp_inspect" = "five-cached-object-type-vector-real-query-10000-passes-v1",
    "12_matrix_transpose" = "column-major-blocked-transpose-32-v1",
    "13_matrix_rowsums" = "column-major-row-sums-v1",
    "14_matrix_rowcol_means" = "column-major-row-means-column-sums-v1",
    "15_dataframe_filter" = "named-column-lookup-positive-x-grouped-x-over-y-sum-v1",
    "16_list_access" = "first-real-element-list-sum-v1",
    "17_string_concat" = "utf8-or-bytes-comma-space-concatenation-v2",
    "18_string_nchar" = "charsxp-byte-length-sum-skip-missing-v2",
    "19_string_encoding" = "utf8-encoding-mark-count-v2",
    "20_factor_ops" = "factor-conversion-code-sum-100-level-one-missing-v2",
    "21_attrib_ops" = "class-and-creator-attribute-set-read-v1",
    "22_s4_slot_access" = "bench-s4-construct-assign-read-slot-v1",
    "23_na_propagation" = "f64-mean-skip-na-and-nan-v1",
    "24_long_vector_idx" = "compact-integer-altrep-every-10000th-element-sum-v1",
    "25_l1_arithmetic" = "f64-scale-add-left-fold-2500-passes-v1",
    "26_matmul" = "blas-dgemm-nn-column-major-v1",
    "27_crossprod" = "blas-dsyrk-upper-transpose-plus-mirror-v1",
    "28_cholesky" = "lapack-dpotrf-upper-plus-zero-lower-v1",
    "29_lm_fit" = "normal-equations-dgemm-dpotrf-two-dtrsm-v1",
    "30_altrep_create" = "base-compact-integer-sequence-construction-v1",
    "31_altrep_materialize" = "compact-integer-duplicate-pointer-endpoint-sum-v1",
    "32_altrep_elt_walk" = "compact-integer-element-walk-sum-v1",
    "33_altrep_region_read" = "compact-integer-4096-region-walk-sum-v1",
    "34_altrep_sum_via_R" = "compact-integer-r-sum-dispatch-v1",
    "35_altrep_sum_native" = "compact-integer-native-element-walk-sum-v1",
    "36_altrep_min_max" = "compact-integer-element-min-max-difference-v1",
    "37_altrep_no_na_query" = "compact-integer-element-missing-query-v1",
    "38_struct_convert" = "ten-field-declaration-order-record-copy-v1",
    "39_r_eval" = "global-environment-direct-eval-sum-plus-mean-v1",
    "40_r_tryeval" = "silent-stop-evaluation-512-errors-v1",
    "41_serialize_roundtrip" = "r-direct-eval-serialize-unserialize-real-sum-v1",
    "42_external_ptr" = "owned-integer-state-constructor-finalizer-v2",
    "43_rng_stress" = "r-inversion-normal-rng-one-million-draws-v2",
    "48_weakref_lifecycle" = "zigr-weakref-create-key-value-check-4096-v1",
    "49_owned_altrep_create" = "zigr-owned-integer-altrep-callback-lifecycle-v1"
  )
  value <- unname(kernels[[task_id]])
  if (is.null(value)) stop(sprintf("task kernel catalog is missing %s", task_id))
  value
}

evidence_optimized_r_kernel <- function(task_id) {
  kernels <- c(
    "01_vectorsum" = "base-sum-primitive-v1",
    "02_elem_ops" = "base-vectorized-abs-ifelse-log-exp-sqrt-cbind-v1",
    "03_memcpy_bandwidth" = "base-r-vector-copy-allocation-sum-two-passes-v1",
    "04_sort" = "base-sort-method-dispatch-v1",
    "06_broadcast" = "base-vector-add-plus-sum-v1",
    "12_matrix_transpose" = "base-transpose-primitive-v1",
    "13_matrix_rowsums" = "base-row-sums-primitive-v1",
    "14_matrix_rowcol_means" = "base-row-means-plus-column-sums-v1",
    "15_dataframe_filter" = "base-dataframe-subset-plus-stats-aggregate-sum-v1",
    "17_string_concat" = "base-paste0-collapse-utf8-or-bytes-v2",
    "18_string_nchar" = "base-nchar-bytes-plus-sum-remove-missing-v2",
    "19_string_encoding" = "base-encoding-utf8-equality-sum-v2",
    "20_factor_ops" = "base-factor-code-sum-100-level-one-missing-v2",
    "21_attrib_ops" = "base-attribute-set-read-nchar-sum-v1",
    "22_s4_slot_access" = "methods-s4-new-assign-read-slot-v1",
    "23_na_propagation" = "base-mean-remove-missing-v1",
    "26_matmul" = "base-matrix-product-blas-v1",
    "27_crossprod" = "base-crossprod-blas-v1",
    "28_cholesky" = "base-chol-lapack-v1",
    "29_lm_fit" = "stats-lm-fit-qr-fortran-v1",
    "30_altrep_create" = "base-seq-len-compact-integer-altrep-v1",
    "31_altrep_materialize" = "base-seq-len-slice-materialization-endpoint-sum-v1",
    "34_altrep_sum_via_R" = "base-sum-altrep-dispatch-v1",
    "36_altrep_min_max" = "base-min-max-compact-integer-v1",
    "37_altrep_no_na_query" = "base-any-is-na-compact-integer-v1",
    "38_struct_convert" = "base-ten-field-scalar-coercion-copy-v1",
    "39_r_eval" = "base-sum-plus-mean-v1",
    "40_r_tryeval" = "base-try-catch-stop-512-errors-v1",
    "41_serialize_roundtrip" = "base-serialize-unserialize-sum-v1",
    "43_rng_stress" = "base-rnorm-inversion-one-million-draws-v2",
    "58_boundary_numeric_small_generated" = "base-sum-numeric-boundary-v1",
    "59_boundary_numeric_small_handwritten" = "base-sum-numeric-boundary-v1",
    "60_boundary_numeric_large_generated" = "base-sum-numeric-boundary-v1",
    "61_boundary_numeric_large_handwritten" = "base-sum-numeric-boundary-v1",
    "62_boundary_altrep_integer_generated" = "base-sum-compact-integer-plus-double-coercion-v1",
    "63_boundary_altrep_integer_handwritten" = "base-sum-compact-integer-plus-double-coercion-v1",
    "64_boundary_string_view_generated" = "base-nonmissing-string-count-v1",
    "65_boundary_string_view_handwritten" = "base-nonmissing-string-count-v1",
    "66_boundary_raw_generated" = "base-raw-to-integer-coercion-plus-sum-v1",
    "67_boundary_raw_handwritten" = "base-raw-to-integer-coercion-plus-sum-v1",
    "68_boundary_complex_generated" = "base-complex-real-part-plus-sum-v1",
    "69_boundary_complex_handwritten" = "base-complex-real-part-plus-sum-v1",
    "74_boundary_external_generated" = "base-vector-add-one-plus-double-coercion-v1",
    "75_boundary_external_handwritten" = "base-vector-add-one-plus-double-coercion-v1",
    "76_string_view_one" = "base-nchar-bytes-sum-one-pass-v1",
    "77_string_cache_build" = "base-length-only-no-string-cache-v1",
    "78_string_cache_one" = "base-nchar-bytes-sum-no-cache-one-pass-v1",
    "79_string_headers_one" = "base-nchar-bytes-sum-no-header-copy-one-pass-v1",
    "80_string_view_repeated" = "base-nchar-bytes-sum-four-passes-v1",
    "81_string_cache_repeated" = "base-nchar-bytes-sum-no-cache-four-passes-v1",
    "82_string_headers_repeated" = "base-nchar-bytes-sum-no-header-copy-four-passes-v1",
    "83_raw_view" = "base-raw-to-integer-coercion-plus-sum-no-view-v1",
    "84_raw_copy" = "base-raw-to-integer-coercion-plus-sum-no-copy-v1",
    "85_complex_view" = "base-complex-real-plus-imaginary-vector-sum-v1",
    "86_complex_return" = "base-complex-vector-add-zero-output-v1"
  )
  value <- unname(kernels[[task_id]])
  if (is.null(value)) stop(sprintf("optimized R kernel catalog is missing %s", task_id))
  value
}

evidence_task_kernel_id <- function(runner, task_id, implementation_role) {
  base <- evidence_task_kernel_base(task_id)
  if (identical(implementation_role, "optimized_base_r")) {
    return(evidence_optimized_r_kernel(task_id))
  }
  if (identical(runner, "zigr") && identical(task_id, "01_vectorsum")) {
    return("f64-simd-chunk-reduction-plus-scalar-tail-v1")
  }
  if (identical(runner, "zigr") && identical(task_id, "06_broadcast")) {
    return("f64-simd-vector-plus-scalar-reduction-v1")
  }
  if (identical(runner, "zigr") && identical(task_id, "14_matrix_rowcol_means")) {
    return("column-major-simd4-row-means-column-sums-v1")
  }
  if (identical(runner, "zigr") && identical(task_id, "09_longjmp_safety")) {
    return("direct-adjusted-sum-plus-try-sum-hoisted-error-call-unwind-four-strategy-512-passes-v1")
  }
  if (identical(runner, "zigr") && identical(task_id, "11_sexp_inspect")) {
    return("five-object-type-vector-real-query-once-plus-10000-accumulation-passes-v1")
  }
  if (identical(runner, "extendr") && identical(task_id, "15_dataframe_filter")) {
    return("positional-column-iteration-positive-x-grouped-x-over-y-sum-v1")
  }
  if (runner %in% c("extendr", "savvy") && identical(task_id, "39_r_eval")) {
    return("global-environment-silent-try-eval-sum-plus-mean-v1")
  }
  if (identical(runner, "zigr") && identical(task_id, "41_serialize_roundtrip")) {
    return("r-persistent-stream-xdr-v3-serialize-unserialize-real-sum-v1")
  }
  if (runner %in% c("extendr", "savvy") && identical(task_id, "41_serialize_roundtrip")) {
    return("r-silent-try-eval-serialize-unserialize-real-sum-v1")
  }
  if (identical(runner, "savvy") && task_id %in% c("07a_protect_shallow", "07b_protect_scaling")) {
    return("savvy-no-protection-loop-zero-control-v1")
  }
  if (identical(runner, "savvy") && identical(task_id, "11_sexp_inspect")) {
    return("five-list-element-type-vector-real-query-10000-passes-v1")
  }
  base
}

evidence_native_invariant_tasks <- function() {
  c(
    "07a_protect_shallow", "07b_protect_scaling", "08_type_dispatch",
    "09_longjmp_safety", "10_sexp_create", "11_sexp_inspect", "42_external_ptr",
    "48_weakref_lifecycle", "49_owned_altrep_create"
  )
}

evidence_task_evidence_use <- function(runner, task_id, implementation_role, comparison_tier) {
  if (identical(implementation_role, "capability_gap")) return("gap")
  if (identical(implementation_role, "pure_r")) return("semantic_oracle")
  if (identical(implementation_role, "optimized_base_r")) return("timed_baseline")
  if (identical(implementation_role, "c_control")) {
    if (task_id %in% evidence_native_invariant_tasks()) return("diagnostic_control")
    return("kernel_comparison")
  }
  if (identical(implementation_role, "product_public_path")) {
    return(switch(comparison_tier,
      tier_a = "product_comparison",
      tier_b = "strategy_comparison",
      tier_d = "diagnostic_control",
      stop(sprintf("product task has invalid comparison tier %s/%s", runner, task_id))
    ))
  }
  if (identical(implementation_role, "language_control")) return("diagnostic_control")
  stop(sprintf("task evidence-use catalog is missing %s/%s/%s", runner, task_id, implementation_role))
}

evidence_task_representation_strategy <- function(runner, task_id, implementation_role) {
  if (identical(implementation_role, "optimized_base_r")) return("runtime_service")
  if (identical(implementation_role, "pure_r")) {
    if (task_id %in% c(
      "01_vectorsum", "06_broadcast", "12_matrix_transpose", "13_matrix_rowsums",
      "14_matrix_rowcol_means", "16_list_access", "23_na_propagation",
      "24_long_vector_idx", "25_l1_arithmetic", "32_altrep_elt_walk",
      "33_altrep_region_read", "35_altrep_sum_native", "36_altrep_min_max",
      "37_altrep_no_na_query"
    )) {
      return("element_access")
    }
    if (task_id %in% c("38_struct_convert", "70_boundary_schema_generated", "71_boundary_schema_handwritten")) return("mixed")
    if (task_id %in% c("50_boundary_zero_generated", "51_boundary_zero_handwritten",
                       "54_boundary_optional_null_generated", "55_boundary_optional_null_handwritten",
                       "56_boundary_optional_typed_na_generated", "57_boundary_optional_typed_na_handwritten")) {
      return("owned_output")
    }
    if (task_id %in% c("52_boundary_scalar_generated", "53_boundary_scalar_handwritten")) return("borrowed_direct")
    return("kernel_specific")
  }
  boundary <- evidence_boundary_group(task_id)
  if (nzchar(boundary)) {
    strategy <- switch(boundary,
      zero = "owned_output", scalar = "owned_output", optional_null = "owned_output",
      optional_typed_na = "owned_output", numeric_small = "borrowed_direct",
      numeric_large = "borrowed_direct", altrep_integer = "region_access",
      string_view = "element_access", raw = "borrowed_direct", complex = "borrowed_direct",
      schema = "mixed", external_method = "external_state", external = "runtime_service"
    )
    if (identical(runner, "zigr") && identical(task_id, "62_boundary_altrep_integer_generated")) {
      strategy <- "copied_contiguous"
    }
    if (identical(runner, "zigr") && task_id %in% c("66_boundary_raw_generated")) {
      strategy <- "copied_contiguous"
    }
    strategy
  } else if (task_id %in% c("76_string_view_one", "80_string_view_repeated")) {
    "element_access"
  } else if (task_id %in% c("77_string_cache_build", "78_string_cache_one", "81_string_cache_repeated")) {
    "cache_construction"
  } else if (task_id %in% c("79_string_headers_one", "82_string_headers_repeated", "84_raw_copy")) {
    "copied_contiguous"
  } else if (task_id %in% c("83_raw_view", "85_complex_view")) {
    "borrowed_direct"
  } else if (identical(task_id, "86_complex_return")) {
    "owned_output"
  } else if (task_id %in% c("01_vectorsum", "02_elem_ops", "06_broadcast", "12_matrix_transpose",
                             "13_matrix_rowsums", "14_matrix_rowcol_means", "23_na_propagation", "25_l1_arithmetic", "26_matmul",
                             "27_crossprod", "28_cholesky", "29_lm_fit")) {
    "borrowed_direct"
  } else if (identical(task_id, "15_dataframe_filter")) {
    "mixed"
  } else if (identical(task_id, "16_list_access")) {
    "element_access"
  } else if (identical(task_id, "03_memcpy_bandwidth")) {
    "mixed"
  } else if (identical(task_id, "04_sort")) {
    "copied_contiguous"
  } else if (task_id %in% c("05_fib_recursive", "08_type_dispatch", "11_sexp_inspect")) {
    "kernel_specific"
  } else if (task_id %in% c("07a_protect_shallow", "07b_protect_scaling", "09_longjmp_safety",
                             "22_s4_slot_access", "30_altrep_create", "34_altrep_sum_via_R",
                             "39_r_eval", "40_r_tryeval", "41_serialize_roundtrip")) {
    "runtime_service"
  } else if (identical(task_id, "10_sexp_create")) {
    "owned_output"
  } else if (task_id %in% c("17_string_concat", "18_string_nchar", "19_string_encoding",
                             "24_long_vector_idx", "32_altrep_elt_walk",
                             "35_altrep_sum_native", "36_altrep_min_max", "37_altrep_no_na_query")) {
    "element_access"
  } else if (task_id %in% c("20_factor_ops", "21_attrib_ops")) {
    "runtime_service"
  } else if (identical(task_id, "31_altrep_materialize")) {
    "materialized_r_vector"
  } else if (identical(task_id, "33_altrep_region_read")) {
    "region_access"
  } else if (task_id %in% c("38_struct_convert", "43_rng_stress")) {
    "owned_output"
  } else if (identical(task_id, "42_external_ptr")) {
    "external_state"
  } else if (task_id %in% c("48_weakref_lifecycle", "49_owned_altrep_create")) {
    "mixed"
  } else {
    stop(sprintf("task representation catalog is missing %s/%s", runner, task_id))
  }
}

hydrate_detailed_task_evidence <- function(rows) {
  for (index in seq_len(nrow(rows))) {
    task_id <- as.character(rows$task[[index]])
    runner <- as.character(rows$runner[[index]])
    implementation_role <- as.character(rows$implementation_role[[index]])
    comparison_tier <- as.character(rows$comparison_tier[[index]])
    expected_evidence_use <- evidence_task_evidence_use(
      runner, task_id, implementation_role, comparison_tier
    )
    if (!identical(as.character(rows$evidence_use[[index]]), expected_evidence_use)) {
      stop(sprintf("task evidence use differs from the exact catalog for %s/%s", runner, task_id))
    }
    if (isTRUE(rows$executable[[index]])) {
      rows$kernel_id[[index]] <- evidence_task_kernel_id(
        runner, task_id, implementation_role
      )
      rows$representation_strategy[[index]] <- evidence_task_representation_strategy(
        runner, task_id, implementation_role
      )
      rows$mutation_policy[[index]] <- task_mutation_policy(task_id)
      rows$setup_policy[[index]] <- "setup_outside_timer"
    } else {
      rows$kernel_id[[index]] <- paste0("not-applicable:", task_id)
      rows$representation_strategy[[index]] <- "not_applicable"
      rows$mutation_policy[[index]] <- "not_applicable"
      rows$setup_policy[[index]] <- "not_timed"
    }
    rows$contract_version[[index]] <- evidence_task_contract_version(task_id)
    rows$fixture_version[[index]] <- "task-input-v2"
    rows$comparison_group[[index]] <- paste0("task:", task_id)
    rows$timing_eligible[[index]] <- isTRUE(rows$executable[[index]])
  }
  rows
}

hydrate_fixture_measurement_evidence <- function(rows) {
  for (index in seq_len(nrow(rows))) {
    admitted <- isTRUE(rows$executable[[index]]) &&
      !identical(as.character(rows$fixture[[index]]), "F11")
    rows$timing_eligible[[index]] <- admitted
    rows$setup_policy[[index]] <- if (admitted) "setup_outside_timer" else "not_timed"
  }
  rows
}

evidence_values <- function(value) {
  if (is.null(value)) character(0) else as.character(unlist(value, use.names = FALSE))
}

evidence_check_keys <- function(value, allowed, required, label) {
  if (!is.list(value) || is.null(names(value)) || any(!nzchar(names(value))) || anyDuplicated(names(value))) {
    stop(sprintf("%s must be a uniquely named object", label))
  }
  missing <- setdiff(required, names(value))
  extra <- setdiff(names(value), allowed)
  if (length(missing) > 0L || length(extra) > 0L) {
    stop(sprintf(
      "%s fields differ from the evidence schema; missing: %s; extra: %s",
      label, paste(missing, collapse = ", "), paste(extra, collapse = ", ")
    ))
  }
  invisible(value)
}

evidence_scalar_character <- function(value, label) {
  if (length(value) != 1L || is.na(value) || !nzchar(as.character(value))) {
    stop(sprintf("%s must be one nonblank string", label))
  }
  as.character(value)
}

evidence_expand_template <- function(value, runner, item) {
  value <- as.character(value)
  value <- gsub("{runner}", runner, value, fixed = TRUE)
  value <- gsub("{task}", item, value, fixed = TRUE)
  gsub("{fixture}", item, value, fixed = TRUE)
}

evidence_select_tasks <- function(group, task_sets, all_tasks) {
  requested_sets <- c(evidence_values(group$include_sets), evidence_values(group$exclude_sets))
  unknown_sets <- setdiff(requested_sets, names(task_sets))
  if (length(unknown_sets) > 0L) {
    stop(sprintf("task disposition selector contains unknown task sets: %s", paste(unknown_sets, collapse = ", ")))
  }
  include <- unique(c(
    unlist(task_sets[evidence_values(group$include_sets)], use.names = FALSE),
    evidence_values(group$include_tasks)
  ))
  if (length(include) == 0L) stop("task disposition group selects no tasks")
  exclude <- unique(c(
    unlist(task_sets[evidence_values(group$exclude_sets)], use.names = FALSE),
    evidence_values(group$exclude_tasks)
  ))
  unknown <- setdiff(c(include, exclude), all_tasks)
  if (length(unknown) > 0L) {
    stop(sprintf("task disposition selector contains unknown tasks: %s", paste(unknown, collapse = ", ")))
  }
  setdiff(include, exclude)
}

evidence_select_fixtures <- function(group, all_fixtures) {
  include <- evidence_values(group$include_fixtures)
  if (length(include) == 0L) stop("fixture disposition group selects no fixtures")
  exclude <- evidence_values(group$exclude_fixtures)
  unknown <- setdiff(c(include, exclude), all_fixtures)
  if (length(unknown) > 0L) {
    stop(sprintf("fixture disposition selector contains unknown fixtures: %s", paste(unknown, collapse = ", ")))
  }
  setdiff(include, exclude)
}

evidence_expand_groups <- function(groups, defaults, universe, task_sets = NULL) {
  identity_field <- if (identical(universe, "task")) "task" else "fixture"
  record_fields <- c(
    "status", "executable", "implementation_role", "evidence_use", "path_kind", "public_path",
    "representation_strategy", "kernel_id", "contract_version", "fixture_version",
    "comparison_tier", "mutation_policy", "setup_policy", "comparison_group",
    "timing_eligible", "reason", "owner"
  )
  selector_fields <- if (identical(universe, "task")) {
    c("include_sets", "include_tasks", "exclude_sets", "exclude_tasks")
  } else {
    c("include_fixtures", "exclude_fixtures")
  }
  required_group_fields <- c(
    "runner", selector_fields[[1L]], "status", "executable", "implementation_role",
    "evidence_use", "path_kind", "representation_strategy", "comparison_tier"
  )
  rows <- list()
  keys <- character(0)
  for (index in seq_along(groups)) {
    group <- groups[[index]]
    evidence_check_keys(
      group,
      allowed = c("runner", selector_fields, record_fields),
      required = required_group_fields,
      label = sprintf("%s disposition group %d", universe, index)
    )
    runner <- evidence_scalar_character(group$runner, sprintf("%s disposition runner", universe))
    items <- if (identical(universe, "task")) {
      evidence_select_tasks(group, task_sets, evidence_values(task_sets$all_tasks))
    } else {
      evidence_select_fixtures(group, evidence_schema_vocabulary()$fixtures)
    }
    for (item in items) {
      key <- paste(runner, item, sep = "\r")
      if (key %in% keys) stop(sprintf("duplicate %s disposition key: %s/%s", universe, runner, item))
      keys <- c(keys, key)
      record <- defaults
      for (field in intersect(record_fields, names(group))) record[[field]] <- group[[field]]
      missing <- record_fields[vapply(record_fields, function(field) is.null(record[[field]]), logical(1))]
      if (length(missing) > 0L) {
        stop(sprintf("%s disposition %s/%s missing fields: %s", universe, runner, item, paste(missing, collapse = ", ")))
      }
      for (field in c("public_path", "kernel_id", "contract_version", "fixture_version", "comparison_group", "reason", "owner")) {
        record[[field]] <- evidence_expand_template(record[[field]], runner, item)
      }
      record$universe <- universe
      record$runner <- runner
      record[[identity_field]] <- item
      rows[[length(rows) + 1L]] <- record[c("universe", "runner", identity_field, record_fields)]
    }
  }
  if (length(rows) == 0L) stop(sprintf("evidence manifest contains no %s dispositions", universe))
  do.call(rbind, lapply(rows, function(row) as.data.frame(row, stringsAsFactors = FALSE, check.names = FALSE)))
}

validate_evidence_rows <- function(rows, label = "evidence") {
  vocabulary <- evidence_schema_vocabulary()
  universes <- unique(as.character(rows$universe))
  if (length(universes) != 1L || !(universes %in% c("task", "fixture"))) {
    stop(sprintf("%s rows must contain exactly one recognized universe", label))
  }
  identity_field <- universes[[1L]]
  required <- c(
    "universe", "runner", identity_field, "status", "executable", "implementation_role", "evidence_use",
    "path_kind", "public_path", "representation_strategy", "kernel_id", "contract_version",
    "fixture_version", "comparison_tier", "mutation_policy", "setup_policy", "comparison_group",
    "timing_eligible", "reason", "owner"
  )
  missing <- setdiff(required, names(rows))
  if (length(missing) > 0L) stop(sprintf("%s rows missing fields: %s", label, paste(missing, collapse = ", ")))
  if (anyDuplicated(paste(rows$universe, rows$runner, rows[[identity_field]], sep = "\r"))) {
    stop(sprintf("%s rows contain duplicate keys", label))
  }
  allowed <- list(
    status = vocabulary$dispositions,
    implementation_role = vocabulary$implementation_roles,
    evidence_use = vocabulary$evidence_uses,
    path_kind = vocabulary$path_kinds,
    representation_strategy = vocabulary$representation_strategies,
    comparison_tier = vocabulary$comparison_tiers,
    mutation_policy = vocabulary$mutation_policies,
    setup_policy = vocabulary$setup_policies
  )
  for (field in names(allowed)) {
    invalid <- setdiff(unique(as.character(rows[[field]])), allowed[[field]])
    if (length(invalid) > 0L) stop(sprintf("%s rows contain unrecognized %s values: %s", label, field, paste(invalid, collapse = ", ")))
  }
  for (field in c("universe", "runner", identity_field, "kernel_id", "contract_version", "fixture_version", "comparison_group")) {
    if (anyNA(rows[[field]]) || any(!nzchar(as.character(rows[[field]])))) {
      stop(sprintf("%s rows contain blank %s values", label, field))
    }
  }
  if (!is.logical(rows$executable) || anyNA(rows$executable)) stop(sprintf("%s rows have invalid executable values", label))
  if (!is.logical(rows$timing_eligible) || anyNA(rows$timing_eligible)) stop(sprintf("%s rows have invalid timing_eligible values", label))

  non_executable <- !rows$executable
  blank_reason <- is.na(rows$reason) | !nzchar(trimws(as.character(rows$reason)))
  blank_owner <- is.na(rows$owner) | !nzchar(trimws(as.character(rows$owner)))
  if (any(non_executable & blank_reason)) stop(sprintf("%s has a non-executable cell with a blank reason", label))
  if (any(non_executable & blank_owner)) stop(sprintf("%s has a non-executable cell with a blank owner", label))
  if (any(rows$status == "fixture_invalid" & (blank_reason | blank_owner))) {
    stop(sprintf("%s has a fixture-invalid cell without a reason and owner", label))
  }

  forced_non_executable <- rows$status %in% c(
    "applicable", "product_gap", "not_meaningful_for_product", "fixture_not_implemented"
  )
  if (any(forced_non_executable & rows$executable)) stop(sprintf("%s marks a non-executable disposition executable", label))
  forced_executable <- rows$status %in% c("control_only", "supported_and_executable")
  if (any(forced_executable & !rows$executable)) stop(sprintf("%s marks an executable disposition non-executable", label))

  if (any(rows$implementation_role == "pure_r" & rows$path_kind == "optimized_base_r")) {
    stop(sprintf("%s labels an optimized base-R path as pure R", label))
  }
  role_paths <- list(
    pure_r = "pure_r",
    optimized_base_r = "optimized_base_r",
    c_control = "registered_c",
    capability_gap = "none",
    language_control = c("handwritten_typed", "direct_export", "raw_ffi", "mixed", "unclassified")
  )
  for (role in names(role_paths)) {
    bad <- rows$implementation_role == role & !(rows$path_kind %in% role_paths[[role]])
    if (any(bad)) stop(sprintf("%s has a %s row with the wrong path kind", label, role))
  }
  product_rows <- rows$implementation_role == "product_public_path"
  if (any(product_rows & rows$path_kind == "raw_ffi")) stop(sprintf("%s labels a raw path as a product public path", label))
  if (any(product_rows & !(rows$path_kind %in% c("generated_typed", "generated_public_adapter")))) {
    stop(sprintf("%s has a product public path with an invalid path kind", label))
  }
  if (any(product_rows & (is.na(rows$public_path) | !nzchar(trimws(as.character(rows$public_path)))))) {
    stop(sprintf("%s has a product public path without a public path description", label))
  }
  if (any(rows$status == "supported_and_executable" & !product_rows)) {
    stop(sprintf("%s marks a non-product row supported and executable", label))
  }
  if (any(rows$status == "control_only" & !(rows$implementation_role %in% c(
    "pure_r", "optimized_base_r", "c_control", "language_control"
  )))) {
    stop(sprintf("%s has a control-only row with a non-control implementation role", label))
  }

  if (any(rows$comparison_tier == "gap" & rows$timing_eligible)) stop(sprintf("%s has a gap with timing data", label))
  gap_shape <- non_executable & (
    rows$implementation_role != "capability_gap" | rows$evidence_use != "gap" |
      rows$path_kind != "none" | rows$comparison_tier != "gap" | rows$timing_eligible
  )
  if (any(gap_shape)) stop(sprintf("%s has a non-executable cell that is not a complete gap record", label))
  invalid_diagnostic_timing <- rows$timing_eligible & rows$status == "fixture_invalid" & !(
    rows$implementation_role == "language_control" &
      rows$evidence_use == "diagnostic_control" &
      rows$comparison_tier == "tier_d"
  )
  if (any(rows$timing_eligible & (!rows$executable | rows$evidence_use == "gap")) ||
      any(invalid_diagnostic_timing)) {
    stop(sprintf("%s has timing eligibility without valid executable evidence", label))
  }
  if (any(rows$comparison_tier %in% c("tier_a", "tier_b") & !product_rows)) {
    stop(sprintf("%s has a product comparison tier on a non-product path", label))
  }

  tier_a <- rows[rows$comparison_tier == "tier_a", , drop = FALSE]
  if (nrow(tier_a) > 0L) {
    group_key <- paste(tier_a$universe, tier_a$comparison_group, sep = "\r")
    for (key in unique(group_key)) {
      group <- tier_a[group_key == key, , drop = FALSE]
      for (field in c(
        "contract_version", "path_kind", "kernel_id", "representation_strategy", "setup_policy"
      )) {
        if (length(unique(as.character(group[[field]]))) != 1L) {
          stop(sprintf("%s has a Tier A group with mixed %s values", label, field))
        }
      }
    }
  }
  invisible(rows)
}

validate_evidence_coverage <- function(rows, runners, items, universe) {
  expected <- as.vector(outer(runners, items, paste, sep = "\r"))
  actual <- paste(rows$runner, rows[[universe]], sep = "\r")
  missing <- setdiff(expected, actual)
  extra <- setdiff(actual, expected)
  if (length(missing) > 0L || length(extra) > 0L) {
    display_keys <- function(keys) gsub("\r", "/", keys, fixed = TRUE)
    stop(sprintf(
      "%s disposition cells differ from the required matrix; missing: %s; extra: %s",
      universe, paste(display_keys(missing), collapse = ", "), paste(display_keys(extra), collapse = ", ")
    ))
  }
  invisible(rows)
}

validate_and_expand_evidence_manifest <- function(raw, task_manifest) {
  top_fields <- c(
    "schema_version", "vocabulary_version", "runners", "fixtures", "task_sets",
    "task_defaults", "fixture_defaults", "task_dispositions", "fixture_dispositions"
  )
  evidence_check_keys(raw, top_fields, top_fields, "evidence manifest")
  if (length(raw$schema_version) != 1L || !is.integer(raw$schema_version) || is.na(raw$schema_version) || raw$schema_version != 1L) {
    stop("unsupported evidence schema version")
  }
  vocabulary <- evidence_schema_vocabulary()
  vocabulary_version <- evidence_scalar_character(raw$vocabulary_version, "evidence vocabulary version")
  if (!identical(vocabulary_version, vocabulary$version)) stop("unsupported evidence vocabulary version")
  runners <- evidence_values(raw$runners)
  fixtures <- evidence_values(raw$fixtures)
  if (!identical(runners, vocabulary$runners)) stop("evidence manifest runner set or order differs from the frozen vocabulary")
  if (!identical(fixtures, vocabulary$fixtures)) stop("evidence manifest fixture set or order differs from F01 through F12")

  if (!is.list(raw$task_sets) || is.null(names(raw$task_sets)) || any(!nzchar(names(raw$task_sets))) || anyDuplicated(names(raw$task_sets))) {
    stop("evidence manifest task_sets must be a uniquely named object")
  }
  if ("all_tasks" %in% names(raw$task_sets)) stop("evidence manifest must not duplicate the canonical all_tasks set")
  task_sets <- c(
    list(all_tasks = as.character(task_manifest$task)),
    lapply(raw$task_sets, evidence_values)
  )
  for (name in names(task_sets)) {
    if (anyDuplicated(task_sets[[name]])) stop(sprintf("evidence task set %s contains duplicate task IDs", name))
  }
  unknown_set_tasks <- setdiff(unique(unlist(task_sets, use.names = FALSE)), task_sets$all_tasks)
  if (length(unknown_set_tasks) > 0L) {
    stop(sprintf("evidence task sets contain unknown task IDs: %s", paste(unknown_set_tasks, collapse = ", ")))
  }

  default_fields <- c(
    "public_path", "kernel_id", "contract_version", "fixture_version", "mutation_policy",
    "setup_policy", "comparison_group", "timing_eligible", "reason", "owner"
  )
  evidence_check_keys(raw$task_defaults, default_fields, default_fields, "task evidence defaults")
  evidence_check_keys(raw$fixture_defaults, default_fields, default_fields, "fixture evidence defaults")
  if (!identical(as.character(raw$task_defaults$kernel_id), "catalog:evidence_task_kernel_id") ||
      !identical(as.character(raw$task_defaults$contract_version), "catalog:evidence_task_contract_version")) {
    stop("task evidence defaults must declare the exact kernel and contract catalog derivation")
  }
  task_rows <- evidence_expand_groups(raw$task_dispositions, raw$task_defaults, "task", task_sets)
  task_rows <- hydrate_detailed_task_evidence(task_rows)
  fixture_rows <- evidence_expand_groups(raw$fixture_dispositions, raw$fixture_defaults, "fixture")
  fixture_rows <- hydrate_fixture_measurement_evidence(fixture_rows)
  validate_evidence_rows(task_rows, "task evidence")
  validate_evidence_rows(fixture_rows, "fixture evidence")
  validate_evidence_coverage(task_rows, runners, task_sets$all_tasks, "task")
  validate_evidence_coverage(fixture_rows, runners, fixtures, "fixture")
  if (any(grepl("unverified|unclassified|legacy|^catalog:", unlist(
    task_rows[c("kernel_id", "contract_version", "fixture_version", "mutation_policy", "setup_policy")],
    use.names = FALSE
  ), fixed = FALSE))) {
    stop("task evidence retains a coarse placeholder")
  }
  if (any(task_rows$executable & task_rows$owner != "accepted_evidence") ||
      any(!task_rows$executable & !nzchar(task_rows$owner))) {
    stop("task evidence has an invalid downstream owner")
  }
  list(
    schema_version = as.integer(raw$schema_version),
    vocabulary_version = as.character(raw$vocabulary_version),
    runners = runners,
    fixtures = fixtures,
    task_sets = task_sets,
    tasks = task_rows,
    fixture_rows = fixture_rows,
    raw = raw
  )
}

load_evidence_manifest <- function(root_dir = normalizePath("."), task_manifest = NULL) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("jsonlite is required to read the evidence manifest")
  if (is.null(task_manifest)) task_manifest <- load_task_manifest(root_dir)
  path <- file.path(root_dir, "evidence_manifest.json")
  if (!file.exists(path)) stop(sprintf("evidence manifest not found: %s", path))
  raw <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  validate_and_expand_evidence_manifest(raw, task_manifest)
}

hydrate_runner_config <- function(manifest, cfg, runner_name, evidence) {
  runner_name <- as.character(runner_name)
  if (length(runner_name) != 1L || !(runner_name %in% evidence$runners)) {
    stop(sprintf("runner %s is absent from the evidence manifest", paste(runner_name, collapse = ", ")))
  }
  if (is.null(cfg$name) || !identical(as.character(cfg$name), runner_name)) {
    stop(sprintf("runner config identity differs from its file name: %s", runner_name))
  }
  rows <- evidence$tasks[evidence$tasks$runner == runner_name, , drop = FALSE]
  executable_tasks <- as.character(rows$task[rows$executable])
  optional_tasks <- as.character(rows$task[!rows$executable])
  if (identical(runner_name, "r")) {
    cfg$exports <- cfg$exports[names(cfg$exports) %in% executable_tasks]
  }
  exports <- if (is.null(cfg$exports)) character(0) else names(cfg$exports)
  if (!setequal(exports, executable_tasks)) {
    stop(sprintf(
      "runner %s exports differ from executable evidence dispositions; missing: %s; extra: %s",
      runner_name, paste(setdiff(executable_tasks, exports), collapse = ", "),
      paste(setdiff(exports, executable_tasks), collapse = ", ")
    ))
  }
  cfg$optional_tasks <- as.list(optional_tasks)
  validate_runner_config(manifest, cfg, runner_name)
  cfg
}

run_disposition_records <- function(evidence, runners, tasks) {
  runners <- as.character(runners)
  tasks <- as.character(tasks)
  records <- list()
  for (runner in runners) {
    runner_rows <- evidence$tasks[evidence$tasks$runner == runner & evidence$tasks$task %in% tasks, , drop = FALSE]
    runner_rows <- runner_rows[match(tasks, runner_rows$task), , drop = FALSE]
    if (nrow(runner_rows) != length(tasks) || anyNA(runner_rows$task)) {
      stop(sprintf("normalized dispositions are incomplete for runner %s", runner))
    }
    records[[runner]] <- lapply(seq_len(nrow(runner_rows)), function(index) {
      row <- runner_rows[index, , drop = FALSE]
      list(
        task = as.character(row$task),
        status = as.character(row$status),
        executable = isTRUE(row$executable),
        reason = if (isTRUE(row$executable)) "not_applicable" else as.character(row$reason),
        evidence_use = as.character(row$evidence_use),
        path_kind = as.character(row$path_kind),
        representation_strategy = as.character(row$representation_strategy),
        kernel_id = as.character(row$kernel_id),
        contract_version = as.character(row$contract_version),
        timing_eligible = isTRUE(row$timing_eligible)
      )
    })
  }
  records
}

comparative_report_files <- function() {
  c(
    comparative = "comparative_metrics.csv",
    capability = "capability_matrix.csv",
    safety = "safety_results.csv"
  )
}

budget_report_files <- function() c(budget = "budget_results.csv")

declared_report_files <- function() c(comparative_report_files(), budget_report_files())

comparative_report_schema_version <- function() "benchmark-report-v5"

combine_report_tracks <- function(outputs) {
  if (!is.list(outputs) || length(outputs) == 0L || is.null(names(outputs)) ||
      any(!nzchar(names(outputs))) || anyDuplicated(names(outputs))) {
    stop("report tracks must be a non-empty uniquely named list")
  }
  if (any(!vapply(outputs, is.data.frame, logical(1)))) stop("every report track must be a data frame")
  if (any(vapply(outputs, function(rows) "report_track" %in% names(rows), logical(1)))) {
    stop("report track inputs must not define report_track")
  }
  columns <- unique(unlist(lapply(outputs, names), use.names = FALSE))
  combined <- lapply(seq_along(outputs), function(index) {
    rows <- outputs[[index]]
    missing <- setdiff(columns, names(rows))
    for (column in missing) rows[[column]] <- NA
    rows <- rows[, columns, drop = FALSE]
    data.frame(report_track = names(outputs)[[index]], rows, check.names = FALSE, stringsAsFactors = FALSE)
  })
  do.call(rbind, combined)
}

split_report_tracks <- function(rows, required_tracks, label) {
  if (!is.data.frame(rows) || !"report_track" %in% names(rows)) {
    stop(sprintf("%s has no report_track column", label))
  }
  required_tracks <- as.character(required_tracks)
  actual <- unique(as.character(rows$report_track))
  if (anyNA(actual) || any(!nzchar(actual)) || !setequal(actual, required_tracks)) {
    stop(sprintf("%s track coverage differs", label))
  }
  tracks <- lapply(required_tracks, function(track) {
    selected <- rows[as.character(rows$report_track) == track, , drop = FALSE]
    if (nrow(selected) == 0L) stop(sprintf("%s track %s is empty", label, track))
    selected
  })
  names(tracks) <- required_tracks
  tracks
}

report_measurement_columns <- function() c(
  "first_call_ms", "rss_endpoint_delta_kb", "rss_endpoint_metric",
  "rss_endpoint_support", "rss_endpoint_support_reason", "peak_rss_kb",
  "loaded_process_rss_kb", "peak_rss_metric", "peak_rss_support",
  "peak_rss_support_reason", "peak_rss_repetitions"
)

report_product_runners <- function() c("zigr", "rcpp", "cpp11", "extendr", "savvy")

report_linked_runners <- function() c(report_product_runners(), "r", "c_call")

report_require_columns <- function(rows, required, label) {
  missing <- setdiff(required, names(rows))
  if (length(missing) > 0L) {
    stop(sprintf("%s missing columns: %s", label, paste(missing, collapse = ", ")))
  }
  invisible(rows)
}

report_require_unique <- function(rows, fields, label) {
  keys <- do.call(paste, c(rows[fields], sep = "\r"))
  if (anyDuplicated(keys)) stop(sprintf("%s contains duplicate keys", label))
  invisible(rows)
}

report_require_no_claims <- function(rows, label) {
  claims <- as.logical(rows$claim_eligible)
  if (anyNA(claims) || any(claims)) stop(sprintf("%s contain a public claim row", label))
  invisible(rows)
}

report_require_one_value <- function(rows, field, label) {
  values <- unique(as.character(rows[[field]]))
  if (length(values) != 1L || is.na(values) || !nzchar(values)) {
    stop(sprintf("%s do not contain one nonblank %s", label, field))
  }
  invisible(rows)
}

classify_comparative_evidence <- function(evidence, policy, direction = "row_over_reference",
                                          minimum_samples = 2L) {
  required <- c(
    "row_median", "row_ci_low", "row_ci_high", "reference_median", "reference_ci_low",
    "reference_ci_high", "row_cv", "reference_cv", "row_floor", "reference_floor",
    "row_samples", "reference_samples", "row_stage", "reference_stage",
    "semantic_admitted", "source_identical", "tier_comparable", "contract_identical",
    "capability_supported", "metric_supported"
  )
  missing <- setdiff(required, names(evidence))
  if (length(missing) > 0L) {
    stop(sprintf("comparative evidence missing fields: %s", paste(missing, collapse = ", ")))
  }
  if (!(direction %in% c("row_over_reference", "reference_over_row"))) {
    stop("comparative evidence has an invalid ratio direction")
  }
  minimum_samples <- as.integer(minimum_samples)
  if (length(minimum_samples) != 1L || is.na(minimum_samples) || minimum_samples < 2L) {
    stop("comparative evidence minimum sample count must be at least two")
  }
  margin <- as.numeric(policy$meaningful_margin_ratio)
  noise_limit <- as.numeric(policy$low_noise_cv_threshold_pct)
  if (length(margin) != 1L || is.na(margin) || !is.finite(margin) || margin <= 1 ||
      length(noise_limit) != 1L || is.na(noise_limit) || !is.finite(noise_limit) || noise_limit <= 0) {
    stop("comparative evidence policy is invalid")
  }

  n <- nrow(evidence)
  result <- rep("INCONCLUSIVE", n)
  reason <- rep("interval_unavailable", n)
  noise_status <- rep("not_measured", n)
  ratio <- ratio_low <- ratio_high <- rep(NA_real_, n)
  semantic <- as.logical(evidence$semantic_admitted) %in% TRUE
  source <- as.logical(evidence$source_identical) %in% TRUE
  tier <- as.logical(evidence$tier_comparable) %in% TRUE
  contract <- as.logical(evidence$contract_identical) %in% TRUE
  capability <- as.logical(evidence$capability_supported) %in% TRUE
  supported <- as.logical(evidence$metric_supported) %in% TRUE
  comparable <- semantic & source & tier & contract & capability & supported
  result[!comparable] <- "NOT_COMPARABLE"
  reason[!semantic] <- "semantic_not_admitted"
  reason[semantic & !source] <- "source_identity_mismatch"
  reason[semantic & source & !tier] <- "tier_mismatch"
  reason[semantic & source & tier & !contract] <- "contract_mismatch"
  reason[semantic & source & tier & contract & !capability] <- "capability_unsupported"
  reason[semantic & source & tier & contract & capability & !supported] <- "metric_unsupported"
  noise_status[!comparable] <- "not_comparable"

  row_median <- as.numeric(evidence$row_median)
  reference_median <- as.numeric(evidence$reference_median)
  finite_medians <- comparable & is.finite(row_median) & row_median > 0 &
    is.finite(reference_median) & reference_median > 0
  if (identical(direction, "reference_over_row")) {
    ratio[finite_medians] <- reference_median[finite_medians] / row_median[finite_medians]
  } else {
    ratio[finite_medians] <- row_median[finite_medians] / reference_median[finite_medians]
  }
  row_low <- as.numeric(evidence$row_ci_low)
  row_high <- as.numeric(evidence$row_ci_high)
  reference_low <- as.numeric(evidence$reference_ci_low)
  reference_high <- as.numeric(evidence$reference_ci_high)
  finite_intervals <- finite_medians & is.finite(row_low) & row_low > 0 & is.finite(row_high) &
    row_high >= row_low & is.finite(reference_low) & reference_low > 0 &
    is.finite(reference_high) & reference_high >= reference_low
  if (identical(direction, "reference_over_row")) {
    ratio_low[finite_intervals] <- reference_low[finite_intervals] / row_high[finite_intervals]
    ratio_high[finite_intervals] <- reference_high[finite_intervals] / row_low[finite_intervals]
  } else {
    ratio_low[finite_intervals] <- row_low[finite_intervals] / reference_high[finite_intervals]
    ratio_high[finite_intervals] <- row_high[finite_intervals] / reference_low[finite_intervals]
  }

  row_cv <- as.numeric(evidence$row_cv)
  reference_cv <- as.numeric(evidence$reference_cv)
  finite_noise <- comparable & is.finite(row_cv) & row_cv >= 0 &
    is.finite(reference_cv) & reference_cv >= 0
  low_noise <- finite_noise & row_cv <= noise_limit & reference_cv <= noise_limit
  noise_status[finite_noise] <- ifelse(low_noise[finite_noise], "low_noise", "high_noise")
  row_stage <- as.character(evidence$row_stage)
  reference_stage <- as.character(evidence$reference_stage)
  confirmation <- comparable & !is.na(row_stage) & !is.na(reference_stage) &
    row_stage == "confirmation" & reference_stage == "confirmation"
  row_samples <- suppressWarnings(as.integer(evidence$row_samples))
  reference_samples <- suppressWarnings(as.integer(evidence$reference_samples))
  enough_samples <- comparable & !is.na(row_samples) & !is.na(reference_samples) &
    row_samples >= minimum_samples & reference_samples >= minimum_samples
  symmetric_samples <- enough_samples & row_samples == reference_samples
  row_floor <- as.character(evidence$row_floor)
  reference_floor <- as.character(evidence$reference_floor)
  above_floor <- comparable & !is.na(row_floor) & !is.na(reference_floor) &
    row_floor == "above_floor" & reference_floor == "above_floor"

  reason[comparable & !confirmation] <- "confirmation_missing"
  reason[comparable & confirmation & !enough_samples] <- "insufficient_samples"
  reason[comparable & confirmation & enough_samples & !symmetric_samples] <- "sample_count_mismatch"
  reason[comparable & confirmation & symmetric_samples & !above_floor] <- "timer_floor"
  reason[comparable & confirmation & symmetric_samples & above_floor & !finite_noise] <- "noise_unavailable"
  reason[comparable & confirmation & symmetric_samples & above_floor & finite_noise & !low_noise] <- "high_noise"
  admitted <- comparable & confirmation & symmetric_samples & above_floor & low_noise & finite_intervals
  inverse_margin <- 1 / margin
  loss <- admitted & ratio_low > margin
  win <- admitted & ratio_high < inverse_margin
  tie <- admitted & ratio_low >= inverse_margin & ratio_high <= margin
  result[loss] <- "LOSS"
  reason[loss] <- "interval_above_margin"
  result[win] <- "WIN"
  reason[win] <- "interval_below_margin"
  result[tie] <- "TIE"
  reason[tie] <- "interval_within_margin"
  reason[admitted & !(loss | win | tie)] <- "interval_overlaps_margin"

  data.frame(
    ratio = ratio, ratio_ci_low = ratio_low, ratio_ci_high = ratio_high,
    noise_status = noise_status, relative_result = result, comparison_reason = reason,
    stringsAsFactors = FALSE
  )
}

report_validate_comparisons <- function(rows, ratio_fields, result_field, label, policy) {
  result <- as.character(rows[[result_field]])
  allowed <- c("LOSS", "WIN", "TIE", "INCONCLUSIVE", "NOT_COMPARABLE")
  if (anyNA(result) || any(!(result %in% allowed))) {
    stop(sprintf("%s contain an invalid relative result", label))
  }
  resolved <- result %in% c("LOSS", "WIN", "TIE")
  not_comparable <- result == "NOT_COMPARABLE"
  for (field in ratio_fields) {
    values <- as.numeric(rows[[field]])
    if (any(!is.finite(values[resolved])) || any(!is.na(values[not_comparable]))) {
      stop(sprintf("%s comparison field %s disagrees with comparability", label, field))
    }
  }
  noise <- as.character(rows$noise_status)
  if (anyNA(noise) || any(noise[resolved] != "low_noise") || any(noise[not_comparable] != "not_comparable") ||
      any(!(noise[!resolved & !not_comparable] %in% c("low_noise", "high_noise", "not_measured")))) {
    stop(sprintf("%s noise status disagrees with comparability", label))
  }
  reason <- as.character(rows$comparison_reason)
  reason_by_result <- list(
    LOSS = "interval_above_margin",
    WIN = "interval_below_margin",
    TIE = "interval_within_margin",
    INCONCLUSIVE = c(
      "confirmation_missing", "insufficient_samples", "timer_floor", "noise_unavailable",
      "sample_count_mismatch", "high_noise", "interval_unavailable", "interval_overlaps_margin"
    ),
    NOT_COMPARABLE = c(
      "semantic_not_admitted", "source_identity_mismatch", "tier_mismatch", "contract_mismatch",
      "capability_unsupported", "metric_unsupported"
    )
  )
  valid_reason <- vapply(seq_along(result), function(index) {
    !is.na(reason[[index]]) && reason[[index]] %in% reason_by_result[[result[[index]]]]
  }, logical(1))
  if (any(!valid_reason)) stop(sprintf("%s comparison reason disagrees with the relative result", label))
  samples <- suppressWarnings(as.integer(rows$n_iterations))
  reference_samples <- suppressWarnings(as.integer(rows$zigr_n_iterations))
  complete_confirmation <- as.character(rows$sample_stage) %in% "confirmation" &
    as.character(rows$zigr_sample_stage) %in% "confirmation" &
    !is.na(samples) & samples >= 2L & !is.na(reference_samples) & reference_samples >= 2L &
    samples == reference_samples & as.character(rows$timer_noise_status) %in% "above_floor" &
    as.character(rows$zigr_timer_noise_status) %in% "above_floor"
  if (any(resolved & !complete_confirmation)) {
    stop(sprintf("%s contain a conclusion without complete confirmation evidence", label))
  }
  margin <- as.numeric(policy$meaningful_margin_ratio)
  if (length(margin) != 1L || is.na(margin) || !is.finite(margin) || margin <= 1) {
    stop(sprintf("%s comparison policy has an invalid meaningful margin", label))
  }
  ratio <- as.numeric(rows[[ratio_fields[[1L]]]])
  ratio_low <- as.numeric(rows[[ratio_fields[[2L]]]])
  ratio_high <- as.numeric(rows[[ratio_fields[[3L]]]])
  valid_interval <- is.finite(ratio) & ratio > 0 & is.finite(ratio_low) & ratio_low > 0 &
    is.finite(ratio_high) & ratio_high >= ratio_low & ratio >= ratio_low & ratio <= ratio_high
  if (any(resolved & !valid_interval)) {
    stop(sprintf("%s contain an invalid resolved confidence interval", label))
  }
  interval_result <- rep("INCONCLUSIVE", nrow(rows))
  interval_result[which(is.finite(ratio_low) & ratio_low > margin)] <- "LOSS"
  interval_result[which(is.finite(ratio_high) & ratio_high < 1 / margin)] <- "WIN"
  interval_result[which(
    is.finite(ratio_low) & is.finite(ratio_high) & ratio_low >= 1 / margin & ratio_high <= margin
  )] <- "TIE"
  if (any(resolved & interval_result != result)) {
    stop(sprintf("%s relative result disagrees with the confidence interval", label))
  }
  invisible(rows)
}

validate_product_metrics <- function(rows, policy = benchmark_timing_policy()) {
  label <- "product metrics"
  required <- c(
    "run_id", "group_id", "runner", "implementation_role", "comparison_tier",
    "input_fingerprint", "kernel_id", "contract_version", "fixture_version", "path_kind",
    "representation_strategy", "mutation_policy", "setup_policy",
    "measurement_status", "report_status", "claim_eligible", "reason", "owner",
    "row_over_zigr_ratio", "row_over_zigr_ratio_ci_low", "row_over_zigr_ratio_ci_high",
    "row_relative_to_zigr", "noise_status", "comparison_reason", "n_iterations", "sample_stage",
    "zigr_n_iterations", "zigr_sample_stage", "timer_noise_status", "zigr_timer_noise_status"
  )
  report_require_columns(rows, c(required, report_measurement_columns()), label)
  report_require_unique(rows, c("group_id", "runner"), label)
  report_require_one_value(rows, "run_id", label)
  linked <- report_linked_runners()
  for (group in unique(as.character(rows$group_id))) {
    runners <- sort(as.character(rows$runner[rows$group_id == group]))
    if (!identical(runners, sort(linked))) {
      stop(sprintf("%s group %s does not contain the exact linked runner set", label, group))
    }
  }
  product <- rows$runner %in% report_product_runners()
  eligible <- as.logical(rows$claim_eligible)
  qualified <- product & rows$implementation_role == "product_public_path" &
    rows$comparison_tier == "tier_a" & rows$measurement_status == "PASS" &
    rows$report_status == "PRODUCT_PASS" & rows$row_relative_to_zigr %in% c("LOSS", "WIN", "TIE")
  if (anyNA(eligible) || !identical(eligible, qualified)) stop("product metrics claim eligibility differs from Tier A product PASS rows")
  if (any(!product & eligible)) stop("product metrics promote a linked control row")
  if (any(product & !(rows$report_status %in% c("PRODUCT_PASS", "PRODUCT_GAP", "EXCLUDED_DIAGNOSTIC"))) ||
      any(!product & rows$report_status != "LINKED_BASELINE")) {
    stop("product metrics contain an invalid report status")
  }
  for (group in unique(as.character(rows$group_id))) {
    tier_a <- rows[
      rows$group_id == group & rows$runner %in% report_product_runners() &
        rows$implementation_role == "product_public_path" & rows$comparison_tier == "tier_a",
      , drop = FALSE
    ]
    for (field in c(
      "input_fingerprint", "kernel_id", "contract_version", "fixture_version", "path_kind",
      "representation_strategy", "mutation_policy", "setup_policy"
    )) {
      values <- unique(as.character(tier_a[[field]]))
      if (length(values) != 1L || is.na(values) || !nzchar(values)) {
        stop(sprintf("product metrics Tier A group %s has mixed %s", group, field))
      }
    }
  }
  visible_gap <- rows$report_status %in% c("PRODUCT_GAP", "EXCLUDED_DIAGNOSTIC")
  if (any(visible_gap & (!nzchar(as.character(rows$reason)) | !nzchar(as.character(rows$owner))))) {
    stop("product metrics contain a gap without reason and owner")
  }
  report_validate_comparisons(
    rows,
    c("row_over_zigr_ratio", "row_over_zigr_ratio_ci_low", "row_over_zigr_ratio_ci_high"),
    "row_relative_to_zigr", label, policy
  )
  invisible(rows)
}

validate_strategy_metrics <- function(rows, policy = benchmark_timing_policy()) {
  label <- "strategy metrics"
  required <- c(
    "run_id", "group_id", "runner", "implementation_role", "comparison_tier",
    "measurement_status", "report_status", "claim_eligible", "reason", "owner",
    "row_over_zigr_ratio", "row_over_zigr_ratio_ci_low", "row_over_zigr_ratio_ci_high",
    "row_relative_to_zigr", "noise_status", "comparison_reason", "n_iterations", "sample_stage",
    "zigr_n_iterations", "zigr_sample_stage", "timer_noise_status", "zigr_timer_noise_status"
  )
  report_require_columns(rows, c(required, report_measurement_columns()), label)
  report_require_unique(rows, c("group_id", "runner"), label)
  report_require_one_value(rows, "run_id", label)
  for (group in unique(as.character(rows$group_id))) {
    group_rows <- rows[rows$group_id == group, , drop = FALSE]
    if (!identical(sort(as.character(group_rows$runner)), sort(report_linked_runners()))) {
      stop(sprintf("%s group %s does not contain the exact linked runner set", label, group))
    }
    if (!any(group_rows$report_status %in% c("STRATEGY_PASS", "STRATEGY_CORRECTNESS_ONLY"))) {
      stop(sprintf("%s group %s has no visible strategy result", label, group))
    }
  }
  report_require_no_claims(rows, label)
  strategy <- rows$report_status %in% c("STRATEGY_PASS", "STRATEGY_CORRECTNESS_ONLY")
  if (any(strategy & !(
    rows$runner %in% report_product_runners() &
      rows$implementation_role == "product_public_path" & rows$comparison_tier == "tier_b"
  ))) stop("strategy metrics label a non-Tier-B product row as strategy evidence")
  if (any(rows$report_status == "STRATEGY_PASS" & rows$measurement_status != "PASS") ||
      any(rows$report_status == "STRATEGY_CORRECTNESS_ONLY" & rows$measurement_status != "CORRECTNESS_ONLY")) {
    stop("strategy metrics report status disagrees with measurement status")
  }
  report_validate_comparisons(
    rows,
    c("row_over_zigr_ratio", "row_over_zigr_ratio_ci_low", "row_over_zigr_ratio_ci_high"),
    "row_relative_to_zigr", label, policy
  )
  invisible(rows)
}

validate_r_baseline_metrics <- function(
    rows, expected_tasks, expected_fixtures, policy = benchmark_timing_policy()) {
  label <- "R baseline metrics"
  required <- c(
    "run_id", "universe", "item_id", "runner", "baseline_class", "measurement_status",
    "claim_eligible", "zigr_relative_to_r", "zigr_over_r_ratio", "zigr_over_r_ratio_ci_low",
    "zigr_over_r_ratio_ci_high", "owner", "backend_provenance", "timer_noise_status", "noise_status",
    "comparison_reason", "n_iterations", "sample_stage", "zigr_n_iterations", "zigr_sample_stage",
    "zigr_timer_noise_status"
  )
  report_require_columns(rows, c(required, report_measurement_columns()), label)
  report_require_unique(rows, c("universe", "item_id", "baseline_class"), label)
  report_require_one_value(rows, "run_id", label)
  task_ids <- as.character(rows$item_id[rows$universe == "task"])
  fixture_ids <- as.character(rows$item_id[rows$universe == "fixture"])
  if (!setequal(task_ids, expected_tasks) || length(task_ids) != length(expected_tasks)) {
    stop("R baseline metrics task coverage differs from the required matrix")
  }
  if (!setequal(fixture_ids, expected_fixtures) || length(fixture_ids) != length(expected_fixtures)) {
    stop("R baseline metrics fixture coverage differs from the required matrix")
  }
  if (any(!(rows$baseline_class %in% c("pure_r", "optimized_base_r", "pure_r_unrepresentable")))) {
    stop("R baseline metrics contain an invalid baseline class")
  }
  report_require_no_claims(rows, label)
  losses <- rows$zigr_relative_to_r == "LOSS"
  if (any(losses & rows$owner != "performance")) stop("R baseline metrics contain an unowned relative loss")
  if (any(!nzchar(as.character(rows$backend_provenance)))) stop("R baseline metrics contain blank backend provenance")
  report_validate_comparisons(
    rows,
    c("zigr_over_r_ratio", "zigr_over_r_ratio_ci_low", "zigr_over_r_ratio_ci_high"),
    "zigr_relative_to_r", label, policy
  )
  invisible(rows)
}

validate_control_metrics <- function(rows, policy = benchmark_timing_policy()) {
  label <- "control metrics"
  required <- c(
    "run_id", "universe", "item_id", "runner", "control_role", "measurement_status",
    "report_status", "claim_eligible", "zigr_relative_to_control", "zigr_over_control_ratio",
    "zigr_over_control_ratio_ci_low", "zigr_over_control_ratio_ci_high", "owner",
    "timer_noise_status", "noise_status", "comparison_reason", "n_iterations", "sample_stage",
    "zigr_n_iterations", "zigr_sample_stage", "zigr_timer_noise_status"
  )
  report_require_columns(rows, c(required, report_measurement_columns()), label)
  report_require_unique(rows, c("universe", "item_id", "runner", "control_role"), label)
  report_require_one_value(rows, "run_id", label)
  if (any(!(rows$control_role %in% c("c_control", "language_control")))) {
    stop("control metrics contain an invalid control role")
  }
  if (any(rows$report_status != "CONTROL_ONLY")) stop("control metrics contain a promoted status")
  report_require_no_claims(rows, label)
  losses <- rows$zigr_relative_to_control == "LOSS"
  if (any(losses & rows$owner != "performance")) stop("control metrics contain an unowned relative loss")
  report_validate_comparisons(
    rows,
    c("zigr_over_control_ratio", "zigr_over_control_ratio_ci_low", "zigr_over_control_ratio_ci_high"),
    "zigr_relative_to_control", label, policy
  )
  invisible(rows)
}

validate_diagnostic_metrics <- function(rows, expected_task_keys) {
  label <- "diagnostic metrics"
  required <- c(
    "run_id", "universe", "item_id", "runner", "measurement_status", "claim_eligible", "source_label",
    "exclusion_reason", "owner", "timer_noise_status", "noise_status"
  )
  report_require_columns(rows, c(required, report_measurement_columns()), label)
  report_require_unique(rows, c("universe", "item_id", "runner"), label)
  report_require_one_value(rows, "run_id", label)
  if (any(rows$universe != "task")) stop("diagnostic metrics contain a non-task row")
  task_rows <- rows[rows$universe == "task", , drop = FALSE]
  actual_task_keys <- paste(task_rows$runner, task_rows$item_id, sep = "\r")
  if (!setequal(actual_task_keys, expected_task_keys) || length(actual_task_keys) != length(expected_task_keys)) {
    stop("diagnostic metrics task coverage differs from the historical matrix")
  }
  report_require_no_claims(rows, label)
  if (any(!nzchar(as.character(rows$exclusion_reason)) | !nzchar(as.character(rows$owner)))) {
    stop("diagnostic metrics contain an invisible exclusion")
  }
  invisible(rows)
}

validate_capability_matrix <- function(rows, expected_keys) {
  label <- "capability matrix"
  required <- c(
    "run_id", "runner", "fixture", "source_class", "source_paths", "verification_digest",
    "fixture_result", "gap_reason", "owner", "claim_eligible"
  )
  report_require_columns(rows, required, label)
  report_require_unique(rows, c("runner", "fixture"), label)
  report_require_one_value(rows, "run_id", label)
  actual <- paste(rows$runner, rows$fixture, sep = "\r")
  if (!setequal(actual, expected_keys) || length(actual) != length(expected_keys)) {
    stop("capability matrix coverage differs from the evidence matrix")
  }
  if (any(!nzchar(as.character(rows$source_class)) |
          !nzchar(as.character(rows$source_paths)) |
          !nzchar(as.character(rows$verification_digest)) |
          !nzchar(as.character(rows$fixture_result)) |
          !nzchar(as.character(rows$owner)))) {
    stop("capability matrix contains incomplete source or ownership evidence")
  }
  gaps <- rows$fixture_result == "GAP"
  if (any(gaps & !nzchar(as.character(rows$gap_reason)))) stop("capability matrix contains a gap without reason")
  report_require_no_claims(rows, label)
  invisible(rows)
}

validate_safety_results <- function(rows, expected_runners) {
  label <- "safety results"
  required <- c(
    "run_id", "runner", "proof_status", "claim_eligible", "allocation_status", "protection_status",
    "recovery_status", "external_pointer_status", "altrep_callback_status", "finalizer_status",
    "source_ledger_identity_digest", "artifact_digest"
  )
  report_require_columns(rows, required, label)
  report_require_unique(rows, "runner", label)
  report_require_one_value(rows, "run_id", label)
  if (!setequal(as.character(rows$runner), expected_runners) || nrow(rows) != length(expected_runners)) {
    stop("safety results runner coverage differs from the run manifest")
  }
  if (any(rows$proof_status != "PASS")) stop("safety results contain a failing proof")
  status_fields <- c(
    "allocation_status", "protection_status", "recovery_status", "external_pointer_status",
    "altrep_callback_status", "finalizer_status"
  )
  if (any(!vapply(rows[status_fields], function(values) all(values %in% c("PASS", "NOT_APPLICABLE")), logical(1)))) {
    stop("safety results contain an invalid domain status")
  }
  report_require_no_claims(rows, label)
  invisible(rows)
}
