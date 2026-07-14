evidence_schema_vocabulary <- function() {
  list(
    version = "p4.0-2026-07-13-r1",
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
      "immutable", "fresh_input_required", "stateful_reset_required", "rng_reset_required", "unclassified"
    ),
    setup_policies = c("setup_outside_timer", "legacy_shared_instance", "not_timed", "unclassified")
  )
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
  if (any(rows$timing_eligible & (!rows$executable | rows$status == "fixture_invalid" | rows$evidence_use == "gap"))) {
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
      for (field in c("contract_version", "kernel_id", "representation_strategy", "setup_policy")) {
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
  task_rows <- evidence_expand_groups(raw$task_dispositions, raw$task_defaults, "task", task_sets)
  fixture_rows <- evidence_expand_groups(raw$fixture_dispositions, raw$fixture_defaults, "fixture")
  validate_evidence_rows(task_rows, "task evidence")
  validate_evidence_rows(fixture_rows, "fixture evidence")
  validate_evidence_coverage(task_rows, runners, task_sets$all_tasks, "task")
  validate_evidence_coverage(fixture_rows, runners, fixtures, "fixture")
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

hydrate_runner_config <- function(manifest, cfg, runner_name, evidence, check_legacy = TRUE) {
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
  legacy <- evidence_values(cfg$optional_tasks)
  if (check_legacy && !is.null(cfg$optional_tasks) && !identical(sort(legacy), sort(optional_tasks))) {
    stop(sprintf("runner %s legacy optional_tasks differs from evidence dispositions", runner_name))
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
        reason = as.character(row$reason),
        owner = as.character(row$owner),
        implementation_role = as.character(row$implementation_role),
        evidence_use = as.character(row$evidence_use),
        path_kind = as.character(row$path_kind),
        public_path = as.character(row$public_path),
        representation_strategy = as.character(row$representation_strategy),
        kernel_id = as.character(row$kernel_id),
        contract_version = as.character(row$contract_version),
        fixture_version = as.character(row$fixture_version),
        comparison_tier = as.character(row$comparison_tier),
        mutation_policy = as.character(row$mutation_policy),
        setup_policy = as.character(row$setup_policy),
        comparison_group = as.character(row$comparison_group),
        timing_eligible = isTRUE(row$timing_eligible)
      )
    })
  }
  records
}
