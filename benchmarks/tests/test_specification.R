#!/usr/bin/env Rscript

script_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(script_argument) != 1L) stop("cannot determine specification test location")
script_path <- normalizePath(sub("^--file=", "", script_argument[[1L]]))
root_dir <- normalizePath(file.path(dirname(script_path), ".."))

source(file.path(root_dir, "lib", "specification.R"))
source(file.path(root_dir, "lib", "measurement.R"))
source(file.path(root_dir, "lib", "provenance.R"))
source(file.path(root_dir, "lib", "run_manifest.R"))

expect_true <- function(condition, label) {
  if (!isTRUE(condition)) stop(sprintf("assertion failed: %s", label), call. = FALSE)
}

expect_error <- function(label, expression, pattern) {
  error <- tryCatch({
    force(expression)
    NULL
  }, error = function(condition) condition)
  if (is.null(error)) stop(sprintf("negative test did not fail: %s", label), call. = FALSE)
  if (!grepl(pattern, conditionMessage(error), perl = TRUE)) {
    stop(sprintf("negative test %s failed for the wrong reason: %s", label, conditionMessage(error)), call. = FALSE)
  }
  invisible(error)
}

clone <- function(value) unserialize(serialize(value, NULL))

manifest <- load_task_manifest(root_dir)
evidence <- load_evidence_manifest(root_dir, manifest)
runner_configs <- load_runner_configs(root_dir)

expect_true(
  identical(names(runner_configs), evidence_schema_vocabulary()$runners),
  "runner config registry has canonical complete ordering"
)
runner_document <- jsonlite::fromJSON(file.path(root_dir, "runners.json"), simplifyVector = FALSE)
assert_invalid_runner_document <- function(label, document, pattern) {
  sandbox <- tempfile("runner-config-")
  dir.create(sandbox)
  on.exit(unlink(sandbox, recursive = TRUE), add = TRUE)
  jsonlite::write_json(document, file.path(sandbox, "runners.json"), auto_unbox = TRUE, pretty = TRUE)
  expect_error(label, load_runner_configs(sandbox), pattern)
}
extra_field <- clone(runner_document)
extra_field$note <- "unowned schema drift"
assert_invalid_runner_document("runner registry rejects extra fields", extra_field, "only schema_version and runners")
mismatched_name <- clone(runner_document)
mismatched_name$runners$r$name <- "base_r"
assert_invalid_runner_document("runner registry rejects key/name drift", mismatched_name, "key/name mismatch for r")
missing_runner <- clone(runner_document)
missing_runner$runners$cpp11 <- NULL
assert_invalid_runner_document("runner registry rejects coverage loss", missing_runner, "coverage differs")
expect_true(
  identical(
    validate_cli_arguments(c("--runner=r", "--check-only"), "runner", "check-only", "test"),
    c("--runner=r", "--check-only")
  ),
  "strict CLI accepts each declared option once"
)
expect_error(
  "strict CLI rejects typo",
  validate_cli_arguments("--runer=r", "runner", label = "test"),
  "unknown test argument"
)
expect_error(
  "strict CLI rejects duplicate",
  validate_cli_arguments(c("--runner=r", "--runner=zigr"), "runner", label = "test"),
  "repeated test argument"
)
expect_error(
  "strict CLI rejects empty value",
  validate_cli_arguments("--runner=", "runner", label = "test"),
  "requires a value"
)
worker_skip_probe <- suppressWarnings(system2(
  "Rscript",
  c(
    "benchmark_worker.R", "--runner=r", "--mode=timing",
    paste0("--output-root=", tempfile("worker-probe-")), "--tasks=vector_sum",
    "--measurement-samples=1", "--batch-repetitions=vector_sum:1",
    paste0("--master-seed=", benchmark_master_seed()), "--skip-probes"
  ),
  stdout = TRUE, stderr = TRUE
))
expect_true(
  identical(attr(worker_skip_probe, "status"), 1L) &&
    any(grepl("allowed only for later sizing rounds", worker_skip_probe, fixed = TRUE)),
  "the worker cannot suppress timing probes"
)
expect_true(identical(parse_task_filter("1,7,86"), c(1L, 7L, 86L)), "task filter parses exact positive integers")
expect_true(
  identical(
    select_task_ids(c("07a_protect_shallow", "07b_protect_scaling", "52_boundary_scalar_generated"), c(7L, 52L)),
    c("07a_protect_shallow", "07b_protect_scaling", "52_boundary_scalar_generated")
  ),
  "numeric task selection retains every distinct task with the selected prefix"
)
expect_true(
  identical(parse_csv_option("07a_protect_shallow,07b_protect_scaling", "task ID filter"),
            c("07a_protect_shallow", "07b_protect_scaling")),
  "internal task ID selection keeps tasks that share a numeric prefix distinct"
)
expect_true(
  identical(unname(vapply(c("tasks", "fixtures", "all"), parse_benchmark_suite, character(1))), c("tasks", "fixtures", "all")) &&
    benchmark_run_includes(list(suite = "tasks"), "task") &&
    !benchmark_run_includes(list(suite = "tasks"), "fixture") &&
    benchmark_run_promotion_eligible(list(suite = "all", full_matrix = TRUE, measurement_mode = "timed")) &&
    !benchmark_run_promotion_eligible(list(suite = "fixtures", full_matrix = TRUE, measurement_mode = "timed")),
  "suite selection admits only its declared benchmark universe"
)
expect_error(
  "suite selection rejects an unknown value",
  parse_benchmark_suite("task"),
  "must be tasks, fixtures, or all"
)
for (case in list(
  list(label = "task filter rejects text", value = "1,no", pattern = "positive integers"),
  list(label = "task filter rejects empty item", value = "1,,2", pattern = "empty or padded"),
  list(label = "task filter rejects normalized duplicate", value = "1,01", pattern = "duplicate task numbers")
)) {
  expect_error(case$label, parse_task_filter(case$value), case$pattern)
}

bad_task_specs <- benchmark_task_specs()
bad_task_specs[[1L]]$name <- "duplicate display metadata"
expect_error("task specs reject unsupported metadata", validate_task_specs(manifest, bad_task_specs), "unsupported fields: name")
expect_true(
  !any(c("input_factory", "aggregate") %in% names(manifest)),
  "task manifest derives input ownership and comparison eligibility instead of copying constants"
)
duplicated_manifest_policy <- manifest
duplicated_manifest_policy$aggregate <- duplicated_manifest_policy$comparison_policy == "comparable"
expect_error(
  "task manifest rejects duplicated comparison policy",
  validate_task_manifest(duplicated_manifest_policy),
  "unsupported columns: aggregate"
)

expect_true(nrow(evidence$tasks) == 83L * 7L, "complete task disposition matrix")
expect_true(nrow(evidence$fixture_rows) == 12L * 7L, "complete fixture disposition matrix")
expect_true(identical(evidence$task_sets$all_tasks, as.character(manifest$task)), "canonical task order")
expect_true(is.null(evidence$raw$task_sets$all_tasks), "evidence manifest does not copy the canonical task universe")
expect_true(
  identical(evidence$raw$task_defaults$kernel_id, "catalog:evidence_task_kernel_id") &&
    identical(evidence$raw$task_defaults$contract_version, "catalog:evidence_task_contract_version"),
  "raw task defaults identify the exact catalogs that replace their derivation markers"
)
expect_true(
  identical(evidence$tasks$timing_eligible, evidence$tasks$executable) &&
    sum(evidence$tasks$timing_eligible) == 368L,
  "every executable historical task is admitted for source-matched timing"
)
expected_fixture_timing <- evidence$fixture_rows$executable & evidence$fixture_rows$fixture != "F11"
expect_true(
  identical(evidence$fixture_rows$timing_eligible, expected_fixture_timing) &&
    sum(evidence$fixture_rows$timing_eligible) == 72L,
  "every executable fixture except correctness-only F11 is admitted for timing"
)
task_tiers <- table(factor(
  evidence$tasks$comparison_tier,
  levels = c("tier_a", "tier_b", "tier_c", "tier_d", "gap")
))
expect_true(
  identical(unname(as.integer(task_tiers)), c(14L, 9L, 142L, 203L, 213L)),
  "task tiers contain exact products, strategy products, controls, diagnostics, and gaps"
)
exact_product_tasks <- sort(unlist(evidence$raw$task_sets$exact_generated_product_tasks, use.names = FALSE))
tier_a_tasks <- evidence$tasks[evidence$tasks$comparison_tier == "tier_a", c("runner", "task"), drop = FALSE]
expect_true(
  nrow(tier_a_tasks) == 2L * length(exact_product_tasks) &&
    identical(sort(unique(tier_a_tasks$runner)), c("cpp11", "zigr")) &&
    identical(sort(unique(tier_a_tasks$task)), exact_product_tasks) &&
    all(table(tier_a_tasks$task) == 2L),
  "Tier A is limited to source-verified matching zigr and cpp11 generated task paths"
)
placeholder_fields <- c(
  "kernel_id", "contract_version", "fixture_version", "representation_strategy",
  "mutation_policy", "setup_policy"
)
expect_true(
  !any(grepl(
    "unverified|unclassified|legacy|declared-kernel|verified-source-catalog|verified-task-contract|^catalog:",
    unlist(evidence$tasks[placeholder_fields], use.names = FALSE)
  )),
  "no normalized task evidence retains a coarse or generic kernel placeholder"
)
expected_contracts <- unname(vapply(as.character(evidence$tasks$task), evidence_task_contract_version, character(1)))
expected_mutation <- unname(vapply(as.character(evidence$tasks$task), task_mutation_policy, character(1)))
expect_true(
  identical(as.character(evidence$tasks$contract_version), expected_contracts),
  "every task cell uses the exact stable or revised contract version"
)
executable_task_rows <- evidence$tasks[evidence$tasks$executable, , drop = FALSE]
expected_kernels <- mapply(
  evidence_task_kernel_id,
  as.character(executable_task_rows$runner),
  as.character(executable_task_rows$task),
  as.character(executable_task_rows$implementation_role),
  USE.NAMES = FALSE
)
expected_strategies <- mapply(
  evidence_task_representation_strategy,
  as.character(executable_task_rows$runner),
  as.character(executable_task_rows$task),
  as.character(executable_task_rows$implementation_role),
  USE.NAMES = FALSE
)
expected_uses <- mapply(
  evidence_task_evidence_use,
  as.character(evidence$tasks$runner),
  as.character(evidence$tasks$task),
  as.character(evidence$tasks$implementation_role),
  as.character(evidence$tasks$comparison_tier),
  USE.NAMES = FALSE
)
expect_true(
  identical(as.character(executable_task_rows$kernel_id), expected_kernels) &&
    identical(as.character(executable_task_rows$representation_strategy), expected_strategies),
  "every executable task cell matches the exact source-backed kernel and representation catalogs"
)
expect_true(
  identical(as.character(evidence$tasks$evidence_use), expected_uses),
  "every task cell matches the role- and tier-specific evidence-use catalog"
)
native_invariant_tasks <- sort(as.character(manifest$task[manifest$correctness_policy == "native_invariant"]))
expect_true(
  identical(sort(evidence_native_invariant_tasks()), native_invariant_tasks),
  "the evidence catalog and task manifest agree on every native-invariant task"
)
c_diagnostics <- sort(as.character(evidence$tasks$task[
  evidence$tasks$runner == "c_call" & evidence$tasks$evidence_use == "diagnostic_control"
]))
expect_true(
  identical(c_diagnostics, intersect(native_invariant_tasks, as.character(evidence$tasks$task[
    evidence$tasks$runner == "c_call" & evidence$tasks$executable
  ]))),
  "registered C native invariants are diagnostic controls rather than false kernel comparisons"
)
kernel_by_runner <- function(task_id) {
  rows <- executable_task_rows[executable_task_rows$task == task_id, c("runner", "kernel_id"), drop = FALSE]
  setNames(as.character(rows$kernel_id), as.character(rows$runner))
}
expect_true(
  identical(
    unname(kernel_by_runner("11_sexp_inspect")[c("c_call", "rcpp", "extendr")]),
    rep("five-cached-object-type-vector-real-query-10000-passes-v1", 3L)
  ) &&
    identical(
      unname(kernel_by_runner("11_sexp_inspect")[["zigr"]]),
      "five-object-type-vector-real-query-once-plus-10000-accumulation-passes-v1"
    ) &&
    identical(
      unname(kernel_by_runner("11_sexp_inspect")[["savvy"]]),
      "five-list-element-type-vector-real-query-10000-passes-v1"
    ),
  "task 11 records cached, hoisted, and repeated list-element query algorithms separately"
)
expect_true(
  grepl("hoisted-error-call", kernel_by_runner("09_longjmp_safety")[["zigr"]], fixed = TRUE) &&
    all(!grepl(
      "hoisted-error-call",
      kernel_by_runner("09_longjmp_safety")[c("c_call", "rcpp", "extendr", "savvy")],
      fixed = TRUE
    )),
  "task 09 records zigr's hoisted error call separately from per-pass construction"
)
expect_true(
  grepl("positional-column-iteration", kernel_by_runner("15_dataframe_filter")[["extendr"]], fixed = TRUE) &&
    all(grepl(
      "named-column-lookup",
      kernel_by_runner("15_dataframe_filter")[c("c_call", "rcpp", "savvy", "zigr")],
      fixed = TRUE
    )),
  "task 15 separates extendr's positional access from the named-column implementations"
)
expect_true(
  all(grepl(
    "direct-eval",
    kernel_by_runner("39_r_eval")[c("c_call", "rcpp", "zigr")],
    fixed = TRUE
  )) &&
    all(grepl(
      "silent-try-eval",
      kernel_by_runner("39_r_eval")[c("extendr", "savvy")],
      fixed = TRUE
    )),
  "task 39 separates direct evaluation from silent try-evaluation"
)
expect_true(
  grepl("persistent-stream-xdr-v3", kernel_by_runner("41_serialize_roundtrip")[["zigr"]], fixed = TRUE) &&
    all(grepl(
      "direct-eval",
      kernel_by_runner("41_serialize_roundtrip")[c("c_call", "rcpp")],
      fixed = TRUE
    )) &&
    all(grepl(
      "silent-try-eval",
      kernel_by_runner("41_serialize_roundtrip")[c("extendr", "savvy")],
      fixed = TRUE
    )),
  "task 41 separates persistent-stream, direct-eval, and silent-try-eval serialization paths"
)
expect_true(
  all(executable_task_rows$representation_strategy[
    executable_task_rows$task == "15_dataframe_filter" &
      executable_task_rows$implementation_role != "optimized_base_r"
  ] == "mixed") &&
    all(executable_task_rows$representation_strategy[executable_task_rows$task == "16_list_access"] == "element_access") &&
    all(executable_task_rows$representation_strategy[executable_task_rows$task %in% c("20_factor_ops", "21_attrib_ops")] == "runtime_service"),
  "data-frame, nested-list, factor, and attribute rows disclose their actual representation services"
)
expect_true(
  all(evidence$tasks$mutation_policy[evidence$tasks$executable] == expected_mutation[evidence$tasks$executable]) &&
    all(evidence$tasks$mutation_policy[!evidence$tasks$executable] == "not_applicable"),
  "every executable task has its exact mutation policy and every gap is not applicable"
)
expect_true(
  all(evidence$tasks$setup_policy[evidence$tasks$executable] == "setup_outside_timer") &&
    all(evidence$tasks$setup_policy[!evidence$tasks$executable] == "not_timed") &&
    all(evidence$tasks$fixture_version == "task-input-v2") &&
    identical(as.character(evidence$tasks$comparison_group), paste0("task:", evidence$tasks$task)),
  "task input, setup, and comparison identities are exact for all 581 cells"
)
expect_true(
  all(evidence$fixture_rows$setup_policy[evidence$fixture_rows$timing_eligible] == "setup_outside_timer") &&
    all(evidence$fixture_rows$setup_policy[!evidence$fixture_rows$timing_eligible] == "not_timed"),
  "fixture setup policy agrees with timing admission"
)
revised_contract_tasks <- c(
  "17_string_concat", "18_string_nchar", "19_string_encoding",
  "20_factor_ops", "41_serialize_roundtrip", "42_external_ptr", "43_rng_stress",
  "72_boundary_external_method_generated", "73_boundary_external_method_handwritten"
)
expect_true(
  all(grepl(":v2$", evidence$tasks$contract_version[evidence$tasks$task %in% revised_contract_tasks])) &&
    all(grepl(":v1$", evidence$tasks$contract_version[!evidence$tasks$task %in% revised_contract_tasks])),
  "only semantic or lifecycle repairs bump the historical task contract"
)
expect_true(
  all(evidence$tasks$owner[evidence$tasks$executable] == "accepted_evidence") &&
    all(nzchar(evidence$tasks$owner[!evidence$tasks$executable])),
  "every executable row routes to source-matched correctness and every gap has an owner"
)
task_names <- setNames(manifest$display_name, manifest$task)
expect_true(
  identical(task_names[["17_string_concat"]], "Encoding-aware string concatenation") &&
    identical(task_names[["20_factor_ops"]], "Factor conversion with 100-level vocabulary") &&
    identical(task_names[["24_long_vector_idx"]], "Compact ALTREP sampled indexing"),
  "task 17, task 20, and task 24 names match their repaired contracts"
)
fixture_tiers <- table(factor(
  evidence$fixture_rows$comparison_tier,
  levels = c("tier_a", "tier_b", "tier_c", "tier_d", "gap")
))
expect_true(
  identical(unname(as.integer(fixture_tiers)), c(9L, 47L, 23L, 0L, 5L)),
  "fixture tiers contain exact products, strategy products, controls, no diagnostics, and gaps"
)
expect_true(
  identical(
    sort(unique(evidence$fixture_rows$fixture[evidence$fixture_rows$comparison_tier == "tier_a"])),
    c("F01", "F12")
  ),
  "Tier A is limited to fixtures with matching public semantics, boundary class, and representation"
)
for (fixture in c("F01", "F12")) {
  product_rows <- evidence$fixture_rows[
    evidence$fixture_rows$fixture == fixture &
      evidence$fixture_rows$runner %in% c("zigr", "rcpp", "cpp11", "extendr", "savvy"),
    , drop = FALSE
  ]
  expect_true(
    nrow(product_rows) == 5L &&
      all(product_rows$comparison_tier == "tier_a" | !product_rows$executable) &&
      all(product_rows$comparison_tier[product_rows$executable] == "tier_a"),
    sprintf("%s Tier A group contains all five products or an explicit gap", fixture)
  )
}
f04_products <- evidence$fixture_rows[
  evidence$fixture_rows$fixture == "F04" &
    evidence$fixture_rows$implementation_role == "product_public_path",
  c("runner", "comparison_tier", "representation_strategy"), drop = FALSE
]
expect_true(
  all(f04_products$comparison_tier == "tier_b") &&
    identical(
      setNames(f04_products$representation_strategy, f04_products$runner)[
        c("zigr", "rcpp", "cpp11", "extendr", "savvy")
      ],
      c(
        zigr = "region_access", rcpp = "materialized_r_vector",
        cpp11 = "region_access", extendr = "element_access",
        savvy = "materialized_r_vector"
      )
    ),
  "F04 records the measured per-product ALTREP strategies as Tier B"
)
expect_true(
  all(evidence$fixture_rows$mutation_policy[
    evidence$fixture_rows$fixture == "F10" & evidence$fixture_rows$executable
  ] == "stateful_reset_required"),
  "every executable native-state fixture declares reset-required mutation"
)
expect_true(
  all(evidence$tasks$path_kind[evidence$tasks$implementation_role == "product_public_path"] %in%
    c("generated_typed", "generated_public_adapter")) &&
    all(evidence$fixture_rows$path_kind[evidence$fixture_rows$implementation_role == "product_public_path"] %in%
      c("generated_typed", "generated_public_adapter")),
  "every product public path is generated and is typed or an explicit public adapter"
)
zigr_task70 <- evidence$tasks[
  evidence$tasks$runner == "zigr" & evidence$tasks$task == "70_boundary_schema_generated",
  , drop = FALSE
]
expect_true(
  nrow(zigr_task70) == 1L && zigr_task70$path_kind == "generated_public_adapter" &&
    zigr_task70$comparison_tier == "tier_b",
  "zigr task 70 retains the accepted explicit fixed-schema adapter label"
)

expected_executable <- c(c_call = 70L, cpp11 = 11L, extendr = 44L, r = 72L, rcpp = 44L, savvy = 44L, zigr = 83L)
actual_executable <- vapply(names(expected_executable), function(runner) {
  sum(evidence$tasks$runner == runner & evidence$tasks$executable)
}, integer(1))
expect_true(identical(actual_executable, expected_executable), "executable coverage matches the declared R split")
expected_task_tiers_by_runner <- rbind(
  c_call = c(gap = 13L, tier_a = 0L, tier_b = 0L, tier_c = 70L, tier_d = 0L),
  cpp11 = c(gap = 72L, tier_a = 7L, tier_b = 3L, tier_c = 0L, tier_d = 1L),
  extendr = c(gap = 39L, tier_a = 0L, tier_b = 0L, tier_c = 0L, tier_d = 44L),
  r = c(gap = 11L, tier_a = 0L, tier_b = 0L, tier_c = 72L, tier_d = 0L),
  rcpp = c(gap = 39L, tier_a = 0L, tier_b = 0L, tier_c = 0L, tier_d = 44L),
  savvy = c(gap = 39L, tier_a = 0L, tier_b = 0L, tier_c = 0L, tier_d = 44L),
  zigr = c(gap = 0L, tier_a = 7L, tier_b = 6L, tier_c = 0L, tier_d = 70L)
)
actual_task_tiers_by_runner <- with(evidence$tasks, table(
  factor(runner, levels = rownames(expected_task_tiers_by_runner)),
  factor(comparison_tier, levels = colnames(expected_task_tiers_by_runner))
))
expect_true(
  identical(as.integer(actual_task_tiers_by_runner), as.integer(expected_task_tiers_by_runner)),
  "each runner has the exact source-backed product, control, diagnostic, and gap split"
)

expected_fixture_executable <- c(
  c_call = 12L, cpp11 = 9L, extendr = 12L, r = 11L, rcpp = 12L, savvy = 12L, zigr = 11L
)
actual_fixture_executable <- vapply(names(expected_fixture_executable), function(runner) {
  sum(evidence$fixture_rows$runner == runner & evidence$fixture_rows$executable)
}, integer(1))
expect_true(
  identical(actual_fixture_executable, expected_fixture_executable),
  "fixture execution coverage matches the implemented matrix"
)

disposition_snapshot <- run_disposition_records(evidence, "r", "52_boundary_scalar_generated")$r[[1L]]
expect_true(
  identical(
    names(disposition_snapshot),
    c(
      "task", "status", "executable", "reason", "evidence_use", "path_kind",
      "representation_strategy", "kernel_id", "contract_version", "timing_eligible"
    )
  ),
  "run disposition snapshots retain only execution-critical evidence fields"
)
expect_true(
  identical(disposition_snapshot$reason, "not_applicable"),
  "executable disposition snapshots do not repeat reporting prose"
)

for (runner in evidence$runners) {
  cfg <- runner_configs[[runner]]
  expect_true(is.null(cfg$optional_tasks), sprintf("%s has no authored optional_tasks", runner))
  hydrated <- hydrate_runner_config(manifest, cfg, runner, evidence)
  expected_optional <- nrow(manifest) - expected_executable[[runner]]
  expect_true(length(hydrated$optional_tasks) == expected_optional, sprintf("%s generated optional coverage", runner))
  export_names <- names(hydrated$exports)
  expect_true(
    length(export_names) == expected_executable[[runner]] && all(nzchar(export_names)),
    sprintf("%s export map remains named", runner)
  )
}

cpp11_f07 <- evidence$fixture_rows[evidence$fixture_rows$runner == "cpp11" & evidence$fixture_rows$fixture == "F07", , drop = FALSE]
cpp11_f10 <- evidence$fixture_rows[evidence$fixture_rows$runner == "cpp11" & evidence$fixture_rows$fixture == "F10", , drop = FALSE]
zigr_f08 <- evidence$fixture_rows[evidence$fixture_rows$runner == "zigr" & evidence$fixture_rows$fixture == "F08", , drop = FALSE]
zigr_f09 <- evidence$fixture_rows[evidence$fixture_rows$runner == "zigr" & evidence$fixture_rows$fixture == "F09", , drop = FALSE]
expect_true(nrow(cpp11_f07) == 1L && cpp11_f07$status == "product_gap", "cpp11 F07 remains a visible gap")
expect_true(
  nrow(cpp11_f10) == 1L && cpp11_f10$status == "product_gap" && !cpp11_f10$executable,
  "cpp11 F10 is an honest source-backed product gap"
)
expect_true(
  nrow(zigr_f08) == 1L && zigr_f08$status == "product_gap" && zigr_f08$owner == "product_capability",
  "zigr F08 does not disguise the untyped escape hatch as a product path"
)
expect_true(
  nrow(zigr_f09) == 1L && zigr_f09$path_kind == "generated_public_adapter",
  "zigr F09 retains the accepted explicit fixed-schema adapter label"
)
rcpp_fixture_adapters <- evidence$fixture_rows[
  evidence$fixture_rows$runner == "rcpp" &
    evidence$fixture_rows$path_kind == "generated_public_adapter",
  "fixture"
]
expect_true(
  identical(
    sort(rcpp_fixture_adapters),
    sort(c("F02", "F03", "F04", "F05", "F06", "F07", "F08", "F10", "F11"))
  ),
  "Rcpp strict RObject boundaries are explicit generated public adapters"
)

rcpp_executable <- evidence$tasks[evidence$tasks$runner == "rcpp" & evidence$tasks$executable, , drop = FALSE]
expect_true(
  sum(rcpp_executable$path_kind == "raw_ffi") == 43L &&
    sum(rcpp_executable$task == "05_fib_recursive" & rcpp_executable$path_kind == "handwritten_typed") == 1L,
  "Rcpp source exception remains distinct from the 43 raw paths"
)

r_baseline_rows <- evidence$tasks[evidence$tasks$runner == "r" & evidence$tasks$executable, , drop = FALSE][1:2, , drop = FALSE]
r_baseline_rows$implementation_role <- c("pure_r", "optimized_base_r")
r_baseline_rows$evidence_use <- c("semantic_oracle", "timed_baseline")
r_baseline_rows$path_kind <- c("pure_r", "optimized_base_r")
r_baseline_rows$timing_eligible <- TRUE
validate_evidence_rows(r_baseline_rows, "R baseline compatibility")
expect_true(
  all(evidence$tasks$implementation_role[evidence$tasks$runner == "r" & evidence$tasks$executable] %in% c("pure_r", "optimized_base_r")),
  "every executable R row has an explicit implementation class"
)
expect_true(
  all(!evidence$tasks$executable[
    evidence$tasks$runner == "r" & evidence$tasks$task %in% evidence$task_sets$r_unrepresentable_tasks
  ]),
  "native-unrepresentable R rows are explicit gaps"
)
r_roles <- table(factor(
  evidence$tasks$implementation_role[evidence$tasks$runner == "r"],
  levels = c("pure_r", "optimized_base_r", "capability_gap")
))
expect_true(
  identical(unname(as.integer(r_roles)), c(17L, 55L, 11L)),
  "R rows are split into 17 pure, 55 optimized, and 11 unrepresentable dispositions"
)

bad <- clone(evidence$raw)
bad$task_dispositions[[length(bad$task_dispositions) + 1L]] <- clone(bad$task_dispositions[[1L]])
expect_error(
  "duplicate keys",
  validate_and_expand_evidence_manifest(bad, manifest),
  "duplicate task disposition key"
)

bad <- clone(evidence$raw)
bad$vocabulary_version <- "invented-vocabulary"
expect_error(
  "unknown vocabulary version",
  validate_and_expand_evidence_manifest(bad, manifest),
  "unsupported evidence vocabulary version"
)

bad <- clone(evidence$raw)
bad$schema_version <- 2L
expect_error(
  "unknown schema version",
  validate_and_expand_evidence_manifest(bad, manifest),
  "unsupported evidence schema version"
)

bad <- clone(evidence$raw)
c_diagnostic_group <- which(vapply(bad$task_dispositions, function(group) {
  identical(as.character(group$runner), "c_call") &&
    identical(as.character(group$evidence_use), "diagnostic_control")
}, logical(1)))[[1L]]
bad$task_dispositions[[c_diagnostic_group]]$evidence_use <- "kernel_comparison"
expect_error(
  "authored evidence use differs from catalog",
  validate_and_expand_evidence_manifest(bad, manifest),
  "task evidence use differs from the exact catalog"
)

bad <- c(clone(evidence$raw), list(schema_version = 1L))
expect_error(
  "duplicate object field",
  validate_and_expand_evidence_manifest(bad, manifest),
  "evidence manifest must be a uniquely named object"
)

bad <- clone(evidence$raw)
group_index <- which(vapply(bad$task_dispositions, function(group) {
  identical(as.character(group$runner), "cpp11") && identical(as.character(group$status), "supported_and_executable")
}, logical(1)))[[1L]]
bad$task_dispositions[[group_index]]$exclude_tasks <- list("50_boundary_zero_generated")
expect_error(
  "missing task disposition",
  validate_and_expand_evidence_manifest(bad, manifest),
  "task disposition cells differ from the required matrix"
)

bad <- clone(evidence$raw)
fixture_group_index <- which(vapply(bad$fixture_dispositions, function(group) {
  identical(as.character(group$runner), "cpp11") && identical(as.character(group$status), "supported_and_executable")
}, logical(1)))[[1L]]
bad$fixture_dispositions[[fixture_group_index]]$exclude_fixtures <- list("F01")
expect_error(
  "missing product fixture cell",
  validate_and_expand_evidence_manifest(bad, manifest),
  "fixture disposition cells differ from the required matrix"
)

bad <- clone(evidence$raw)
bad$task_dispositions[[group_index]]$path_kind <- "raw_ffi"
expect_error(
  "raw path labeled product",
  validate_and_expand_evidence_manifest(bad, manifest),
  "labels a raw path as a product public path"
)

bad_rows <- evidence$tasks[evidence$tasks$implementation_role == "product_public_path", , drop = FALSE][1L, , drop = FALSE]
bad_rows$path_kind <- "handwritten_typed"
expect_error(
  "handwritten path labeled product",
  validate_evidence_rows(bad_rows, "negative evidence"),
  "product public path with an invalid path kind"
)

bad_rows <- evidence$tasks[evidence$tasks$implementation_role == "product_public_path", , drop = FALSE][1L, , drop = FALSE]
bad_rows$implementation_role <- "language_control"
bad_rows$evidence_use <- "diagnostic_control"
expect_error(
  "generated product path demoted to control",
  validate_evidence_rows(bad_rows, "negative evidence"),
  "language_control row with the wrong path kind"
)

bad_rows <- evidence$tasks[1L, , drop = FALSE]
bad_rows$implementation_role <- "pure_r"
bad_rows$path_kind <- "optimized_base_r"
expect_error(
  "optimized base R labeled pure R",
  validate_evidence_rows(bad_rows, "negative evidence"),
  "labels an optimized base-R path as pure R"
)

bad_rows <- evidence$tasks[!evidence$tasks$executable, , drop = FALSE][1L, , drop = FALSE]
bad_rows$timing_eligible <- TRUE
expect_error(
  "gap with timing data",
  validate_evidence_rows(bad_rows, "negative evidence"),
  "gap with timing data|complete gap record"
)

tier_a_rows <- evidence$tasks[
  evidence$tasks$task == "50_boundary_zero_generated" & evidence$tasks$runner %in% c("cpp11", "zigr"),
  , drop = FALSE
]
tier_a_rows$comparison_tier <- "tier_a"
tier_a_rows$comparison_group <- "negative-tier-a"
tier_a_rows$kernel_id <- "negative-kernel-v1"
tier_a_rows$representation_strategy <- c("borrowed_direct", "copied_contiguous")
tier_a_rows$setup_policy <- "setup_outside_timer"
expect_error(
  "Tier A mixed strategies",
  validate_evidence_rows(tier_a_rows, "negative evidence"),
  "Tier A group with mixed representation_strategy values"
)

tier_a_paths <- evidence$fixture_rows[
  evidence$fixture_rows$fixture == "F01" &
    evidence$fixture_rows$runner %in% c("rcpp", "zigr"),
  , drop = FALSE
]
tier_a_paths$path_kind[[1L]] <- "generated_public_adapter"
expect_error(
  "Tier A mixed generated boundary classes",
  validate_evidence_rows(tier_a_paths, "negative evidence"),
  "Tier A group with mixed path_kind values"
)

bad_rows <- evidence$tasks[1L, , drop = FALSE]
bad_rows$path_kind <- "invented_path"
expect_error(
  "unrecognized path kind",
  validate_evidence_rows(bad_rows, "negative evidence"),
  "unrecognized path_kind values"
)

bad_rows <- evidence$tasks[1L, , drop = FALSE]
bad_rows$comparison_tier <- "tier_unknown"
expect_error(
  "unrecognized comparison tier",
  validate_evidence_rows(bad_rows, "negative evidence"),
  "unrecognized comparison_tier values"
)

bad_rows <- evidence$tasks[!evidence$tasks$executable, , drop = FALSE][1L, , drop = FALSE]
bad_rows$reason <- ""
expect_error(
  "blank non-executable reason",
  validate_evidence_rows(bad_rows, "negative evidence"),
  "non-executable cell with a blank reason"
)

bad_rows <- evidence$tasks[!evidence$tasks$executable, , drop = FALSE][1L, , drop = FALSE]
bad_rows$owner <- ""
expect_error(
  "blank non-executable owner",
  validate_evidence_rows(bad_rows, "negative evidence"),
  "non-executable cell with a blank owner"
)

# Hard boundary table: catches swapped type, length, dimension, and name predicates.
result_contract_cases <- list(
  real_scalar = list(valid = list(0, NA_real_, NaN, -0.0), invalid = list(numeric(0), c(1, 2), 1L, matrix(1))),
  integer_scalar = list(valid = list(0L, NA_integer_), invalid = list(integer(0), 1:2, 1.0, matrix(1L))),
  real_vector = list(valid = list(numeric(0), 1.0, c(NA_real_, NaN)), invalid = list(1L, matrix(1.0), 1 + 1i)),
  complex_vector = list(valid = list(complex(0), 1 + 1i, NA_complex_), invalid = list(1.0, matrix(1 + 1i), 1L)),
  named_real_vector = list(valid = list(c(x = 1.0), setNames(numeric(0), character(0))),
                           invalid = list(1.0, c(1L), matrix(1.0))),
  real_matrix = list(valid = list(matrix(numeric(0), 0, 0), matrix(1.0, 1, 1)),
                     invalid = list(numeric(0), array(1.0, c(1, 1, 1)), matrix(1L))),
  real_list = list(valid = list(list(), list(1)), invalid = list(NULL, 1.0)),
  data_frame_list = list(valid = list(data.frame(), data.frame(x = 1)), invalid = list(list(), matrix(1))),
  named_list = list(valid = list(list(x = 1), setNames(list(), character(0))), invalid = list(list(1), NULL)),
  r_object = list(valid = list(0L, list(), NA), invalid = list(NULL))
)
for (contract in names(result_contract_cases)) {
  cases <- result_contract_cases[[contract]]
  expect_true(
    all(vapply(cases$valid, function(value) isTRUE(validate_result_contract(value, contract)$ok), logical(1))) &&
      all(vapply(cases$invalid, function(value) !isTRUE(validate_result_contract(value, contract)$ok), logical(1))),
    sprintf("%s contract rejects adjacent type, shape, length, and naming classes", contract)
  )
}
expect_true(
  !validate_result_contract(1.0, "invented_contract")$ok,
  "unknown result contracts fail closed"
)

configs <- lapply(names(runner_configs), function(runner) {
  hydrate_runner_config(manifest, runner_configs[[runner]], runner, evidence)
})
names(configs) <- names(runner_configs)

source(file.path(root_dir, "src", "r", "run_all.R"))
r_config <- runner_configs$r
r_map <- r_config$exports
r_references <- r_reference_map(r_config)
r_provenance <- build_run_r_provenance(
  manifest$task, r_map, r_references, manifest,
  evidence$tasks[evidence$tasks$runner == "r", , drop = FALSE]
)
r_provenance_by_task <- named_r_provenance_records(r_provenance, "runner_rows")
r_reference_by_task <- named_r_provenance_records(r_provenance, "reference_rows")
run_r_provenance <- compact_run_r_provenance(r_provenance)
run_r_provenance_by_task <- named_r_provenance_records(run_r_provenance, "runner_rows")
compare_r_provenance_records(
  run_r_provenance_by_task[["01_vectorsum"]], r_provenance_by_task[["01_vectorsum"]], "compact snapshot"
)
tampered_r_provenance <- run_r_provenance_by_task[["01_vectorsum"]]
tampered_r_provenance$record_digest <- paste0("0", substring(tampered_r_provenance$record_digest, 2L))
expect_error(
  "compact R provenance digest drift",
  compare_r_provenance_records(tampered_r_provenance, r_provenance_by_task[["01_vectorsum"]], "tampered snapshot"),
  "record digest differs"
)
reference_classes <- table(factor(
  vapply(r_reference_by_task, function(record) record$implementation_class, character(1)),
  levels = c("pure_r", "optimized_base_r")
))
expect_true(
  identical(unname(as.integer(reference_classes)), c(28L, 46L)),
  "R correctness uses 28 source-validated pure oracles and 46 explicitly optimized references"
)
expect_true(
  identical(
    unname(vapply(r_pure_reference_tasks(), function(task_id) {
      as.character(r_reference_by_task[[task_id]]$function_name)
    }, character(1))),
    unname(unlist(r_config$reference_overrides, use.names = FALSE))
  ),
  "every new pure-R reference override has its exact source function"
)
expect_equivalent <- function(actual, expected, label) {
  expect_true(
    isTRUE(all.equal(actual, expected, tolerance = sqrt(.Machine$double.eps), check.attributes = TRUE)),
    label
  )
}
oracle_vector <- c(-3.5, 0.25, 7.0, NA_real_)
expect_equivalent(r_oracle_vectorsum(oracle_vector[1:3]), sum(oracle_vector[1:3]), "pure vector-sum oracle")
expect_equivalent(
  r_oracle_broadcast(oracle_vector[1:3], 0.75),
  sum(oracle_vector[1:3] + 0.75),
  "pure broadcast oracle"
)
oracle_matrix <- matrix(as.double(seq_len(12L)), nrow = 3L)
expect_equivalent(r_oracle_transpose(oracle_matrix), t(oracle_matrix), "pure transpose oracle")
expect_equivalent(r_oracle_rowsums(oracle_matrix), rowSums(oracle_matrix), "pure row-sum oracle")
expect_equivalent(
  r_oracle_rowcol_means(oracle_matrix),
  list(rowMeans(oracle_matrix), colSums(oracle_matrix)),
  "pure row and column oracle"
)
oracle_list <- list(c(1.5, 8.0), c(-2.0, 9.0), c(4.25, 10.0))
expect_equivalent(r_bench_list_access(oracle_list), 3.75, "pure nested-list runner")
expect_equivalent(r_oracle_na_mean(c(1.0, NA_real_, NaN, 5.0)), 3.0, "pure missing-value mean oracle")
expect_true(is.nan(r_oracle_na_mean(c(NA_real_, NaN))), "pure missing-value oracle preserves NaN for an empty denominator")
expect_equivalent(r_oracle_altrep_min_max(257L), 256L, "pure ALTREP min-max oracle")
expect_true(identical(r_oracle_altrep_no_na(257L), 0L), "pure ALTREP missingness oracle")
oracle_record <- list(
  id = 42L, count = 7L, level = 3L, flag = TRUE, enabled = FALSE,
  ratio = 1.25, offset = -3.5, scale = 9.75,
  weights = c(0.5, 1.5), indices = c(3L, 1L)
)
expect_true(identical(r_oracle_struct_convert(oracle_record), oracle_record), "pure fixed-record oracle")
expect_true(
  identical(
    sort(unlist(r_provenance_by_task[["22_s4_slot_access"]]$backend_identity_keys)),
    c("methods_package", "r_runtime")
  ),
  "methods runtime has an exact package identity"
)
expect_true(
  identical(
    sort(unlist(r_provenance_by_task[["29_lm_fit"]]$backend_identity_keys)),
    c("fortran_runtime", "r_runtime", "stats_package")
  ),
  "stats Fortran backend has package and runtime identities"
)
records <- verify_source_paths(root_dir, configs, evidence, r_provenance)
keys <- vapply(records, function(record) paste(record$runner, record$task, sep = "\r"), character(1))
names(records) <- keys
record <- function(runner, task) records[[paste(runner, task, sep = "\r")]]

expect_true(length(records) == 83L * 7L, "complete source verification matrix")
expect_true(
  sum(vapply(records, function(row) identical(row$runner, "zigr") && isTRUE(row$product_eligible), logical(1))) == 13L,
  "only thirteen generated zigr boundary rows are product eligible"
)
expect_true(
  sum(vapply(records, function(row) identical(row$runner, "cpp11") && isTRUE(row$product_eligible), logical(1))) == 11L,
  "all configured cpp11 rows use the generated typed path"
)
expect_true(
  all(vapply(records, function(row) {
    !(row$runner %in% c("rcpp", "extendr", "savvy")) || !isTRUE(row$product_eligible)
  }, logical(1))),
  "current Rcpp, extendr, and Savvy rows are not accepted as product evidence"
)

raw_extendr <- unname(sort(vapply(records[vapply(records, function(row) {
  identical(row$runner, "extendr") && identical(row$source_class, "raw_ffi_substitution")
}, logical(1))], function(row) row$task, character(1))))
expect_true(
  identical(raw_extendr, sort(c("26_matmul", "38_struct_convert", "42_external_ptr", "43_rng_stress"))),
  "exact four raw extendr substitutions"
)
expect_true(
  sum(vapply(records, function(row) identical(row$runner, "c_call") && isTRUE(row$accepted_control), logical(1))) == 70L,
  "all configured C rows are registered controls"
)
expect_true(
  all(vapply(records, function(row) {
    !identical(row$runner, "savvy") || !isTRUE(row$executable) || isTRUE(row$configured_symbol_present)
  }, logical(1))),
  "every configured Savvy symbol has a handwritten C wrapper, macro wrapper, or Rust definition"
)

read_source <- function(path) paste(readLines(file.path(root_dir, path), warn = FALSE), collapse = "\n")
source_region <- function(source, start_marker, end_marker) {
  start <- regexpr(start_marker, source, fixed = TRUE)[[1L]]
  if (start < 1L) stop(sprintf("source region start is missing: %s", start_marker))
  tail <- substring(source, start)
  end <- regexpr(end_marker, tail, fixed = TRUE)[[1L]]
  if (end < 1L) stop(sprintf("source region end is missing: %s", end_marker))
  substring(tail, 1L, end - 1L)
}
appears_before <- function(source, first, second) {
  first_position <- regexpr(first, source, fixed = TRUE)[[1L]]
  second_position <- regexpr(second, source, fixed = TRUE)[[1L]]
  first_position > 0L && second_position > 0L && first_position < second_position
}
c_tasks_source <- read_source("src/c_call/tasks.c")
zigr_tasks_source <- read_source("src/zig/tasks.zig")
c_task41 <- source_region(c_tasks_source, "/* Task 41_serialize_roundtrip */", "/* Task 42_external_ptr */")
rcpp_source <- read_source("src/cpp/main.cpp")
extendr_source <- read_source("src/extendr/rust/src/lib.rs")
savvy_source <- read_source("src/savvy/rust/src/lib.rs")
zigr_task09 <- source_region(zigr_tasks_source, "const task_09_longjmp_safety", "const task_10_sexp_create")
zigr_task11 <- source_region(zigr_tasks_source, "const task_11_sexp_inspect", "const task_12_matrix_transpose")
c_task09 <- source_region(c_tasks_source, "/* Task 09_longjmp_safety */", "/* Task 10_sexp_create */")
c_task11 <- source_region(c_tasks_source, "/* Task 11_sexp_inspect */", "/* Task 12_matrix_transpose */")
zigr_task15 <- source_region(zigr_tasks_source, "const task_15_dataframe_filter", "const task_16_list_access")
c_task15 <- source_region(c_tasks_source, "/* Task 15_dataframe_filter */", "/* Task 16_list_access */")
zigr_task39 <- source_region(zigr_tasks_source, "const task_39_r_eval", "const task_40_r_tryeval")
c_task39 <- source_region(c_tasks_source, "/* Task 39_r_eval */", "/* Task 40_r_tryeval */")
zigr_task41 <- source_region(zigr_tasks_source, "const task_41_serialize_roundtrip", "const task_42_external_ptr")
zigr_eval_source <- read_source("../src/eval.zig")
zigr_serialize_source <- read_source("../src/serialize.zig")
rcpp_task09 <- source_region(rcpp_source, "SEXP rcpp_bench_longjmp_safety", "SEXP rcpp_bench_sexp_create")
rcpp_task11 <- source_region(rcpp_source, "SEXP rcpp_bench_sexp_inspect", "#define BLOCK 32")
rcpp_task15 <- source_region(rcpp_source, "SEXP rcpp_bench_dataframe_filter", "SEXP rcpp_bench_list_access")
rcpp_task39 <- source_region(rcpp_source, "SEXP rcpp_bench_r_eval", "SEXP rcpp_bench_r_tryeval")
extendr_task09 <- source_region(extendr_source, "fn extendr_bench_longjmp_safety", "fn extendr_bench_sexp_create")
extendr_task11 <- source_region(extendr_source, "fn extendr_bench_sexp_inspect", "fn extendr_bench_matrix_transpose")
extendr_task15 <- source_region(extendr_source, "fn extendr_bench_dataframe_filter", "fn extendr_bench_list_access")
extendr_task39 <- source_region(extendr_source, "fn extendr_bench_r_eval", "fn extendr_bench_r_tryeval")
savvy_task09 <- source_region(savvy_source, "fn savvy_bench_longjmp_safety__ffi", "ffi_stub!(savvy_bench_translate_c_cost__ffi")
savvy_task11 <- source_region(savvy_source, "fn savvy_bench_sexp_inspect__ffi", "fn savvy_bench_matrix_rowsums__ffi")
savvy_task15 <- source_region(savvy_source, "fn savvy_bench_dataframe_filter__ffi", "fn savvy_bench_list_access__ffi")
savvy_task39 <- source_region(savvy_source, "fn savvy_bench_r_eval__impl", "fn savvy_bench_r_tryeval__impl")
expect_true(
  appears_before(zigr_task09, "const stop_call", "for (0..repeats)") &&
    appears_before(c_task09, "for (int rep", "SEXP stop_call") &&
    appears_before(rcpp_task09, "for (int rep", "SEXP stop_call") &&
    appears_before(extendr_task09, "for rep in 0..EXTENDR_REPEATS", "let stop_call") &&
    appears_before(savvy_task09, "for rep in 0..SAVVY_REPEATS", "let stop_call"),
  "task 09 source supports the distinct hoisted and per-pass error-call kernel identities"
)
expect_true(
  appears_before(zigr_task11, "var per_elt", "for (0..10000)") &&
    appears_before(c_task11, "for (int iter", "TYPEOF(elts[i])") &&
    appears_before(rcpp_task11, "for (int iter", "TYPEOF(elts[i])") &&
    appears_before(extendr_task11, "for _ in 0..10000", "TYPEOF(s)") &&
    appears_before(savvy_task11, "for _ in 0..10000", "VECTOR_ELT(x, i)"),
  "task 11 source supports the cached, hoisted, and repeated list-element query identities"
)
expect_true(
  grepl("let (_, x_robj) = it.next()", extendr_task15, fixed = TRUE) &&
    grepl("columnIndex(\"x\")", zigr_task15, fixed = TRUE) &&
    all(vapply(c(c_task15, rcpp_task15, savvy_task15), grepl, logical(1), pattern = "STRING_ELT", fixed = TRUE)),
  "task 15 source supports the positional extendr and named-column kernel identities"
)
expect_true(
  grepl("callIn(\"sum\"", zigr_task39, fixed = TRUE) &&
    grepl("return R.Rf_eval(call_expr.get(), envir);", zigr_eval_source, fixed = TRUE) &&
    grepl("Rf_eval(sum_call", c_task39, fixed = TRUE) &&
    grepl("Rf_eval(sum_call", rcpp_task39, fixed = TRUE) &&
    grepl("R_tryEvalSilent(sum_call", extendr_task39, fixed = TRUE) &&
    grepl("R_tryEvalSilent(sum_call", savvy_task39, fixed = TRUE),
  "task 39 source supports direct-eval and silent-try-eval kernel identities"
)
rcpp_task41 <- source_region(rcpp_source, "SEXP rcpp_bench_serialize_roundtrip", "struct RcppBenchmarkState")
extendr_task41 <- source_region(extendr_source, "fn extendr_bench_serialize_roundtrip", "fn extendr_bench_external_ptr")
savvy_task41 <- source_region(
  savvy_source,
  "pub unsafe extern \"C\" fn savvy_bench_serialize_roundtrip__impl",
  "pub unsafe extern \"C\" fn savvy_bench_external_ptr__impl"
)
expect_true(
  grepl("serialize.toVector", zigr_task41, fixed = TRUE) &&
    grepl("serialize.fromVector", zigr_task41, fixed = TRUE) &&
    grepl("R.R_InitOutPStream", zigr_serialize_source, fixed = TRUE) &&
    grepl("R.R_InitInPStream", zigr_serialize_source, fixed = TRUE) &&
    grepl("Rf_eval(ser_call", c_task41, fixed = TRUE) &&
    grepl("Rf_eval(ser_call", rcpp_task41, fixed = TRUE) &&
    grepl("R_tryEvalSilent(ser_call", extendr_task41, fixed = TRUE) &&
    grepl("R_tryEvalSilent(ser_call", savvy_task41, fixed = TRUE),
  "task 41 source supports persistent-stream, direct-eval, and silent-try-eval kernel identities"
)
expect_true(
  grepl("SEXP conn = PROTECT", c_task41, fixed = TRUE) && grepl("UNPROTECT(4);", c_task41, fixed = TRUE) &&
    !grepl("UNPROTECT(1);", c_task41, fixed = TRUE) &&
    grepl("SEXP conn = PROTECT", rcpp_task41, fixed = TRUE) && grepl("UNPROTECT(4);", rcpp_task41, fixed = TRUE) &&
    !grepl("UNPROTECT(1);", rcpp_task41, fixed = TRUE) &&
    grepl("let conn = unsafe { extendr_ffi::Rf_protect", extendr_task41, fixed = TRUE) &&
      grepl("extendr_ffi::Rf_unprotect(4)", extendr_task41, fixed = TRUE) &&
      !grepl("extendr_ffi::Rf_unprotect(1)", extendr_task41, fixed = TRUE) &&
    grepl("let conn = savvy_ffi::Rf_protect", savvy_task41, fixed = TRUE) &&
      grepl("savvy_ffi::Rf_unprotect(4)", savvy_task41, fixed = TRUE) &&
      !grepl("savvy_ffi::Rf_unprotect(1)", savvy_task41, fixed = TRUE),
  "every repaired serialization control retains the complete protection stack through unserialization"
)

c_task42 <- source_region(c_tasks_source, "/* Task 42_external_ptr */", "/* Task 43_rng_stress */")
zigr_task42 <- source_region(zigr_tasks_source, "const task_42_external_ptr", "const task_43_rng_stress")
cpp11_source <- read_source("src/cpp11/src/fixture.cpp")
expect_true(
  all(vapply(c("malloc", "R_SetExternalPtrAddr", "R_RegisterCFinalizerEx", "free(state)", "R_ClearExternalPtr"),
    grepl, logical(1), x = c_task42, fixed = TRUE)) &&
    all(vapply(c("new (std::nothrow) RcppBenchmarkState", "R_SetExternalPtrAddr", "R_RegisterCFinalizerEx", "delete state", "R_ClearExternalPtr"),
      grepl, logical(1), x = rcpp_source, fixed = TRUE)) &&
    all(vapply(c("Box::into_raw", "R_SetExternalPtrAddr", "R_RegisterCFinalizerEx", "Box::from_raw", "R_ClearExternalPtr"),
      grepl, logical(1), x = extendr_source, fixed = TRUE)) &&
    all(vapply(c("Box::into_raw", "R_SetExternalPtrAddr", "R_RegisterCFinalizerEx", "Box::from_raw", "R_ClearExternalPtr"),
      grepl, logical(1), x = savvy_source, fixed = TRUE)) &&
    grepl("externalptr.createTyped", zigr_task42, fixed = TRUE) &&
    grepl("external_pointer<FixtureState>(new FixtureState(value[0]))", cpp11_source, fixed = TRUE),
  "every executable native task-42 control owns its state and declares a finalizer path"
)
expect_true(
  grepl("externalptr.createTyped(FixtureState", read_source("src/zig/main.zig"), fixed = TRUE) &&
    grepl("R_RegisterCFinalizerEx(result, c_fixture_state_finalizer", read_source("src/c_call/register.c"), fixed = TRUE),
  "historical task-72 constructors no longer return borrowed static state"
)

registration_only <- c(
  "extern SEXP missing_control(SEXP);",
  '{"missing_control", (DL_FUNC) &missing_control, 1}'
)
expect_true(
  !source_ledger_c_definition_present(registration_only, "missing_control"),
  "a C declaration and registration do not impersonate a definition"
)
expect_true(
  source_ledger_c_definition_present("SEXP present_control(SEXP value) {", "present_control"),
  "a C function definition is recognized"
)
expect_true(
  !source_ledger_cpp11_wrapper_present(
    '{"_fixture_missing", (DL_FUNC) &_fixture_missing, 1}',
    "_fixture_missing"
  ),
  "cpp11 registration does not impersonate a generated native wrapper"
)
expect_true(
  source_ledger_cpp11_wrapper_present(
    'extern "C" SEXP _fixture_present(SEXP value) {',
    "_fixture_present"
  ),
  "a cpp11 native wrapper definition is recognized"
)
expect_true(
  !source_ledger_r_call_present("registered <- '_fixture_missing'", "_fixture_missing"),
  "a cpp11 symbol mention does not impersonate a generated R wrapper"
)
expect_true(
  source_ledger_r_call_present(".Call(`_fixture_present`, value)", "_fixture_present"),
  "a cpp11 generated R call is recognized"
)
expect_true(
  !source_ledger_savvy_wrapper_present(
    c("extern SEXP savvy_missing__impl(SEXP);", '{"savvy_missing__impl", (DL_FUNC) &savvy_missing__impl, 1}'),
    character(0),
    "savvy_missing__impl"
  ),
  "Savvy registration without a C wrapper, macro invocation, or Rust definition is rejected"
)
expect_true(
  source_ledger_savvy_wrapper_present(
    "SAVVY_WRAP1(present, c_arg__value)",
    character(0),
    "savvy_present__impl"
  ),
  "a Savvy macro wrapper invocation is recognized"
)

r_classes <- table(factor(
  vapply(records[vapply(records, function(row) identical(row$runner, "r"), logical(1))],
    function(row) row$source_class, character(1)),
  levels = c("pure_r", "optimized_base_r", "pure_r_unrepresentable")
))
expect_true(
  identical(unname(as.integer(r_classes)), c(17L, 55L, 11L)),
  "R runner remains mixed rather than uniformly pure R"
)

validate_product_source_record(record("zigr", "52_boundary_scalar_generated"))
validate_product_source_record(record("cpp11", "52_boundary_scalar_generated"))
expect_error(
  "Rcpp product label",
  validate_product_source_record(record("rcpp", "05_fib_recursive")),
  "rejects rcpp/05_fib_recursive.*annotation_present"
)
expect_error(
  "Savvy product label",
  validate_product_source_record(record("savvy", "01_vectorsum")),
  "rejects savvy/01_vectorsum.*annotation_present"
)
expect_error(
  "extendr mixed fixture product label",
  validate_product_source_record(record("extendr", "01_vectorsum")),
  "generated_r_wrapper|forced_symbols"
)
expect_error(
  "direct zigr product label",
  validate_product_source_record(record("zigr", "51_boundary_zero_handwritten")),
  "rejects zigr/51_boundary_zero_handwritten"
)

damaged_zigr <- record("zigr", "52_boundary_scalar_generated")
damaged_zigr$generated_native_wrapper <- FALSE
expect_error(
  "missing zigr generated wrapper",
  validate_product_source_record(damaged_zigr),
  "generated_native_wrapper"
)

damaged_cpp11 <- record("cpp11", "52_boundary_scalar_generated")
damaged_cpp11$annotation_present <- FALSE
expect_error(
  "missing cpp11 annotation",
  validate_product_source_record(damaged_cpp11),
  "annotation_present"
)

drift_root <- tempfile("source-ledger-drift-")
dir.create(drift_root)
on.exit(unlink(drift_root, recursive = TRUE), add = TRUE)
writeLines("original source", file.path(drift_root, "fixture.c"))
recorded_source <- source_ledger_file_identity(drift_root, "fixture.c", "drift fixture")
writeLines("changed source", file.path(drift_root, "fixture.c"))
changed_source <- source_ledger_file_identity(drift_root, "fixture.c", "drift fixture")
expect_error(
  "real source content drift",
  source_ledger_require_digest(changed_source$digest, recorded_source$digest, "runner source"),
  "runner source drift detected"
)

writeLines("first installed file", file.path(drift_root, "DESCRIPTION"))
recorded_tree <- source_ledger_directory_identity(drift_root, "installed source tree")
writeLines("second installed file", file.path(drift_root, "NAMESPACE"))
changed_tree <- source_ledger_directory_identity(drift_root, "installed source tree")
expect_error(
  "real installed tree drift",
  source_ledger_require_digest(changed_tree$digest, recorded_tree$digest, "installed package source"),
  "installed package source drift detected"
)

recorded_dependency <- source_ledger_object_digest(list(name = "libR.so", md5 = "recorded"))
changed_dependency <- source_ledger_object_digest(list(name = "libR.so", md5 = "changed"))
expect_error(
  "real dependency identity drift",
  source_ledger_require_digest(changed_dependency, recorded_dependency, "artifact dependencies"),
  "artifact dependencies drift detected"
)

spec <- load_source_ledger_spec(root_dir)
expect_true(
  identical(as.character(spec$admission$zig_version), "0.16.0") &&
    identical(as.character(spec$admission$r_version), "4.6.1") &&
    identical(as.character(spec$admission$r_packages$Rcpp), "1.1.2") &&
    identical(as.character(spec$admission$r_packages$cpp11), "0.5.5") &&
    identical(as.character(spec$admission$cargo_packages[["extendr-api"]]), "0.9.0") &&
    identical(as.character(spec$admission$cargo_packages$savvy), "0.10.2") &&
    identical(
      as.character(unlist(spec$admission$zigr_access_modes, use.names = FALSE)),
      c("r4_6_x86_64", "checked_r_api")
    ),
  "source ledger freezes the declared next-run version and R access-mode set"
)
fixture_artifacts <- lapply(
  evidence_schema_vocabulary()$runners,
  function(runner) source_ledger_fixture_artifact_paths(root_dir, runner, must_work = FALSE)
)
names(fixture_artifacts) <- evidence_schema_vocabulary()$runners
expect_true(
  identical(basename(fixture_artifacts$r), "run_all.R") &&
    identical(basename(fixture_artifacts$c_call), paste0("bench", .Platform$dynlib.ext)) &&
    identical(basename(fixture_artifacts$zigr), paste0("zigrFixture", .Platform$dynlib.ext)) &&
    identical(basename(fixture_artifacts$rcpp), paste0("zigrRcpp", .Platform$dynlib.ext)) &&
    identical(basename(fixture_artifacts$cpp11), paste0("zigrCpp11", .Platform$dynlib.ext)) &&
    identical(basename(fixture_artifacts$extendr), paste0("zigrExtendr", .Platform$dynlib.ext)) &&
    identical(basename(fixture_artifacts$savvy), paste0("zigrSavvy", .Platform$dynlib.ext)),
  "every runner has one explicit normalized fixture artifact path"
)
spec_test_root <- tempfile("source-ledger-spec-")
dir.create(spec_test_root)
on.exit(unlink(spec_test_root, recursive = TRUE), add = TRUE)
write_spec <- function(value) {
  jsonlite::write_json(
    value[names(value) != "path"],
    file.path(spec_test_root, "source_ledger.json"),
    auto_unbox = TRUE,
    pretty = TRUE
  )
}
bad_spec <- spec
bad_spec$unexpected <- "not allowed"
write_spec(bad_spec)
expect_error(
  "unexpected source ledger field",
  load_source_ledger_spec(spec_test_root),
  "specification fields differ from the schema"
)
bad_spec <- spec
bad_spec$admission$r_packages$not_rcpp <- bad_spec$admission$r_packages$Rcpp
bad_spec$admission$r_packages$Rcpp <- NULL
write_spec(bad_spec)
expect_error(
  "unknown admission package",
  load_source_ledger_spec(spec_test_root),
  "r_packages has an invalid package map"
)
bad_spec <- spec
bad_spec$runners$c_call$source_globs <- list("")
write_spec(bad_spec)
expect_error(
  "blank source selector",
  load_source_ledger_spec(spec_test_root),
  "field source_globs must be a unique nonblank list"
)
bad_spec <- spec
bad_spec$runners$savvy$role <- "product_reference"
write_spec(bad_spec)
expect_error(
  "drifted runner identity",
  load_source_ledger_spec(spec_test_root),
  "savvy identity differs from the frozen runner map"
)
bad_spec <- spec
bad_spec$fixture_runners$rcpp$verifier_kind <- "raw_fixture"
write_spec(bad_spec)
expect_error(
  "drifted fixture verifier",
  load_source_ledger_spec(spec_test_root),
  "fixture runner rcpp verifier differs from the frozen map"
)
selection_counts <- integer(0)
for (scope in c("runners", "fixture_runners")) {
  for (runner in names(spec[[scope]])) {
    runner_spec <- spec[[scope]][[runner]]
    selectors <- list(
      source = runner_spec$source_globs,
      build = runner_spec$build_files,
      glue = runner_spec$generated_glue$paths
    )
    for (kind in names(selectors)) {
      key <- paste(scope, runner, kind, sep = "/")
      selection_counts[[key]] <- source_ledger_file_identity(
        root_dir, selectors[[kind]], key
      )$file_count
    }
  }
}
expect_true(
  all(selection_counts > 0L),
  sprintf("every source/build/glue selector resolves; empty: %s", paste(names(selection_counts)[selection_counts < 1L], collapse = ", "))
)
build_settings <- list(
  optimization = "ReleaseFast",
  target = "native",
  cpu_features = "default",
  checked_sexp = FALSE,
  cache_dir = normalizePath(file.path(root_dir, ".zig-cache"), mustWork = FALSE),
  global_cache_dir = normalizePath(file.path(root_dir, ".zig-global-cache"), mustWork = FALSE),
  requested_rebuild = FALSE
)
invocation_checks <- logical(0)
for (runner in names(spec$runners)) {
  invocation <- source_ledger_build_invocation(root_dir, runner, build_settings)
  fixture_invocations <- source_ledger_fixture_build_invocations(root_dir, runner, build_settings)
  invocation_checks[[runner]] <-
    nzchar(invocation$executable) && dir.exists(invocation$working_directory) &&
    !isTRUE(invocation$executed_in_run) && length(fixture_invocations) >= 1L &&
    all(vapply(fixture_invocations, function(record) {
      nzchar(record$executable) && dir.exists(record$working_directory) && !isTRUE(record$executed_in_run)
    }, logical(1)))
}
expect_true(
  all(invocation_checks),
  sprintf("prebuilt build and fixture invocations resolve without claiming execution; failed: %s", paste(names(invocation_checks)[!invocation_checks], collapse = ", "))
)
c_invocation <- source_ledger_build_invocation(root_dir, "c_call", build_settings)
expect_true(
  all(c(
    paste0("CC=", source_ledger_r_config("CC")),
    paste0("R_CFLAGS=", source_ledger_r_config("CFLAGS"))
  ) %in% as.character(unlist(c_invocation$arguments, use.names = FALSE))) &&
    length(c_invocation$environment) == 0L,
  "C control invocation pins the compiler command and flags recorded from R"
)
c_identity <- capture_c_control_identity(list(cc = list(
  command = source_ledger_r_config("CC"),
  flags = source_ledger_r_config("CFLAGS")
)))
expect_true(
  identical(c_identity$command, source_ledger_r_config("CC")) &&
    nzchar(c_identity$effective_standard) && nzchar(c_identity$stdc_version) &&
    identical(
      c_identity$link_command,
      paste(
        c_identity$command, c_identity$package_cflags,
        "-fPIC -shared -o bench.so ./tasks.c ./register.c", c_identity$package_libs
      )
    ) &&
    identical(as.character(unlist(c_identity$linked_libraries, use.names = FALSE)), c("R", "pthread", "blas")),
  "C control provenance records its actual compiler, standard, link command, and libraries"
)
admitted_records <- list(
  list(name = "zigr", toolchain = list(
    version = "0.16.0", checked_sexp = FALSE,
    r_access_mode = "r4_6_x86_64", effective_target = "x86_64-linux-gnu"
  )),
  list(name = "rcpp", toolchain = list(version = "1.1.2")),
  list(name = "cpp11", toolchain = list(version = "0.5.5")),
  list(name = "extendr", toolchain = list(packages = list(
    list(name = "extendr-api", version = "0.9.0", selected = TRUE)
  ))),
  list(name = "savvy", toolchain = list(packages = list(
    list(name = "savvy", version = "0.10.2", selected = TRUE)
  )))
)
rebuilt_records <- lapply(admitted_records, function(record) {
  record$build_invocation <- list(executed_in_run = TRUE)
  record$fixture <- list(build_invocations = list(list(executed_in_run = TRUE)))
  record
})
expect_true(
  isTRUE(source_ledger_validate_admission(
    spec, list(runtime_version = "4.6.1", header_version = "4.6.1"), admitted_records
  )),
  "the frozen tool, API, dependency, and R access-mode set is admitted"
)
expect_true(
  isTRUE(source_ledger_validate_admission(
    spec, list(runtime_version = "4.6.1", header_version = "4.6.1"),
    admitted_records[c(1L, 2L)]
  )),
  "filtered runs admit exactly the selected runner toolchains"
)
checked_records <- clone(admitted_records)
checked_records[[1L]]$toolchain$checked_sexp <- TRUE
checked_records[[1L]]$toolchain$r_access_mode <- "checked_r_api"
expect_true(
  isTRUE(source_ledger_validate_admission(
    spec, list(runtime_version = "4.6.1", header_version = "4.6.1"), checked_records
  )),
  "the checked R access mode remains a distinct admitted identity"
)
expect_true(
  isTRUE(source_ledger_validate_admission(
    spec, list(runtime_version = "4.6.1", header_version = "4.6.1"), rebuilt_records,
    require_rebuilt = TRUE
  )),
  "promotion admission accepts source-matched runner and fixture rebuilds"
)
prebuilt_records <- clone(rebuilt_records)
prebuilt_records[[2L]]$build_invocation$executed_in_run <- FALSE
expect_error(
  "prebuilt artifact promotion",
  source_ledger_validate_admission(
    spec, list(runtime_version = "4.6.1", header_version = "4.6.1"), prebuilt_records,
    require_rebuilt = TRUE
  ),
  "promotion requires rebuilt runner and fixture artifacts: rcpp"
)
prebuilt_records <- clone(rebuilt_records)
prebuilt_records[[2L]]$fixture$build_invocations[[1L]]$executed_in_run <- FALSE
expect_error(
  "prebuilt fixture promotion",
  source_ledger_validate_admission(
    spec, list(runtime_version = "4.6.1", header_version = "4.6.1"), prebuilt_records,
    require_rebuilt = TRUE
  ),
  "promotion requires rebuilt runner and fixture artifacts: rcpp"
)
drifted_records <- clone(admitted_records)
drifted_records[[1L]]$toolchain$version <- "0.16.1"
expect_error(
  "resolved Zig drift",
  source_ledger_validate_admission(
    spec, list(runtime_version = "4.6.1", header_version = "4.6.1"), drifted_records
  ),
  "Zig version 0.16.1 is not admitted"
)
drifted_records <- clone(admitted_records)
drifted_records[[2L]]$toolchain$version <- "1.1.1"
expect_error(
  "resolved dependency drift",
  source_ledger_validate_admission(
    spec, list(runtime_version = "4.6.1", header_version = "4.6.1"), drifted_records
  ),
  "Rcpp version 1.1.1 is not admitted"
)
drifted_records <- clone(admitted_records)
drifted_records[[5L]]$toolchain$packages[[1L]]$version <- "0.10.1"
expect_error(
  "resolved Cargo dependency drift",
  source_ledger_validate_admission(
    spec, list(runtime_version = "4.6.1", header_version = "4.6.1"), drifted_records
  ),
  "savvy version 0.10.1 is not admitted"
)
drifted_records <- clone(admitted_records)
drifted_records[[1L]]$toolchain$effective_target <- "aarch64-linux-gnu"
expect_error(
  "direct access target drift",
  source_ledger_validate_admission(
    spec, list(runtime_version = "4.6.1", header_version = "4.6.1"), drifted_records
  ),
  "R access mode r4_6_x86_64 differs from resolved mode checked_r_api"
)
expect_error(
  "R header drift",
  source_ledger_validate_admission(
    spec, list(runtime_version = "4.6.1", header_version = "4.6.0"), admitted_records
  ),
  "R headers version 4.6.0 is not admitted"
)
expect_error(
  "R runtime drift",
  source_ledger_validate_admission(
    spec, list(runtime_version = "4.6.0", header_version = "4.6.1"), admitted_records
  ),
  "R runtime version 4.6.0 is not admitted"
)
zigr_invocation <- source_ledger_build_invocation(root_dir, "zigr", build_settings)
expect_true(
  identical(
    as.character(unlist(zigr_invocation$arguments, use.names = FALSE)),
    c(
      "build", "-Doptimize=ReleaseFast",
      paste0("-Dr-include=", source_ledger_effective_build_path("R_INCLUDE", R.home("include"))),
      paste0("-Dr-lib=", source_ledger_effective_build_path("R_LIB", R.home("lib"))),
      "--cache-dir", build_settings$cache_dir,
      "--global-cache-dir", build_settings$global_cache_dir
    )
  ),
  "resolved zigr invocation matches build_all.sh defaults"
)
zigr_fixture_invocations <- source_ledger_fixture_build_invocations(root_dir, "zigr", build_settings)
expect_true(
  length(zigr_fixture_invocations) == 2L &&
    identical(zigr_fixture_invocations[[1L]], zigr_invocation) &&
    identical(
      as.character(unlist(zigr_fixture_invocations[[2L]]$arguments, use.names = FALSE)),
      c(
        "CMD", "INSTALL", "--preclean", "--clean", "--no-multiarch",
        paste0("--library=", normalizePath(file.path(root_dir, "tmp", "fixture-library"), mustWork = FALSE)),
        "src/zig/fixture"
      )
    ),
  "zigr fixture identity records both the Zig build and package installation"
)
expect_error(
  "one missing required path",
  source_ledger_file_identity(
    root_dir,
    c("source_ledger.json", "missing-required-source-ledger-path"),
    "required path test"
  ),
  "patterns select no regular files: missing-required-source-ledger-path"
)
expect_true(
  identical(
    c(
      cpp11 = isTRUE(spec$runners$cpp11$generated_glue$retained_output),
      extendr = isTRUE(spec$runners$extendr$generated_glue$retained_output),
      zigr = isTRUE(spec$runners$zigr$generated_glue$retained_output)
    ),
    c(cpp11 = TRUE, extendr = FALSE, zigr = FALSE)
  ) && identical(spec$runners$savvy$generated_glue$kind, "handwritten_savvy_shaped_glue_not_generated"),
  "generated-glue retention and handwritten Savvy classification are exact"
)

extendr_profile <- parse_cargo_release_profile(file.path(root_dir, spec$runners$extendr$cargo_manifest))
savvy_profile <- parse_cargo_release_profile(file.path(root_dir, spec$runners$savvy$cargo_manifest))
expect_true(
  identical(
    c(extendr_lto = extendr_profile$lto, extendr_panic = extendr_profile$panic,
      savvy_lto = savvy_profile$lto, savvy_panic = savvy_profile$panic),
    c(extendr_lto = "true", extendr_panic = "unwind (Cargo release default)",
      savvy_lto = "true", savvy_panic = "abort")
  ),
  "Cargo release LTO and panic strategies remain exact"
)
cargo_lock_checks <- logical(0)
for (runner in c("extendr", "savvy")) {
  locked <- parse_cargo_lock(file.path(root_dir, spec$runners[[runner]]$cargo_lock))
  registry <- locked[vapply(locked, function(package) startsWith(package$source, "registry+"), logical(1))]
  cargo_lock_checks[[runner]] <-
    all(vapply(locked, function(package) nzchar(package$name) && nzchar(package$version), logical(1))) &&
    length(registry) > 0L && all(vapply(registry, function(package) nzchar(package$checksum), logical(1)))
}
expect_true(
  all(cargo_lock_checks),
  sprintf("Cargo locks retain package identities and registry checksums; failed: %s", paste(names(cargo_lock_checks)[!cargo_lock_checks], collapse = ", "))
)

direct_runners <- c("r", "c_call", "zigr", "rcpp", "cpp11", "extendr", "savvy")
direct_specs <- benchmark_revision_task_specs()
direct_tasks <- vapply(direct_specs, `[[`, character(1), "id")
expect_true(
  identical(direct_tasks, c(
    "vector_sum", "numeric_transform", "broadcast", "sort", "missing_mean",
    "transpose", "rowcol", "matmul", "dataframe", "list_sum", "string_concat",
    "string_metadata", "factor", "attributes", "s4", "logical_counts", "raw_copy",
    "complex_conjugate", "schema", "altrep_sum", "altrep_index",
    "altrep_materialize", "external_state", "eval", "serialize", "rng", "outputs"
  )),
  "the direct benchmark retains exactly the approved 27-task order"
)

direct_master_seed <- benchmark_master_seed() + 17L
direct_input_seeds <- setNames(lapply(direct_tasks, function(task) {
  task_input_seed(direct_master_seed, task, "revision-v1")
}), direct_tasks)
direct_artifacts <- setNames(lapply(direct_runners, function(runner) {
  list(runner = runner, relative_path = paste0("artifact/", runner), md5 = paste0("digest-", runner))
}), direct_runners)
direct_metadata <- list(
  schema_version = 4L,
  artifact_layout = "direct-v1",
  run_id = "direct-manifest-test",
  status = "running",
  started_at = run_manifest_timestamp(),
  runners = as.list(direct_runners),
  tasks = as.list(direct_tasks),
  master_seed = direct_master_seed,
  input_recipe_version = "revision-v1",
  input_seeds = direct_input_seeds,
  rng_event_seed = task_input_seed(direct_master_seed, "rng", "direct-timing-v1"),
  source_tree = list(method = "test", digest = "source-digest", file_count = 1L),
  artifacts = direct_artifacts,
  timing_policy = benchmark_timing_policy(),
  measurement_mode = "timed",
  command = list("Rscript", "run_benchmarks.R")
)
validate_direct_run_manifest(direct_metadata)

incomplete_direct <- clone(direct_metadata)
incomplete_direct$status <- "incomplete"
incomplete_direct$finished_at <- run_manifest_timestamp()
incomplete_direct$status_message <- "batch sizing cannot meet the shared policy"
validate_direct_run_manifest(incomplete_direct)
bad_direct <- clone(incomplete_direct)
bad_direct$status_message <- NULL
expect_error(
  "incomplete manifest requires its diagnostic",
  validate_direct_run_manifest(bad_direct),
  "incomplete status message"
)

bad_direct <- clone(direct_metadata)
bad_direct$timing_policy$batch_repetitions$vector_sum <- 7L
expect_error(
  "direct manifest rejects a repetition outside its sizing ladder",
  validate_direct_run_manifest(bad_direct),
  "outside the sizing ladder"
)
bad_direct <- clone(direct_metadata)
bad_direct$timing_policy$sizing_policy$maximum_batch_ms <- 4
expect_error(
  "direct manifest rejects an impossible sizing policy",
  validate_direct_run_manifest(bad_direct),
  "invalid batch bounds"
)
bad_direct <- clone(direct_metadata)
bad_direct$timing_policy$r_jit_policy <- "default"
expect_error(
  "direct manifest rejects an unpinned R JIT policy",
  validate_direct_run_manifest(bad_direct),
  "invalid R JIT policy"
)
bad_direct <- clone(direct_metadata)
bad_direct$timing_policy$distribution_policy$cv_pct_limit <- 0
expect_error(
  "direct manifest rejects an invalid distribution policy",
  validate_direct_run_manifest(bad_direct),
  "invalid limits"
)
bad_direct <- clone(direct_metadata)
bad_direct$timing_policy$allocation_policy$large_output_vcells$complex_conjugate <- 1L
expect_error(
  "direct manifest rejects an invalid allocating-event policy",
  validate_direct_run_manifest(bad_direct),
  "invalid complex output size"
)
worker_source <- read_source("benchmark_worker.R")
timing_phase_source <- source_region(worker_source, "for (spec in specs) {\n  truth <- phase_truth(spec)", "samples <- do.call(rbind, sample_rows)")
expect_true(
  appears_before(worker_source, "compiler::enableJIT(0L)", "source(file.path(root_dir, \"src\", \"r\", \"run_all.R\")") &&
    appears_before(worker_source, "runner_entries <-", "if (identical(mode, \"sizing\"))") &&
    appears_before(worker_source, "vector_heap_trigger_vcells <- direct_vector_heap_trigger_vcells(gc(full = TRUE))\n  repetitions <-", "calibration_result <-") &&
    appears_before(worker_source, "calibration_result <-", "last_result <- NULL"),
  "workers disable JIT, resolve entries, and force GC before the local calibration boundary"
)
expect_true(
  appears_before(worker_source, "if (!skip_probes)", "for (spec in specs) {\n  truth <- phase_truth(spec)") &&
    length(gregexpr("gc(full = TRUE)", timing_phase_source, fixed = TRUE)[[1L]]) == 3L &&
    !grepl("gc(", source_region(timing_phase_source, "calibration_result <-", "last_result <- NULL"), fixed = TRUE) &&
    appears_before(timing_phase_source, "last_result <- NULL", "for (sample in seq_len(measurement_samples))") &&
    appears_before(worker_source, "reset_rng(spec)\n  measured <- if (timed)", "if (verify_result)"),
  "probes precede task phases, forced GC stops before measurement, and reset stays outside the timer"
)

bad_direct <- clone(direct_metadata)
bad_direct$input_seeds[[direct_tasks[[1L]]]] <- bad_direct$input_seeds[[direct_tasks[[1L]]]] + 1L
expect_error(
  "direct manifest rejects a claimed task seed that was not executed",
  validate_direct_run_manifest(bad_direct),
  "input seed differs"
)
bad_direct <- clone(direct_metadata)
bad_direct$rng_event_seed <- bad_direct$rng_event_seed + 1L
expect_error(
  "direct manifest rejects a claimed RNG state that was not executed",
  validate_direct_run_manifest(bad_direct),
  "RNG event seed differs"
)
bad_direct <- clone(direct_metadata)
bad_direct$timing_execution <- list(stage = "pilot")
expect_error(
  "direct manifest rejects the removed timing schema",
  validate_direct_run_manifest(bad_direct),
  "unsupported fields"
)

direct_run_dir <- tempfile("direct-manifest-")
dir.create(direct_run_dir)
on.exit(unlink(direct_run_dir, recursive = TRUE), add = TRUE)
complete_direct <- clone(direct_metadata)
complete_direct$status <- "complete"
complete_direct$finished_at <- run_manifest_timestamp()
probe_names <- measurement_probe_names()
probe_sequence <- lapply(seq_len(length(probe_names) * 101L), function(index) {
  probe <- probe_names[[((index - 1L) %/% 101L) + 1L]]
  sample <- ((index - 1L) %% 101L) + 1L
  elapsed <- c(
    noop_native = 0.00812345678901234,
    noop_r = 0.00984999860520475,
    cpu = 0.612345678901234,
    allocate = 0.112345678901234
  )[[probe]]
  list(
    probe = probe, probe_sample = sample, batch_repetitions = 1L,
    batch_elapsed_ms = elapsed, elapsed_per_event_ms = elapsed, gc_elapsed_ms = 0
  )
})
probe_floor <- measurement_probe_timer_floor(data.frame(
  probe = vapply(probe_sequence, `[[`, character(1), "probe"),
  elapsed_per_event_ms = vapply(probe_sequence, `[[`, numeric(1), "elapsed_per_event_ms"),
  stringsAsFactors = FALSE
))
complete_direct$measurement_probes <- setNames(lapply(direct_runners, function(runner) {
  list(
    timer_floor_ms = probe_floor,
    nanotime_elapsed_ms = 20,
    independent_elapsed_ms = 20,
    samples = probe_sequence
  )
}), direct_runners)
complete_direct$outputs <- list(
  correctness = list(relative_path = "correctness.csv", md5 = ""),
  timing_samples = list(relative_path = "timing_samples.csv", md5 = ""),
  timing_summary = list(relative_path = "timing_summary.csv", md5 = "")
)
for (name in names(complete_direct$outputs)) {
  path <- file.path(direct_run_dir, complete_direct$outputs[[name]]$relative_path)
  if (identical(name, "timing_samples")) {
    rows <- do.call(rbind, lapply(direct_runners, function(runner) {
      do.call(rbind, lapply(direct_tasks, function(task) {
        repetitions <- complete_direct$timing_policy$batch_repetitions[[task]]
        data.frame(
          runner = runner, task = task, phase = "measurement", measurement_sample = 1:11,
          batch_repetitions = repetitions, batch_elapsed_ms = as.numeric(repetitions),
          elapsed_per_event_ms = 1, gc_elapsed_ms = 0,
          vector_heap_trigger_vcells = 8388608L, stringsAsFactors = FALSE
        )
      }))
    }))
    write.csv(rows, path, row.names = FALSE)
  } else {
    writeLines(name, path)
  }
  complete_direct$outputs[[name]]$md5 <- unname(as.character(tools::md5sum(path))[[1L]])
}
summary_samples <- read.csv(file.path(direct_run_dir, "timing_samples.csv"), stringsAsFactors = FALSE)
summary_first_calls <- expand.grid(
  runner = direct_runners, task = direct_tasks, stringsAsFactors = FALSE
)
summary_first_calls$first_call_ms <- 1
summary_rows <- summarize_direct_timing(
  summary_samples, summary_first_calls,
  setNames(rep(probe_floor, length(direct_runners)), direct_runners),
  complete_direct$timing_policy$distribution_policy
)
write.csv(summary_rows, file.path(direct_run_dir, "timing_summary.csv"), row.names = FALSE)
complete_direct$outputs$timing_summary$md5 <- unname(as.character(tools::md5sum(
  file.path(direct_run_dir, "timing_summary.csv")
)[[1L]]))

bad_direct <- clone(complete_direct)
bad_direct$measurement_probes$r$samples[[2L]]$probe_sample <- 1L
expect_error(
  "direct manifest rejects duplicate probe samples",
  validate_direct_run_manifest(bad_direct),
  "raw order is invalid"
)
bad_direct <- clone(complete_direct)
first_probe <- bad_direct$measurement_probes$r$samples[[1L]]
bad_direct$measurement_probes$r$samples[[1L]] <- bad_direct$measurement_probes$r$samples[[2L]]
bad_direct$measurement_probes$r$samples[[2L]] <- first_probe
expect_error(
  "direct manifest rejects reordered probe samples",
  validate_direct_run_manifest(bad_direct),
  "raw order is invalid"
)
bad_direct <- clone(complete_direct)
bad_direct$measurement_probes$r$samples[[101L]]$probe_sample <- 102L
expect_error(
  "direct manifest rejects nonconsecutive probe samples",
  validate_direct_run_manifest(bad_direct),
  "raw order is invalid"
)
bad_direct <- clone(complete_direct)
bad_direct$measurement_probes$r$samples <- bad_direct$measurement_probes$r$samples[-1L]
expect_error(
  "direct manifest rejects missing probe samples",
  validate_direct_run_manifest(bad_direct),
  "raw sequence is incomplete"
)
bad_direct <- clone(complete_direct)
bad_direct$measurement_probes$r$samples[[1L]]$batch_elapsed_ms <- "0.01"
expect_error(
  "direct manifest rejects nonnumeric probe values",
  validate_direct_run_manifest(bad_direct),
  "valid numeric"
)
bad_direct <- clone(complete_direct)
bad_direct$measurement_probes$r$samples[[1L]]$batch_repetitions <- 2L
expect_error(
  "direct manifest rejects a probe repetition other than one",
  validate_direct_run_manifest(bad_direct),
  "raw values are invalid"
)
bad_direct <- clone(complete_direct)
bad_direct$measurement_probes$r$samples[[1L]]$batch_elapsed_ms <- -0.01
bad_direct$measurement_probes$r$samples[[1L]]$elapsed_per_event_ms <- -0.01
expect_error(
  "direct manifest rejects a negative probe duration",
  validate_direct_run_manifest(bad_direct),
  "raw values are invalid"
)
bad_direct <- clone(complete_direct)
bad_direct$measurement_probes$r$samples[[1L]]$elapsed_per_event_ms <- 0.5
expect_error(
  "direct manifest rejects inconsistent batch and per-event durations",
  validate_direct_run_manifest(bad_direct),
  "raw values are invalid"
)
bad_direct <- clone(complete_direct)
bad_direct$measurement_probes$r$samples[[1L]]$gc_elapsed_ms <- -0.01
expect_error(
  "direct manifest rejects a negative probe GC duration",
  validate_direct_run_manifest(bad_direct),
  "raw values are invalid"
)
bad_direct <- clone(complete_direct)
bad_direct$measurement_probes$r$timer_floor_ms <- probe_floor + 1
expect_error(
  "direct manifest recomputes the timer floor",
  validate_direct_run_manifest(bad_direct),
  "timer floor differs"
)
bad_direct <- clone(complete_direct)
bad_direct$measurement_probes$r$independent_elapsed_ms <- 5
expect_error(
  "direct manifest rejects a unit interval below independent-clock resolution",
  validate_direct_run_manifest(bad_direct),
  "summary is invalid"
)
bad_direct <- clone(complete_direct)
bad_direct$measurement_probes$r$nanotime_elapsed_ms <- 100
expect_error(
  "direct manifest revalidates clock agreement",
  validate_direct_run_manifest(bad_direct),
  "summary is invalid"
)
direct_run_source <- read_source("run_benchmarks.R")
probe_handoff_source <- source_region(
  direct_run_source,
  "metadata$measurement_probes <-",
  "summary <- summarize_direct_timing("
)
expect_true(
  appears_before(
    direct_run_source,
    "validate_measurement_probe_record(",
    "summary <- summarize_direct_timing("
  ),
  "the orchestrator validates worker probe records before product distribution analysis"
)
expect_true(
  grepl("write_run_manifest(run_dir, metadata)", probe_handoff_source, fixed = TRUE),
  "the orchestrator retains validated probes before product distribution analysis"
)
write_run_manifest(direct_run_dir, complete_direct)
roundtrip_direct <- read_run_manifest(direct_run_dir)
expect_true(
  identical(as.character(roundtrip_direct$status), "complete") &&
    identical(
      as.numeric(roundtrip_direct$measurement_probes$r$timer_floor_ms),
      probe_floor
    ),
  "the direct manifest preserves probe precision and validates final output identities"
)
forged_direct <- clone(roundtrip_direct)
forged_samples <- read.csv(file.path(direct_run_dir, "timing_samples.csv"), stringsAsFactors = FALSE)
forged_samples$batch_repetitions[forged_samples$task == direct_tasks[[1L]]] <- 8L
forged_samples$batch_elapsed_ms[forged_samples$task == direct_tasks[[1L]]] <- 8
write.csv(forged_samples, file.path(direct_run_dir, "timing_samples.csv"), row.names = FALSE)
forged_direct$outputs$timing_samples$md5 <- unname(as.character(tools::md5sum(
  file.path(direct_run_dir, "timing_samples.csv")
)[[1L]]))
jsonlite::write_json(forged_direct, run_manifest_path(direct_run_dir), auto_unbox = TRUE,
                     pretty = TRUE, null = "null", digits = NA)
expect_error(
  "completed manifest rejects timing repetitions that differ from its sizing map",
  read_run_manifest(direct_run_dir),
  "differ from the manifest sizing map"
)
restored_samples <- do.call(rbind, lapply(direct_runners, function(runner) {
  do.call(rbind, lapply(direct_tasks, function(task) {
    repetitions <- complete_direct$timing_policy$batch_repetitions[[task]]
    data.frame(
      runner = runner, task = task, phase = "measurement", measurement_sample = 1:11,
      batch_repetitions = repetitions, batch_elapsed_ms = as.numeric(repetitions),
      elapsed_per_event_ms = 1, gc_elapsed_ms = 0,
      vector_heap_trigger_vcells = 8388608L, stringsAsFactors = FALSE
    )
  }))
}))
write.csv(restored_samples, file.path(direct_run_dir, "timing_samples.csv"), row.names = FALSE)
write_run_manifest(direct_run_dir, complete_direct)
forged_direct <- clone(complete_direct)
forged_summary <- read.csv(file.path(direct_run_dir, "timing_summary.csv"), stringsAsFactors = FALSE)
forged_summary$median_ms[[1L]] <- forged_summary$median_ms[[1L]] + 1
write.csv(forged_summary, file.path(direct_run_dir, "timing_summary.csv"), row.names = FALSE)
forged_direct$outputs$timing_summary$md5 <- unname(as.character(tools::md5sum(
  file.path(direct_run_dir, "timing_summary.csv")
)[[1L]]))
jsonlite::write_json(forged_direct, run_manifest_path(direct_run_dir), auto_unbox = TRUE,
                     pretty = TRUE, null = "null", digits = NA)
expect_error(
  "completed manifest rejects a summary that differs from raw samples",
  read_run_manifest(direct_run_dir),
  "summary differs from raw samples or distribution policy"
)
write.csv(summary_rows, file.path(direct_run_dir, "timing_summary.csv"), row.names = FALSE)
write_run_manifest(direct_run_dir, complete_direct)
forged_direct <- clone(complete_direct)
forged_summary <- read.csv(file.path(direct_run_dir, "timing_summary.csv"), stringsAsFactors = FALSE)
forged_summary$distribution_policy_digest[[1L]] <- "forged-distribution-policy-digest"
write.csv(forged_summary, file.path(direct_run_dir, "timing_summary.csv"), row.names = FALSE)
forged_direct$outputs$timing_summary$md5 <- unname(as.character(tools::md5sum(
  file.path(direct_run_dir, "timing_summary.csv")
)[[1L]]))
jsonlite::write_json(forged_direct, run_manifest_path(direct_run_dir), auto_unbox = TRUE,
                     pretty = TRUE, null = "null", digits = NA)
expect_error(
  "completed manifest rejects a forged distribution-policy digest",
  read_run_manifest(direct_run_dir),
  "summary differs from raw samples or distribution policy"
)
write.csv(summary_rows, file.path(direct_run_dir, "timing_summary.csv"), row.names = FALSE)
write_run_manifest(direct_run_dir, complete_direct)
forged_direct <- clone(complete_direct)
forged_direct$timing_policy$distribution_policy$cv_pct_limit <- 51
jsonlite::write_json(forged_direct, run_manifest_path(direct_run_dir), auto_unbox = TRUE,
                     pretty = TRUE, null = "null", digits = NA)
expect_error(
  "completed manifest rejects a valid but different distribution policy",
  read_run_manifest(direct_run_dir),
  "summary differs from raw samples or distribution policy"
)
write_run_manifest(direct_run_dir, complete_direct)
writeLines("drift", file.path(direct_run_dir, "timing_summary.csv"))
expect_error(
  "direct manifest detects final output drift",
  read_run_manifest(direct_run_dir),
  "output digest differs"
)
bad_direct <- clone(complete_direct)
bad_direct$measurement_mode <- "correctness_only"
expect_error(
  "direct manifest rejects a timed completion with correctness-only identity",
  validate_direct_run_manifest(bad_direct),
  "status disagrees"
)

cat(sprintf(
  "Specification trust passed: %d task cells, %d fixture cells, %d direct tasks, source identity, and adversarial drift checks.\n",
  nrow(evidence$tasks), nrow(evidence$fixture_rows), length(direct_tasks)
))
