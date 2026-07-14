#!/usr/bin/env Rscript

script_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(script_argument) != 1L) stop("cannot determine evidence schema test location")
script_path <- normalizePath(sub("^--file=", "", script_argument[[1L]]))
root_dir <- normalizePath(file.path(dirname(script_path), ".."))

source(file.path(root_dir, "lib", "task_manifest.R"))
source(file.path(root_dir, "lib", "evidence_schema.R"))

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
    stop(sprintf(
      "negative test %s failed for the wrong reason: %s",
      label, conditionMessage(error)
    ), call. = FALSE)
  }
  invisible(error)
}

clone <- function(value) unserialize(serialize(value, NULL))

manifest <- load_task_manifest(root_dir)
evidence <- load_evidence_manifest(root_dir, manifest)

expect_true(nrow(evidence$tasks) == 83L * 7L, "complete task disposition matrix")
expect_true(nrow(evidence$fixture_rows) == 12L * 7L, "complete fixture disposition matrix")
expect_true(identical(evidence$task_sets$all_tasks, as.character(manifest$task)), "canonical task order")
expect_true(is.null(evidence$raw$task_sets$all_tasks), "evidence manifest does not copy the canonical task universe")
expect_true(!any(evidence$tasks$timing_eligible), "legacy task evidence is not timing eligible")
expect_true(!any(evidence$fixture_rows$timing_eligible), "fixture evidence is not timing eligible")
expect_true(!any(evidence$tasks$comparison_tier == "tier_a"), "no legacy task is prematurely Tier A")
fixture_tiers <- table(factor(
  evidence$fixture_rows$comparison_tier,
  levels = c("tier_a", "tier_b", "tier_c", "tier_d", "gap")
))
expect_true(
  identical(unname(as.integer(fixture_tiers)), c(9L, 47L, 23L, 0L, 5L)),
  "P4.4 fixture tiers contain exact products, strategy products, controls, no diagnostics, and gaps"
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

expected_executable <- c(c_call = 70L, cpp11 = 11L, extendr = 44L, r = 72L, rcpp = 44L, savvy = 44L, zigr = 83L)
actual_executable <- vapply(names(expected_executable), function(runner) {
  sum(evidence$tasks$runner == runner & evidence$tasks$executable)
}, integer(1))
expect_true(identical(actual_executable, expected_executable), "executable coverage matches the P4.1 R split")

expected_fixture_executable <- c(
  c_call = 12L, cpp11 = 9L, extendr = 12L, r = 11L, rcpp = 12L, savvy = 12L, zigr = 11L
)
actual_fixture_executable <- vapply(names(expected_fixture_executable), function(runner) {
  sum(evidence$fixture_rows$runner == runner & evidence$fixture_rows$executable)
}, integer(1))
expect_true(
  identical(actual_fixture_executable, expected_fixture_executable),
  "fixture execution coverage matches the implemented P4.3 matrix"
)

disposition_snapshot <- run_disposition_records(evidence, "r", "52_boundary_scalar_generated")$r[[1L]]
expect_true(
  identical(
    names(disposition_snapshot),
    c(
      "task", "status", "executable", "reason", "owner", "implementation_role", "evidence_use",
      "path_kind", "public_path", "representation_strategy", "kernel_id", "contract_version",
      "fixture_version", "comparison_tier", "mutation_policy", "setup_policy", "comparison_group",
      "timing_eligible"
    )
  ),
  "run disposition snapshots preserve every normalized evidence field"
)

for (runner in evidence$runners) {
  path <- file.path(root_dir, "runners", paste0(runner, ".json"))
  cfg <- jsonlite::fromJSON(path, simplifyVector = FALSE)
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
  nrow(zigr_f08) == 1L && zigr_f08$status == "product_gap" && zigr_f08$owner == "P5",
  "zigr F08 does not disguise the untyped escape hatch as a product path"
)
expect_true(
  nrow(zigr_f09) == 1L && zigr_f09$path_kind == "generated_public_adapter",
  "zigr F09 retains the P1-approved explicit fixed-schema adapter label"
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
  identical(unname(as.integer(r_roles)), c(16L, 56L, 11L)),
  "R rows remain split into 16 pure, 56 optimized, and 11 unrepresentable dispositions"
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
bad$task_dispositions[[group_index]]$exclude_tasks <- list("42_external_ptr")
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

legacy_cfg <- jsonlite::fromJSON(file.path(root_dir, "runners", "c_call.json"), simplifyVector = FALSE)
legacy_cfg$optional_tasks <- list("48_weakref_lifecycle")
expect_error(
  "legacy optional mismatch",
  hydrate_runner_config(manifest, legacy_cfg, "c_call", evidence),
  "legacy optional_tasks differs from evidence dispositions"
)

cat(sprintf(
  "Evidence schema passed: %d task cells, %d fixture cells, seven runners, and eighteen negative checks.\n",
  nrow(evidence$tasks), nrow(evidence$fixture_rows)
))
