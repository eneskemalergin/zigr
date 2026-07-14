#!/usr/bin/env Rscript

script_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(script_argument) != 1L) stop("cannot determine product fixture test location")
script_path <- normalizePath(sub("^--file=", "", script_argument[[1L]]))
root_dir <- normalizePath(file.path(dirname(script_path), ".."))

source(file.path(root_dir, "lib", "task_manifest.R"))
source(file.path(root_dir, "lib", "evidence_schema.R"))
source(file.path(root_dir, "lib", "input_contract.R"))
source(file.path(root_dir, "lib", "r_provenance.R"))
source(file.path(root_dir, "lib", "source_ledger.R"))
source(file.path(root_dir, "lib", "product_fixtures.R"))

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
      "negative test %s failed for the wrong reason: %s", label, conditionMessage(error)
    ), call. = FALSE)
  }
  invisible(error)
}

manifest <- load_task_manifest(root_dir)
evidence <- load_evidence_manifest(root_dir, manifest)
verification <- verify_fixture_source_paths(root_dir, evidence)
records <- verification$records

expect_true(length(records) == 84L, "complete seven-runner fixture matrix")
expect_true(
  identical(names(verification$optimized_r_provenance), c("F03", "F04")) &&
    all(vapply(verification$optimized_r_provenance, function(record) {
      identical(record$implementation_class, "optimized_base_r") &&
        !identical(record$compiled_backend, "none")
    }, logical(1))),
  "separate optimized base-R baselines retain compiled-backend provenance"
)
expect_true(
  sum(vapply(records, function(record) isTRUE(record$product_eligible), logical(1))) == 56L,
  "all 56 supported product cells use eligible generated public paths"
)
expect_true(
  sum(vapply(records, function(record) identical(record$source_class, "generated_typed"), logical(1))) == 55L &&
    sum(vapply(records, function(record) identical(record$source_class, "generated_public_adapter"), logical(1))) == 1L,
  "source records distinguish 55 typed paths from the one approved fixed-schema adapter"
)
expect_true(
  sum(vapply(records, function(record) isTRUE(record$accepted_control), logical(1))) == 23L,
  "all 23 executable R and C control cells are accepted"
)
forced_runners <- unique(vapply(records[vapply(records, function(record) {
  isTRUE(record$forced_symbols)
}, logical(1))], function(record) record$runner, character(1)))
expect_true(
  identical(sort(forced_runners), c("c_call", "cpp11", "zigr")),
  "source records preserve each generator's real force-symbol convention"
)
gaps <- vapply(records, function(record) !isTRUE(record$executable), logical(1))
gap_keys <- sort(vapply(records[gaps], function(record) {
  paste(record$runner, record$fixture, sep = "/")
}, character(1)))
expect_true(
  identical(gap_keys, sort(c("cpp11/F07", "cpp11/F10", "cpp11/F12", "r/F10", "zigr/F08"))),
  "only five source-backed fixture gaps remain"
)

cpp11_source <- source_ledger_read_lines(root_dir, "src/cpp11/src/fixture.cpp")
cpp11_native <- source_ledger_read_lines(root_dir, "src/cpp11/src/cpp11.cpp")
cpp11_generated_r <- source_ledger_read_lines(root_dir, "src/cpp11/R/cpp11.R")
cpp11_dependency <- cpp11_fixture_dependency_source()
for (fixture in c("F07", "F10", "F12")) {
  expect_true(
    cpp11_gap_source_backed(
      fixture, cpp11_source, cpp11_native, cpp11_generated_r, cpp11_dependency
    ),
    paste("cpp11", fixture, "gap matches the fixture and installed public headers")
  )
}
fake_partial_output <- c(
  cpp11_source,
  "[[cpp11::register]] cpp11::list fixture_outputs() { return cpp11::writable::list(); }"
)
expect_true(
  !cpp11_gap_source_backed(
    "F12", fake_partial_output, cpp11_native, cpp11_generated_r, cpp11_dependency
  ),
  "an exposed partial cpp11 F12 implementation invalidates the non-executable gap"
)
fake_complex_dependency <- cpp11_dependency
fake_complex_dependency$header_paths <- c(
  fake_complex_dependency$header_paths, "include/cpp11/complexes.hpp"
)
expect_true(
  !cpp11_gap_source_backed(
    "F07", cpp11_source, cpp11_native, cpp11_generated_r, fake_complex_dependency
  ),
  "a cpp11 complex API invalidates the capability-gap evidence"
)
external_pointer_path <- grep(
  "external_pointer\\.hpp$", names(cpp11_dependency$header_contents), value = TRUE
)
fake_tagged_dependency <- cpp11_dependency
fake_tagged_dependency$header_contents[[external_pointer_path]] <- c(
  fake_tagged_dependency$header_contents[[external_pointer_path]],
  "R_ExternalPtrTag(pointer);"
)
expect_true(
  !cpp11_gap_source_backed(
    "F10", cpp11_source, cpp11_native, cpp11_generated_r, fake_tagged_dependency
  ),
  "a cpp11 tag contract invalidates the native-state capability-gap evidence"
)
expect_true(
  fixture_signatures_are_typed(
    c("fn fixtureNumeric(value: []const f64) R.SEXP {"), "\\bR\\.SEXP\\b"
  ) &&
    !fixture_signatures_are_typed(
      c("fn fixtureNumeric(value: R.SEXP) R.SEXP {"), "\\bR\\.SEXP\\b"
    ) &&
    fixture_signatures_are_typed(
      c("fn fixture_schema(value: ListSexp) -> savvy::Result<Sexp> {"),
      "\\b(SEXP|Sexp)\\b"
    ),
  "typed signature checks reject raw arguments without rejecting typed SEXP wrappers"
)

damaged <- unserialize(serialize(records, NULL))
damaged_index <- which(vapply(damaged, function(record) {
  identical(record$runner, "zigr") && identical(record$fixture, "F01")
}, logical(1)))
damaged[[damaged_index]]$product_eligible <- FALSE
expect_error(
  "damaged product source path",
  validate_fixture_source_gate(damaged, evidence),
  "product label is not source eligible for zigr/F01"
)

damaged_gap <- unserialize(serialize(records, NULL))
damaged_gap_index <- which(vapply(damaged_gap, function(record) {
  identical(record$runner, "zigr") && identical(record$fixture, "F08")
}, logical(1)))
damaged_gap[[damaged_gap_index]]$source_class <- "unsupported_claim_invalid"
expect_error(
  "unbacked product gap",
  validate_fixture_source_gate(damaged_gap, evidence),
  "fixture gap is not source backed for zigr/F08"
)

damaged_adapter_evidence <- evidence
adapter_index <- which(
  damaged_adapter_evidence$fixture_rows$runner == "zigr" &
    damaged_adapter_evidence$fixture_rows$fixture == "F01"
)
damaged_adapter_evidence$fixture_rows$path_kind[[adapter_index]] <- "generated_public_adapter"
expect_error(
  "invented public adapter label",
  validate_fixture_source_gate(records, damaged_adapter_evidence),
  "adapter label has no approved public adapter for zigr/F01"
)

spec <- load_source_ledger_spec(root_dir)
for (runner in names(spec$fixture_runners)) {
  fixture_spec <- spec$fixture_runners[[runner]]
  expect_true(
    source_ledger_file_identity(
      root_dir, fixture_spec$source_globs, paste(runner, "fixture source")
    )$file_count > 0L,
    paste(runner, "fixture source selection")
  )
  expect_true(
    source_ledger_file_identity(
      root_dir, fixture_spec$build_files, paste(runner, "fixture build")
    )$file_count > 0L,
    paste(runner, "fixture build selection")
  )
  expect_true(
    source_ledger_file_identity(
      root_dir, fixture_spec$generated_glue$paths, paste(runner, "fixture glue")
    )$file_count > 0L,
    paste(runner, "fixture generated-glue selection")
  )
}

arguments <- commandArgs(trailingOnly = TRUE)
live <- "--live" %in% arguments
runner_argument <- grep("^--runner=", arguments, value = TRUE)
if (length(runner_argument) > 1L) stop("product fixture test accepts at most one runner")
live_runner <- if (length(runner_argument) == 1L) {
  sub("^--runner=", "", runner_argument[[1L]])
} else {
  ""
}
if (live && nzchar(live_runner)) {
  run_live_product_fixture_gate(root_dir, evidence, live_runner)
} else if (live) {
  # The benchmark harness isolates runners by process. Keep generated class
  # registries isolated here too while testing load, unload, and reload inside
  # each runner process.
  statuses <- vapply(evidence$runners, function(runner) {
    system2(
      file.path(R.home("bin"), "Rscript"),
      c(script_path, "--live", paste0("--runner=", runner))
    )
  }, integer(1))
  if (any(statuses != 0L)) {
    failed <- names(statuses)[statuses != 0L]
    stop(sprintf("isolated live fixture gates failed for: %s", paste(failed, collapse = ", ")))
  }
}

cat(sprintf(
  paste0(
    "Product fixture %s gate passed: 84 cells, 56 product paths, ",
    "23 controls, and five source-backed gaps.\n"
  ),
  if (live && nzchar(live_runner)) {
    paste("source and live", live_runner)
  } else if (live) {
    "source and isolated live"
  } else {
    "source"
  }
))
