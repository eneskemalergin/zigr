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
  sum(vapply(records, function(record) identical(record$source_class, "generated_typed"), logical(1))) == 46L &&
    sum(vapply(records, function(record) identical(record$source_class, "generated_public_adapter"), logical(1))) == 10L,
  "source records distinguish 46 typed paths from ten approved public adapters"
)
adapter_keys <- sort(vapply(records[vapply(records, function(record) {
  identical(record$source_class, "generated_public_adapter")
}, logical(1))], function(record) {
  paste(record$runner, record$fixture, sep = "/")
}, character(1)))
expect_true(
  identical(adapter_keys, sort(c(
    "zigr/F09", "rcpp/F02", "rcpp/F03", "rcpp/F04", "rcpp/F05",
    "rcpp/F06", "rcpp/F07", "rcpp/F08", "rcpp/F10", "rcpp/F11"
  ))),
  "only the reviewed zigr schema and strict Rcpp boundaries are public adapters"
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

contract_cases <- fixture_contract_cases()
case_counts <- vapply(contract_cases, length, integer(1))
expect_true(
  identical(
    case_counts,
    c(F01 = 1L, F02 = 11L, F03 = 4L, F04 = 6L, F05 = 4L, F06 = 3L,
      F07 = 3L, F08 = 3L, F09 = 16L, F12 = 1L)
  ),
  "semantic matrix retains every declared value and output case"
)
expect_true(
  identical(fixture_expected_value_counts(names(contract_cases)),
            c(valid = 24L, invalid = 28L, allocation = 28L, copy = 3L)),
  "semantic matrix has exact valid, invalid, fresh-allocation, and copy coverage"
)
expect_true(
  all(vapply(contract_cases, function(cases) {
    ids <- vapply(cases, `[[`, character(1), "id")
    all(nzchar(ids)) && !anyDuplicated(ids)
  }, logical(1))),
  "case identifiers are nonblank and unique within every fixture"
)
expect_true(
  all(c("missing scalar", "integer type", "logical type", "empty", "length two") %in%
        vapply(contract_cases$F02, `[[`, character(1), "id")) &&
    all(c("reordered", "duplicate names", "extra attribute", "missing ratio",
          "wrong enabled type") %in%
        vapply(contract_cases$F09, `[[`, character(1), "id")),
  "strict scalar and fixed-schema invalid families remain present"
)
expect_error(
  "NA and NaN metadata distinction",
  fixture_assert_same(NA_real_, NaN, "negative metadata probe"),
  "fixture values differ"
)
expect_error(
  "signed zero metadata distinction",
  fixture_assert_same(-0.0, 0.0, "negative metadata probe"),
  "fixture values differ"
)
expect_error(
  "attribute metadata distinction",
  fixture_assert_same(1:2, structure(1:2, names = c("a", "b")), "negative metadata probe"),
  "fixture values differ"
)
utf8_probe <- enc2utf8("façade")
latin1_probe <- iconv("façade", from = "UTF-8", to = "latin1")
Encoding(utf8_probe) <- "UTF-8"
Encoding(latin1_probe) <- "latin1"
expect_true(identical(utf8_probe, latin1_probe), "encoding probe has equal R string values")
expect_error(
  "encoding metadata distinction",
  fixture_assert_same(utf8_probe, latin1_probe, "negative metadata probe"),
  "fixture values differ"
)

expected_runner_values <- list(
  c_call = c(valid = 24L, invalid = 28L, allocation = 28L, copy = 3L),
  cpp11 = c(valid = 21L, invalid = 27L, allocation = 18L, copy = 2L),
  extendr = c(valid = 24L, invalid = 28L, allocation = 28L, copy = 3L),
  r = c(valid = 24L, invalid = 28L, allocation = 28L, copy = 3L),
  rcpp = c(valid = 24L, invalid = 28L, allocation = 28L, copy = 3L),
  savvy = c(valid = 24L, invalid = 28L, allocation = 28L, copy = 3L),
  zigr = c(valid = 22L, invalid = 27L, allocation = 26L, copy = 3L)
)
for (runner in names(expected_runner_values)) {
  supported <- evidence$fixture_rows$fixture[
    evidence$fixture_rows$runner == runner & evidence$fixture_rows$executable
  ]
  expect_true(
    identical(fixture_expected_value_counts(supported), expected_runner_values[[runner]]),
    sprintf("%s has the expected semantic case disposition", runner)
  )
}

source_has_all <- function(path, needles) {
  source <- source_ledger_read_lines(root_dir, path)
  all(vapply(needles, function(needle) any(grepl(needle, source, fixed = TRUE)), logical(1)))
}
expect_true(
  source_has_all("src/c_call/register.c", c(
    "c_p4_lifecycle_reset", "c_p4_lifecycle_snapshot", "c_p4_same_data_pointer",
    "c_p4_wrong_pointer", "c_p4_cleared_pointer_like", "c_p4_altrep_new",
    "R_set_altinteger_Get_region_method"
  )),
  "C control retains lifecycle, pointer, storage, and ALTREP diagnostics"
)
expect_true(
  source_has_all("src/zig/fixture.zig", c(
    "lifecycle_counts.constructor += 1", "lifecycle_counts.method += 1",
    "lifecycle_counts.error_count += 1", "lifecycle_counts.finalizer += 1",
    ".name = \"fixture_lifecycle_reset\"", ".name = \"fixture_lifecycle_counts\""
  )) &&
    source_has_all("src/zig/fixture/R/fixture.R", c(
      "fixture_lifecycle_reset", "fixture_lifecycle_counts"
    )),
  "zigr lifecycle instrumentation is retained through its public package path"
)
expect_true(
  source_has_all("src/cpp/fixture/src/fixture.cpp", c(
    "++lifecycle_counts.constructor", "++lifecycle_counts.method",
    "++lifecycle_counts.error", "++lifecycle_counts.finalizer",
    "fixture_lifecycle_reset", "fixture_lifecycle_counts"
  )) &&
    source_has_all("src/cpp/fixture/src/RcppExports.cpp", c(
      "_zigrRcpp_fixture_lifecycle_reset", "_zigrRcpp_fixture_lifecycle_counts"
    )),
  "Rcpp lifecycle instrumentation is retained in source and official generated glue"
)
expect_true(
  source_has_all("src/cpp11/src/fixture.cpp", c(
    "++lifecycle_counts.error", "fixture_lifecycle_reset", "fixture_lifecycle_counts"
  )) &&
    source_has_all("src/cpp11/src/cpp11.cpp", c(
      "_zigrCpp11_fixture_lifecycle_reset", "_zigrCpp11_fixture_lifecycle_counts"
    )),
  "cpp11 error instrumentation is retained in source and official generated glue"
)
expect_true(
  source_has_all("src/extendr/fixture/src/rust/src/lib.rs", c(
    "CONSTRUCTOR_COUNT.fetch_add", "METHOD_COUNT.fetch_add", "ERROR_COUNT.fetch_add",
    "FINALIZER_COUNT.fetch_add", "fn fixture_lifecycle_reset", "fn fixture_lifecycle_counts"
  )) &&
    source_has_all("src/extendr/fixture/R/extendr-wrappers.R", c(
      "wrap__fixture_lifecycle_reset", "wrap__fixture_lifecycle_counts"
    )),
  "extendr lifecycle instrumentation is retained through official generated wrappers"
)
expect_true(
  source_has_all("src/savvy/fixture/src/rust/src/lib.rs", c(
    "CONSTRUCTOR_COUNT.fetch_add", "METHOD_COUNT.fetch_add", "ERROR_COUNT.fetch_add",
    "FINALIZER_COUNT.fetch_add", "fn fixture_lifecycle_reset", "fn fixture_lifecycle_counts"
  )) &&
    source_has_all("src/savvy/fixture/src/init.c", c(
      "savvy_fixture_lifecycle_reset__impl", "savvy_fixture_lifecycle_counts__impl"
    )),
  "Savvy lifecycle instrumentation is retained through official generated glue"
)

r_supported <- evidence$fixture_rows$fixture[
  evidence$fixture_rows$runner == "r" & evidence$fixture_rows$executable
]
synthetic_proof <- list(
  values = fixture_expected_value_counts(r_supported),
  output = c(construction = 1L, retention = 1L),
  altrep = fixture_altrep_expectation("r"),
  recovery = c(error = 32L, recovery = 32L),
  state = c(constructor = 0L, method = 0L, finalizer = 0L)
)
fixture_validate_proof(synthetic_proof, "r", r_supported)
damaged_proof <- synthetic_proof
damaged_proof$recovery[["recovery"]] <- 31L
expect_error(
  "damaged recovery proof",
  fixture_validate_proof(damaged_proof, "r", r_supported),
  "recovery counts differ"
)

output_value <- list(numeric = c(-0.0, NA_real_, NaN))
torture_states <- logical()
output_functions <- list(fixture_outputs = function() {
  torture_enabled <- gctorture(FALSE)
  gctorture(torture_enabled)
  torture_states <<- c(torture_states, torture_enabled)
  output_value
})
output_proof <- fixture_run_output_proof(
  "forced-GC probe", output_functions, "F12",
  list(fixture_outputs = function() output_value)
)
expect_true(
  identical(output_proof, c(construction = 1L, retention = 1L)) &&
    identical(torture_states, c(TRUE, FALSE)),
  "F12 construction runs under forced GC and restores the prior GC mode before retention"
)
damaged_output_proof <- synthetic_proof
damaged_output_proof$output[["construction"]] <- 0L
expect_error(
  "damaged output protection proof",
  fixture_validate_proof(damaged_output_proof, "r", r_supported),
  "output protection proof differs"
)
expect_error(
  "missing native-state lifecycle diagnostics",
  fixture_run_state_lifecycle("missing diagnostics", list(), "F10", list(), NULL),
  "F10 lifecycle counters are missing"
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
expect_true(
  fixture_definition_contains(
    c(
      "double fixture_scalar(Rcpp::RObject value) {",
      "  if (!Rcpp::is<Rcpp::NumericVector>(value)) Rcpp::stop(\"wrong type\");",
      "}",
      "double unrelated(Rcpp::RObject value) {",
      "  if (!Rcpp::is<Rcpp::RawVector>(value)) Rcpp::stop(\"wrong type\");",
      "}"
    ),
    "^[[:space:]]*.*\\bfixture_scalar[[:space:]]*\\([[:space:]]*Rcpp::RObject",
    "Rcpp::is<Rcpp::NumericVector>(value)"
  ) &&
    !fixture_definition_contains(
      c(
        "double fixture_scalar(Rcpp::RObject value) { return 1.0; }",
        "double unrelated(Rcpp::RObject value) {",
        "  if (!Rcpp::is<Rcpp::NumericVector>(value)) Rcpp::stop(\"wrong type\");",
        "}"
      ),
      "^[[:space:]]*.*\\bfixture_scalar[[:space:]]*\\([[:space:]]*Rcpp::RObject",
      "Rcpp::is<Rcpp::NumericVector>(value)"
    ),
  "public-adapter guards must occur in the reviewed Rcpp definition"
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
    "Product fixture %s gate passed: 84 cells, 56 product paths ",
    "(9 Tier A and 47 Tier B), 23 controls, and five source-backed gaps.\n"
  ),
  if (live && nzchar(live_runner)) {
    paste("source and live", live_runner)
  } else if (live) {
    "source and isolated live"
  } else {
    "source"
  }
))
