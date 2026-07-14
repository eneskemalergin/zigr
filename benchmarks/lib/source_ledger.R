source_ledger_schema_version <- function() "p4.3-tool-source-ledger-v2"

source_ledger_scalar <- function(value, fallback = "") {
  value <- as.character(value)
  if (length(value) == 0L || is.na(value[[1L]]) || !nzchar(value[[1L]])) fallback else value[[1L]]
}

source_ledger_digest_lines <- function(lines, prefix = "source-ledger-") {
  temporary <- tempfile(prefix)
  on.exit(unlink(temporary), add = TRUE)
  writeLines(enc2utf8(as.character(lines)), temporary, useBytes = TRUE)
  unname(as.character(tools::md5sum(temporary))[[1L]])
}

source_ledger_object_digest <- function(value) {
  temporary <- tempfile("source-ledger-object-")
  on.exit(unlink(temporary), add = TRUE)
  jsonlite::write_json(value, temporary, auto_unbox = TRUE, null = "null", digits = NA)
  unname(as.character(tools::md5sum(temporary))[[1L]])
}

source_ledger_relative_path <- function(root_dir, path) {
  root <- normalizePath(root_dir)
  parent <- normalizePath(file.path(root, ".."))
  absolute <- normalizePath(path)
  root_prefix <- paste0(root, .Platform$file.sep)
  parent_prefix <- paste0(parent, .Platform$file.sep)
  if (startsWith(absolute, root_prefix)) {
    return(gsub("\\\\", "/", substring(absolute, nchar(root_prefix) + 1L)))
  }
  if (startsWith(absolute, parent_prefix)) {
    return(paste0("../", gsub("\\\\", "/", substring(absolute, nchar(parent_prefix) + 1L))))
  }
  stop(sprintf("source ledger path is outside the repository: %s", absolute))
}

source_ledger_expand_paths <- function(root_dir, patterns, label) {
  patterns <- as.character(patterns)
  selections <- lapply(patterns, function(pattern) {
    paths <- Sys.glob(file.path(root_dir, pattern), dirmark = FALSE)
    paths <- unique(normalizePath(paths[file.exists(paths)], mustWork = TRUE))
    paths[!file.info(paths)$isdir]
  })
  missing_patterns <- patterns[lengths(selections) == 0L]
  if (length(missing_patterns) > 0L) {
    stop(sprintf("%s patterns select no regular files: %s", label, paste(missing_patterns, collapse = ", ")))
  }
  paths <- sort(unique(unlist(selections, use.names = FALSE)))
  relative_paths <- unname(vapply(
    paths,
    function(path) source_ledger_relative_path(root_dir, path),
    character(1)
  ))
  relative_order <- order(relative_paths)
  list(
    paths = unname(paths[relative_order]),
    relative_paths = relative_paths[relative_order]
  )
}

source_ledger_directory_identity <- function(path, label) {
  root <- normalizePath(path, mustWork = TRUE)
  paths <- list.files(root, full.names = TRUE, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  paths <- sort(paths[file.exists(paths) & !file.info(paths)$isdir])
  if (length(paths) == 0L) stop(sprintf("%s contains no regular files", label))
  relative <- gsub("\\\\", "/", substring(paths, nchar(paste0(root, .Platform$file.sep)) + 1L))
  list(
    path = root,
    method = "sorted-relative-path-and-file-md5",
    digest = source_ledger_digest_lines(paste(relative, unname(as.character(tools::md5sum(paths))), sep = "\t")),
    file_count = length(paths)
  )
}

source_ledger_file_identity <- function(root_dir, patterns, label) {
  selection <- source_ledger_expand_paths(root_dir, patterns, label)
  file_md5 <- unname(as.character(tools::md5sum(selection$paths)))
  if (anyNA(file_md5)) stop(sprintf("could not hash every %s file", label))
  list(
    method = "sorted-relative-path-and-file-md5",
    digest = source_ledger_digest_lines(paste(selection$relative_paths, file_md5, sep = "\t")),
    file_count = length(selection$paths),
    paths = as.list(selection$relative_paths)
  )
}

load_source_ledger_spec <- function(root_dir) {
  path <- file.path(root_dir, "source_ledger.json")
  if (!file.exists(path)) stop("source ledger specification is missing")
  spec <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  if (is.null(names(spec)) || any(!nzchar(names(spec))) || anyDuplicated(names(spec))) {
    stop("source ledger specification must be a uniquely named object")
  }
  if (!setequal(names(spec), c("schema_version", "vocabulary_version", "runners", "fixture_runners"))) {
    stop("source ledger specification fields differ from the schema")
  }
  if (!identical(as.integer(spec$schema_version), 2L)) stop("unsupported source ledger schema version")
  if (!identical(as.character(spec$vocabulary_version), "p4.3-source-ledger-2026-07-13")) {
    stop("unsupported source ledger vocabulary version")
  }
  expected_runners <- c("c_call", "cpp11", "extendr", "r", "rcpp", "savvy", "zigr")
  if (is.null(spec$runners) || !identical(sort(names(spec$runners)), expected_runners)) {
    stop("source ledger runner set differs from the seven-runner comparison set")
  }
  required <- c("role", "tool_kind", "verifier_kind", "source_globs", "build_files", "build_recipe", "generated_glue")
  expected_roles <- c(
    c_call = "c_control", cpp11 = "product", extendr = "product", r = "reference_and_baseline",
    rcpp = "product", savvy = "product", zigr = "product"
  )
  expected_tool_kinds <- c(
    c_call = "c", cpp11 = "r_package", extendr = "cargo", r = "r",
    rcpp = "r_package", savvy = "cargo", zigr = "zig"
  )
  expected_verifiers <- c(
    c_call = "registered_c", cpp11 = "cpp11_generated", extendr = "extendr_mixed",
    r = "r_provenance", rcpp = "rcpp_legacy", savvy = "savvy_legacy", zigr = "zigr_mixed"
  )
  for (runner in expected_runners) {
    record <- spec$runners[[runner]]
    if (!is.list(record) || is.null(names(record)) || any(!nzchar(names(record))) || anyDuplicated(names(record))) {
      stop(sprintf("source ledger runner %s must be a uniquely named object", runner))
    }
    conditional <- switch(as.character(record$tool_kind),
      r_package = "package",
      cargo = c("cargo_manifest", "cargo_lock"),
      character(0)
    )
    allowed <- c(required, conditional)
    extra <- setdiff(names(record), allowed)
    missing <- required[vapply(required, function(field) is.null(record[[field]]), logical(1))]
    conditional_missing <- conditional[vapply(conditional, function(field) is.null(record[[field]]), logical(1))]
    if (length(c(missing, conditional_missing, extra)) > 0L) {
      stop(sprintf(
        "source ledger runner %s fields differ from the schema; missing: %s; extra: %s",
        runner,
        paste(c(missing, conditional_missing), collapse = ", "),
        paste(extra, collapse = ", ")
      ))
    }
    for (field in c("role", "tool_kind", "verifier_kind", "build_recipe", conditional)) {
      value <- record[[field]]
      if (length(value) != 1L || is.na(value) || !nzchar(as.character(value))) {
        stop(sprintf("source ledger runner %s field %s must be one nonblank string", runner, field))
      }
    }
    for (field in c("source_globs", "build_files")) {
      values <- as.character(unlist(record[[field]], use.names = FALSE))
      if (length(values) == 0L || anyNA(values) || any(!nzchar(values)) || anyDuplicated(values)) {
        stop(sprintf("source ledger runner %s field %s must be a unique nonblank list", runner, field))
      }
    }
    if (!identical(as.character(record$role), unname(expected_roles[[runner]])) ||
        !identical(as.character(record$tool_kind), unname(expected_tool_kinds[[runner]])) ||
        !identical(as.character(record$verifier_kind), unname(expected_verifiers[[runner]]))) {
      stop(sprintf("source ledger runner %s identity differs from the frozen runner map", runner))
    }
    glue <- record$generated_glue
    if (!is.list(glue) || is.null(names(glue)) || any(!nzchar(names(glue))) || anyDuplicated(names(glue)) ||
        !setequal(names(glue), c("kind", "retained_output", "paths"))) {
      stop(sprintf("source ledger runner %s has incomplete generated-glue identity", runner))
    }
    glue_paths <- as.character(unlist(glue$paths, use.names = FALSE))
    if (length(glue$kind) != 1L || is.na(glue$kind) || !nzchar(as.character(glue$kind)) ||
        length(glue$retained_output) != 1L || !is.logical(glue$retained_output) || is.na(glue$retained_output) ||
        length(glue_paths) == 0L || anyNA(glue_paths) || any(!nzchar(glue_paths)) || anyDuplicated(glue_paths)) {
      stop(sprintf("source ledger runner %s has invalid generated-glue identity", runner))
    }
  }
  expected_fixture_verifiers <- c(
    c_call = "registered_c_fixture", cpp11 = "cpp11_package_fixture",
    extendr = "extendr_package_fixture", r = "r_fixture_provenance",
    rcpp = "rcpp_package_fixture", savvy = "savvy_package_fixture",
    zigr = "zigr_package_fixture"
  )
  if (is.null(spec$fixture_runners) ||
      !identical(sort(names(spec$fixture_runners)), expected_runners)) {
    stop("source ledger fixture runner set differs from the seven-runner comparison set")
  }
  fixture_required <- c(
    "verifier_kind", "source_globs", "build_files", "build_recipe", "generated_glue"
  )
  for (runner in expected_runners) {
    record <- spec$fixture_runners[[runner]]
    if (!is.list(record) || is.null(names(record)) || any(!nzchar(names(record))) ||
        anyDuplicated(names(record)) || !setequal(names(record), fixture_required)) {
      stop(sprintf("source ledger fixture runner %s fields differ from the schema", runner))
    }
    if (length(record$verifier_kind) != 1L || is.na(record$verifier_kind) ||
        !identical(as.character(record$verifier_kind), unname(expected_fixture_verifiers[[runner]]))) {
      stop(sprintf("source ledger fixture runner %s verifier differs from the frozen map", runner))
    }
    if (length(record$build_recipe) != 1L || is.na(record$build_recipe) ||
        !nzchar(as.character(record$build_recipe))) {
      stop(sprintf("source ledger fixture runner %s build recipe must be one nonblank string", runner))
    }
    for (field in c("source_globs", "build_files")) {
      values <- as.character(unlist(record[[field]], use.names = FALSE))
      if (length(values) == 0L || anyNA(values) || any(!nzchar(values)) || anyDuplicated(values)) {
        stop(sprintf(
          "source ledger fixture runner %s field %s must be a unique nonblank list",
          runner, field
        ))
      }
    }
    glue <- record$generated_glue
    if (!is.list(glue) || is.null(names(glue)) || any(!nzchar(names(glue))) ||
        anyDuplicated(names(glue)) || !setequal(names(glue), c("kind", "retained_output", "paths"))) {
      stop(sprintf("source ledger fixture runner %s has incomplete generated-glue identity", runner))
    }
    glue_paths <- as.character(unlist(glue$paths, use.names = FALSE))
    if (length(glue$kind) != 1L || is.na(glue$kind) || !nzchar(as.character(glue$kind)) ||
        length(glue$retained_output) != 1L || !is.logical(glue$retained_output) ||
        is.na(glue$retained_output) || length(glue_paths) == 0L || anyNA(glue_paths) ||
        any(!nzchar(glue_paths)) || anyDuplicated(glue_paths)) {
      stop(sprintf("source ledger fixture runner %s has invalid generated-glue identity", runner))
    }
  }
  attr(spec, "path") <- normalizePath(path)
  spec
}

source_ledger_read_lines <- function(root_dir, relative_path) {
  readLines(file.path(root_dir, relative_path), warn = FALSE, encoding = "UTF-8")
}

source_ledger_signature <- function(lines, pattern, max_lines = 8L) {
  index <- grep(pattern, lines, perl = TRUE)
  if (length(index) == 0L) return("")
  start <- index[[1L]]
  end <- min(length(lines), start + max_lines - 1L)
  selected <- character(0)
  for (line in lines[start:end]) {
    selected <- c(selected, trimws(line))
    if (grepl("\\{", line)) break
  }
  paste(selected, collapse = " ")
}

source_ledger_annotation_before <- function(lines, signature_pattern, annotation, distance = 3L) {
  indices <- grep(signature_pattern, lines, perl = TRUE)
  if (length(indices) == 0L) return(FALSE)
  any(vapply(indices, function(index) {
    start <- max(1L, index - distance)
    any(grepl(annotation, lines[start:index], fixed = TRUE))
  }, logical(1)))
}

source_ledger_definition_present <- function(lines, signature_pattern, max_lines = 8L) {
  indices <- grep(signature_pattern, lines, perl = TRUE)
  if (length(indices) == 0L) return(FALSE)
  any(vapply(indices, function(index) {
    end <- min(length(lines), index + max_lines - 1L)
    for (line in lines[index:end]) {
      brace <- regexpr("{", line, fixed = TRUE)[[1L]]
      semicolon <- regexpr(";", line, fixed = TRUE)[[1L]]
      if (brace > 0L && (semicolon < 0L || brace < semicolon)) return(TRUE)
      if (semicolon > 0L) return(FALSE)
    }
    FALSE
  }, logical(1)))
}

source_ledger_c_definition_present <- function(lines, symbol) {
  signature_pattern <- sprintf(
    "^[[:space:]]*(static[[:space:]]+)?SEXP[[:space:]]+%s[[:space:]]*\\(",
    symbol
  )
  source_ledger_definition_present(lines, signature_pattern)
}

source_ledger_cpp11_wrapper_present <- function(lines, symbol) {
  signature_pattern <- sprintf(
    "^[[:space:]]*extern[[:space:]]+\"C\"[[:space:]]+SEXP[[:space:]]+%s[[:space:]]*\\(",
    symbol
  )
  source_ledger_definition_present(lines, signature_pattern)
}

source_ledger_r_call_present <- function(lines, symbol) {
  any(grepl(paste0(".Call(`", symbol, "`"), lines, fixed = TRUE))
}

source_ledger_rust_definition_present <- function(lines, symbol) {
  signature_pattern <- sprintf(
    "^[[:space:]]*(pub[[:space:]]+)?(unsafe[[:space:]]+)?(extern[[:space:]]+\"C\"[[:space:]]+)?fn[[:space:]]+%s[[:space:]]*\\(",
    symbol
  )
  source_ledger_definition_present(lines, signature_pattern)
}

source_ledger_savvy_wrapper_present <- function(init, rust, symbol) {
  if (source_ledger_c_definition_present(init, symbol) ||
      source_ledger_rust_definition_present(rust, symbol)) {
    return(TRUE)
  }
  wrapper_name <- sub("^savvy_", "", sub("__impl$", "", symbol))
  any(grepl(
    sprintf("^[[:space:]]*SAVVY_WRAP[12][[:space:]]*\\([[:space:]]*%s[[:space:]]*,", wrapper_name),
    init,
    perl = TRUE
  ))
}

source_ledger_task_record <- function(runner, task, symbol, evidence_row) {
  list(
    runner = runner,
    task = task,
    configured_symbol = symbol,
    executable = isTRUE(evidence_row$executable),
    declared_role = as.character(evidence_row$implementation_role),
    declared_path_kind = as.character(evidence_row$path_kind),
    source_class = if (nzchar(symbol)) "unverified" else "not_executable",
    annotation_present = FALSE,
    typed_public_signature = FALSE,
    generated_native_wrapper = FALSE,
    generated_r_wrapper = FALSE,
    registered_entry = FALSE,
    configured_symbol_present = FALSE,
    forced_symbols = FALSE,
    product_eligible = FALSE,
    accepted_control = FALSE,
    source_paths = list(),
    reason = if (nzchar(symbol)) "source path has not been classified" else as.character(evidence_row$reason)
  )
}

verify_cpp11_source_row <- function(record, root_dir) {
  fixture <- source_ledger_read_lines(root_dir, "src/cpp11/src/fixture.cpp")
  native_glue <- source_ledger_read_lines(root_dir, "src/cpp11/src/cpp11.cpp")
  r_glue <- source_ledger_read_lines(root_dir, "src/cpp11/R/cpp11.R")
  symbol <- record$configured_symbol
  function_name <- sub("^_zigrCpp11_", "", symbol)
  signature_pattern <- sprintf("^[[:space:]]*([^/].*)?\\b%s[[:space:]]*\\(", function_name)
  signature <- source_ledger_signature(fixture, signature_pattern)
  record$annotation_present <- source_ledger_annotation_before(
    fixture, signature_pattern, "[[cpp11::register]]", distance = 2L
  )
  record$typed_public_signature <- nzchar(signature) && !grepl("\\bSEXP\\b", signature)
  record$generated_native_wrapper <- source_ledger_cpp11_wrapper_present(native_glue, symbol)
  record$generated_r_wrapper <- source_ledger_r_call_present(r_glue, symbol)
  record$registered_entry <- any(grepl(paste0("{\"", symbol, "\""), native_glue, fixed = TRUE))
  record$configured_symbol_present <- record$generated_native_wrapper
  record$forced_symbols <- any(grepl("R_forceSymbols(dll, TRUE)", native_glue, fixed = TRUE)) &&
    any(grepl("R_useDynamicSymbols(dll, FALSE)", native_glue, fixed = TRUE))
  record$product_eligible <- all(c(
    record$annotation_present,
    record$typed_public_signature,
    record$generated_native_wrapper,
    record$generated_r_wrapper,
    record$registered_entry,
    record$configured_symbol_present,
    record$forced_symbols
  ))
  record$source_class <- if (record$product_eligible) "generated_typed" else "generated_path_invalid"
  record$source_paths <- as.list(c("src/cpp11/src/fixture.cpp", "src/cpp11/src/cpp11.cpp", "src/cpp11/R/cpp11.R"))
  record$reason <- if (record$product_eligible) {
    "cpp11 annotation, typed signature, generated native and R wrappers, registration, and configured symbol are present"
  } else {
    "cpp11 product path is missing at least one required source marker"
  }
  record
}

verify_rcpp_source_row <- function(record, root_dir) {
  lines <- source_ledger_read_lines(root_dir, "src/cpp/main.cpp")
  symbol <- record$configured_symbol
  signature <- source_ledger_signature(lines, sprintf("^[[:space:]]*SEXP[[:space:]]+%s[[:space:]]*\\(", symbol))
  record$configured_symbol_present <- nzchar(signature)
  typed_internals <- identical(record$task, "05_fib_recursive") &&
    any(grepl("Rcpp::as<long long>", lines, fixed = TRUE)) && any(grepl("Rcpp::wrap", lines, fixed = TRUE))
  record$source_class <- if (typed_internals) "handwritten_typed_internals" else "raw_ffi"
  record$typed_public_signature <- FALSE
  record$source_paths <- list("src/cpp/main.cpp")
  record$reason <- if (typed_internals) {
    "the handwritten raw-SEXP function uses Rcpp conversion internally but has no Rcpp export or generated wrapper"
  } else {
    "the configured function has a handwritten raw-SEXP signature and no Rcpp export or generated wrapper"
  }
  record
}

verify_extendr_source_row <- function(record, root_dir) {
  rust <- source_ledger_read_lines(root_dir, "src/extendr/rust/src/lib.rs")
  entry <- source_ledger_read_lines(root_dir, "src/extendr/entrypoint.c")
  symbol <- record$configured_symbol
  raw <- startsWith(symbol, "extendr_ffi_")
  if (raw) {
    signature <- source_ledger_signature(rust, sprintf("fn[[:space:]]+%s[[:space:]]*\\(", symbol))
    record$source_class <- "raw_ffi_substitution"
    record$typed_public_signature <- FALSE
    record$registered_entry <- any(grepl(paste0("{\"", symbol, "\""), entry, fixed = TRUE))
    record$configured_symbol_present <- nzchar(signature) && record$registered_entry
    record$source_paths <- as.list(c("src/extendr/rust/src/lib.rs", "src/extendr/entrypoint.c"))
    record$reason <- "the configured row bypasses #[extendr] and calls a separately registered raw-FFI export"
    return(record)
  }
  function_name <- sub("^wrap__", "", symbol)
  signature_pattern <- sprintf("^[[:space:]]*(pub[[:space:]]+)?fn[[:space:]]+%s[[:space:]]*\\(", function_name)
  signature <- source_ledger_signature(rust, signature_pattern)
  record$annotation_present <- source_ledger_annotation_before(rust, signature_pattern, "#[extendr]", distance = 2L)
  record$typed_public_signature <- nzchar(signature) && !grepl("extendr_ffi::SEXP|\\bSEXP\\b", signature)
  record$generated_native_wrapper <- record$annotation_present && any(grepl(paste0("fn ", function_name, ";"), rust, fixed = TRUE))
  record$registered_entry <- record$generated_native_wrapper
  record$configured_symbol_present <- record$generated_native_wrapper
  record$forced_symbols <- any(grepl("R_useDynamicSymbols((DllInfo *)dll, FALSE)", entry, fixed = TRUE))
  record$source_class <- "generated_macro_fixture_invalid"
  record$source_paths <- as.list(c("src/extendr/rust/src/lib.rs", "src/extendr/entrypoint.c"))
  record$reason <- "the macro wrapper exists, but generated R glue is not retained and the mixed runner keeps dynamic symbol lookup enabled"
  record
}

verify_savvy_source_row <- function(record, root_dir) {
  rust <- source_ledger_read_lines(root_dir, "src/savvy/rust/src/lib.rs")
  init <- source_ledger_read_lines(root_dir, "src/savvy/init.c")
  symbol <- record$configured_symbol
  typed_name <- sub("^savvy_", "", sub("__impl$", "", symbol))
  typed_stub <- any(grepl(sprintf("^[[:space:]]*fn[[:space:]]+%s[[:space:]]*\\(", typed_name), rust, perl = TRUE))
  record$source_class <- "raw_ffi_with_handwritten_savvy_shaped_glue"
  record$typed_public_signature <- FALSE
  record$registered_entry <- any(grepl(paste0("{\"", symbol, "\""), init, fixed = TRUE))
  record$configured_symbol_present <- source_ledger_savvy_wrapper_present(init, rust, symbol)
  record$forced_symbols <- FALSE
  record$source_paths <- as.list(c("src/savvy/rust/src/lib.rs", "src/savvy/init.c"))
  record$reason <- if (typed_stub) {
    "an unused typed Rust function exists, but the configured symbol uses handwritten raw FFI and handwritten C glue"
  } else {
    "the configured symbol uses handwritten raw FFI and handwritten C glue without a #[savvy] public signature"
  }
  record
}

verify_zigr_source_row <- function(record, root_dir) {
  main <- source_ledger_read_lines(root_dir, "src/zig/main.zig")
  generator <- source_ledger_read_lines(root_dir, "../src/export.zig")
  symbol <- record$configured_symbol
  product_candidate <- grepl("^[0-9]+_boundary_.*_generated$", record$task)
  method_symbol <- identical(symbol, "main_FixtureState__increment")
  block_start <- if (method_symbol) {
    grep("const FixtureMethods = zigr.@\"export\".generateMethods", main, fixed = TRUE)
  } else {
    grep("const FixtureExports = zigr.@\"export\".generateExports", main, fixed = TRUE)
  }
  generated_block <- character(0)
  if (length(block_start) == 1L) {
    block_end <- grep("\\);[[:space:]]*$", main[block_start:length(main)], perl = TRUE)
    if (length(block_end) > 0L) {
      generated_block <- main[block_start:(block_start + block_end[[1L]] - 1L)]
    }
  }
  declaration_name <- if (method_symbol) "increment" else symbol
  declaration <- grep(paste0(".name = \"", declaration_name, "\""), generated_block, fixed = TRUE, value = TRUE)
  function_name <- ""
  if (length(declaration) > 0L) {
    function_name <- sub(".*\\.func = ([A-Za-z0-9_]+).*", "\\1", declaration[[1L]])
  }
  signature <- if (nzchar(function_name)) {
    source_ledger_signature(main, sprintf("^[[:space:]]*fn[[:space:]]+%s[[:space:]]*\\(", function_name))
  } else {
    ""
  }
  record$annotation_present <- product_candidate && nzchar(function_name)
  typed_fixed_schema <- identical(symbol, "zigr_fixture_schema") &&
    any(grepl("convert.fromSEXP(FixtureSchema", main, fixed = TRUE))
  record$typed_public_signature <- record$annotation_present && nzchar(signature) &&
    (!grepl("R.SEXP", signature, fixed = TRUE) || typed_fixed_schema)
  generator_name <- if (method_symbol) "pub fn generateMethods" else "pub fn generateExports"
  generated_name <- if (method_symbol) "FixtureMethods" else "FixtureExports"
  record$generated_native_wrapper <- record$annotation_present && any(grepl(generator_name, generator, fixed = TRUE))
  record$registered_entry <- record$generated_native_wrapper &&
    any(grepl(paste0(generated_name, ".init(info)"), main, fixed = TRUE)) &&
    any(grepl(paste0(generated_name, ".call_defs"), main, fixed = TRUE) |
        grepl(paste0(generated_name, ".ext_defs"), main, fixed = TRUE))
  record$configured_symbol_present <- if (method_symbol) {
    nzchar(function_name) &&
      any(grepl('safeName(T) ++ "__" ++ exp.name', generator, fixed = TRUE))
  } else {
    length(declaration) == 1L
  }
  record$forced_symbols <- any(grepl("R.R_forceSymbols(info, 1)", main, fixed = TRUE))
  record$product_eligible <- product_candidate && all(c(
    record$annotation_present,
    record$typed_public_signature,
    record$generated_native_wrapper,
    record$registered_entry,
    record$configured_symbol_present,
    record$forced_symbols
  ))
  record$source_class <- if (record$product_eligible) "generated_typed" else "direct_or_diagnostic"
  record$source_paths <- if (record$product_eligible) {
    as.list(c("src/zig/main.zig", "../src/export.zig"))
  } else {
    as.list(c("src/zig/main.zig", "src/zig/*.zig"))
  }
  record$reason <- if (record$product_eligible) {
    "the boundary row uses a typed generateExports or generateMethods declaration and forced registration"
  } else {
    "the row is a direct, raw-signature, representation, or diagnostic path rather than current generated boundary product evidence"
  }
  record
}

verify_c_source_row <- function(record, root_dir) {
  register <- source_ledger_read_lines(root_dir, "src/c_call/register.c")
  source_paths <- Sys.glob(file.path(root_dir, "src/c_call/*.c"))
  sources <- lapply(source_paths, function(path) readLines(path, warn = FALSE, encoding = "UTF-8"))
  symbol <- record$configured_symbol
  record$source_class <- "registered_c_control"
  record$registered_entry <- any(grepl(paste0("{\"", symbol, "\""), register, fixed = TRUE))
  record$configured_symbol_present <- any(vapply(
    sources,
    source_ledger_c_definition_present,
    logical(1),
    symbol = symbol
  ))
  record$forced_symbols <- any(grepl("R_forceSymbols(dll, TRUE)", register, fixed = TRUE)) &&
    any(grepl("R_useDynamicSymbols(dll, FALSE)", register, fixed = TRUE))
  record$accepted_control <- record$registered_entry && record$configured_symbol_present && record$forced_symbols
  record$source_paths <- as.list(c("src/c_call/register.c", "src/c_call/*.c"))
  record$reason <- if (record$accepted_control) {
    "the handwritten C symbol is explicitly registered with dynamic lookup disabled and forced symbols enabled"
  } else {
    "the C control lacks its definition, registration entry, or forced-symbol policy"
  }
  record
}

verify_r_source_row <- function(record, provenance_record) {
  if (is.null(provenance_record)) stop(sprintf("R source provenance is missing for %s", record$task))
  record$source_class <- as.character(provenance_record$implementation_class)
  record$configured_symbol_present <- isTRUE(record$executable)
  record$accepted_control <- isTRUE(record$executable)
  record$source_paths <- list("src/r/run_all.R")
  record$reason <- if (identical(record$source_class, "pure_r")) {
    "the R function passes its contract-specific AST allowlist and has no declared compiled backend"
  } else if (identical(record$source_class, "optimized_base_r")) {
    "the R function is a separately labeled optimized or runtime-backed baseline"
  } else {
    as.character(provenance_record$reason)
  }
  record
}

validate_product_source_record <- function(record) {
  required <- c(
    "annotation_present", "typed_public_signature", "generated_native_wrapper",
    "registered_entry", "configured_symbol_present", "forced_symbols"
  )
  missing <- required[!vapply(required, function(field) isTRUE(record[[field]]), logical(1))]
  if (record$runner %in% c("cpp11", "extendr") && !isTRUE(record$generated_r_wrapper)) {
    missing <- c(missing, "generated_r_wrapper")
  }
  if (length(missing) > 0L) {
    stop(sprintf(
      "source verifier rejects %s/%s as product evidence; missing: %s",
      record$runner, record$task, paste(unique(missing), collapse = ", ")
    ))
  }
  invisible(record)
}

verify_source_paths <- function(root_dir, configs, evidence, r_provenance, enforce_current_gate = TRUE) {
  records <- list()
  r_records <- named_r_provenance_records(r_provenance, "runner_rows")
  task_ids <- names(r_records)
  for (runner in sort(names(configs))) {
    cfg <- configs[[runner]]
    rows <- evidence$tasks[evidence$tasks$runner == runner, , drop = FALSE]
    rows <- rows[rows$task %in% task_ids, , drop = FALSE]
    rows <- rows[match(task_ids, rows$task), , drop = FALSE]
    for (index in seq_len(nrow(rows))) {
      row <- rows[index, , drop = FALSE]
      task <- as.character(row$task)
      symbol <- source_ledger_scalar(cfg$exports[[task]])
      record <- source_ledger_task_record(runner, task, symbol, row)
      if (identical(runner, "r")) {
        record <- verify_r_source_row(record, r_records[[task]])
      } else if (nzchar(symbol)) {
        record <- switch(runner,
          c_call = verify_c_source_row(record, root_dir),
          cpp11 = verify_cpp11_source_row(record, root_dir),
          extendr = verify_extendr_source_row(record, root_dir),
          rcpp = verify_rcpp_source_row(record, root_dir),
          savvy = verify_savvy_source_row(record, root_dir),
          zigr = verify_zigr_source_row(record, root_dir),
          stop(sprintf("no source verifier for runner %s", runner))
        )
      }
      record$verification_digest <- source_ledger_object_digest(record)
      records[[length(records) + 1L]] <- record
    }
  }
  validate_source_path_gate(records, evidence, enforce_current_gate = enforce_current_gate)
  records
}

validate_source_path_gate <- function(records, evidence, enforce_current_gate = TRUE) {
  keys <- vapply(records, function(record) paste(record$runner, record$task, sep = "\r"), character(1))
  selected_runners <- sort(unique(vapply(records, function(record) record$runner, character(1))))
  selected_tasks <- sort(unique(vapply(records, function(record) record$task, character(1))))
  selected_evidence <- evidence$tasks[
    evidence$tasks$runner %in% selected_runners & evidence$tasks$task %in% selected_tasks,
    ,
    drop = FALSE
  ]
  expected_keys <- paste(selected_evidence$runner, selected_evidence$task, sep = "\r")
  if (anyDuplicated(keys) || !identical(sort(keys), sort(expected_keys))) {
    stop("source verification cells differ from the normalized task matrix")
  }
  named <- records
  names(named) <- keys
  for (index in seq_len(nrow(selected_evidence))) {
    row <- selected_evidence[index, , drop = FALSE]
    record <- named[[paste(row$runner, row$task, sep = "\r")]]
    if (isTRUE(row$executable) && identical(as.character(row$implementation_role), "product_public_path")) {
      validate_product_source_record(record)
      if (!isTRUE(record$product_eligible)) {
        stop(sprintf("normalized product label is not source eligible for %s/%s", row$runner, row$task))
      }
    }
  }
  if (!isTRUE(enforce_current_gate)) return(invisible(records))
  count <- function(runner, predicate) sum(vapply(
    records,
    function(record) identical(record$runner, runner) && isTRUE(predicate(record)),
    logical(1)
  ))
  if (count("zigr", function(record) record$product_eligible) != 13L) {
    stop("source verifier does not identify exactly thirteen generated zigr boundary product rows")
  }
  if (count("cpp11", function(record) record$product_eligible) != 11L) {
    stop("source verifier does not identify all eleven configured cpp11 product rows")
  }
  for (runner in c("rcpp", "extendr", "savvy")) {
    if (count(runner, function(record) record$product_eligible) != 0L) {
      stop(sprintf("source verifier incorrectly accepts a current %s product row", runner))
    }
  }
  raw_extendr <- sort(vapply(
    records[vapply(records, function(record) {
      identical(record$runner, "extendr") && identical(record$source_class, "raw_ffi_substitution")
    }, logical(1))],
    function(record) record$task,
    character(1)
  ))
  expected_raw_extendr <- sort(c("26_matmul", "38_struct_convert", "42_external_ptr", "43_rng_stress"))
  if (!identical(raw_extendr, expected_raw_extendr)) stop("source verifier does not identify the four raw extendr substitutions")
  if (count("c_call", function(record) record$accepted_control) != 70L) {
    stop("source verifier does not accept every configured registered C control")
  }
  r_classes <- table(factor(
    vapply(records[vapply(records, function(record) identical(record$runner, "r"), logical(1))],
      function(record) record$source_class, character(1)),
    levels = c("pure_r", "optimized_base_r", "pure_r_unrepresentable")
  ))
  if (!identical(unname(as.integer(r_classes)), c(16L, 56L, 11L))) {
    stop("source verifier does not reject the mixed R runner as uniformly pure R")
  }
  invisible(records)
}

source_ledger_command <- function(command, args = character(0), label = command) {
  stderr_file <- tempfile("source-ledger-stderr-")
  on.exit(unlink(stderr_file), add = TRUE)
  output <- tryCatch(
    system2(command, args, stdout = TRUE, stderr = stderr_file),
    error = function(error) stop(sprintf("cannot capture %s: %s", label, conditionMessage(error)))
  )
  status <- attr(output, "status")
  if (!is.null(status) && status != 0L) {
    detail <- paste(readLines(stderr_file, warn = FALSE), collapse = " ")
    stop(sprintf("%s exited with code %d: %s", label, status, detail))
  }
  trimws(paste(output, collapse = "\n"))
}

source_ledger_r_config <- function(name) {
  source_ledger_command(file.path(R.home("bin"), "R"), c("CMD", "config", name), sprintf("R CMD config %s", name))
}

source_ledger_command_executable <- function(command_line) {
  first <- strsplit(trimws(command_line), "[[:space:]]+", perl = TRUE)[[1L]][[1L]]
  resolved <- Sys.which(first)
  if (!nzchar(resolved)) stop(sprintf("compiler executable is unavailable: %s", first))
  normalizePath(resolved)
}

source_ledger_resolve_command <- function(name, environment_name = "", fallback = "") {
  configured <- if (nzchar(environment_name)) Sys.getenv(environment_name, unset = "") else ""
  candidate <- source_ledger_scalar(configured, name)
  resolved <- Sys.which(candidate)
  if (!nzchar(resolved) && nzchar(fallback) && file.exists(fallback)) resolved <- fallback
  if (!nzchar(resolved)) stop(sprintf("required executable is unavailable: %s", candidate))
  normalizePath(resolved)
}

source_ledger_resolve_zig_executable <- function(root_dir) {
  configured <- Sys.getenv("ZIG", unset = "")
  candidates <- c(
    configured,
    Sys.which("zig"),
    file.path(root_dir, "..", "zig-0.16.0", "zig"),
    file.path(root_dir, "..", "zig-0.16.0", "zig.exe")
  )
  candidates <- candidates[nzchar(candidates)]
  existing <- candidates[file.exists(candidates)]
  if (length(existing) == 0L) stop("Zig executable not found; set ZIG or install zig")
  normalizePath(existing[[1L]])
}

source_ledger_library_identity <- function(path) {
  if (length(path) != 1L || !nzchar(path) || !file.exists(path)) {
    return(list(path = source_ledger_scalar(path), resolved_path = "", exists = FALSE, md5 = ""))
  }
  list(
    path = normalizePath(path, mustWork = FALSE),
    resolved_path = normalizePath(path, mustWork = TRUE),
    exists = TRUE,
    md5 = unname(as.character(tools::md5sum(path))[[1L]])
  )
}

source_ledger_require_digest <- function(actual, recorded, label) {
  if (!identical(as.character(actual), as.character(recorded))) stop(sprintf("%s drift detected", label))
  invisible(recorded)
}

source_ledger_resolve_link_library <- function(compiler, name) {
  candidate <- source_ledger_command(compiler, paste0("-print-file-name=lib", name, .Platform$dynlib.ext), paste("resolve", name))
  source_ledger_library_identity(candidate)
}

capture_r_build_identity <- function() {
  cc <- source_ledger_r_config("CC")
  cxx <- source_ledger_r_config("CXX")
  cc_executable <- source_ledger_command_executable(cc)
  cxx_executable <- source_ledger_command_executable(cxx)
  lib_r_candidates <- unique(c(
    file.path(R.home("lib"), paste0("libR", .Platform$dynlib.ext)),
    Sys.glob(file.path(R.home(), "bin", "*", "R.dll")),
    file.path(R.home("bin"), "R.dll")
  ))
  lib_r_candidates <- lib_r_candidates[file.exists(lib_r_candidates)]
  if (length(lib_r_candidates) == 0L) stop("R shared library could not be located")
  list(
    r_home = normalizePath(R.home()),
    r_include = normalizePath(R.home("include")),
    r_header_tree = source_ledger_directory_identity(R.home("include"), "R header tree"),
    r_library = normalizePath(R.home("lib")),
    lib_r = source_ledger_library_identity(lib_r_candidates[[1L]]),
    stats_package = capture_r_package_identity("stats"),
    methods_package = capture_r_package_identity("methods"),
    cc = list(
      command = cc,
      executable = cc_executable,
      version = source_ledger_command(cc_executable, "--version", "C compiler version"),
      cppflags = source_ledger_r_config("CPPFLAGS"),
      flags = source_ledger_r_config("CFLAGS"),
      pic_flags = source_ledger_r_config("CPICFLAGS")
    ),
    cxx = list(
      command = cxx,
      executable = cxx_executable,
      version = source_ledger_command(cxx_executable, "--version", "C++ compiler version"),
      cppflags = source_ledger_r_config("CPPFLAGS"),
      flags = source_ledger_r_config("CXXFLAGS"),
      pic_flags = source_ledger_r_config("CXXPICFLAGS")
    ),
    linker = list(
      flags = source_ledger_r_config("LDFLAGS"),
      shared_c_flags = source_ledger_r_config("SHLIB_LDFLAGS"),
      shared_cxx_flags = source_ledger_r_config("SHLIB_CXXLDFLAGS"),
      dynamic_flags = source_ledger_r_config("DYLIB_LDFLAGS"),
      blas_flags = source_ledger_r_config("BLAS_LIBS"),
      lapack_flags = source_ledger_r_config("LAPACK_LIBS"),
      fortran_flags = source_ledger_r_config("FLIBS")
    ),
    blas = source_ledger_resolve_link_library(cc_executable, "blas"),
    lapack = source_ledger_resolve_link_library(cc_executable, "lapack"),
    fortran_runtime = source_ledger_resolve_link_library(cc_executable, "gfortran")
  )
}

capture_c_control_identity <- function(r_build) {
  cc <- source_ledger_scalar(Sys.getenv("CC", unset = ""), "cc")
  cc_executable <- source_ledger_command_executable(cc)
  r_include <- source_ledger_effective_build_path("R_INCLUDE", R.home("include"))
  r_library <- source_ledger_effective_build_path("R_LIB", R.home("lib"))
  r_cflags <- source_ledger_scalar(Sys.getenv("R_CFLAGS", unset = ""), r_build$cc$flags)
  list(
    command = cc,
    executable = cc_executable,
    version = source_ledger_command(cc_executable, "--version", "C control compiler version"),
    r_include = r_include,
    r_library = r_library,
    r_cflags = r_cflags,
    package_cflags = paste(paste0("-I", r_include), r_cflags),
    compile_and_shared_link_flags = "-fPIC -shared",
    package_libs = paste(paste0("-L", r_library), "-lR -lpthread -lblas")
  )
}

capture_r_package_identity <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) stop(sprintf("required R package is unavailable: %s", package))
  path <- normalizePath(find.package(package))
  description <- utils::packageDescription(package)
  selected <- unique(c(
    file.path(path, "DESCRIPTION"),
    file.path(path, "NAMESPACE"),
    list.files(file.path(path, "R"), full.names = TRUE, recursive = TRUE, all.files = TRUE, no.. = TRUE),
    list.files(file.path(path, "include"), full.names = TRUE, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  ))
  selected <- sort(selected[file.exists(selected) & !file.info(selected)$isdir])
  if (length(selected) == 0L) stop(sprintf("R package %s has no installed source-facing files", package))
  relative <- substring(selected, nchar(paste0(path, .Platform$file.sep)) + 1L)
  tree_digest <- source_ledger_digest_lines(paste(relative, unname(as.character(tools::md5sum(selected))), sep = "\t"))
  sha_manifest <- file.path(path, "SHA256")
  list(
    package = package,
    version = as.character(description$Version),
    package_version = as.character(utils::packageVersion(package)),
    library_path = path,
    built = source_ledger_scalar(description$Built),
    source_tree = list(
      method = "DESCRIPTION-NAMESPACE-R-include-sorted-md5",
      digest = tree_digest,
      file_count = length(selected)
    ),
    installed_sha256_manifest = list(
      exists = file.exists(sha_manifest),
      path = normalizePath(sha_manifest, mustWork = FALSE),
      md5 = if (file.exists(sha_manifest)) unname(as.character(tools::md5sum(sha_manifest))[[1L]]) else "",
      entry_count = if (file.exists(sha_manifest)) length(readLines(sha_manifest, warn = FALSE)) else 0L
    )
  )
}

parse_cargo_lock <- function(path) {
  lines <- readLines(path, warn = FALSE)
  packages <- list()
  current <- NULL
  flush <- function() {
    if (!is.null(current)) packages[[length(packages) + 1L]] <<- current
  }
  for (line in lines) {
    if (identical(trimws(line), "[[package]]")) {
      flush()
      current <- list(name = "", version = "", source = "", checksum = "")
      next
    }
    if (is.null(current)) next
    match <- regexec("^(name|version|source|checksum)[[:space:]]*=[[:space:]]*\"(.*)\"$", trimws(line))
    captures <- regmatches(trimws(line), match)[[1L]]
    if (length(captures) == 3L) current[[captures[[2L]]]] <- captures[[3L]]
  }
  flush()
  if (length(packages) == 0L || any(vapply(packages, function(record) !nzchar(record$name), logical(1)))) {
    stop(sprintf("Cargo lock contains no valid packages: %s", path))
  }
  packages
}

parse_cargo_release_profile <- function(path) {
  lines <- readLines(path, warn = FALSE)
  start <- grep("^[[:space:]]*\\[profile\\.release\\][[:space:]]*$", lines)
  values <- list()
  if (length(start) > 0L && start[[1L]] < length(lines)) {
    for (line in lines[(start[[1L]] + 1L):length(lines)]) {
      if (grepl("^[[:space:]]*\\[", line)) break
      captures <- regmatches(trimws(line), regexec("^([A-Za-z0-9_-]+)[[:space:]]*=[[:space:]]*(.*)$", trimws(line)))[[1L]]
      if (length(captures) == 3L) values[[captures[[2L]]]] <- gsub('^\"|\"$', "", captures[[3L]])
    }
  }
  list(
    profile = "release",
    settings = values,
    panic = source_ledger_scalar(values$panic, "unwind (Cargo release default)"),
    lto = source_ledger_scalar(values$lto, "false (Cargo release default)"),
    codegen_units = source_ledger_scalar(values[["codegen-units"]], "16 (Cargo release default)")
  )
}

capture_cargo_identity <- function(root_dir, runner_spec) {
  manifest <- normalizePath(file.path(root_dir, runner_spec$cargo_manifest))
  lock <- normalizePath(file.path(root_dir, runner_spec$cargo_lock))
  cargo_executable <- source_ledger_resolve_command(
    "cargo",
    fallback = file.path(path.expand("~"), ".cargo", "bin", "cargo")
  )
  rustc_executable <- source_ledger_resolve_command(
    "rustc",
    environment_name = "RUSTC",
    fallback = file.path(path.expand("~"), ".cargo", "bin", "rustc")
  )
  metadata_text <- source_ledger_command(
    cargo_executable,
    c("metadata", "--locked", "--offline", "--format-version=1", "--manifest-path", manifest),
    "Cargo metadata"
  )
  metadata <- jsonlite::fromJSON(metadata_text, simplifyVector = FALSE)
  nodes <- metadata$resolve$nodes
  feature_by_id <- setNames(lapply(nodes, function(node) as.character(unlist(node$features, use.names = FALSE))),
                            vapply(nodes, function(node) as.character(node$id), character(1)))
  packages <- parse_cargo_lock(lock)
  packages <- lapply(packages, function(record) {
    suffix <- paste0("#", record$name, "@", record$version)
    matches <- names(feature_by_id)[endsWith(names(feature_by_id), suffix)]
    if (length(matches) == 0L) {
      matches <- names(feature_by_id)[grepl(paste0("#", record$name, "@", record$version), names(feature_by_id), fixed = TRUE)]
    }
    if (length(matches) > 1L) {
      stop(sprintf("Cargo metadata has ambiguous package identity for %s %s", record$name, record$version))
    }
    record$selected <- length(matches) == 1L
    record$metadata_id <- if (record$selected) matches[[1L]] else ""
    record$features <- if (record$selected) as.list(sort(feature_by_id[[matches[[1L]]]])) else list()
    record
  })
  list(
    cargo_executable = cargo_executable,
    cargo = source_ledger_command(cargo_executable, "-V", "Cargo version"),
    rustc_executable = rustc_executable,
    rustc = source_ledger_command(rustc_executable, "-vV", "Rust compiler version"),
    manifest_path = source_ledger_relative_path(root_dir, manifest),
    lock_path = source_ledger_relative_path(root_dir, lock),
    lock_digest = unname(as.character(tools::md5sum(lock))[[1L]]),
    packages = packages,
    selected_package_ids = as.list(sort(names(feature_by_id))),
    profile = parse_cargo_release_profile(manifest),
    environment = as.list(Sys.getenv(
      c(
        "RUSTC", "RUSTC_WRAPPER", "RUSTC_WORKSPACE_WRAPPER", "RUSTUP_TOOLCHAIN",
        "CARGO_HOME", "CARGO_BUILD_TARGET", "CARGO_PROFILE_RELEASE_LTO",
        "CARGO_PROFILE_RELEASE_PANIC", "RUSTFLAGS", "CARGO_ENCODED_RUSTFLAGS"
      ),
      unset = ""
    ))
  )
}

source_ledger_parse_ldd <- function(path) {
  output <- source_ledger_command("ldd", path, sprintf("ldd %s", basename(path)))
  lines <- strsplit(output, "\n", fixed = TRUE)[[1L]]
  if (any(grepl("not found", lines, fixed = TRUE))) stop(sprintf("artifact has an unresolved dependency: %s", path))
  records <- lapply(lines, function(line) {
    line <- trimws(line)
    if (!nzchar(line)) return(NULL)
    if (grepl(" => ", line, fixed = TRUE)) {
      name <- sub(" => .*", "", line)
      resolved <- sub("^[^=]+=>[[:space:]]*", "", line)
      resolved <- sub("[[:space:]]+\\(0x[0-9a-fA-F]+\\).*$", "", resolved)
    } else {
      name <- sub("[[:space:]].*$", "", line)
      resolved <- name
    }
    if (!startsWith(resolved, "/")) return(list(name = name, path = resolved, exists = FALSE, md5 = ""))
    identity <- source_ledger_library_identity(resolved)
    list(name = name, path = identity$resolved_path, exists = identity$exists, md5 = identity$md5)
  })
  Filter(Negate(is.null), records)
}

capture_artifact_dependency_closure <- function(paths) {
  paths <- sort(unique(normalizePath(as.character(paths), mustWork = TRUE)))
  artifacts <- lapply(paths, function(path) {
    dependencies <- if (identical(Sys.info()[["sysname"]], "Linux")) {
      source_ledger_parse_ldd(path)
    } else {
      stop("exact transitive artifact dependency capture is currently implemented only for the active Linux host")
    }
    list(
      path = path,
      md5 = unname(as.character(tools::md5sum(path))[[1L]]),
      method = "ldd transitive loader closure",
      dependencies = dependencies
    )
  })
  list(artifacts = artifacts, digest = source_ledger_object_digest(artifacts))
}

capture_zig_identity <- function(root_dir, build_settings) {
  zig <- source_ledger_resolve_zig_executable(root_dir)
  env <- source_ledger_command(zig, "env", "Zig environment")
  target <- regmatches(env, regexec("[.]target = \"([^\"]+)\"", env))[[1L]]
  cpu_flags <- ""
  if (file.exists("/proc/cpuinfo")) {
    flags <- grep("^flags[[:space:]]*:", readLines("/proc/cpuinfo", warn = FALSE), value = TRUE)
    if (length(flags) > 0L) cpu_flags <- trimws(sub("^[^:]+:", "", flags[[1L]]))
  }
  requested_target <- source_ledger_scalar(build_settings$target, "native")
  resolved_host_target <- if (length(target) == 2L) target[[2L]] else ""
  effective_target <- if (identical(requested_target, "native")) resolved_host_target else requested_target
  list(
    executable = zig,
    version = source_ledger_command(zig, "version", "Zig version"),
    target = requested_target,
    effective_target = effective_target,
    resolved_host_target = resolved_host_target,
    optimization = source_ledger_scalar(build_settings$optimization),
    checked_sexp = isTRUE(build_settings$checked_sexp),
    requested_cpu_features = source_ledger_scalar(build_settings$cpu_features, "default"),
    host_cpu_flags = cpu_flags,
    lto = if (grepl("linux", effective_target, fixed = TRUE)) "full" else "build default",
    generated_registration_source = as.list(c("src/zig/main.zig", "src/zig/task_28_only_main.zig", "../src/export.zig"))
  )
}

source_ledger_effective_build_path <- function(name, fallback) {
  configured <- Sys.getenv(name, unset = "")
  path <- if (nzchar(configured)) configured else fallback
  if (!dir.exists(path)) stop(sprintf("configured %s directory is unavailable: %s", name, path))
  normalizePath(path)
}

source_ledger_build_invocation <- function(root_dir, runner, build_settings) {
  r_executable <- normalizePath(file.path(R.home("bin"), "R"))
  make_executable <- Sys.which("make")
  if (!nzchar(make_executable) && runner %in% c("c_call", "extendr", "savvy")) {
    stop(sprintf("make is unavailable for runner %s", runner))
  }
  if (nzchar(make_executable)) make_executable <- normalizePath(make_executable)
  r_include <- source_ledger_effective_build_path("R_INCLUDE", R.home("include"))
  r_library <- source_ledger_effective_build_path("R_LIB", R.home("lib"))
  executed <- isTRUE(build_settings$requested_rebuild)
  inherited_environment <- function(names) as.list(Sys.getenv(names, unset = ""))
  invocation <- switch(runner,
    c_call = list(
      executable = make_executable,
      arguments = as.list(c("-f", "Makefile", paste0("R_INCLUDE=", r_include), paste0("R_LIB=", r_library))),
      working_directory = normalizePath(file.path(root_dir, "src", "c_call")),
      environment = inherited_environment(c("CC", "R_CFLAGS"))
    ),
    cpp11 = list(
      executable = r_executable,
      arguments = as.list(c(
        "CMD", "INSTALL", "--preclean", "--clean", "--no-multiarch",
        paste0("--library=", normalizePath(file.path(root_dir, "tmp", "cpp11-library"), mustWork = FALSE)),
        "src/cpp11"
      )),
      working_directory = normalizePath(root_dir),
      environment = inherited_environment(c(
        "R_MAKEVARS_SITE", "R_MAKEVARS_USER", "CXX", "CXXFLAGS", "LDFLAGS"
      ))
    ),
    extendr = list(
      executable = make_executable,
      arguments = as.list(c("-C", "src/extendr")),
      working_directory = normalizePath(root_dir),
      environment = inherited_environment(c(
        "R_HOME", "R_INCLUDE", "CARGO_HOME", "RUSTC", "RUSTC_WRAPPER", "RUSTFLAGS",
        "CARGO_ENCODED_RUSTFLAGS", "CARGO_BUILD_TARGET"
      ))
    ),
    r = list(
      executable = "not_applicable",
      arguments = list(),
      working_directory = normalizePath(root_dir),
      environment = list()
    ),
    rcpp = list(
      executable = r_executable,
      arguments = as.list(c("CMD", "SHLIB", "-o", "src/cpp/rcpp_benchmarks.so", "src/cpp/main.cpp")),
      working_directory = normalizePath(root_dir),
      environment = c(
        list(PKG_CPPFLAGS = paste0("-I", normalizePath(system.file("include", package = "Rcpp")))),
        inherited_environment(c("R_MAKEVARS_SITE", "R_MAKEVARS_USER", "CXX", "CXXFLAGS", "LDFLAGS"))
      )
    ),
    savvy = list(
      executable = make_executable,
      arguments = as.list(c("-C", "src/savvy")),
      working_directory = normalizePath(root_dir),
      environment = inherited_environment(c(
        "R_INCLUDE", "CARGO_HOME", "RUSTC", "RUSTC_WRAPPER", "RUSTFLAGS",
        "CARGO_ENCODED_RUSTFLAGS", "CARGO_BUILD_TARGET"
      ))
    ),
    zigr = {
      arguments <- c(
        "build",
        paste0("-Doptimize=", source_ledger_scalar(build_settings$optimization)),
        paste0("-Dr-include=", r_include),
        paste0("-Dr-lib=", r_library)
      )
      if (isTRUE(build_settings$checked_sexp)) arguments <- c(arguments, "-Dchecked-sexp=true")
      target <- source_ledger_scalar(build_settings$target, "native")
      cpu <- source_ledger_scalar(build_settings$cpu_features, "default")
      if (!identical(target, "native")) arguments <- c(arguments, paste0("-Dtarget=", target))
      if (!identical(cpu, "default")) arguments <- c(arguments, paste0("-Dcpu=", cpu))
      cache_dir <- source_ledger_scalar(
        build_settings$cache_dir,
        normalizePath(file.path(root_dir, ".zig-cache"), mustWork = FALSE)
      )
      global_cache_dir <- source_ledger_scalar(
        build_settings$global_cache_dir,
        normalizePath(file.path(root_dir, ".zig-global-cache"), mustWork = FALSE)
      )
      list(
        executable = source_ledger_resolve_zig_executable(root_dir),
        arguments = as.list(c(arguments, "--cache-dir", cache_dir, "--global-cache-dir", global_cache_dir)),
        working_directory = normalizePath(root_dir),
        environment = list(R_INCLUDE = r_include, R_LIB = r_library)
      )
    },
    stop(sprintf("no configured build invocation for runner %s", runner))
  )
  invocation$executed_in_run <- executed && !identical(runner, "r")
  invocation
}

source_ledger_runner_record <- function(ledger, runner) {
  matches <- ledger$runners[vapply(ledger$runners, function(record) identical(as.character(record$name), runner), logical(1))]
  if (length(matches) != 1L) stop(sprintf("tool source ledger has no unique runner record for %s", runner))
  matches[[1L]]
}

source_ledger_verification_record <- function(ledger, runner, task) {
  matches <- ledger$source_verification[vapply(ledger$source_verification, function(record) {
    identical(as.character(record$runner), runner) && identical(as.character(record$task), task)
  }, logical(1))]
  if (length(matches) != 1L) stop(sprintf("tool source ledger has no unique verification for %s/%s", runner, task))
  matches[[1L]]
}

source_ledger_tool_label <- function(record) {
  switch(as.character(record$tool_kind),
    r_package = sprintf(
      "%s %s at %s",
      record$toolchain$package,
      record$toolchain$version,
      record$toolchain$library_path
    ),
    cargo = sprintf(
      "%s; %s; lock %s",
      sub("\n.*$", "", as.character(record$toolchain$rustc)),
      as.character(record$toolchain$cargo),
      as.character(record$toolchain$lock_digest)
    ),
    zig = sprintf(
      "Zig %s target %s optimize %s checked-SEXP %s",
      record$toolchain$version,
      record$toolchain$target,
      record$toolchain$optimization,
      if (isTRUE(record$toolchain$checked_sexp)) "true" else "false"
    ),
    c = sprintf("C control via %s", sub("\n.*$", "", as.character(record$toolchain$version))),
    r = as.character(record$toolchain$version),
    stop(sprintf("unsupported tool label kind: %s", record$tool_kind))
  )
}

capture_tool_source_ledger <- function(root_dir, configs, evidence, r_provenance, build_settings) {
  spec <- load_source_ledger_spec(root_dir)
  r_build <- capture_r_build_identity()
  verification <- verify_source_paths(root_dir, configs, evidence, r_provenance, enforce_current_gate = FALSE)
  runner_records <- lapply(sort(names(configs)), function(runner) {
    runner_spec <- spec$runners[[runner]]
    fixture_spec <- spec$fixture_runners[[runner]]
    cfg <- configs[[runner]]
    source_identity <- source_ledger_file_identity(root_dir, runner_spec$source_globs, sprintf("%s source", runner))
    build_identity <- source_ledger_file_identity(root_dir, runner_spec$build_files, sprintf("%s build", runner))
    build_invocation <- source_ledger_build_invocation(root_dir, runner, build_settings)
    glue_identity <- source_ledger_file_identity(
      root_dir,
      runner_spec$generated_glue$paths,
      sprintf("%s generated glue", runner)
    )
    fixture_source_identity <- source_ledger_file_identity(
      root_dir, fixture_spec$source_globs, sprintf("%s fixture source", runner)
    )
    fixture_build_identity <- source_ledger_file_identity(
      root_dir, fixture_spec$build_files, sprintf("%s fixture build", runner)
    )
    fixture_glue_identity <- source_ledger_file_identity(
      root_dir, fixture_spec$generated_glue$paths, sprintf("%s fixture generated glue", runner)
    )
    toolchain <- switch(as.character(runner_spec$tool_kind),
      r_package = capture_r_package_identity(as.character(runner_spec$package)),
      cargo = capture_cargo_identity(root_dir, runner_spec),
      zig = capture_zig_identity(root_dir, build_settings),
      c = capture_c_control_identity(r_build),
      r = list(version = R.version.string, executable = file.path(R.home("bin"), "R")),
      stop(sprintf("unsupported source ledger tool kind for %s", runner))
    )
    artifact_paths <- if (identical(runner, "r")) {
      file.path(root_dir, "src/r/run_all.R")
    } else {
      file.path(root_dir, c(as.character(cfg$so_path), as.character(unlist(cfg$extra_so_paths, use.names = FALSE))))
    }
    artifact_dependencies <- if (identical(runner, "r")) {
      list(artifacts = list(), digest = "not_applicable")
    } else {
      capture_artifact_dependency_closure(artifact_paths)
    }
    record <- list(
      name = runner,
      role = as.character(runner_spec$role),
      tool_kind = as.character(runner_spec$tool_kind),
      verifier_kind = as.character(runner_spec$verifier_kind),
      build_recipe = as.character(runner_spec$build_recipe),
      source_identity = source_identity,
      build_identity = build_identity,
      build_invocation = build_invocation,
      build_digest = source_ledger_object_digest(list(build_identity, build_invocation)),
      generated_glue = list(
        kind = as.character(runner_spec$generated_glue$kind),
        retained_output = isTRUE(runner_spec$generated_glue$retained_output),
        identity = glue_identity
      ),
      fixture = list(
        verifier_kind = as.character(fixture_spec$verifier_kind),
        build_recipe = as.character(fixture_spec$build_recipe),
        source_identity = fixture_source_identity,
        build_identity = fixture_build_identity,
        generated_glue = list(
          kind = as.character(fixture_spec$generated_glue$kind),
          retained_output = isTRUE(fixture_spec$generated_glue$retained_output),
          identity = fixture_glue_identity
        )
      ),
      toolchain = toolchain,
      artifact_dependencies = artifact_dependencies
    )
    record$dependency_digest <- source_ledger_object_digest(list(toolchain, artifact_dependencies))
    record
  })
  ledger <- list(
    schema_version = source_ledger_schema_version(),
    captured_at = run_manifest_timestamp(),
    benchmark_root = normalizePath(root_dir),
    specification = list(
      path = "source_ledger.json",
      digest = unname(as.character(tools::md5sum(attr(spec, "path")))[[1L]])
    ),
    r_build = r_build,
    runners = runner_records,
    source_verification = verification,
    source_verification_digest = source_ledger_object_digest(verification)
  )
  ledger$identity_digest <- source_ledger_object_digest(ledger[names(ledger) != "captured_at"])
  ledger
}

validate_tool_source_ledger <- function(root_dir, ledger, runner = NULL) {
  if (is.null(ledger) || !identical(as.character(ledger$schema_version), source_ledger_schema_version())) {
    stop("environment has no supported tool source ledger")
  }
  spec <- load_source_ledger_spec(root_dir)
  actual_spec_digest <- unname(as.character(tools::md5sum(attr(spec, "path")))[[1L]])
  source_ledger_require_digest(actual_spec_digest, ledger$specification$digest, "source ledger specification")
  actual_r_build <- capture_r_build_identity()
  source_ledger_require_digest(
    source_ledger_object_digest(actual_r_build),
    source_ledger_object_digest(ledger$r_build),
    "R build, compiler, and numerical library identity"
  )
  for (record in ledger$runners) {
    runner_name <- as.character(record$name)
    if (!is.null(runner) && !identical(runner_name, runner)) next
    runner_spec <- spec$runners[[runner_name]]
    fixture_spec <- spec$fixture_runners[[runner_name]]
    actual_source <- source_ledger_file_identity(root_dir, runner_spec$source_globs, sprintf("%s source", runner_name))
    actual_build <- source_ledger_file_identity(root_dir, runner_spec$build_files, sprintf("%s build", runner_name))
    actual_glue <- source_ledger_file_identity(root_dir, runner_spec$generated_glue$paths, sprintf("%s generated glue", runner_name))
    actual_fixture_source <- source_ledger_file_identity(
      root_dir, fixture_spec$source_globs, sprintf("%s fixture source", runner_name)
    )
    actual_fixture_build <- source_ledger_file_identity(
      root_dir, fixture_spec$build_files, sprintf("%s fixture build", runner_name)
    )
    actual_fixture_glue <- source_ledger_file_identity(
      root_dir, fixture_spec$generated_glue$paths, sprintf("%s fixture generated glue", runner_name)
    )
    source_ledger_require_digest(actual_source$digest, record$source_identity$digest, sprintf("source for runner %s", runner_name))
    source_ledger_require_digest(actual_build$digest, record$build_identity$digest, sprintf("build recipe for runner %s", runner_name))
    source_ledger_require_digest(
      source_ledger_object_digest(list(record$build_identity, record$build_invocation)),
      record$build_digest,
      sprintf("resolved build invocation for runner %s", runner_name)
    )
    source_ledger_require_digest(
      actual_glue$digest,
      record$generated_glue$identity$digest,
      sprintf("generated glue for runner %s", runner_name)
    )
    source_ledger_require_digest(
      actual_fixture_source$digest,
      record$fixture$source_identity$digest,
      sprintf("fixture source for runner %s", runner_name)
    )
    source_ledger_require_digest(
      actual_fixture_build$digest,
      record$fixture$build_identity$digest,
      sprintf("fixture build recipe for runner %s", runner_name)
    )
    source_ledger_require_digest(
      actual_fixture_glue$digest,
      record$fixture$generated_glue$identity$digest,
      sprintf("fixture generated glue for runner %s", runner_name)
    )
    actual_toolchain <- switch(as.character(record$tool_kind),
      r_package = capture_r_package_identity(as.character(runner_spec$package)),
      cargo = capture_cargo_identity(root_dir, runner_spec),
      zig = capture_zig_identity(root_dir, list(
        target = record$toolchain$target,
        optimization = record$toolchain$optimization,
        checked_sexp = record$toolchain$checked_sexp,
        cpu_features = record$toolchain$requested_cpu_features
      )),
      c = capture_c_control_identity(actual_r_build),
      r = list(version = R.version.string, executable = file.path(R.home("bin"), "R")),
      stop(sprintf("unsupported source ledger tool kind for %s", runner_name))
    )
    source_ledger_require_digest(
      source_ledger_object_digest(actual_toolchain),
      source_ledger_object_digest(record$toolchain),
      sprintf("toolchain for runner %s", runner_name)
    )
    if (!identical(runner_name, "r")) {
      artifact_paths <- vapply(record$artifact_dependencies$artifacts, function(artifact) as.character(artifact$path), character(1))
      actual_dependencies <- capture_artifact_dependency_closure(artifact_paths)
      source_ledger_require_digest(
        actual_dependencies$digest,
        record$artifact_dependencies$digest,
        sprintf("artifact dependencies for runner %s", runner_name)
      )
    } else {
      actual_dependencies <- list(artifacts = list(), digest = "not_applicable")
    }
    source_ledger_require_digest(
      source_ledger_object_digest(list(actual_toolchain, actual_dependencies)),
      record$dependency_digest,
      sprintf("dependency closure for runner %s", runner_name)
    )
  }
  actual_identity <- source_ledger_object_digest(ledger[names(ledger) != "captured_at" & names(ledger) != "identity_digest"])
  if (!identical(actual_identity, as.character(ledger$identity_digest))) stop("tool source ledger identity digest differs")
  invisible(ledger)
}
