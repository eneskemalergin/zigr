fixture_function_map <- function() {
  list(
    F01 = "fixture_zero",
    F02 = "fixture_scalar",
    F03 = "fixture_numeric",
    F04 = "fixture_altrep_integer",
    F05 = "fixture_strings",
    F06 = "fixture_raw",
    F07 = "fixture_complex",
    F08 = "fixture_logical_counts",
    F09 = "fixture_schema",
    F10 = c("fixture_new", "fixture_method", "fixture_read"),
    F11 = c("fixture_scalar", "fixture_error"),
    F12 = "fixture_outputs"
  )
}

fixture_r_function_map <- function() {
  c(
    F01 = "r_fixture_zero",
    F02 = "r_fixture_scalar",
    F03 = "r_fixture_numeric",
    F04 = "r_fixture_altrep_integer",
    F05 = "r_fixture_strings",
    F06 = "r_fixture_raw",
    F07 = "r_fixture_complex",
    F08 = "r_fixture_logical_counts",
    F09 = "r_fixture_schema",
    F11 = "r_fixture_error",
    F12 = "r_fixture_outputs"
  )
}

fixture_c_symbol_map <- function() {
  lapply(fixture_function_map(), function(functions) paste0("c_benchmark_", functions))
}

direct_runner_packages <- function(root_dir) {
  fixture_library <- file.path(root_dir, "tmp", "fixture-library")
  list(
    zigr = list(package = "zigrFixture", library = fixture_library, dll = "zigrFixture"),
    rcpp = list(package = "zigrRcpp", library = fixture_library, dll = "zigrRcpp"),
    cpp11 = list(
      package = "zigrCpp11", library = file.path(root_dir, "tmp", "cpp11-library"),
      dll = "zigrCpp11"
    ),
    extendr = list(package = "zigrExtendr", library = fixture_library, dll = "zigrExtendr"),
    savvy = list(package = "zigrSavvy", library = fixture_library, dll = "zigrSavvy")
  )
}

direct_runner_package <- function(root_dir, runner) {
  if (length(runner) != 1L || is.na(runner) || !nzchar(runner)) {
    stop("direct runner package requires one runner")
  }
  package <- direct_runner_packages(root_dir)[[runner]]
  if (is.null(package)) stop(sprintf("no direct runner package is declared for %s", runner))
  package
}

direct_runner_artifact_path <- function(root_dir, runner) {
  if (identical(runner, "r")) {
    return(file.path(root_dir, "src", "r", "run_all.R"))
  }
  if (identical(runner, "c_call")) {
    return(file.path(root_dir, "src", "c_call", "bench.so"))
  }
  package <- direct_runner_package(root_dir, runner)
  file.path(
    package$library, package$package, "libs",
    paste0(package$dll, .Platform$dynlib.ext)
  )
}

direct_runner_environment <- function(root_dir, runner) {
  if (runner %in% c("r", "c_call")) return(.GlobalEnv)
  package <- direct_runner_package(root_dir, runner)
  loadNamespace(package$package, lib.loc = package$library)
}

fixture_source_record <- function(row) {
  list(
    runner = as.character(row$runner),
    fixture = as.character(row$fixture),
    executable = isTRUE(row$executable),
    status = as.character(row$status),
    source_class = if (isTRUE(row$executable)) "unverified" else "source_backed_gap",
    annotation_present = FALSE,
    typed_public_signature = FALSE,
    approved_public_adapter = FALSE,
    generated_native_wrapper = FALSE,
    generated_r_wrapper = FALSE,
    registered_entry = FALSE,
    configured_symbol_present = FALSE,
    dynamic_lookup_disabled = FALSE,
    forced_symbols = FALSE,
    product_eligible = FALSE,
    accepted_control = FALSE,
    source_paths = list(),
    reason = as.character(row$reason)
  )
}

fixture_all_present <- function(values) length(values) > 0L && all(values)

fixture_signature_arguments <- function(signature) {
  if (length(signature) != 1L || is.na(signature) || !nzchar(signature)) return("")
  open <- regexpr("(", signature, fixed = TRUE)[[1L]]
  if (open < 0L) return("")
  tail <- substring(signature, open + 1L)
  close <- regexpr(")", tail, fixed = TRUE)[[1L]]
  if (close < 0L) return("")
  substring(tail, 1L, close - 1L)
}

fixture_signatures_are_typed <- function(signatures, forbidden_pattern, raw_allowed = NULL) {
  if (length(signatures) == 0L || any(!nzchar(signatures))) return(FALSE)
  if (is.null(raw_allowed)) raw_allowed <- rep(FALSE, length(signatures))
  if (length(raw_allowed) != length(signatures)) stop("typed signature allowance length differs")
  arguments <- vapply(signatures, fixture_signature_arguments, character(1))
  all(raw_allowed | !grepl(forbidden_pattern, arguments, perl = TRUE))
}

fixture_definition_contains <- function(lines, signature_pattern, required_text, max_lines = 64L) {
  indices <- grep(signature_pattern, lines, perl = TRUE)
  if (length(indices) == 0L || !nzchar(required_text)) return(FALSE)
  any(vapply(indices, function(index) {
    end <- min(length(lines), index + max_lines - 1L)
    body <- character(0)
    depth <- 0L
    opened <- FALSE
    for (line in lines[index:end]) {
      body <- c(body, line)
      open_count <- lengths(regmatches(line, gregexpr("{", line, fixed = TRUE)))
      close_count <- lengths(regmatches(line, gregexpr("}", line, fixed = TRUE)))
      if (!opened && open_count > 0L) opened <- TRUE
      if (opened) {
        depth <- depth + open_count - close_count
        if (depth <= 0L) break
      } else if (grepl(";", line, fixed = TRUE)) {
        return(FALSE)
      }
    }
    opened && depth == 0L && any(grepl(required_text, body, fixed = TRUE))
  }, logical(1)))
}

cpp11_fixture_dependency_source <- function() {
  package_root <- system.file(package = "cpp11")
  if (!nzchar(package_root)) stop("cpp11 is required for fixture source verification")
  version <- as.character(utils::packageVersion("cpp11"))
  if (!identical(version, "0.5.5")) {
    stop(sprintf("cpp11 fixture source verification requires 0.5.5, found %s", version))
  }
  header_root <- file.path(package_root, "include", "cpp11")
  headers <- sort(list.files(
    header_root, pattern = "\\.(h|hpp)$", recursive = TRUE, full.names = TRUE
  ))
  if (length(headers) == 0L) stop("cpp11 installed public headers are missing")
  relative_paths <- substring(headers, nchar(package_root) + 2L)
  contents <- lapply(headers, readLines, warn = FALSE, encoding = "UTF-8")
  names(contents) <- relative_paths
  list(
    version = version,
    header_paths = relative_paths,
    header_contents = contents,
    source_digest = source_ledger_object_digest(list(version = version, headers = contents))
  )
}

cpp11_normalized_fixture_absent <- function(functions, source, native, generated_r) {
  symbols <- paste0("_zigrCpp11_", functions)
  all(vapply(functions, function(function_name) {
    !source_ledger_definition_present(
      source, sprintf("^[[:space:]]*.*\\b%s[[:space:]]*\\(", function_name)
    )
  }, logical(1))) &&
    all(vapply(symbols, function(symbol) {
      !source_ledger_cpp11_wrapper_present(native, symbol) &&
        !source_ledger_r_call_present(generated_r, symbol) &&
        !any(grepl(paste0("{\"", symbol, "\""), native, fixed = TRUE))
    }, logical(1)))
}

cpp11_gap_source_backed <- function(fixture, source, native, generated_r, dependency) {
  functions <- fixture_function_map()[[fixture]]
  if (!cpp11_normalized_fixture_absent(functions, source, native, generated_r)) return(FALSE)
  all_headers <- unlist(dependency$header_contents, use.names = FALSE)
  complex_api_absent <-
    !any(grepl("complex", dependency$header_paths, ignore.case = TRUE)) &&
    !any(grepl("\\b(CPLXSXP|Rcomplex|complexes?)\\b", all_headers, perl = TRUE, ignore.case = TRUE))
  if (fixture %in% c("F07", "F12")) return(complex_api_absent)
  if (!identical(fixture, "F10")) stop("unexpected cpp11 fixture gap")
  external_pointer_path <- grep("external_pointer\\.hpp$", names(dependency$header_contents), value = TRUE)
  if (length(external_pointer_path) != 1L) return(FALSE)
  external_pointer <- dependency$header_contents[[external_pointer_path]]
  untagged_constructor <-
    any(grepl("external_pointer(SEXP data) : data_(valid_type(data))", external_pointer, fixed = TRUE)) &&
    any(grepl("R_MakeExternalPtr]((void*)p, R_NilValue, R_NilValue)", external_pointer, fixed = TRUE)) &&
    !any(grepl("R_(ExternalPtrTag|SetExternalPtrTag)", external_pointer, perl = TRUE))
  untagged_constructor
}

verify_zigr_fixture_source <- function(record, root_dir) {
  source <- source_ledger_read_lines(root_dir, "src/zig/fixture.zig")
  wrappers <- source_ledger_read_lines(root_dir, "src/zig/fixture/R/fixture.R")
  generator <- source_ledger_read_lines(root_dir, "../src/export.zig")
  if (!isTRUE(record$executable)) {
    if (!identical(record$fixture, "F08")) stop("unexpected zigr fixture gap")
    raw_substitute <- any(grepl('.name = "fixture_logical_counts"', source, fixed = TRUE))
    typed_logical_support <- any(grepl("toLogicalSlice", generator, fixed = TRUE))
    record$source_class <- if (!raw_substitute && !typed_logical_support) {
      "source_backed_product_gap"
    } else {
      "unsupported_claim_invalid"
    }
    record$source_paths <- as.list(c("src/zig/fixture.zig", "../src/export.zig"))
    record$reason <- if (identical(record$source_class, "source_backed_product_gap")) {
      "generateExports has no typed logical-slice parameter and the fixture does not register a raw substitute"
    } else {
      "zigr F08 gap does not match the generator and fixture source"
    }
    return(record)
  }
  functions <- fixture_function_map()[[record$fixture]]
  zig_names <- paste0(
    "fixture",
    vapply(strsplit(sub("^fixture_", "", functions), "_", fixed = TRUE), function(parts) {
      paste0(vapply(parts, function(part) {
        paste0(toupper(substring(part, 1L, 1L)), substring(part, 2L))
      }, character(1)), collapse = "")
    }, character(1))
  )
  declarations <- vapply(functions, function(function_name) {
    if (function_name %in% c("fixture_method", "fixture_read")) {
      method <- if (identical(function_name, "fixture_method")) "increment" else "read"
      any(grepl(paste0(".name = \"", method, "\""), source, fixed = TRUE))
    } else {
      any(grepl(paste0(".name = \"", function_name, "\""), source, fixed = TRUE))
    }
  }, logical(1))
  zig_names[zig_names == "fixtureMethod"] <- "fixtureIncrement"
  signatures <- vapply(zig_names, function(function_name) {
    source_ledger_signature(
      source, sprintf("^[[:space:]]*fn[[:space:]]+%s[[:space:]]*\\(", function_name)
    )
  }, character(1))
  wrapper_calls <- vapply(functions, function(function_name) {
    any(grepl(paste0("C_", function_name), wrappers, fixed = TRUE)) ||
      (identical(function_name, "fixture_method") &&
        any(grepl("C_fixture_FixtureState__increment", wrappers, fixed = TRUE))) ||
      (identical(function_name, "fixture_read") &&
        any(grepl("C_fixture_FixtureState__read", wrappers, fixed = TRUE)))
  }, logical(1))
  record$annotation_present <- fixture_all_present(declarations)
  no_direct_vector_api <-
    !any(grepl("R\\.(REAL|INTEGER|LOGICAL|RAW|COMPLEX|STRING_ELT|VECTOR_ELT)\\(", source, perl = TRUE))
  record$typed_public_signature <-
    fixture_signatures_are_typed(signatures, "\\bR\\.SEXP\\b") && no_direct_vector_api
  record$approved_public_adapter <-
    identical(record$fixture, "F09") && length(signatures) == 1L &&
    identical(trimws(fixture_signature_arguments(signatures[[1L]])), "value: R.SEXP") &&
    no_direct_vector_api
  record$generated_native_wrapper <-
    any(grepl("pub fn generateExports", generator, fixed = TRUE)) &&
    any(grepl("pub fn generateMethods", generator, fixed = TRUE))
  record$generated_r_wrapper <- fixture_all_present(wrapper_calls)
  record$registered_entry <-
    any(grepl("FixtureExports.call_defs", source, fixed = TRUE)) &&
    any(grepl("FixtureMethods.call_defs", source, fixed = TRUE))
  record$configured_symbol_present <- fixture_all_present(declarations)
  record$dynamic_lookup_disabled <- any(grepl("R.R_useDynamicSymbols(info, 0)", source, fixed = TRUE))
  record$forced_symbols <- any(grepl("R.R_forceSymbols(info, 1)", source, fixed = TRUE))
  record$product_eligible <- all(c(
    record$annotation_present,
    record$typed_public_signature || record$approved_public_adapter,
    record$generated_native_wrapper,
    record$generated_r_wrapper, record$registered_entry, record$configured_symbol_present,
    record$dynamic_lookup_disabled, record$forced_symbols
  ))
  record$source_class <- if (!record$product_eligible) {
    "generated_path_invalid"
  } else if (record$approved_public_adapter) {
    "generated_public_adapter"
  } else {
    "generated_typed"
  }
  record$source_paths <- as.list(c(
    "src/zig/fixture.zig", "src/zig/fixture/R/fixture.R", "../src/export.zig"
  ))
  record$reason <- if (record$approved_public_adapter) {
    "generated package symbols use the accepted explicit R.SEXP fixed-schema adapter"
  } else if (record$product_eligible) {
    "typed zigr declarations, generated wrappers, package symbol objects, and forced registration are present"
  } else {
    "zigr fixture lacks at least one typed generated package-path marker"
  }
  record
}

verify_rcpp_fixture_source <- function(record, root_dir) {
  source <- source_ledger_read_lines(root_dir, "src/cpp/fixture/src/fixture.cpp")
  native <- source_ledger_read_lines(root_dir, "src/cpp/fixture/src/RcppExports.cpp")
  generated_r <- source_ledger_read_lines(root_dir, "src/cpp/fixture/R/RcppExports.R")
  adapters <- source_ledger_read_lines(root_dir, "src/cpp/fixture/R/fixture.R")
  namespace <- source_ledger_read_lines(root_dir, "src/cpp/fixture/NAMESPACE")
  functions <- fixture_function_map()[[record$fixture]]
  ordinary <- setdiff(functions, c("fixture_new", "fixture_method", "fixture_read"))
  ordinary_signatures <- vapply(ordinary, function(function_name) {
    source_ledger_signature(
      source, sprintf("^[[:space:]]*.*\\b%s[[:space:]]*\\(", function_name)
    )
  }, character(1))
  annotation <- vapply(ordinary, function(function_name) {
    signature <- sprintf("^[[:space:]]*.*\\b%s[[:space:]]*\\(", function_name)
    source_ledger_annotation_before(source, signature, "// [[Rcpp::export]]", distance = 2L)
  }, logical(1))
  native_wrappers <- vapply(ordinary, function(function_name) {
    source_ledger_definition_present(
      native, paste0("^[[:space:]]*RcppExport[[:space:]]+SEXP[[:space:]]+_zigrRcpp_", function_name, "[[:space:]]*\\(")
    )
  }, logical(1))
  r_wrappers <- vapply(ordinary, function(function_name) {
    any(grepl(paste0("_zigrRcpp_", function_name), generated_r, fixed = TRUE))
  }, logical(1))
  module_cell <- identical(record$fixture, "F10")
  record$annotation_present <- if (module_cell) {
    any(grepl("RCPP_MODULE(zigr_fixture_module)", source, fixed = TRUE))
  } else {
    fixture_all_present(annotation)
  }
  module_signatures_typed <- all(vapply(c(
    "^[[:space:]]*FixtureState[[:space:]]*\\(",
    "^[[:space:]]*int[[:space:]]+increment[[:space:]]*\\(",
    "^[[:space:]]*int[[:space:]]+read[[:space:]]*\\("
  ), function(pattern) source_ledger_definition_present(source, pattern), logical(1)))
  r_object_arguments <- grepl("\\bRcpp::RObject\\b", ordinary_signatures, perl = TRUE)
  adapter_guard <- switch(record$fixture,
    F02 = "Rcpp::is<Rcpp::NumericVector>(value)",
    F03 = "Rcpp::is<Rcpp::NumericVector>(value)",
    F04 = "Rcpp::is<Rcpp::IntegerVector>(value)",
    F05 = "Rcpp::is<Rcpp::CharacterVector>(value)",
    F06 = "Rcpp::is<Rcpp::RawVector>(value)",
    F07 = "Rcpp::is<Rcpp::ComplexVector>(value)",
    F08 = "Rcpp::is<Rcpp::LogicalVector>(value)",
    F10 = "Rcpp::is<Rcpp::IntegerVector>(amount)",
    F11 = "Rcpp::is<Rcpp::NumericVector>(value)",
    ""
  )
  module_adapter <- module_cell && source_ledger_definition_present(
    source, "^[[:space:]]*int[[:space:]]+increment[[:space:]]*\\([[:space:]]*Rcpp::RObject"
  )
  ordinary_adapter <- !module_cell && any(r_object_arguments)
  adapter_function <- if (module_cell) {
    "increment"
  } else if (identical(record$fixture, "F11")) {
    "fixture_scalar"
  } else {
    ordinary[[1L]]
  }
  adapter_signature <- sprintf(
    "^[[:space:]]*.*\\b%s[[:space:]]*\\([[:space:]]*Rcpp::RObject", adapter_function
  )
  record$approved_public_adapter <- record$annotation_present &&
    (module_adapter || ordinary_adapter) && nzchar(adapter_guard) &&
    fixture_definition_contains(source, adapter_signature, adapter_guard)
  record$typed_public_signature <- record$annotation_present &&
    (if (module_cell) module_signatures_typed && !module_adapter else {
      fixture_signatures_are_typed(ordinary_signatures, "\\bSEXP\\b") &&
        !any(r_object_arguments)
    }) &&
    !any(grepl("extern[[:space:]]+\"C\"|R_MakeExternalPtr|R_RegisterCFinalizer", source, perl = TRUE))
  record$generated_native_wrapper <- if (module_cell) {
    any(grepl("_rcpp_module_boot_zigr_fixture_module", native, fixed = TRUE))
  } else {
    fixture_all_present(native_wrappers)
  }
  record$generated_r_wrapper <- if (module_cell) {
    all(vapply(functions, function(function_name) {
      any(grepl(paste0(function_name, " <- function"), adapters, fixed = TRUE))
    }, logical(1)))
  } else {
    fixture_all_present(r_wrappers)
  }
  record$registered_entry <- record$generated_native_wrapper &&
    any(grepl("R_registerRoutines", native, fixed = TRUE))
  record$configured_symbol_present <- record$generated_native_wrapper
  record$dynamic_lookup_disabled <-
    any(grepl("R_useDynamicSymbols(dll, FALSE)", native, fixed = TRUE)) &&
    any(grepl(".registration = TRUE", namespace, fixed = TRUE))
  record$forced_symbols <- any(grepl("R_forceSymbols(dll, TRUE)", native, fixed = TRUE))
  record$product_eligible <- all(c(
    record$annotation_present,
    record$typed_public_signature || record$approved_public_adapter,
    record$generated_native_wrapper,
    record$generated_r_wrapper, record$registered_entry, record$configured_symbol_present,
    record$dynamic_lookup_disabled
  ))
  record$source_class <- if (!record$product_eligible) {
    "generated_path_invalid"
  } else if (record$approved_public_adapter) {
    "generated_public_adapter"
  } else {
    "generated_typed"
  }
  record$source_paths <- as.list(c(
    "src/cpp/fixture/src/fixture.cpp", "src/cpp/fixture/src/RcppExports.cpp",
    "src/cpp/fixture/R/RcppExports.R", "src/cpp/fixture/R/fixture.R"
  ))
  record$reason <- if (record$approved_public_adapter) {
    "Rcpp generated glue enters through Rcpp::RObject, applies an exact Rcpp type guard, and then constructs the typed Rcpp view"
  } else if (record$product_eligible) {
    "Rcpp attributes or Modules, retained generated native glue, and the package R path are present"
  } else {
    "Rcpp fixture lacks at least one generated package-path marker"
  }
  record
}

verify_cpp11_fixture_source <- function(record, root_dir, dependency) {
  if (!isTRUE(record$executable)) {
    source <- source_ledger_read_lines(root_dir, "src/cpp11/src/fixture.cpp")
    native <- source_ledger_read_lines(root_dir, "src/cpp11/src/cpp11.cpp")
    generated_r <- source_ledger_read_lines(root_dir, "src/cpp11/R/cpp11.R")
    record$source_paths <- as.list(c(
      "src/cpp11/src/fixture.cpp", "src/cpp11/src/cpp11.cpp", "src/cpp11/R/cpp11.R",
      dependency$header_paths
    ))
    record$dependency_version <- dependency$version
    record$dependency_source_digest <- dependency$source_digest
    source_backed <- cpp11_gap_source_backed(
      record$fixture, source, native, generated_r, dependency
    )
    record$source_class <- if (source_backed) {
      "source_backed_product_gap"
    } else {
      "unsupported_claim_invalid"
    }
    record$reason <- if (!source_backed) {
      "cpp11 gap does not match the retained fixture source"
    } else {
      switch(record$fixture,
        F07 = "cpp11 0.5.5 exposes no public complex vector wrapper",
        F10 = "cpp11 0.5.5 external_pointer constructs untagged pointers and validates only EXTPTRSXP",
        F12 = "cpp11 0.5.5 cannot construct the complete complex-bearing F12 output set"
      )
    }
    return(record)
  }
  functions <- fixture_function_map()[[record$fixture]]
  symbols <- paste0("_zigrCpp11_", functions)
  source <- source_ledger_read_lines(root_dir, "src/cpp11/src/fixture.cpp")
  native <- source_ledger_read_lines(root_dir, "src/cpp11/src/cpp11.cpp")
  generated_r <- source_ledger_read_lines(root_dir, "src/cpp11/R/cpp11.R")
  annotations <- vapply(functions, function(function_name) {
    source_ledger_annotation_before(
      source, sprintf("^[[:space:]]*.*\\b%s[[:space:]]*\\(", function_name),
      "[[cpp11::register]]", distance = 2L
    )
  }, logical(1))
  signatures <- vapply(functions, function(function_name) {
    source_ledger_signature(
      source, sprintf("^[[:space:]]*.*\\b%s[[:space:]]*\\(", function_name)
    )
  }, character(1))
  record$annotation_present <- fixture_all_present(annotations)
  record$typed_public_signature <- record$annotation_present &&
    fixture_signatures_are_typed(signatures, "\\bSEXP\\b")
  record$generated_native_wrapper <- fixture_all_present(vapply(symbols, function(symbol) {
    source_ledger_cpp11_wrapper_present(native, symbol)
  }, logical(1)))
  record$generated_r_wrapper <- fixture_all_present(vapply(symbols, function(symbol) {
    source_ledger_r_call_present(generated_r, symbol)
  }, logical(1)))
  record$registered_entry <- fixture_all_present(vapply(symbols, function(symbol) {
    any(grepl(paste0("{\"", symbol, "\""), native, fixed = TRUE))
  }, logical(1)))
  record$configured_symbol_present <- record$generated_native_wrapper
  record$dynamic_lookup_disabled <- any(grepl("R_useDynamicSymbols(dll, FALSE)", native, fixed = TRUE))
  record$forced_symbols <- any(grepl("R_forceSymbols(dll, TRUE)", native, fixed = TRUE))
  record$product_eligible <- all(c(
    record$annotation_present, record$typed_public_signature, record$generated_native_wrapper,
    record$generated_r_wrapper, record$registered_entry, record$configured_symbol_present,
    record$dynamic_lookup_disabled, record$forced_symbols
  ))
  record$source_class <- if (record$product_eligible) "generated_typed" else "generated_path_invalid"
  record$source_paths <- as.list(c(
    "src/cpp11/src/fixture.cpp", "src/cpp11/src/cpp11.cpp", "src/cpp11/R/cpp11.R"
  ))
  record$reason <- if (record$product_eligible) {
    "cpp11 annotation, retained generated native and R wrappers, and forced registration are present"
  } else {
    "cpp11 fixture lacks at least one generated package-path marker"
  }
  record
}

verify_extendr_fixture_source <- function(record, root_dir) {
  source <- source_ledger_read_lines(root_dir, "src/extendr/fixture/src/rust/src/lib.rs")
  generated_r <- source_ledger_read_lines(root_dir, "src/extendr/fixture/R/extendr-wrappers.R")
  entrypoint <- source_ledger_read_lines(root_dir, "src/extendr/fixture/src/entrypoint.c")
  functions <- fixture_function_map()[[record$fixture]]
  rust_functions <- if (identical(record$fixture, "F10")) {
    c("new", "increment", "read")
  } else {
    functions
  }
  annotations <- if (identical(record$fixture, "F10")) {
    c(
      source_ledger_annotation_before(
        source, "^[[:space:]]*impl[[:space:]]+FixtureState", "#[extendr]", distance = 2L
      ),
      vapply(rust_functions, function(function_name) {
        source_ledger_definition_present(
          source, sprintf("^[[:space:]]*fn[[:space:]]+%s[[:space:]]*\\(", function_name)
        )
      }, logical(1))
    )
  } else {
    vapply(rust_functions, function(function_name) {
      source_ledger_annotation_before(
        source, sprintf("^[[:space:]]*fn[[:space:]]+%s[[:space:]]*\\(", function_name),
        "#[extendr]", distance = 2L
      )
    }, logical(1))
  }
  signatures <- vapply(rust_functions, function(function_name) {
    source_ledger_signature(
      source, sprintf("^[[:space:]]*fn[[:space:]]+%s[[:space:]]*\\(", function_name)
    )
  }, character(1))
  r_symbols <- if (identical(record$fixture, "F10")) {
    paste0("wrap__FixtureState__", c("new", "increment", "read"))
  } else {
    paste0("wrap__", functions)
  }
  record$annotation_present <- fixture_all_present(annotations) &&
    any(grepl("extendr_module!", source, fixed = TRUE))
  record$typed_public_signature <- record$annotation_present &&
    fixture_signatures_are_typed(signatures, "\\b(Robj|SEXP)\\b") &&
    !any(grepl("extendr_ffi|extern[[:space:]]+\"C\"", source, perl = TRUE))
  record$generated_native_wrapper <- record$annotation_present &&
    any(grepl("R_init_zigr_extendr_extendr", entrypoint, fixed = TRUE))
  record$generated_r_wrapper <- fixture_all_present(vapply(r_symbols, function(symbol) {
    any(grepl(symbol, generated_r, fixed = TRUE))
  }, logical(1)))
  record$registered_entry <- record$generated_native_wrapper
  record$configured_symbol_present <- record$generated_r_wrapper
  record$forced_symbols <- FALSE
  record$product_eligible <- all(c(
    record$annotation_present, record$typed_public_signature, record$generated_native_wrapper,
    record$generated_r_wrapper, record$registered_entry, record$configured_symbol_present
  ))
  record$source_class <- if (record$product_eligible) "generated_typed" else "generated_path_invalid"
  record$source_paths <- as.list(c(
    "src/extendr/fixture/src/rust/src/lib.rs", "src/extendr/fixture/src/entrypoint.c",
    "src/extendr/fixture/R/extendr-wrappers.R"
  ))
  record$reason <- if (record$product_eligible) {
    "extendr attributes, module registration, and retained generated R wrappers are present"
  } else {
    "extendr fixture lacks at least one generated package-path marker"
  }
  record
}

verify_savvy_fixture_source <- function(record, root_dir) {
  source <- source_ledger_read_lines(root_dir, "src/savvy/fixture/src/rust/src/lib.rs")
  header <- source_ledger_read_lines(root_dir, "src/savvy/fixture/src/rust/api.h")
  init <- source_ledger_read_lines(root_dir, "src/savvy/fixture/src/init.c")
  generated_r <- source_ledger_read_lines(root_dir, "src/savvy/fixture/R/000-wrappers.R")
  functions <- fixture_function_map()[[record$fixture]]
  rust_functions <- if (identical(record$fixture, "F10")) {
    c("new", "increment", "read")
  } else {
    functions
  }
  annotations <- if (identical(record$fixture, "F10")) {
    c(
      source_ledger_annotation_before(
        source, "^[[:space:]]*impl[[:space:]]+FixtureState", "#[savvy]", distance = 2L
      ),
      vapply(rust_functions, function(function_name) {
        source_ledger_definition_present(
          source, sprintf("^[[:space:]]*fn[[:space:]]+%s[[:space:]]*\\(", function_name)
        )
      }, logical(1))
    )
  } else {
    vapply(rust_functions, function(function_name) {
      source_ledger_annotation_before(
        source, sprintf("^[[:space:]]*fn[[:space:]]+%s[[:space:]]*\\(", function_name),
        "#[savvy]", distance = 2L
      )
    }, logical(1))
  }
  signatures <- vapply(rust_functions, function(function_name) {
    source_ledger_signature(
      source, sprintf("^[[:space:]]*fn[[:space:]]+%s[[:space:]]*\\(", function_name)
    )
  }, character(1))
  symbols <- if (identical(record$fixture, "F10")) {
    paste0("savvy_FixtureState_", c("new", "increment", "read"), "__impl")
  } else {
    paste0("savvy_", functions, "__impl")
  }
  ffi_symbols <- sub("__impl$", "__ffi", symbols)
  record$annotation_present <- fixture_all_present(annotations)
  record$typed_public_signature <- record$annotation_present &&
    fixture_signatures_are_typed(signatures, "\\b(SEXP|Sexp)\\b") &&
    !any(grepl("savvy_ffi|extern[[:space:]]+\"C\"", source, perl = TRUE))
  record$generated_native_wrapper <- fixture_all_present(vapply(seq_along(symbols), function(index) {
    any(grepl(ffi_symbols[[index]], header, fixed = TRUE)) &&
      source_ledger_c_definition_present(init, symbols[[index]])
  }, logical(1)))
  record$generated_r_wrapper <- fixture_all_present(vapply(symbols, function(symbol) {
    any(grepl(symbol, generated_r, fixed = TRUE))
  }, logical(1)))
  record$registered_entry <- fixture_all_present(vapply(symbols, function(symbol) {
    any(grepl(paste0("{\"", symbol, "\""), init, fixed = TRUE))
  }, logical(1)))
  record$configured_symbol_present <- record$generated_native_wrapper && record$generated_r_wrapper
  record$dynamic_lookup_disabled <- any(grepl("R_useDynamicSymbols(dll, FALSE)", init, fixed = TRUE))
  record$forced_symbols <- any(grepl("R_forceSymbols(dll, TRUE)", init, fixed = TRUE))
  record$product_eligible <- all(c(
    record$annotation_present, record$typed_public_signature, record$generated_native_wrapper,
    record$generated_r_wrapper, record$registered_entry, record$configured_symbol_present,
    record$dynamic_lookup_disabled
  ))
  record$source_class <- if (record$product_eligible) "generated_typed" else "generated_path_invalid"
  record$source_paths <- as.list(c(
    "src/savvy/fixture/src/rust/src/lib.rs", "src/savvy/fixture/src/rust/api.h",
    "src/savvy/fixture/src/init.c", "src/savvy/fixture/R/000-wrappers.R"
  ))
  record$reason <- if (record$product_eligible) {
    "Savvy attributes and retained bindgen C and R outputs are present"
  } else {
    "Savvy fixture lacks at least one generated package-path marker"
  }
  record
}

verify_c_fixture_source <- function(record, root_dir) {
  source <- source_ledger_read_lines(root_dir, "src/c_call/register.c")
  symbols <- fixture_c_symbol_map()[[record$fixture]]
  record$source_class <- "registered_c_control"
  record$registered_entry <- fixture_all_present(vapply(symbols, function(symbol) {
    any(grepl(paste0("{\"", symbol, "\""), source, fixed = TRUE))
  }, logical(1)))
  record$configured_symbol_present <- fixture_all_present(vapply(symbols, function(symbol) {
    source_ledger_c_definition_present(source, symbol)
  }, logical(1)))
  record$dynamic_lookup_disabled <- any(grepl("R_useDynamicSymbols(dll, FALSE)", source, fixed = TRUE))
  record$forced_symbols <- any(grepl("R_forceSymbols(dll, TRUE)", source, fixed = TRUE))
  record$accepted_control <- all(c(
    record$registered_entry, record$configured_symbol_present,
    record$dynamic_lookup_disabled, record$forced_symbols
  ))
  record$source_paths <- list("src/c_call/register.c")
  record$reason <- if (record$accepted_control) {
    "the handwritten C control is defined and registered with forced symbols"
  } else {
    "the C fixture lacks a definition, registration entry, or forced-symbol policy"
  }
  record
}

build_fixture_r_provenance <- function(evidence) {
  rows <- evidence$fixture_rows[evidence$fixture_rows$runner == "r", , drop = FALSE]
  function_map <- fixture_r_function_map()
  records <- lapply(as.character(rows$fixture), function(fixture) {
    row <- rows[rows$fixture == fixture, , drop = FALSE]
    if (isTRUE(row$executable)) {
      build_r_provenance(fixture, unname(function_map[[fixture]]), row)
    } else {
      r_unrepresentable_provenance(fixture, as.character(row$reason))
    }
  })
  names(records) <- as.character(rows$fixture)
  records
}

build_fixture_optimized_r_provenance <- function() {
  row <- data.frame(implementation_role = "optimized_base_r", stringsAsFactors = FALSE)
  list(
    F03 = build_r_provenance("F03", "r_optimized_fixture_numeric", row),
    F04 = build_r_provenance("F04", "r_optimized_fixture_altrep_integer", row)
  )
}

verify_r_fixture_source <- function(record, provenance) {
  provenance_record <- provenance[[record$fixture]]
  if (is.null(provenance_record)) stop(sprintf("R fixture provenance is missing for %s", record$fixture))
  record$source_class <- as.character(provenance_record$implementation_class)
  record$configured_symbol_present <- isTRUE(record$executable)
  record$accepted_control <- isTRUE(record$executable)
  record$source_paths <- list("src/r/run_all.R")
  record$reason <- if (record$accepted_control) {
    "the explicit-loop R fixture passes its fixture-specific AST allowlist"
  } else {
    as.character(provenance_record$reason)
  }
  record
}

validate_fixture_source_gate <- function(records, evidence) {
  keys <- vapply(records, function(record) paste(record$runner, record$fixture, sep = "\r"), character(1))
  expected <- paste(evidence$fixture_rows$runner, evidence$fixture_rows$fixture, sep = "\r")
  if (anyDuplicated(keys) || !identical(sort(keys), sort(expected))) {
    stop("fixture source verification cells differ from the normalized fixture matrix")
  }
  names(records) <- keys
  for (index in seq_len(nrow(evidence$fixture_rows))) {
    row <- evidence$fixture_rows[index, , drop = FALSE]
    record <- records[[paste(row$runner, row$fixture, sep = "\r")]]
    if (isTRUE(row$executable) && identical(as.character(row$implementation_role), "product_public_path") &&
        !isTRUE(record$product_eligible)) {
      stop(sprintf("fixture product label is not source eligible for %s/%s", row$runner, row$fixture))
    }
    if (isTRUE(row$executable) && identical(as.character(row$path_kind), "generated_typed") &&
        !isTRUE(record$typed_public_signature)) {
      stop(sprintf("fixture typed label has no typed public signature for %s/%s", row$runner, row$fixture))
    }
    if (isTRUE(row$executable) && identical(as.character(row$path_kind), "generated_public_adapter") &&
        !isTRUE(record$approved_public_adapter)) {
      stop(sprintf("fixture adapter label has no approved public adapter for %s/%s", row$runner, row$fixture))
    }
    if (isTRUE(row$executable) && row$runner %in% c("r", "c_call") &&
        !isTRUE(record$accepted_control)) {
      stop(sprintf("fixture control is not source eligible for %s/%s", row$runner, row$fixture))
    }
    if (!isTRUE(row$executable) && !grepl("gap|unrepresentable", record$source_class)) {
      stop(sprintf("fixture gap is not source backed for %s/%s", row$runner, row$fixture))
    }
  }
  product_counts <- vapply(c("zigr", "rcpp", "cpp11", "extendr", "savvy"), function(runner) {
    sum(vapply(records, function(record) {
      identical(record$runner, runner) && isTRUE(record$product_eligible)
    }, logical(1)))
  }, integer(1))
  if (!identical(product_counts, c(zigr = 11L, rcpp = 12L, cpp11 = 9L, extendr = 12L, savvy = 12L))) {
    stop("fixture source verifier product counts differ from the supported capability matrix")
  }
  control_counts <- vapply(c("r", "c_call"), function(runner) {
    sum(vapply(records, function(record) {
      identical(record$runner, runner) && isTRUE(record$accepted_control)
    }, logical(1)))
  }, integer(1))
  if (!identical(control_counts, c(r = 11L, c_call = 12L))) {
    stop("fixture source verifier control counts differ from the reference matrix")
  }
  invisible(records)
}

verify_fixture_source_paths <- function(root_dir, evidence) {
  source(file.path(root_dir, "src", "r", "run_all.R"), local = .GlobalEnv)
  r_provenance <- build_fixture_r_provenance(evidence)
  optimized_r_provenance <- build_fixture_optimized_r_provenance()
  cpp11_dependency <- cpp11_fixture_dependency_source()
  records <- lapply(seq_len(nrow(evidence$fixture_rows)), function(index) {
    row <- evidence$fixture_rows[index, , drop = FALSE]
    record <- fixture_source_record(row)
    record <- switch(record$runner,
      zigr = verify_zigr_fixture_source(record, root_dir),
      rcpp = verify_rcpp_fixture_source(record, root_dir),
      cpp11 = verify_cpp11_fixture_source(record, root_dir, cpp11_dependency),
      extendr = verify_extendr_fixture_source(record, root_dir),
      savvy = verify_savvy_fixture_source(record, root_dir),
      c_call = verify_c_fixture_source(record, root_dir),
      r = verify_r_fixture_source(record, r_provenance),
      stop(sprintf("no fixture source verifier for runner %s", record$runner))
    )
    record$verification_digest <- source_ledger_object_digest(record)
    record
  })
  validate_fixture_source_gate(records, evidence)
  list(
    records = records,
    r_provenance = r_provenance,
    optimized_r_provenance = optimized_r_provenance
  )
}

fixture_expect_error <- function(expression, label) {
  error <- tryCatch({
    force(expression)
    NULL
  }, error = function(condition) condition)
  if (is.null(error)) stop(sprintf("fixture probe did not fail: %s", label), call. = FALSE)
  invisible(error)
}

fixture_schema_value <- function(ratio = 0.5) {
  list(id = 1L, count = 2L, ratio = ratio, enabled = TRUE)
}

fixture_case <- function(id, function_name, arguments, valid = TRUE, fresh_output = FALSE,
                         copied_output = FALSE,
                         allocation_nodes = if (fresh_output) 1L else 0L) {
  list(
    id = id,
    function_name = function_name,
    arguments = arguments,
    valid = valid,
    fresh_output = fresh_output,
    copied_output = copied_output,
    allocation_nodes = allocation_nodes
  )
}

fixture_contract_cases <- function() {
  schema_extra_attribute <- function() {
    value <- fixture_schema_value()
    attr(value, "extra") <- "not allowed"
    value
  }
  schema_reordered <- function() {
    value <- fixture_schema_value()
    value[c("count", "id", "ratio", "enabled")]
  }
  schema_duplicate_names <- function() {
    value <- fixture_schema_value()
    names(value)[[2L]] <- "id"
    value
  }
  schema_field <- function(name, value) {
    result <- fixture_schema_value()
    result[[name]] <- value
    result
  }
  list(
    F01 = list(
      fixture_case("integer one", "fixture_zero", function() list(), fresh_output = TRUE)
    ),
    F02 = list(
      fixture_case("finite", "fixture_scalar", function() list(2.5), fresh_output = TRUE),
      fixture_case("negative zero", "fixture_scalar", function() list(-0.0), fresh_output = TRUE),
      fixture_case("positive infinity", "fixture_scalar", function() list(Inf), fresh_output = TRUE),
      fixture_case("negative infinity", "fixture_scalar", function() list(-Inf), fresh_output = TRUE),
      fixture_case("NaN", "fixture_scalar", function() list(NaN), fresh_output = TRUE),
      fixture_case("missing scalar", "fixture_scalar", function() list(NA_real_), valid = FALSE),
      fixture_case("integer type", "fixture_scalar", function() list(1L), valid = FALSE),
      fixture_case("logical type", "fixture_scalar", function() list(TRUE), valid = FALSE),
      fixture_case("null", "fixture_scalar", function() list(NULL), valid = FALSE),
      fixture_case("empty", "fixture_scalar", function() list(numeric()), valid = FALSE),
      fixture_case("length two", "fixture_scalar", function() list(c(1, 2)), valid = FALSE)
    ),
    F03 = list(
      fixture_case("empty", "fixture_numeric", function() list(numeric()), fresh_output = TRUE,
                   copied_output = TRUE),
      fixture_case(
        "special values and attributes", "fixture_numeric",
        function() list(structure(c(-0.0, 1.5, NA_real_, NaN, Inf, -Inf), names = letters[1:6])),
        fresh_output = TRUE, copied_output = TRUE
      ),
      fixture_case("integer type", "fixture_numeric", function() list(1:3), valid = FALSE),
      fixture_case("logical type", "fixture_numeric", function() list(c(TRUE, FALSE)), valid = FALSE)
    ),
    F04 = list(
      fixture_case("compact sequence", "fixture_altrep_integer", function() list(1:100000),
                   fresh_output = TRUE),
      fixture_case("empty", "fixture_altrep_integer", function() list(integer()),
                   fresh_output = TRUE),
      fixture_case("negative and large", "fixture_altrep_integer",
                   function() list(c(-7L, 0L, .Machine$integer.max, .Machine$integer.max)),
                   fresh_output = TRUE),
      fixture_case("missing integer", "fixture_altrep_integer",
                   function() list(c(1L, NA_integer_, 3L)), fresh_output = TRUE),
      fixture_case("double type", "fixture_altrep_integer", function() list(c(1, 2)), valid = FALSE),
      fixture_case("logical type", "fixture_altrep_integer", function() list(c(TRUE, FALSE)),
                   valid = FALSE)
    ),
    F05 = list(
      fixture_case("encodings empty and missing", "fixture_strings",
                   function() list(benchmark_encoded_strings()), fresh_output = TRUE),
      fixture_case("empty vector", "fixture_strings", function() list(character()),
                   fresh_output = TRUE),
      fixture_case("factor type", "fixture_strings", function() list(factor("a")), valid = FALSE),
      fixture_case("raw type", "fixture_strings", function() list(charToRaw("a")), valid = FALSE)
    ),
    F06 = list(
      fixture_case("empty", "fixture_raw", function() list(raw()), fresh_output = TRUE,
                   copied_output = TRUE),
      fixture_case("all byte edges", "fixture_raw",
                   function() list(structure(as.raw(c(0, 1, 127, 128, 255)), names = letters[1:5])),
                   fresh_output = TRUE, copied_output = TRUE),
      fixture_case("integer type", "fixture_raw", function() list(1:3), valid = FALSE)
    ),
    F07 = list(
      fixture_case("empty", "fixture_complex", function() list(complex()), fresh_output = TRUE,
                   copied_output = TRUE),
      fixture_case(
        "missing NaN infinity and attributes", "fixture_complex",
        function() list(structure(c(
          1 + 2i, NA_complex_, complex(real = NaN, imaginary = 3),
          complex(real = Inf, imaginary = -Inf), complex(real = -0.0, imaginary = 0.0)
        ), names = letters[1:5])),
        fresh_output = TRUE, copied_output = TRUE
      ),
      fixture_case("double type", "fixture_complex", function() list(c(1, 2)), valid = FALSE)
    ),
    F08 = list(
      fixture_case("all states", "fixture_logical_counts",
                   function() list(c(FALSE, TRUE, NA, FALSE, NA, TRUE)), fresh_output = TRUE),
      fixture_case("empty", "fixture_logical_counts", function() list(logical()),
                   fresh_output = TRUE),
      fixture_case("integer type", "fixture_logical_counts", function() list(c(0L, 1L)),
                   valid = FALSE)
    ),
    F09 = list(
      fixture_case("finite schema", "fixture_schema", function() list(fixture_schema_value())),
      fixture_case("NaN ratio", "fixture_schema", function() list(fixture_schema_value(NaN))),
      fixture_case("infinite ratio", "fixture_schema", function() list(fixture_schema_value(Inf))),
      fixture_case("unnamed", "fixture_schema", function() list(unname(fixture_schema_value())),
                   valid = FALSE),
      fixture_case("reordered", "fixture_schema", function() list(schema_reordered()), valid = FALSE),
      fixture_case("duplicate names", "fixture_schema", function() list(schema_duplicate_names()),
                   valid = FALSE),
      fixture_case("extra field", "fixture_schema",
                   function() list(c(fixture_schema_value(), list(extra = 1L))), valid = FALSE),
      fixture_case("extra attribute", "fixture_schema", function() list(schema_extra_attribute()),
                   valid = FALSE),
      fixture_case("missing id", "fixture_schema",
                   function() list(schema_field("id", NA_integer_)), valid = FALSE),
      fixture_case("missing count", "fixture_schema",
                   function() list(schema_field("count", NA_integer_)), valid = FALSE),
      fixture_case("missing ratio", "fixture_schema",
                   function() list(schema_field("ratio", NA_real_)), valid = FALSE),
      fixture_case("missing enabled", "fixture_schema",
                   function() list(schema_field("enabled", NA)), valid = FALSE),
      fixture_case("wrong id type", "fixture_schema",
                   function() list(schema_field("id", 1)), valid = FALSE),
      fixture_case("wrong count length", "fixture_schema",
                   function() list(schema_field("count", c(1L, 2L))), valid = FALSE),
      fixture_case("wrong ratio type", "fixture_schema",
                   function() list(schema_field("ratio", 1L)), valid = FALSE),
      fixture_case("wrong enabled type", "fixture_schema",
                   function() list(schema_field("enabled", 1L)), valid = FALSE)
    ),
    F12 = list(
      fixture_case(
        "complete owned output", "fixture_outputs", function() list(),
        fresh_output = TRUE, allocation_nodes = 8L
      )
    )
  )
}

fixture_capture <- function(expression) {
  tryCatch(
    list(ok = TRUE, value = force(expression), condition = NULL),
    error = function(condition) list(ok = FALSE, value = NULL, condition = condition)
  )
}

fixture_value_metadata <- function(value) {
  attributes_value <- attributes(value)
  list(
    type = typeof(value),
    length = length(value),
    attribute_names = names(attributes_value),
    encoding = if (is.character(value)) Encoding(value) else character(0),
    missing = if (is.atomic(value)) {
      ifelse(is.na(value), ifelse(is.nan(value), "nan", "na"), "present")
    } else {
      character(0)
    },
    zero_sign = if (is.double(value)) {
      ifelse(!is.na(value) & value == 0, ifelse(1 / value < 0, "negative", "positive"), "none")
    } else if (is.complex(value)) {
      list(
        real = ifelse(!is.na(Re(value)) & Re(value) == 0,
                      ifelse(1 / Re(value) < 0, "negative", "positive"), "none"),
        imaginary = ifelse(!is.na(Im(value)) & Im(value) == 0,
                           ifelse(1 / Im(value) < 0, "negative", "positive"), "none")
      )
    } else {
      character(0)
    },
    values = if (is.list(value)) lapply(value, fixture_value_metadata) else NULL,
    attributes = if (is.null(attributes_value)) NULL else {
      lapply(unname(attributes_value), fixture_value_metadata)
    }
  )
}

fixture_assert_same <- function(expected, actual, label) {
  if (!identical(expected, actual) ||
      !identical(fixture_value_metadata(expected), fixture_value_metadata(actual))) {
    stop(sprintf("fixture values differ for %s", label), call. = FALSE)
  }
  invisible(TRUE)
}

fixture_assert_error <- function(outcome, label, message_pattern = NULL) {
  if (isTRUE(outcome$ok) || !inherits(outcome$condition, "error")) {
    stop(sprintf("fixture call did not return an R error for %s", label), call. = FALSE)
  }
  message <- conditionMessage(outcome$condition)
  if (!nzchar(message)) stop(sprintf("fixture error message is blank for %s", label), call. = FALSE)
  if (!is.null(message_pattern) && !grepl(message_pattern, message, ignore.case = TRUE, perl = TRUE)) {
    stop(sprintf("fixture error message differs for %s: %s", label, message), call. = FALSE)
  }
  invisible(outcome$condition)
}

fixture_visible_output_nodes <- function(value) {
  if (!is.list(value)) return(1L)
  1L + sum(vapply(value, fixture_visible_output_nodes, integer(1)))
}

fixture_assert_fresh_tree <- function(left, right, diagnostics, label) {
  if ((is.list(left) || length(left) != 1L) && isTRUE(diagnostics$same_sexp(left, right))) {
    stop(sprintf("fixture reused an output SEXP for %s", label), call. = FALSE)
  }
  if (is.list(left) && is.list(right)) {
    if (length(left) != length(right)) {
      stop(sprintf("fixture output trees differ for %s", label), call. = FALSE)
    }
    for (index in seq_along(left)) {
      fixture_assert_fresh_tree(
        left[[index]], right[[index]], diagnostics,
        sprintf("%s/child-%d", label, index)
      )
    }
  }
  invisible(TRUE)
}

fixture_run_value_matrix <- function(runner, functions, supported, reference_functions,
                                     control_functions, control_diagnostics, optimized_functions) {
  cases <- fixture_contract_cases()
  counts <- c(valid = 0L, invalid = 0L, allocation = 0L, copy = 0L)
  for (fixture in intersect(names(cases), supported)) {
    for (case in cases[[fixture]]) {
      label <- sprintf("%s/%s/%s", runner, fixture, case$id)
      reference_arguments <- case$arguments()
      reference <- fixture_capture(do.call(
        reference_functions[[case$function_name]], reference_arguments
      ))
      target_arguments <- case$arguments()
      target_before <- unserialize(serialize(target_arguments, NULL))
      target <- fixture_capture(do.call(functions[[case$function_name]], target_arguments))
      fixture_assert_same(target_before, target_arguments, paste(label, "input preservation"))

      control_arguments <- case$arguments()
      control <- fixture_capture(do.call(control_functions[[case$function_name]], control_arguments))
      if (isTRUE(case$valid)) {
        if (!isTRUE(reference$ok)) fixture_assert_error(reference, paste(label, "R oracle"))
        if (!isTRUE(target$ok)) fixture_assert_error(target, paste(label, "target"))
        if (!isTRUE(control$ok)) fixture_assert_error(control, paste(label, "C control"))
        fixture_assert_same(reference$value, target$value, paste(label, "R comparison"))
        fixture_assert_same(reference$value, control$value, paste(label, "C comparison"))
        counts[["valid"]] <- counts[["valid"]] + 1L

        if (isTRUE(case$fresh_output)) {
          if (length(target_arguments) > 0L &&
              isTRUE(control_diagnostics$same_sexp(target_arguments[[1L]], target$value))) {
            stop(sprintf("fixture returned its input SEXP for %s", label), call. = FALSE)
          }
          repeated <- fixture_capture(do.call(
            functions[[case$function_name]], case$arguments()
          ))
          if (!isTRUE(repeated$ok)) fixture_assert_error(repeated, paste(label, "repeat"))
          fixture_assert_same(target$value, repeated$value, paste(label, "repeat comparison"))
          fixture_assert_fresh_tree(
            target$value, repeated$value, control_diagnostics, paste(label, "fresh output")
          )
          allocation_nodes <- fixture_visible_output_nodes(target$value)
          if (!identical(allocation_nodes, case$allocation_nodes)) {
            stop(sprintf(
              "R-visible output allocation count differs for %s: got %d; expected %d",
              label, allocation_nodes, case$allocation_nodes
            ), call. = FALSE)
          }
          counts[["allocation"]] <- counts[["allocation"]] + allocation_nodes
        }
        if (isTRUE(case$copied_output) && length(target_arguments[[1L]]) > 0L) {
          if (isTRUE(control_diagnostics$same_data_pointer(
            target_arguments[[1L]], target$value
          ))) {
            stop(sprintf("fixture output aliases input storage for %s", label), call. = FALSE)
          }
          counts[["copy"]] <- counts[["copy"]] + 1L
        }
        if (fixture %in% names(optimized_functions)) {
          optimized <- fixture_capture(do.call(optimized_functions[[fixture]], case$arguments()))
          if (!isTRUE(optimized$ok)) fixture_assert_error(optimized, paste(label, "optimized R"))
          fixture_assert_same(reference$value, optimized$value, paste(label, "optimized R comparison"))
        }
      } else {
        fixture_assert_error(reference, paste(label, "R oracle"))
        fixture_assert_error(target, paste(label, "target"))
        fixture_assert_error(control, paste(label, "C control"))
        counts[["invalid"]] <- counts[["invalid"]] + 1L
      }
    }
  }

  counts
}

fixture_run_output_proof <- function(runner, functions, supported, reference_functions) {
  if (!"F12" %in% supported) return(c(construction = 0L, retention = 0L))

  expected <- reference_functions$fixture_outputs()
  previous_torture <- gctorture(TRUE)
  torture_restored <- FALSE
  on.exit({
    if (!torture_restored) gctorture(previous_torture)
  }, add = TRUE)
  output <- functions$fixture_outputs()
  gctorture(previous_torture)
  torture_restored <- TRUE
  fixture_assert_same(
    expected, output, sprintf("%s/F12/construction under forced GC", runner)
  )

  retained <- functions$fixture_outputs()
  retained_expected <- unserialize(serialize(retained, NULL))
  pressure <- lapply(seq_len(64L), function(index) raw(1024L + index))
  gc()
  fixture_assert_same(retained_expected, retained, sprintf("%s/F12/GC retention", runner))
  if (length(pressure) != 64L) stop("fixture GC pressure was not constructed")
  c(construction = 1L, retention = 1L)
}

fixture_r_functions <- function(root_dir) {
  environment <- new.env(parent = .GlobalEnv)
  sys.source(file.path(root_dir, "src", "r", "run_all.R"), envir = environment)
  mapping <- fixture_r_function_map()
  functions <- lapply(unique(unlist(fixture_function_map(), use.names = FALSE)), function(name) NULL)
  names(functions) <- unique(unlist(fixture_function_map(), use.names = FALSE))
  for (fixture in setdiff(names(mapping), "F11")) {
    functions[[fixture_function_map()[[fixture]][[1L]]]] <- environment[[mapping[[fixture]]]]
  }
  functions$fixture_scalar <- environment$r_fixture_scalar
  functions$fixture_error <- environment$r_fixture_error
  list(
    functions = functions,
    optimized = list(
      F03 = environment$r_optimized_fixture_numeric,
      F04 = environment$r_optimized_fixture_altrep_integer
    )
  )
}

fixture_c_context <- function(root_dir) {
  path <- file.path(root_dir, "src", "c_call", paste0("bench", .Platform$dynlib.ext))
  if (!file.exists(path)) stop("registered C fixture library is missing")
  dll <- dyn.load(path)
  routines <- getDLLRegisteredRoutines(dll)[[".Call"]]
  call <- function(name, ...) {
    routine <- routines[[name]]
    if (is.null(routine)) stop(sprintf("registered C diagnostic is missing: %s", name))
    do.call(.Call, c(list(routine), list(...)))
  }
  functions <- list()
  for (fixture in names(fixture_function_map())) {
    function_names <- fixture_function_map()[[fixture]]
    for (function_name in function_names) {
      functions[[function_name]] <- local({
        symbol <- paste0("c_benchmark_", function_name)
        function(...) call(symbol, ...)
      })
    }
  }
  list(
    dll = dll,
    close = function() {
      gc()
      dyn.unload(dll[["path"]])
    },
    functions = functions,
    diagnostics = list(
      same_sexp = function(left, right) call("c_benchmark_same_sexp", left, right),
      same_data_pointer = function(left, right) {
        call("c_benchmark_same_data_pointer", left, right)
      },
      wrong_pointer = function() call("c_benchmark_wrong_pointer"),
      cleared_pointer_like = function(value) call("c_benchmark_cleared_pointer_like", value),
      altrep_new = function(start, length) call("c_benchmark_altrep_new", start, length),
      altrep_reset = function() call("c_benchmark_altrep_reset"),
      altrep_snapshot = function(value) call("c_benchmark_altrep_snapshot", value),
      lifecycle_reset = function() call("c_benchmark_lifecycle_reset"),
      lifecycle_counts = function() call("c_benchmark_lifecycle_snapshot")
    )
  )
}

fixture_altrep_expectation <- function(runner) {
  expected <- list(
    zigr = c(element = 0L, region = 1L, pointer = 0L, materialization = 0L,
             is_materialized = 0L),
    rcpp = c(element = 0L, region = 0L, pointer = 1L, materialization = 1L,
             is_materialized = 1L),
    cpp11 = c(element = 0L, region = 7L, pointer = 0L, materialization = 0L,
              is_materialized = 0L),
    extendr = c(element = 257L, region = 0L, pointer = 0L, materialization = 0L,
                is_materialized = 0L),
    savvy = c(element = 0L, region = 0L, pointer = 1L, materialization = 1L,
              is_materialized = 1L),
    r = c(element = 257L, region = 0L, pointer = 0L, materialization = 0L,
          is_materialized = 0L),
    c_call = c(element = 0L, region = 1L, pointer = 0L, materialization = 0L,
               is_materialized = 0L)
  )[[runner]]
  if (is.null(expected)) stop(sprintf("ALTREP strategy is not declared for %s", runner))
  expected
}

fixture_run_altrep_probe <- function(runner, functions, supported, diagnostics) {
  if (!"F04" %in% supported) return(integer())
  expected <- fixture_altrep_expectation(runner)

  input <- diagnostics$altrep_new(1L, 257L)
  diagnostics$altrep_reset()
  result <- functions$fixture_altrep_integer(input)
  counts <- diagnostics$altrep_snapshot(input)
  fixture_assert_same(33153, result, sprintf("%s/F04/instrumented ALTREP result", runner))
  if (!identical(counts, expected)) {
    stop(sprintf(
      "ALTREP callback counts differ for %s: got %s; expected %s",
      runner,
      paste(sprintf("%s=%d", names(counts), counts), collapse = ", "),
      paste(sprintf("%s=%d", names(expected), expected), collapse = ", ")
    ), call. = FALSE)
  }
  counts
}

fixture_lifecycle_api <- function(functions, control_diagnostics = NULL) {
  if (!is.null(control_diagnostics)) {
    return(list(
      reset = control_diagnostics$lifecycle_reset,
      counts = control_diagnostics$lifecycle_counts
    ))
  }
  if (is.null(functions$fixture_lifecycle_reset) || is.null(functions$fixture_lifecycle_counts)) {
    return(NULL)
  }
  list(reset = functions$fixture_lifecycle_reset, counts = functions$fixture_lifecycle_counts)
}

fixture_run_error_recovery <- function(runner, functions, supported, lifecycle) {
  if (!"F11" %in% supported) return(c(error = 0L, recovery = 0L))
  gc()
  if (!is.null(lifecycle)) lifecycle$reset()
  attempts <- list(
    wrong_type = function() functions$fixture_scalar(1L),
    wrong_length = function() functions$fixture_scalar(c(1, 2)),
    missing_scalar = function() functions$fixture_scalar(NA_real_),
    native_error = function() functions$fixture_error(1.0)
  )
  counts <- c(error = 0L, recovery = 0L)
  for (iteration in seq_len(8L)) {
    for (name in names(attempts)) {
      outcome <- fixture_capture(attempts[[name]]())
      fixture_assert_error(
        outcome,
        sprintf("%s/F11/%s/repetition-%d", runner, name, iteration),
        if (identical(name, "native_error")) "fixture error" else NULL
      )
      counts[["error"]] <- counts[["error"]] + 1L
      fixture_assert_same(
        2.5,
        functions$fixture_scalar(2.5),
        sprintf("%s/F11/recovery-%s-%d", runner, name, iteration)
      )
      counts[["recovery"]] <- counts[["recovery"]] + 1L
    }
  }
  if (!is.null(lifecycle)) {
    expected <- c(constructor = 0L, method = 0L, error = 8L, finalizer = 0L)
    actual <- lifecycle$counts()
    if (!identical(actual, expected)) {
      stop(sprintf("native error lifecycle counts differ for %s", runner), call. = FALSE)
    }
  }
  counts
}

fixture_state_pointer <- function(runner, state) {
  switch(runner,
    zigr = state,
    c_call = state,
    extendr = state,
    rcpp = get(".pointer", envir = methods::slot(state, ".xData"), inherits = FALSE),
    savvy = get("self", envir = environment(state$increment), inherits = FALSE),
    stop(sprintf("no state-pointer observation for %s", runner))
  )
}

fixture_state_with_pointer <- function(runner, state, pointer) {
  switch(runner,
    zigr = pointer,
    c_call = pointer,
    extendr = {
      class(pointer) <- class(state)
      pointer
    },
    rcpp = {
      assign(".pointer", pointer, envir = methods::slot(state, ".xData"))
      state
    },
    savvy = {
      assign("self", pointer, envir = environment(state$increment))
      state
    },
    stop(sprintf("no state-pointer replacement for %s", runner))
  )
}

fixture_run_state_lifecycle <- function(runner, functions, supported, diagnostics, lifecycle) {
  if (!"F10" %in% supported) return(c(constructor = 0L, method = 0L, finalizer = 0L))
  if (is.null(lifecycle)) stop(sprintf("F10 lifecycle counters are missing for %s", runner))
  gc()
  lifecycle$reset()
  state <- functions$fixture_new()
  fixture_assert_same(0L, functions$fixture_read(state), sprintf("%s/F10/initial read", runner))
  fixture_assert_same(3L, functions$fixture_method(state, 3L), sprintf("%s/F10/increment", runner))
  fixture_assert_same(2L, functions$fixture_method(state, -1L), sprintf("%s/F10/decrement", runner))
  fixture_assert_same(2L, functions$fixture_read(state), sprintf("%s/F10/read", runner))

  wrong_pointer <- diagnostics$wrong_pointer()
  invalid <- list(
    wrong_receiver = function() functions$fixture_method(list(), 1L),
    wrong_pointer_type = function() functions$fixture_method(wrong_pointer, 1L),
    wrong_amount_type = function() functions$fixture_method(state, 1.0),
    wrong_amount_length = function() functions$fixture_method(state, c(1L, 2L)),
    missing_amount = function() functions$fixture_method(state, NA_integer_)
  )
  for (iteration in seq_len(8L)) {
    for (name in names(invalid)) {
      fixture_assert_error(
        fixture_capture(invalid[[name]]()),
        sprintf("%s/F10/%s/repetition-%d", runner, name, iteration)
      )
    }
  }

  owned_pointer <- fixture_state_pointer(runner, state)
  cleared_pointer <- diagnostics$cleared_pointer_like(owned_pointer)
  cleared_state <- fixture_state_with_pointer(runner, state, cleared_pointer)
  if (!identical(class(cleared_state), class(state))) {
    stop(sprintf("cleared fixture receiver class differs for %s", runner), call. = FALSE)
  }
  for (iteration in seq_len(8L)) {
    fixture_assert_error(
      fixture_capture(functions$fixture_method(cleared_state, 1L)),
      sprintf("%s/F10/cleared-pointer/repetition-%d", runner, iteration)
    )
  }
  if (runner %in% c("rcpp", "savvy")) {
    state <- fixture_state_with_pointer(runner, state, owned_pointer)
  }
  fixture_assert_same(3L, functions$fixture_method(state, 1L), sprintf("%s/F10/recovery", runner))

  rm(cleared_state, cleared_pointer, owned_pointer, wrong_pointer)
  rm(state)
  gc()
  gc()
  expected <- c(constructor = 1L, method = 5L, error = 0L, finalizer = 1L)
  actual <- lifecycle$counts()
  if (!identical(actual, expected)) {
    stop(sprintf(
      "native state lifecycle counts differ for %s: got %s; expected %s",
      runner,
      paste(sprintf("%s=%d", names(actual), actual), collapse = ", "),
      paste(sprintf("%s=%d", names(expected), expected), collapse = ", ")
    ), call. = FALSE)
  }
  actual[c("constructor", "method", "finalizer")]
}

fixture_expected_value_counts <- function(supported) {
  contract_cases <- fixture_contract_cases()
  cases <- unlist(
    contract_cases[intersect(names(contract_cases), supported)],
    recursive = FALSE, use.names = FALSE
  )
  valid <- vapply(cases, function(case) isTRUE(case$valid), logical(1))
  c(
    valid = sum(valid),
    invalid = sum(!valid),
    allocation = sum(vapply(cases[valid], function(case) case$allocation_nodes, integer(1))),
    copy = sum(valid & vapply(cases, function(case) {
      isTRUE(case$copied_output) && length(case$arguments()[[1L]]) > 0L
    }, logical(1)))
  )
}

fixture_validate_proof <- function(proof, runner, supported) {
  required <- c("values", "output", "altrep", "recovery", "state")
  if (!is.list(proof) || !identical(names(proof), required)) {
    stop(sprintf("fixture proof fields differ for %s", runner), call. = FALSE)
  }
  expected_values <- fixture_expected_value_counts(supported)
  if (!identical(proof$values, expected_values)) {
    stop(sprintf("fixture semantic case counts differ for %s", runner), call. = FALSE)
  }
  expected_output <- if ("F12" %in% supported) {
    c(construction = 1L, retention = 1L)
  } else {
    c(construction = 0L, retention = 0L)
  }
  if (!identical(proof$output, expected_output)) {
    stop(sprintf("fixture output protection proof differs for %s", runner), call. = FALSE)
  }
  expected_altrep <- if ("F04" %in% supported) fixture_altrep_expectation(runner) else integer()
  if (!identical(proof$altrep, expected_altrep)) {
    stop(sprintf("fixture ALTREP proof differs for %s", runner), call. = FALSE)
  }
  expected_recovery <- if ("F11" %in% supported) {
    c(error = 32L, recovery = 32L)
  } else {
    c(error = 0L, recovery = 0L)
  }
  if (!identical(proof$recovery, expected_recovery)) {
    stop(sprintf("fixture recovery counts differ for %s", runner), call. = FALSE)
  }
  expected_state <- if ("F10" %in% supported) {
    c(constructor = 1L, method = 5L, finalizer = 1L)
  } else {
    c(constructor = 0L, method = 0L, finalizer = 0L)
  }
  if (!identical(proof$state, expected_state)) {
    stop(sprintf("fixture state lifecycle proof differs for %s", runner), call. = FALSE)
  }
  invisible(proof)
}

fixture_registered_surface <- function(dll, required_symbols) {
  routines <- getDLLRegisteredRoutines(dll)[[".Call"]]
  if (!all(required_symbols %in% names(routines))) {
    stop(sprintf("registered fixture symbols are missing from %s", dll[["name"]]))
  }
  zero_symbol <- routines[[required_symbols[[1L]]]]
  fixture_expect_error(
    do.call(.Call, list(zero_symbol, 1)), sprintf("%s arity probe", dll[["name"]])
  )
  fixture_expect_error(
    do.call(.External, list(zero_symbol)), sprintf("%s wrong-interface probe", dll[["name"]])
  )
  fixture_expect_error(
    getNativeSymbolInfo("fixture_symbol_that_does_not_exist", PACKAGE = dll),
    sprintf("%s missing-symbol probe", dll[["name"]])
  )
  invisible(TRUE)
}

fixture_package_symbols <- function(runner, supported) {
  functions <- unique(unlist(fixture_function_map()[supported], use.names = FALSE))
  switch(runner,
    zigr = vapply(functions, function(function_name) {
      if (identical(function_name, "fixture_method")) return("fixture_FixtureState__increment")
      if (identical(function_name, "fixture_read")) return("fixture_FixtureState__read")
      function_name
    }, character(1)),
    rcpp = unique(c(
      paste0("_zigrRcpp_", setdiff(functions, c("fixture_new", "fixture_method", "fixture_read"))),
      if ("F10" %in% supported) "_rcpp_module_boot_zigr_fixture_module" else character(0)
    )),
    cpp11 = paste0("_zigrCpp11_", functions),
    extendr = unique(c(
      paste0("wrap__", setdiff(functions, c("fixture_new", "fixture_method", "fixture_read"))),
      if ("F10" %in% supported) paste0("wrap__FixtureState__", c("new", "increment", "read")) else character(0)
    )),
    savvy = unique(c(
      paste0("savvy_", setdiff(functions, c("fixture_new", "fixture_method", "fixture_read")), "__impl"),
      if ("F10" %in% supported) paste0("savvy_FixtureState_", c("new", "increment", "read"), "__impl") else character(0)
    )),
    stop(sprintf("no package symbol map for %s", runner))
  )
}

fixture_lifecycle_symbols <- function(runner) {
  switch(runner,
    zigr = c("fixture_lifecycle_reset", "fixture_lifecycle_counts"),
    rcpp = paste0("_zigrRcpp_fixture_lifecycle_", c("reset", "counts")),
    cpp11 = paste0("_zigrCpp11_fixture_lifecycle_", c("reset", "counts")),
    extendr = paste0("wrap__fixture_lifecycle_", c("reset", "counts")),
    savvy = paste0("savvy_fixture_lifecycle_", c("reset", "counts"), "__impl"),
    stop(sprintf("no lifecycle symbol map for %s", runner))
  )
}

run_fixture_package_gate <- function(root_dir, runner, evidence) {
  package <- direct_runner_package(root_dir, runner)
  rows <- evidence$fixture_rows[
    evidence$fixture_rows$runner == runner & evidence$fixture_rows$executable,
    , drop = FALSE
  ]
  supported <- as.character(rows$fixture)
  if (!dir.exists(file.path(package$library, package$package))) {
    stop(sprintf("installed fixture package is missing for %s", runner))
  }
  old_paths <- .libPaths()
  .libPaths(c(package$library, old_paths))
  on.exit(.libPaths(old_paths), add = TRUE)
  if (package$package %in% loadedNamespaces()) unloadNamespace(package$package)
  loadNamespace(package$package, lib.loc = package$library)
  unloadNamespace(package$package)
  loadNamespace(package$package, lib.loc = package$library)
  dll <- getLoadedDLLs()[[package$dll]]
  if (is.null(dll)) stop(sprintf("fixture DLL did not load for %s", runner))
  if (isTRUE(dll[["dynamicLookup"]])) {
    stop(sprintf("fixture DLL leaves dynamic lookup enabled for %s", runner))
  }
  expected_forced <- runner %in% c("zigr", "cpp11")
  if (!identical(isTRUE(dll[["forceSymbols"]]), expected_forced)) {
    stop(sprintf("fixture DLL force-symbol policy differs from generated %s convention", runner))
  }
  fixture_registered_surface(
    dll, c(fixture_package_symbols(runner, supported), fixture_lifecycle_symbols(runner))
  )
  namespace <- asNamespace(package$package)
  gap_fixtures <- as.character(evidence$fixture_rows$fixture[
    evidence$fixture_rows$runner == runner & !evidence$fixture_rows$executable
  ])
  if (length(gap_fixtures) > 0L) {
    gap_functions <- unique(unlist(fixture_function_map()[gap_fixtures], use.names = FALSE))
    exposed_functions <- gap_functions[vapply(gap_functions, function(name) {
      exists(name, envir = namespace, mode = "function", inherits = FALSE)
    }, logical(1))]
    registered <- names(getDLLRegisteredRoutines(dll)[[".Call"]])
    exposed_symbols <- intersect(fixture_package_symbols(runner, gap_fixtures), registered)
    if (length(exposed_functions) > 0L || length(exposed_symbols) > 0L) {
      stop(sprintf("non-executable fixture gaps remain exposed for %s", runner))
    }
  }
  public_names <- c(
    unique(unlist(fixture_function_map(), use.names = FALSE)),
    "fixture_lifecycle_reset", "fixture_lifecycle_counts"
  )
  functions <- lapply(public_names, function(name) {
    if (exists(name, envir = namespace, mode = "function", inherits = FALSE)) {
      get(name, envir = namespace, mode = "function", inherits = FALSE)
    } else {
      NULL
    }
  })
  names(functions) <- public_names

  reference <- fixture_r_functions(root_dir)
  control <- fixture_c_context(root_dir)
  on.exit(control$close(), add = TRUE)
  lifecycle <- fixture_lifecycle_api(functions)
  proof <- list(
    values = fixture_run_value_matrix(
      runner, functions, supported, reference$functions, control$functions,
      control$diagnostics, reference$optimized
    ),
    output = fixture_run_output_proof(runner, functions, supported, reference$functions),
    altrep = fixture_run_altrep_probe(runner, functions, supported, control$diagnostics),
    recovery = fixture_run_error_recovery(runner, functions, supported, lifecycle),
    state = fixture_run_state_lifecycle(
      runner, functions, supported, control$diagnostics, lifecycle
    )
  )
  fixture_validate_proof(proof, runner, supported)
  invisible(proof)
}

run_fixture_r_gate <- function(root_dir, evidence) {
  reference <- fixture_r_functions(root_dir)
  supported <- evidence$fixture_rows$fixture[
    evidence$fixture_rows$runner == "r" & evidence$fixture_rows$executable
  ]
  functions <- reference$functions
  control <- fixture_c_context(root_dir)
  on.exit(control$close(), add = TRUE)
  proof <- list(
    values = fixture_run_value_matrix(
      "r", functions, supported, reference$functions, control$functions,
      control$diagnostics, reference$optimized
    ),
    output = fixture_run_output_proof("r", functions, supported, reference$functions),
    altrep = fixture_run_altrep_probe("r", functions, supported, control$diagnostics),
    recovery = fixture_run_error_recovery("r", functions, supported, lifecycle = NULL),
    state = c(constructor = 0L, method = 0L, finalizer = 0L)
  )
  optimized <- build_fixture_optimized_r_provenance()
  stopifnot(
    all(vapply(optimized, function(record) {
      identical(record$implementation_class, "optimized_base_r") &&
        !identical(record$compiled_backend, "none")
    }, logical(1))),
    identical(reference$optimized$F03(c(1.5, -2.0, NA_real_)), c(3, -4, NA_real_)),
    identical(reference$optimized$F04(1:100000), 5000050000)
  )
  fixture_validate_proof(proof, "r", supported)
  invisible(proof)
}

run_fixture_c_gate <- function(root_dir, evidence) {
  path <- file.path(root_dir, "src", "c_call", paste0("bench", .Platform$dynlib.ext))
  if (!file.exists(path)) stop("registered C fixture library is missing")
  dll <- dyn.load(path)
  dyn.unload(dll[["path"]])
  control <- fixture_c_context(root_dir)
  dll <- control$dll
  on.exit(control$close(), add = TRUE)
  if (isTRUE(dll[["dynamicLookup"]]) || !isTRUE(dll[["forceSymbols"]])) {
    stop("registered C fixture does not enforce disabled lookup and forced symbols")
  }
  symbols <- c(
    unique(unlist(fixture_c_symbol_map(), use.names = FALSE)),
    "c_benchmark_lifecycle_reset", "c_benchmark_lifecycle_snapshot",
    "c_benchmark_same_sexp", "c_benchmark_same_data_pointer", "c_benchmark_wrong_pointer",
    "c_benchmark_cleared_pointer_like",
    "c_benchmark_altrep_new", "c_benchmark_altrep_reset", "c_benchmark_altrep_snapshot"
  )
  fixture_registered_surface(dll, symbols)
  functions <- control$functions
  supported <- evidence$fixture_rows$fixture[
    evidence$fixture_rows$runner == "c_call" & evidence$fixture_rows$executable
  ]
  reference <- fixture_r_functions(root_dir)
  lifecycle <- fixture_lifecycle_api(functions, control$diagnostics)
  proof <- list(
    values = fixture_run_value_matrix(
      "c_call", functions, supported, reference$functions, functions,
      control$diagnostics, reference$optimized
    ),
    output = fixture_run_output_proof(
      "c_call", functions, supported, reference$functions
    ),
    altrep = fixture_run_altrep_probe(
      "c_call", functions, supported, control$diagnostics
    ),
    recovery = fixture_run_error_recovery(
      "c_call", functions, supported, lifecycle
    ),
    state = fixture_run_state_lifecycle(
      "c_call", functions, supported, control$diagnostics, lifecycle
    )
  )
  fixture_validate_proof(proof, "c_call", supported)
  invisible(proof)
}

run_live_product_fixture_gate <- function(root_dir, evidence, runner) {
  if (length(runner) != 1L || is.na(runner)) stop("live fixture gate requires one runner")
  proof <- if (runner %in% c("zigr", "rcpp", "cpp11", "extendr", "savvy")) {
    run_fixture_package_gate(root_dir, runner, evidence)
  } else if (identical(runner, "r")) {
    run_fixture_r_gate(root_dir, evidence)
  } else if (identical(runner, "c_call")) {
    run_fixture_c_gate(root_dir, evidence)
  } else {
    stop(sprintf("no live fixture gate for runner %s", runner))
  }
  invisible(proof)
}

revision_native_call <- function(dll, symbol, arguments = list()) {
  address <- getNativeSymbolInfo(symbol, dll)$address
  do.call(.Call, c(list(address), arguments))
}

revision_assert_same <- function(expected, actual, task_id, tolerance = FALSE, path = "result") {
  if (!identical(typeof(expected), typeof(actual)) ||
      !identical(length(expected), length(actual)) ||
      !identical(attributes(expected), attributes(actual))) {
    stop(sprintf("%s %s type, length, or attributes differ", task_id, path))
  }
  if (is.list(expected)) {
    for (index in seq_along(expected)) {
      revision_assert_same(
        expected[[index]], actual[[index]], task_id, tolerance,
        sprintf("%s[[%d]]", path, index)
      )
    }
    return(invisible(actual))
  }
  if (!tolerance) {
    if (!identical(expected, actual)) stop(sprintf("%s %s values differ", task_id, path))
    return(invisible(actual))
  }
  expected_na <- is.na(expected) & !is.nan(expected)
  actual_na <- is.na(actual) & !is.nan(actual)
  expected_nan <- is.nan(expected)
  actual_nan <- is.nan(actual)
  expected_infinite <- is.infinite(expected)
  actual_infinite <- is.infinite(actual)
  if (!identical(expected_na, actual_na) || !identical(expected_nan, actual_nan) ||
      !identical(expected_infinite, actual_infinite) ||
      !identical(expected[expected_infinite], actual[actual_infinite])) {
    stop(sprintf("%s %s special-value positions differ", task_id, path))
  }
  finite <- is.finite(expected)
  if (any(finite)) {
    scale <- pmax(1, abs(expected[finite]))
    if (any(abs(expected[finite] - actual[finite]) / scale > 1e-12)) {
      stop(sprintf("%s %s finite values exceed relative tolerance", task_id, path))
    }
  }
  invisible(actual)
}

run_benchmark_revision_gate <- function(
    root_dir, runner, task_ids = NULL, master_seed = benchmark_master_seed()) {
  if (!runner %in% c("r", "c_call", "zigr", "rcpp", "cpp11", "extendr", "savvy")) {
    stop(sprintf("unknown revision runner: %s", runner))
  }
  source(file.path(root_dir, "src", "r", "run_all.R"), local = .GlobalEnv)
  c_dll <- dyn.load(file.path(root_dir, "src", "c_call", "bench.so"), local = TRUE, now = TRUE)
  on.exit(try(dyn.unload(c_dll[["path"]]), silent = TRUE), add = TRUE)

  runner_environment <- .GlobalEnv
  if (!runner %in% c("r", "c_call")) {
    runner_environment <- direct_runner_environment(root_dir, runner)
  }

  same_sexp <- function(x, y) {
    isTRUE(revision_native_call(c_dll, "c_benchmark_same_sexp", list(x, y)))
  }
  is_altrep <- function(x) {
    isTRUE(revision_native_call(c_dll, "c_revision_is_altrep", list(x)))
  }
  is_unmaterialized_altrep <- function(x) {
    isTRUE(revision_native_call(c_dll, "c_revision_altrep_unmaterialized", list(x)))
  }
  suitability <- validate_direct_task_suitability()
  allocating <- suitability$task[suitability$large_output | suitability$small_output]
  altrep_tasks <- suitability$task[suitability$representation_changing]
  master_seed <- input_scalar_integer(master_seed, "revision correctness master seed")
  rng_seed <- task_input_seed(master_seed, "rng", "direct-timing-v1")

  specs <- benchmark_revision_task_specs()
  if (!is.null(task_ids)) {
    available <- vapply(specs, `[[`, character(1), "id")
    if (any(!nzchar(task_ids)) || anyDuplicated(task_ids) || !all(task_ids %in% available)) {
      stop("revision correctness selection contains an unknown or duplicate task")
    }
    specs <- specs[match(task_ids, available)]
  }

  for (spec in specs) {
    runner_invoke <- function(arguments) {
      if (identical(runner, "r")) {
        return(do.call(get(spec$function_name, envir = .GlobalEnv), arguments))
      }
      if (identical(runner, "c_call")) {
        return(revision_native_call(c_dll, paste0("c_revision_", spec$id), arguments))
      }
      do.call(get(spec$function_name, envir = runner_environment), arguments)
    }
    r_arguments <- benchmark_revision_arguments(spec, master_seed)
    r_input_fingerprint <- if (length(r_arguments) && !spec$id %in% altrep_tasks) {
      task_arguments_fingerprint(spec$id, r_arguments, "ordinary_r_object")
    } else NULL
    if (spec$id %in% altrep_tasks && !is_unmaterialized_altrep(r_arguments[[1L]])) {
      stop(sprintf("R truth did not start with unmaterialized compact ALTREP input for %s", spec$id))
    }

    if (isTRUE(spec$rng)) {
      set.seed(rng_seed, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
    }
    r_result <- do.call(get(spec$function_name, envir = .GlobalEnv), r_arguments)
    if (!is.null(r_input_fingerprint)) {
      assert_immutable_input(spec$id, r_arguments, r_input_fingerprint, "ordinary_r_object")
    }
    r_rng <- if (isTRUE(spec$rng)) rng_state_snapshot() else NULL
    rm(r_arguments)

    c_arguments <- benchmark_revision_arguments(spec, master_seed)
    c_input_fingerprint <- if (length(c_arguments) && !spec$id %in% altrep_tasks) {
      task_arguments_fingerprint(spec$id, c_arguments, "ordinary_r_object")
    } else NULL
    if (spec$id %in% altrep_tasks && !is_unmaterialized_altrep(c_arguments[[1L]])) {
      stop(sprintf("C truth did not start with unmaterialized compact ALTREP input for %s", spec$id))
    }
    if (isTRUE(spec$rng)) {
      set.seed(rng_seed, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
    }
    c_result <- revision_native_call(c_dll, paste0("c_revision_", spec$id), c_arguments)
    if (!is.null(c_input_fingerprint)) {
      assert_immutable_input(spec$id, c_arguments, c_input_fingerprint, "ordinary_r_object")
    }
    c_rng <- if (isTRUE(spec$rng)) rng_state_snapshot() else NULL
    revision_assert_same(r_result, c_result, paste0(spec$id, " R/C"), isTRUE(spec$tolerance))
    if (isTRUE(spec$rng)) assert_rng_state_equivalent(r_rng, c_rng, spec$id)
    rm(c_arguments)

    runner_arguments <- benchmark_revision_arguments(spec, master_seed)
    runner_input_fingerprint <- if (length(runner_arguments) && !spec$id %in% altrep_tasks) {
      task_arguments_fingerprint(spec$id, runner_arguments, "ordinary_r_object")
    } else NULL
    if (spec$id %in% altrep_tasks && !is_unmaterialized_altrep(runner_arguments[[1L]])) {
      stop(sprintf("%s did not start with unmaterialized compact ALTREP input for %s", runner, spec$id))
    }
    if (isTRUE(spec$rng)) {
      set.seed(rng_seed, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
    }
    runner_result <- runner_invoke(runner_arguments)
    if (!is.null(runner_input_fingerprint)) {
      assert_immutable_input(spec$id, runner_arguments, runner_input_fingerprint, "ordinary_r_object")
    }
    runner_rng <- if (isTRUE(spec$rng)) rng_state_snapshot() else NULL
    revision_assert_same(r_result, runner_result, paste0(spec$id, " R/", runner), isTRUE(spec$tolerance))
    revision_assert_same(c_result, runner_result, paste0(spec$id, " C/", runner), isTRUE(spec$tolerance))
    if (isTRUE(spec$rng)) {
      assert_rng_state_equivalent(r_rng, runner_rng, spec$id)
      assert_rng_state_equivalent(c_rng, runner_rng, spec$id)
    }

    if (identical(direct_task_batchability(spec$id), "repeat")) {
      runner_repeat_result <- runner_invoke(runner_arguments)
      if (!is.null(runner_input_fingerprint)) {
        assert_immutable_input(spec$id, runner_arguments, runner_input_fingerprint, "ordinary_r_object")
      }
      revision_assert_same(
        runner_result, runner_repeat_result, paste0(spec$id, " repeated ", runner),
        isTRUE(spec$tolerance)
      )
      if (spec$id %in% allocating && same_sexp(runner_result, runner_repeat_result)) {
        stop(sprintf("%s/%s reused its prior allocating result", runner, spec$id))
      }
      rm(runner_repeat_result)
    }

    if (spec$id %in% allocating && length(runner_arguments) > 0L &&
        same_sexp(runner_arguments[[1L]], runner_result)) {
      stop(sprintf("%s/%s returned its input instead of a fresh result", runner, spec$id))
    }
    if (identical(spec$id, "altrep_materialize") && is_altrep(runner_result)) {
      stop(sprintf("%s/altrep_materialize returned an ALTREP result", runner))
    }
    if (identical(spec$id, "external_state")) {
      second <- if (identical(runner, "r")) {
        get(spec$function_name, envir = .GlobalEnv)()
      } else if (identical(runner, "c_call")) {
        revision_native_call(c_dll, "c_revision_external_state")
      } else {
        get(spec$function_name, envir = runner_environment)()
      }
      if (!identical(second, 700L)) stop(sprintf("%s external state did not reset", runner))
    }
    if (identical(spec$id, "outputs")) {
      second <- if (identical(runner, "r")) {
        get(spec$function_name, envir = .GlobalEnv)()
      } else if (identical(runner, "c_call")) {
        revision_native_call(c_dll, "c_revision_outputs")
      } else {
        get(spec$function_name, envir = runner_environment)()
      }
      fixture_assert_fresh_tree(
        runner_result, second, list(same_sexp = same_sexp),
        sprintf("%s/outputs", runner)
      )
    }

    rm(runner_arguments, r_result, c_result, runner_result)
    gc(FALSE)
  }
  invisible(length(specs))
}
