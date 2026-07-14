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
  lapply(fixture_function_map(), function(functions) paste0("c_p4_", functions))
}

fixture_package_map <- function(root_dir) {
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
    "generated package symbols use the P1-approved explicit R.SEXP fixed-schema adapter"
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
  record$typed_public_signature <- record$annotation_present &&
    (if (module_cell) module_signatures_typed else {
      fixture_signatures_are_typed(ordinary_signatures, "\\bSEXP\\b")
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
    record$annotation_present, record$typed_public_signature, record$generated_native_wrapper,
    record$generated_r_wrapper, record$registered_entry, record$configured_symbol_present,
    record$dynamic_lookup_disabled
  ))
  record$source_class <- if (record$product_eligible) "generated_typed" else "generated_path_invalid"
  record$source_paths <- as.list(c(
    "src/cpp/fixture/src/fixture.cpp", "src/cpp/fixture/src/RcppExports.cpp",
    "src/cpp/fixture/R/RcppExports.R", "src/cpp/fixture/R/fixture.R"
  ))
  record$reason <- if (record$product_eligible) {
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

fixture_expected_outputs <- function() {
  list(
    numeric = c(1.5, NA_real_),
    string = "fixture",
    raw = as.raw(c(1, 2, 3)),
    complex = c(1 + 2i, NA_complex_),
    logical = c(FALSE, TRUE, NA),
    list = list(value = 7L)
  )
}

fixture_public_contract <- function(functions, supported) {
  input_strings <- c(enc2utf8("façade"), iconv("façade", from = "UTF-8", to = "latin1"), "bytes", NA_character_)
  Encoding(input_strings[[1L]]) <- "UTF-8"
  Encoding(input_strings[[2L]]) <- "latin1"
  Encoding(input_strings[[3L]]) <- "bytes"
  scalar <- function(name, ...) functions[[name]](...)
  if ("F01" %in% supported) stopifnot(identical(scalar("fixture_zero"), 1L))
  if ("F02" %in% supported) stopifnot(identical(scalar("fixture_scalar", 2.5), 2.5))
  if ("F03" %in% supported) {
    input <- c(1.5, -2.0, NA_real_)
    stopifnot(identical(scalar("fixture_numeric", input), input * 2.0))
  }
  if ("F04" %in% supported) {
    input <- 1:100000
    stopifnot(identical(scalar("fixture_altrep_integer", input), 5000050000))
  }
  if ("F05" %in% supported) stopifnot(identical(scalar("fixture_strings", input_strings), 3L))
  if ("F06" %in% supported) {
    input <- as.raw(c(0, 1, 127, 255))
    stopifnot(identical(scalar("fixture_raw", input), input))
  }
  if ("F07" %in% supported) {
    input <- c(1 + 2i, NA_complex_, complex(real = NaN, imaginary = 3))
    stopifnot(identical(scalar("fixture_complex", input), input))
  }
  if ("F08" %in% supported) {
    expected <- c(false = 2L, true = 2L, missing = 2L)
    stopifnot(identical(scalar("fixture_logical_counts", c(FALSE, TRUE, NA, FALSE, NA, TRUE)), expected))
  }
  if ("F09" %in% supported) {
    input <- list(id = 1L, count = 2L, ratio = 0.5, enabled = TRUE)
    stopifnot(identical(scalar("fixture_schema", input), input))
  }
  if ("F10" %in% supported) {
    state <- scalar("fixture_new")
    stopifnot(
      identical(scalar("fixture_method", state, 3L), 3L),
      identical(scalar("fixture_read", state), 3L)
    )
  }
  if ("F11" %in% supported) {
    fixture_expect_error(scalar("fixture_scalar", 1L), "wrong scalar type")
    fixture_expect_error(scalar("fixture_scalar", c(1, 2)), "wrong scalar length")
    fixture_expect_error(scalar("fixture_error", 1), "native fixture error")
    stopifnot(identical(scalar("fixture_scalar", 2.5), 2.5))
  }
  if ("F12" %in% supported) {
    stopifnot(identical(scalar("fixture_outputs"), fixture_expected_outputs()))
  }
  invisible(TRUE)
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
    getNativeSymbolInfo("p4_fixture_symbol_that_does_not_exist", PACKAGE = dll),
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

run_fixture_package_gate <- function(root_dir, runner, evidence) {
  package <- fixture_package_map(root_dir)[[runner]]
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
  fixture_registered_surface(dll, fixture_package_symbols(runner, supported))
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
  functions <- lapply(unique(unlist(fixture_function_map(), use.names = FALSE)), function(name) {
    if (exists(name, envir = namespace, mode = "function", inherits = FALSE)) {
      get(name, envir = namespace, mode = "function", inherits = FALSE)
    } else {
      NULL
    }
  })
  names(functions) <- unique(unlist(fixture_function_map(), use.names = FALSE))
  fixture_public_contract(functions, supported)
  invisible(TRUE)
}

run_fixture_r_gate <- function(root_dir, evidence) {
  source(file.path(root_dir, "src", "r", "run_all.R"), local = .GlobalEnv)
  supported <- evidence$fixture_rows$fixture[
    evidence$fixture_rows$runner == "r" & evidence$fixture_rows$executable
  ]
  mapping <- fixture_r_function_map()
  functions <- lapply(unique(unlist(fixture_function_map(), use.names = FALSE)), function(name) NULL)
  names(functions) <- unique(unlist(fixture_function_map(), use.names = FALSE))
  for (fixture in setdiff(supported, "F11")) {
    public_name <- fixture_function_map()[[fixture]][[1L]]
    functions[[public_name]] <- get(unname(mapping[[fixture]]), mode = "function", inherits = TRUE)
  }
  if ("F11" %in% supported) {
    functions$fixture_scalar <- r_fixture_scalar
    functions$fixture_error <- r_fixture_error
  }
  fixture_public_contract(functions, supported)
  optimized <- build_fixture_optimized_r_provenance()
  stopifnot(
    all(vapply(optimized, function(record) {
      identical(record$implementation_class, "optimized_base_r") &&
        !identical(record$compiled_backend, "none")
    }, logical(1))),
    identical(r_optimized_fixture_numeric(c(1.5, -2.0, NA_real_)), c(3, -4, NA_real_)),
    identical(r_optimized_fixture_altrep_integer(1:100000), 5000050000)
  )
  invisible(TRUE)
}

run_fixture_c_gate <- function(root_dir, evidence) {
  path <- file.path(root_dir, "src", "c_call", paste0("bench", .Platform$dynlib.ext))
  if (!file.exists(path)) stop("registered C fixture library is missing")
  dll <- dyn.load(path)
  dyn.unload(dll[["path"]])
  dll <- dyn.load(path)
  on.exit({
    gc()
    dyn.unload(dll[["path"]])
  }, add = TRUE)
  if (isTRUE(dll[["dynamicLookup"]]) || !isTRUE(dll[["forceSymbols"]])) {
    stop("registered C fixture does not enforce disabled lookup and forced symbols")
  }
  symbols <- unique(unlist(fixture_c_symbol_map(), use.names = FALSE))
  fixture_registered_surface(dll, symbols)
  flat <- list()
  for (fixture in names(fixture_function_map())) {
    names <- fixture_function_map()[[fixture]]
    for (index in seq_along(names)) {
      address <- getNativeSymbolInfo(paste0("c_p4_", names[[index]]), PACKAGE = dll)$address
      flat[[names[[index]]]] <- local({
        native_address <- address
        function(...) .Call(native_address, ...)
      })
    }
  }
  supported <- evidence$fixture_rows$fixture[
    evidence$fixture_rows$runner == "c_call" & evidence$fixture_rows$executable
  ]
  fixture_public_contract(flat, supported)
  invisible(TRUE)
}

run_live_product_fixture_gate <- function(root_dir, evidence, runner) {
  if (length(runner) != 1L || is.na(runner)) stop("live fixture gate requires one runner")
  if (runner %in% c("zigr", "rcpp", "cpp11", "extendr", "savvy")) {
    run_fixture_package_gate(root_dir, runner, evidence)
  } else if (identical(runner, "r")) {
    run_fixture_r_gate(root_dir, evidence)
  } else if (identical(runner, "c_call")) {
    run_fixture_c_gate(root_dir, evidence)
  } else {
    stop(sprintf("no live fixture gate for runner %s", runner))
  }
  invisible(TRUE)
}
