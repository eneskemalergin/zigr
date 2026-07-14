#!/usr/bin/env Rscript

source("src/r/run_all.R")
dll <- dyn.load("zig-out/lib/zigr_benchmarks.so")
if (!isFALSE(dll[["dynamicLookup"]])) stop("zigr benchmark library must disable dynamic symbol lookup")

symbol <- getNativeSymbolInfo(
  "zigr_bench_string_nchar",
  PACKAGE = dll,
  withRegistrationInfo = TRUE
)$address

dynamic_lookup <- tryCatch({
  .Call("zigr_bench_string_nchar", c("abc", ""), PACKAGE = dll[["name"]])
  NULL
}, error = function(error) error)
if (is.null(dynamic_lookup)) stop("forced registration accepted character-name dynamic lookup")

check <- function(label, got, want) {
  if (!identical(got, want)) {
    cat(label, "FAIL\n")
    print(got)
    print(want)
    quit(status = 1)
  }
  cat(label, "OK\n")
}

check("task18-nchar",
  .Call(symbol, c("abc", "", "xyz")),
  r_bench_string_nchar(c("abc", "", "xyz")))

check("task18-nchar-na",
  .Call(symbol, c("abc", NA_character_, "")),
  r_bench_string_nchar(c("abc", NA_character_, "")))
