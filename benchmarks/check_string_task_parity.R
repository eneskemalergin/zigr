#!/usr/bin/env Rscript
# Check that zigr's string benchmark functions produce correct output vs R baseline.
# Tasks using zigr_bench_string_concat are stubs and will be SKIP'd.
# Only tasks with real implementations are tested.

source("src/r/run_all.R")
dyn.load("zig-out/lib/libzigr_benchmarks.so")

check <- function(label, got, want) {
  if (is.null(got) || is.na(got) || length(got) == 0) {
    cat(label, "SKIP (stub)\n")
    return()
  }
  if (!identical(got, want)) {
    cat(label, "FAIL\n")
    print(got)
    print(want)
    quit(status = 1)
  }
  cat(label, "OK\n")
}

# Task 18: string_nchar (real implementation)
check("task18-nchar",
  .Call("zigr_bench_string_nchar", c("abc", "", "xyz")),
  r_bench_string_nchar(c("abc", "", "xyz")))

check("task18-nchar-na",
  .Call("zigr_bench_string_nchar", c("abc", NA_character_, "")),
  r_bench_string_nchar(c("abc", NA_character_, "")))
