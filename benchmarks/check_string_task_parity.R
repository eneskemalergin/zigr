#!/usr/bin/env Rscript

source("src/r/run_all.R")
dyn.load("zig-out/lib/zigr_benchmarks.so")

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

check("task18-nchar",
  .Call("zigr_bench_string_nchar", c("abc", "", "xyz")),
  r_bench_string_nchar(c("abc", "", "xyz")))

check("task18-nchar-na",
  .Call("zigr_bench_string_nchar", c("abc", NA_character_, "")),
  r_bench_string_nchar(c("abc", NA_character_, "")))
