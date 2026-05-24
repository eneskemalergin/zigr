#!/usr/bin/env Rscript

source("src/r/run_all.R")

dyn.load("zig-out/lib/libzigr_benchmarks.so")

legacy_task35_bytes <- function(x) {
  valid <- !is.na(x)

  byte_len <- function(s) length(charToRaw(enc2utf8(s)))
  byte_prefix3 <- function(s) {
    bytes <- charToRaw(enc2utf8(s))
    rawToChar(bytes[seq_len(min(length(bytes), 3L))])
  }
  ascii_upper_bytes <- function(s) {
    bytes <- charToRaw(enc2utf8(s))
    ints <- as.integer(bytes)
    lower <- ints >= 0x61 & ints <= 0x7A
    ints[lower] <- ints[lower] - 32L
    rawToChar(as.raw(ints))
  }

  setNames(
    list(
      paste0(x[valid], collapse = ","),
      as.integer(sum(vapply(x[valid], byte_len, integer(1)))),
      as.integer(sum(startsWith(x[valid], "abc"))),
      unname(vapply(x, function(s) if (is.na(s)) NA_character_ else byte_prefix3(s), character(1))),
      unname(vapply(x, function(s) if (is.na(s)) NA_character_ else ascii_upper_bytes(s), character(1)))
    ),
    c("concat", "nchar_sum", "prefix_match", "extract_substr", "to_upper")
  )
}

check <- function(label, got, want) {
  if (!identical(got, want)) {
    cat(label, "FAIL\n")
    print(got)
    print(want)
    quit(status = 1)
  }
  cat(label, "OK\n")
}

x04 <- c("abc", "", "xyz")
check(
  "task04-ascii",
  .Call("zigr_bench_strings", x04, ",", PACKAGE = "libzigr_benchmarks"),
  r_bench_strings(x04, ",")
)

x04_na <- c("abc", NA_character_, "")
check(
  "task04-na",
  .Call("zigr_bench_strings", x04_na, ",", PACKAGE = "libzigr_benchmarks"),
  r_bench_strings(x04_na, ",")
)

x21 <- c("abc", "", "xyz")
check(
  "task21-ascii",
  .Call("zigr_bench_string_nchar", x21, PACKAGE = "libzigr_benchmarks"),
  r_bench_string_nchar(x21)
)

x21_na <- c("abc", NA_character_, "")
check(
  "task21-na",
  .Call("zigr_bench_string_nchar", x21_na, PACKAGE = "libzigr_benchmarks"),
  r_bench_string_nchar(x21_na)
)

x35 <- c("abcxx", "", "xyz", NA_character_)
check(
  "task35-ascii-na",
  .Call("zigr_bench_string_variants", x35, PACKAGE = "libzigr_benchmarks"),
  r_bench_string_variants(x35)
)

x35_utf8 <- c("abcde", "naïve", "lambda", NA_character_)
check(
  "task35-utf8-fallback",
  .Call("zigr_bench_string_variants", x35_utf8, PACKAGE = "libzigr_benchmarks"),
  legacy_task35_bytes(x35_utf8)
)