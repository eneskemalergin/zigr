#!/usr/bin/env Rscript
# Runs Layer 7 system diagnostic tasks (44-47).
# These are zigr-only, not comparative.
# Usage:
#   Rscript run_system_tasks.R          # all tasks
#   Rscript run_system_tasks.R --tasks=44,45  # subset

args <- commandArgs(trailingOnly = TRUE)
task_filter <- NULL
for (a in args) {
  if (grepl("^--tasks=", a)) task_filter <- as.integer(strsplit(sub("^--tasks=", "", a), ",")[[1]])
}

root_dir <- normalizePath(".")
zig_bin <- Sys.which("zig")
if (zig_bin == "") {
  zig_bin <- file.path(root_dir, "..", "zig-0.16.0", "zig")
  if (!file.exists(zig_bin)) stop("zig not found on PATH or at ", zig_bin)
}
build_dir <- root_dir
r_include <- Sys.getenv("R_INCLUDE", "/usr/share/R/include")
r_lib <- Sys.getenv("R_LIB", "/usr/lib/R/lib")

run_cmd <- function(cmd, desc = "") {
  t0 <- proc.time()
  code <- system(cmd, ignore.stdout = FALSE, ignore.stderr = FALSE)
  elapsed <- (proc.time() - t0)["elapsed"]
  if (code != 0) cat(sprintf("  [WARN] exit code %d: %s\n", code, desc))
  elapsed
}

build_flags <- sprintf('-Dr-include="%s" -Dr-lib="%s" -Doptimize=ReleaseFast', r_include, r_lib)

results <- list()

# ── Task 44: build_time ──
t44 <- function() {
  cat("Task 44: build_time\n")

  cat("  Cold build (clean + build)...\n")
  unlink("zig-out", recursive = TRUE)
  unlink(".zig-cache", recursive = TRUE)
  cold <- run_cmd(sprintf('"%s" build %s', zig_bin, build_flags), "cold build")

  cat("  Warm build (no changes)...\n")
  warm <- run_cmd(sprintf('"%s" build %s', zig_bin, build_flags), "warm build")

  cat("  Incremental build (modify source + rebuild)...\n")
  incr_file <- "src/zig/task_01_vectorsum.zig"
  con <- file(incr_file, open = "a")
  cat(" ", file = con)
  close(con)
  incr <- run_cmd(sprintf('"%s" build %s', zig_bin, build_flags), "incremental build")
  # Restore file (remove the trailing space we added)
  restore_cmd <- sprintf('sed -i "s/ *$//" "%s"', incr_file)
  system(restore_cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)

  list(cold_s = cold, warm_s = warm, incremental_s = incr)
}

# ── Task 45: binary_size ──
t45 <- function() {
  cat("Task 45: binary_size\n")

  # Native (x86_64-linux)
  cat("  Building x86_64-linux...\n")
  unlink("zig-out", recursive = TRUE)
  run_cmd(sprintf('"%s" build %s', zig_bin, build_flags), "native build")
  native_so <- "zig-out/lib/libzigr_benchmarks.so"
  native_size <- if (file.exists(native_so)) file.info(native_so)$size else NA

  # Cross: aarch64-linux (compile only, link may fail without R lib for target)
  cat("  Building aarch64-linux...\n")
  unlink("zig-out", recursive = TRUE)
  aarch64_time <- run_cmd(
    sprintf('"%s" build %s -Dtarget=aarch64-linux', zig_bin, build_flags),
    "aarch64-linux build"
  )
  aarch64_so <- "zig-out/lib/libzigr_benchmarks.so"
  aarch64_size <- if (file.exists(aarch64_so)) file.info(aarch64_so)$size else NA

  # Cross: x86_64-windows (compile only, link likely fails)
  cat("  Building x86_64-windows...\n")
  unlink("zig-out", recursive = TRUE)
  win_time <- run_cmd(
    sprintf('"%s" build %s -Dtarget=x86_64-windows', zig_bin, build_flags),
    "x86_64-windows build"
  )
  win_so <- "zig-out/lib/libzigr_benchmarks.dll"
  win_size <- if (file.exists(win_so)) file.info(win_so)$size else NA

  list(
    x86_64_linux_bytes = native_size,
    aarch64_linux_bytes = aarch64_size,
    x86_64_windows_bytes = win_size
  )
}

# ── Task 46: cross_compile_time ──
t46 <- function() {
  cat("Task 46: cross_compile_time\n")

  targets <- c("x86_64-linux", "aarch64-linux", "x86_64-windows", "aarch64-macos")
  times <- list()
  for (tgt in targets) {
    cat(sprintf("  Building %s...\n", tgt))
    unlink("zig-out", recursive = TRUE)
    elapsed <- run_cmd(
      sprintf('"%s" build %s -Dtarget=%s', zig_bin, build_flags, tgt),
      sprintf("cross-build %s", tgt)
    )
    times[[tgt]] <- elapsed
  }
  times
}

# ── Task 47: mem_alloc_count ──
t47 <- function() {
  cat("Task 47: mem_alloc_count\n")

  src <- "src/system/task47_mem_alloc_count.zig"
  bin <- "task47_alloc_count"

  if (!file.exists(src)) {
    cat("  [SKIP] source not found:", src, "\n")
    return(NULL)
  }

  # Build standalone Zig executable
  cat("  Building counting allocator binary...\n")
  build_cmd <- sprintf('"%s" build-exe "%s" --name %s --cache-dir .zig-cache -lc', zig_bin, src, bin)
  t0 <- proc.time()
  code <- system(build_cmd, ignore.stdout = FALSE, ignore.stderr = FALSE)
  build_elapsed <- (proc.time() - t0)["elapsed"]
  if (code != 0) {
    cat("  [FAIL] counting allocator build failed\n")
    return(NULL)
  }

  # Run the binary
  cat("  Running vectorsum + matrix_mult with counting allocator...\n")
  bin_path <- if (.Platform$OS.type == "windows") paste0(bin, ".exe") else bin
  output <- system2(file.path(".", bin_path), stdout = TRUE, stderr = TRUE)
  cat(paste("  ", output, collapse = "\n"), "\n")

  # Parse output
  parse_val <- function(lines, key) {
    for (l in lines) {
      if (grepl(paste0("^  ", key, ":"), l)) {
        val <- sub(paste0("^  ", key, ": "), "", l)
        if (grepl("^[0-9]+$", val)) return(as.numeric(val))
        if (val == "true") return(TRUE)
        if (val == "false") return(FALSE)
        return(val)
      }
    }
    NA
  }

  vec_alloc <- parse_val(output, "c_alloc_count")
  vec_free <- parse_val(output, "c_free_count")
  vec_bytes <- parse_val(output, "c_bytes_allocated")
  vec_resident <- parse_val(output, "c_bytes_resident")
  correct <- parse_val(output, "correct")

  mx_alloc <- NA
  mx_free <- NA
  mx_bytes <- NA
  mx_resident <- NA

  # Find matrix mult section
  mx_start <- grep("task47_matrix_mult:", output)
  if (length(mx_start) > 0) {
    mx_lines <- output[mx_start[1]:length(output)]
    mx_alloc <- parse_val(mx_lines, "c_alloc_count")
    mx_free <- parse_val(mx_lines, "c_free_count")
    mx_bytes <- parse_val(mx_lines, "c_bytes_allocated")
    mx_resident <- parse_val(mx_lines, "c_bytes_resident")
  }

  list(
    build_ms = round(build_elapsed * 1000),
    vectorsum_correct = correct,
    vectorsum_alloc_count = vec_alloc,
    vectorsum_free_count = vec_free,
    vectorsum_bytes = vec_bytes,
    vectorsum_bytes_resident = vec_resident,
    matrix_mult_alloc_count = mx_alloc,
    matrix_mult_free_count = mx_free,
    matrix_mult_bytes = mx_bytes,
    matrix_mult_bytes_resident = mx_resident
  )
}

if (is.null(task_filter) || 44 %in% task_filter) results[["44_build_time"]] <- t44()
if (is.null(task_filter) || 45 %in% task_filter) results[["45_binary_size"]] <- t45()
if (is.null(task_filter) || 46 %in% task_filter) results[["46_cross_compile_time"]] <- t46()
if (is.null(task_filter) || 47 %in% task_filter) results[["47_mem_alloc_count"]] <- t47()

# Print summary
cat("\n=== System Diagnostics Summary ===\n\n")

for (tid in names(results)) {
  r <- results[[tid]]
  if (is.null(r)) next
  cat(tid, ":\n", sep = "")
  for (k in names(r)) {
    v <- r[[k]]
    if (is.numeric(v)) {
      if (grepl("bytes|resident|_count|ms$", k)) {
        cat(sprintf("  %s: %s\n", k, format(v, scientific = FALSE)))
      } else if (grepl("s$", k) && !grepl("_s$", k) && k != "correct") {
        cat(sprintf("  %s: %.3f\n", k, v))
      } else if (k == "correct") {
        cat(sprintf("  %s: %s\n", k, if (isTRUE(v)) "PASS" else "FAIL"))
      } else {
        cat(sprintf("  %s: %s\n", k, format(v, scientific = FALSE)))
      }
    } else {
      if (is.na(v)) {
        cat(sprintf("  %s: N/A\n", k))
      } else {
        cat(sprintf("  %s: %s\n", k, v))
      }
    }
  }
  cat("\n")
}
