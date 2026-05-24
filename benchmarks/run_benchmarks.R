#!/usr/bin/env Rscript
# zigR Benchmark Harness — runner-agnostic.
# Discovers runners from runners/*.json, spawns a fresh R subprocess per runner.
# Usage:
#   Rscript run_benchmarks.R                          # all runners
#   Rscript run_benchmarks.R --runners=zigr,c_call    # subset
#   Rscript run_benchmarks.R --tasks=1,2,6            # subset of tasks
#   Rscript run_benchmarks.R --build                  # rebuild all runners first

library(jsonlite)

args <- commandArgs(trailingOnly = TRUE)
runners_filter <- NULL
tasks_filter   <- NULL
do_build       <- FALSE
for (a in args) {
  if (grepl("^--runners=", a)) runners_filter <- strsplit(sub("^--runners=", "", a), ",")[[1]]
  if (grepl("^--tasks=", a))  tasks_filter  <- as.integer(strsplit(sub("^--tasks=", "", a), ",")[[1]])
  if (a == "--build")         do_build      <- TRUE
}

cat("╔══════════════════════════════════════╗\n")
cat("║     zigR Benchmark Harness v0.0.7    ║\n")
cat("╚══════════════════════════════════════╝\n\n")

# Discover runners from JSON configs
runner_files <- Sys.glob("runners/*.json")
if (length(runner_files) == 0) stop("no runner configs found in runners/")

all_runners <- list()
for (f in runner_files) {
  cfg <- fromJSON(f, simplifyVector = FALSE)
  if (!is.null(cfg$status) && cfg$status == "broken") next
  all_runners[[cfg$name]] <- cfg
}

if (!is.null(runners_filter)) {
  all_runners <- all_runners[intersect(names(all_runners), runners_filter)]
}

cat(sprintf("Runners: %s\n\n", paste(names(all_runners), collapse = ", ")))

# Build if requested
if (do_build) {
  cat("── Build phase ──\n")
  code <- system("bash build_all.sh", ignore.stdout = FALSE, ignore.stderr = FALSE)
  if (code != 0) stop(sprintf("build phase failed with exit code %d", code))
  cat("\n")
}

# Run each runner in a fresh subprocess
for (rn in names(all_runners)) {
  cfg <- all_runners[[rn]]
  cat(sprintf("── Runner: %s (%s) ──\n", rn, cfg$label))

  tf <- if (is.null(tasks_filter)) "" else sprintf("--tasks=%s", paste(tasks_filter, collapse = ","))
  cmd <- sprintf("Rscript runner_subprocess.R --runner=%s %s", shQuote(rn), tf)
  code <- system(cmd, ignore.stdout = FALSE, ignore.stderr = FALSE)
  if (code != 0) {
    cat(sprintf("  [SUB] exited with code %d\n", code))
  }
  cat("\n")
}

if (is.null(runners_filter) && is.null(tasks_filter)) {
  cat("── Comparative metrics ──\n")
  code <- system("Rscript export_comparative_metrics.R", ignore.stdout = FALSE, ignore.stderr = FALSE)
  if (code != 0) stop(sprintf("comparative metrics export failed with exit code %d", code))
  cat("\n")
} else {
  cat("── Comparative metrics ──\n")
  cat("Skipping comparative export for filtered benchmark runs.\n\n")
}

cat("Done.\n")
