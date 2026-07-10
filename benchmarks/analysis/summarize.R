#!/usr/bin/env Rscript
#
# Aggregate one completed run into a summary table.
# Usage: Rscript analysis/summarize.R --run-dir=results/runs/<run_id>

args <- commandArgs(trailingOnly = TRUE)
run_dir <- NULL
for (arg in args) {
  if (grepl("^--run-dir=", arg)) run_dir <- sub("^--run-dir=", "", arg)
}
if (is.null(run_dir)) stop("--run-dir= is required")

root_dir <- normalizePath(".")
source(file.path(root_dir, "lib", "run_manifest.R"))
results_dir <- normalizePath(run_dir, mustWork = FALSE)
run_metadata <- read_run_manifest(results_dir)
if (!identical(as.character(run_metadata$status), "complete")) stop("run must be complete before analysis")
validate_run_artifacts(results_dir, run_metadata)
out_csv <- file.path(results_dir, "analysis_summary.csv")

# ── Collect all result CSVs ──────────────────────────────────

runner_dirs <- file.path(results_dir, run_manifest_values(run_metadata$runners))

rows <- list()

for (rd in runner_dirs) {
  runner <- basename(rd)
  csv_files <- list.files(rd, pattern = "^task_.*\\.csv$", full.names = TRUE)

  for (cf in csv_files) {
    task <- gsub("^task_|\\.csv$", "", basename(cf))
    data <- read.csv(cf, stringsAsFactors = FALSE)

    mean_ms <- mean(data$wall_ms, na.rm = TRUE)
    sd_ms   <- sd(data$wall_ms, na.rm = TRUE)
    min_ms  <- min(data$wall_ms, na.rm = TRUE)
    max_ms  <- max(data$wall_ms, na.rm = TRUE)
    median_ms <- median(data$wall_ms, na.rm = TRUE)
    rss     <- mean(data$peak_rss_kb, na.rm = TRUE)

    rows[[length(rows) + 1]] <- data.frame(
      runner     = runner,
      task       = task,
      mean_ms    = round(mean_ms, 3),
      sd_ms      = round(sd_ms, 3),
      median_ms  = round(median_ms, 3),
      min_ms     = round(min_ms, 3),
      max_ms     = round(max_ms, 3),
      mean_rss_kb = round(rss, 0),
      n_runs     = nrow(data),
      stringsAsFactors = FALSE
    )
  }
}

if (length(rows) == 0) {
  cat("No results found in ", results_dir, "\n")
  quit(save = "no", status = 0)
}

agg <- do.call(rbind, rows)

# ── Cold start data (per-runner) ─────────────────────────────

cold_all <- NULL
for (rd in runner_dirs) {
  runner <- basename(rd)
  cold_csv <- file.path(rd, "cold_start.csv")
  if (file.exists(cold_csv)) {
    cold <- read.csv(cold_csv, stringsAsFactors = FALSE)
    cold_all <- rbind(cold_all, cold)
  }
}
if (!is.null(cold_all)) {
  cold_all <- aggregate(wall_ms ~ runner + task, cold_all, mean)
  names(cold_all)[names(cold_all) == "wall_ms"] <- "cold_start_ms"
  agg <- merge(agg, cold_all, by = c("runner", "task"), all.x = TRUE)
}

# ── Write summary ─────────────────────────────────────────────

write.csv(agg, out_csv, row.names = FALSE)
cat(sprintf("Summary written to %s\n", out_csv))

# ── Print table ──────────────────────────────────────────────

cat("\n── Summary ─────────────────────────────────────\n")
cat(sprintf("%-8s %-14s %10s %8s %8s\n",
    "Runner", "Task", "Mean (ms)", "SD (ms)", "RSS (KB)"))
cat(sprintf("%-8s %-14s %10s %8s %8s\n",
    "------", "---", "--------", "------", "-------"))

for (i in seq_len(nrow(agg))) {
  cat(sprintf("%-8s %-14s %10.1f %8.2f %8.0f\n",
      agg$runner[i], agg$task[i],
      agg$mean_ms[i], agg$sd_ms[i], agg$mean_rss_kb[i]))
}
