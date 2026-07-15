#!/usr/bin/env Rscript

library(jsonlite)
root_dir <- normalizePath(".")
source(file.path(root_dir, "lib", "specification.R"))
manifest <- load_task_manifest(root_dir)
evidence <- load_evidence_manifest(root_dir, manifest)

args <- commandArgs(trailingOnly = TRUE)
validate_cli_arguments(args, value_options = "tasks", flag_options = "quick", label = "coverage")
task_filter <- NULL
quick <- FALSE
for (arg in args) {
  if (grepl("^--tasks=", arg)) task_filter <- parse_task_filter(sub("^--tasks=", "", arg))
  if (identical(arg, "--quick")) quick <- TRUE
}

configs <- load_runner_configs(root_dir)
runner_names <- names(configs)
active <- vapply(configs, function(cfg) is.null(cfg$status) || cfg$status != "broken", logical(1))
for (runner in runner_names[active]) {
  configs[[runner]] <- hydrate_runner_config(manifest, configs[[runner]], runner, evidence)
}

if (!quick) {
  suites <- c(
    specification = "test_specification.R",
    measurement = "test_measurement.R",
    product_fixtures = "test_product_fixtures.R"
  )
  for (suite in names(suites)) {
    status <- system2("Rscript", args = file.path("tests", suites[[suite]]))
    if (!identical(status, 0L)) stop(sprintf("%s tests failed with exit code %d", suite, status))
  }
}

manifest_task_numbers <- as.integer(sub("([0-9]+).*", "\\1", manifest$task))
selected_task_count <- if (is.null(task_filter)) nrow(manifest) else sum(manifest_task_numbers %in% task_filter)
if (selected_task_count == 0L) stop("task filter selected no manifest tasks")

runner_args <- c("benchmark_worker.R", "--kind=task", "--runner=r", "--check-only")
if (!is.null(task_filter)) runner_args <- c(runner_args, sprintf("--tasks=%s", paste(task_filter, collapse = ",")))
status <- system2("Rscript", args = runner_args)
if (!identical(status, 0L)) stop(sprintf("task-spec preflight failed with exit code %d", status))

cat(sprintf(
  "%s coverage is valid for %d active runners, %d catalog tasks (%d selected), and %d fixture cells.\n",
  if (quick) "Quick" else "Full",
  sum(active), nrow(manifest), selected_task_count, nrow(evidence$fixture_rows)
))
