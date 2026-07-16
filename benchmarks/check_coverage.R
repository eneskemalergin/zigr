#!/usr/bin/env Rscript

root_dir <- normalizePath(".")
source(file.path(root_dir, "lib", "specification.R"))
source(file.path(root_dir, "lib", "measurement.R"))

arguments <- commandArgs(trailingOnly = TRUE)
validate_cli_arguments(arguments, value_options = "tasks", flag_options = "quick", label = "coverage")

task_option <- grep("^--tasks=", arguments, value = TRUE)
selected_tasks <- if (length(task_option) == 0L) {
  vapply(benchmark_revision_task_specs(), `[[`, character(1), "id")
} else {
  strsplit(sub("^--tasks=", "", task_option[[1L]]), ",", fixed = TRUE)[[1L]]
}
available_tasks <- vapply(benchmark_revision_task_specs(), `[[`, character(1), "id")
if (any(!nzchar(selected_tasks)) || anyDuplicated(selected_tasks) ||
    !all(selected_tasks %in% available_tasks)) {
  stop("coverage selection contains an unknown or duplicate retained task")
}

expected_runners <- c("r", "c_call", "zigr", "rcpp", "cpp11", "extendr", "savvy")
if (!setequal(names(load_runner_configs(root_dir)), expected_runners)) {
  stop("runner registry does not cover the seven direct runners")
}

run_gate <- function(script) {
  status <- system2("Rscript", script, stdout = "", stderr = "")
  if (!identical(status, 0L)) stop(sprintf("coverage gate failed: %s", script))
}
run_gate(file.path("tests", "test_specification.R"))
if (!("--quick" %in% arguments)) run_gate(file.path("tests", "test_product_fixtures.R"))

cat(sprintf(
  "Direct coverage passed for %d retained tasks across %d runners%s.\n",
  length(selected_tasks), length(expected_runners),
  if ("--quick" %in% arguments) " (quick)" else ""
))
