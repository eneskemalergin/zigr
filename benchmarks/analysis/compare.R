#!/usr/bin/env Rscript

results_dir <- "results"
out_dir <- "analysis"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

summary_files <- Sys.glob(file.path(results_dir, "*_summary.csv"))
summary_files <- summary_files[!grepl("^fixture_", basename(summary_files))]
if (length(summary_files) == 0) stop("no summary CSVs found in ", results_dir)

runners <- list()
for (f in summary_files) {
  runner <- gsub("_summary\\.csv$", "", basename(f))
  runners[[runner]] <- read.csv(f, stringsAsFactors = FALSE)
}

all_tasks <- unique(unlist(lapply(runners, `[[`, "task")))
all_tasks <- sort(all_tasks)

task_names <- c(
  "01_fib"="fib", "02_vectorsum"="vectorsum", "03_transpose"="transpose",
  "04_strings"="strings", "05_dataframe"="dataframe", "06_na_prop"="na_prop",
  "07_parallel"="parallel", "09_protect"="protect", "10_blas_matmul"="blas_matmul",
  "11_crossprod"="crossprod", "12_cholesky"="cholesky", "13_lm"="lm",
  "14_rowsums"="rowsums", "15_elem_ops"="elem_ops", "16_rowcol_means"="rowcol_means",
  "17_broadcast"="broadcast", "18_sort"="sort", "19_cumsum"="cumsum",
  "20_rnorm"="rnorm", "21_string_nchar"="string_nchar", "22_which_na"="which_na",
  "23_altrep_sum"="altrep_sum", "24_altrep_read"="altrep_read",
  "25_altrep_create"="altrep_create", "26_comptime_dispatch"="comptime_dispatch",
  "27_struct_convert"="struct_convert", "28_na_prop_vary"="na_prop_vary",
  "29_scale_law"="scale_law", "30_arena_vs_rmalloc"="arena_vs_rmalloc",
  "31_prot_overhead"="prot_overhead", "32_longjmp_safety"="longjmp_safety",
  "33_binary_growth"="binary_growth", "34_translate_c_cost"="translate_c_cost",
  "35_string_variants"="string_variants", "36_parallel_scaling"="parallel_scaling",
  "37_memory_bandwidth"="memory_bandwidth",
  "38_owned_altrep_real_sum"="owned_altrep_real_sum",
  "39_owned_altrep_int_sum"="owned_altrep_int_sum",
  "40_owned_altrep_logical_sum"="owned_altrep_logical_sum",
  "41_owned_altrep_int_min"="owned_altrep_int_min",
  "42_owned_altrep_int_max"="owned_altrep_int_max",
  "43_owned_altrep_int_argmin"="owned_altrep_int_argmin",
  "44_owned_altrep_int_argmax"="owned_altrep_int_argmax",
  "45_owned_altrep_logical_min"="owned_altrep_logical_min",
  "46_owned_altrep_logical_max"="owned_altrep_logical_max",
  "47_owned_altrep_logical_argmin"="owned_altrep_logical_argmin",
  "48_owned_altrep_logical_argmax"="owned_altrep_logical_argmax"
)

rows <- list()
for (t in all_tasks) {
  row <- list(task = if (t %in% names(task_names)) task_names[[t]] else t)
  statuses <- c()
  for (rn in names(runners)) {
    rdata <- runners[[rn]]
    idx <- which(rdata$task == t)
    if (length(idx) == 0) {
      row[[rn]] <- NA; row[[paste0(rn,"_cv")]] <- NA; row[[paste0(rn,"_status")]] <- "N/A"
    } else {
      s <- rdata[idx[1], ]
      row[[rn]] <- s$mean_ms
      row[[paste0(rn,"_cv")]] <- s$cv_pct
      row[[paste0(rn,"_status")]] <- s$status
    }
  }
  rows[[length(rows) + 1]] <- row
}

tab <- do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors=FALSE))

write.csv(tab, file.path(out_dir, "comparison.csv"), row.names = FALSE)
cat(sprintf("Comparison written to %s\n\n", file.path(out_dir, "comparison.csv")))

rnames <- names(runners)
header <- sprintf("%-16s", "Task")
for (rn in rnames) {
  header <- paste0(header, sprintf("  %12s", rn))
  header <- paste0(header, sprintf(" %5s", "CV%"))
}
cat(header, "\n")
cat(strrep("-", nchar(header)), "\n")

for (i in seq_len(nrow(tab))) {
  line <- sprintf("%-16s", tab[i, "task"])
  for (rn in rnames) {
    m <- tab[i, rn][[1]]
    cv <- tab[i, paste0(rn,"_cv")][[1]]
    st <- tab[i, paste0(rn,"_status")][[1]]
    if (is.na(m)) {
      line <- paste0(line, sprintf("  %12s %5s", st, ""))
    } else {
      ms <- sprintf("%8.2fms", m)
      cv_str <- if (st == "PARTIAL") sprintf(" %4.1f*", cv) else sprintf(" %4.1f", cv)
      line <- paste0(line, sprintf("  %12s", ms), cv_str)
    }
  }
  cat(line, "\n")
}

cat("\n── Winners (fastest per task) ──\n")
for (i in seq_len(nrow(tab))) {
  vals <- sapply(rnames, function(rn) tab[i, rn][[1]])
  if (all(is.na(vals))) next
  best <- which.min(vals)
  task_name <- tab[i, "task"]
  cat(sprintf("  %-16s %s (%8.2fms)\n", task_name, rnames[best], vals[best]))
}
