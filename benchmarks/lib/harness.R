library(microbenchmark)

peak_rss_kb <- function() {
  if (.Platform$OS.type != "unix") return(NA_integer_)
  tryCatch({
    lines <- readLines("/proc/self/status")
    line  <- grep("^VmPeak:", lines, value = TRUE)
    if (length(line) == 0) return(NA_integer_)
    as.integer(sub(".*?([0-9]+).*", "\\1", line[1]))
  }, error = function(e) NA_integer_)
}

runner_artifact_metrics <- function(cfg, root_dir) {
  artifact_path <- if (identical(cfg$call_type %||% ".Call", "r")) {
    file.path(root_dir, "src/r/run_all.R")
  } else {
    file.path(root_dir, cfg$so_path)
  }

  info <- file.info(artifact_path)
  if (nrow(info) == 0L || is.na(info$size[[1]])) {
    stop(sprintf("artifact not found: %s", artifact_path))
  }

  n_exports <- length(cfg$exports %||% list())
  bytes <- as.numeric(info$size[[1]])
  setNames(
    c(bytes, as.numeric(n_exports), bytes / max(n_exports, 1L)),
    c("bytes", "exports", "bytes_per_export")
  )
}

make_call_expr <- function(cfun, args, call_type) {
  if (call_type == "r") {
    as.call(c(list(as.name(cfun)), args))
  } else if (call_type == ".C") {
    as.call(c(list(quote(.C), cfun), args))
  } else {
    as.call(c(list(quote(.Call), cfun), args))
  }
}

timed_call <- function(cfun, args, call_type = ".Call", expr = NULL) {
  gc()
  rss_before <- peak_rss_kb()
  expr <- expr %||% make_call_expr(cfun, args, call_type)
  error <- NA_character_
  wall_start <- get_nanotime()
  tryCatch(eval(expr), error = function(e) { error <<- conditionMessage(e) })
  wall_end <- get_nanotime()
  rss_after <- peak_rss_kb()
  list(
    wall_ms     = (wall_end - wall_start) / 1e6,
    peak_rss_kb = max(rss_before, rss_after, na.rm = TRUE),
    error       = error
  )
}

get_nanotime <- function() {
  if (exists("get_nanotime", where = "package:microbenchmark", mode = "function")) {
    microbenchmark::get_nanotime()
  } else {
    as.numeric(Sys.time()) * 1e9
  }
}

benchmark_call <- function(cfun, args, call_type, warmup = 10L, times = 100L, expr = NULL) {
  expr <- expr %||% make_call_expr(cfun, args, call_type)
  peak_rss_val <- NA_integer_

  for (i in seq_len(warmup)) {
    r <- tryCatch(eval(expr), error = function(e) NULL)
    if (is.null(r)) return(list(error = "warmup failed"))
  }

  mb <- tryCatch(
    microbenchmark(eval(expr), times = times, unit = "ms"),
    error = function(e) return(list(error = conditionMessage(e)))
  )
  if (is.list(mb) && !is.null(mb$error)) return(mb)

  times_vec <- mb$time / 1e6
  n <- length(times_vec)
  mean_ms <- mean(times_vec)
  sd_ms <- sd(times_vec)
  cv_pct <- if (mean_ms > 0) sd_ms / mean_ms * 100 else 0

  peak_rss_val <- peak_rss_kb()

  list(
    times      = times_vec,
    converged  = TRUE,
    n_runs     = n,
    mean_ms    = mean_ms,
    sd_ms      = sd_ms,
    cv_pct     = cv_pct,
    cv_gap     = 0,
    peak_rss   = peak_rss_val,
    error      = NA_character_
  )
}

write_csv <- function(df, path, append = FALSE) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  write.table(df, path, sep = ",", row.names = FALSE, quote = FALSE, na = "",
              append = append, col.names = !append)
}

log_cold_start <- function(runner, task, wall_ms, dir = "results") {
  path <- file.path(dir, runner, "cold_start.csv")
  header <- !file.exists(path)
  df <- data.frame(runner = runner, task = task, wall_ms = round(wall_ms, 3),
                   stringsAsFactors = FALSE)
  write_csv(df, path, append = !header)
}

log_error <- function(runner, task, msg, dir = "results") {
  path <- file.path(dir, runner, "errors.csv")
  header <- !file.exists(path)
  df <- data.frame(runner = runner, task = task, error = msg,
                   stringsAsFactors = FALSE)
  write_csv(df, path, append = !header)
}
