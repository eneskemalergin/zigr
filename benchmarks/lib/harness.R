library(microbenchmark)

current_rss_kb <- function() {
  if (.Platform$OS.type != "unix") return(NA_integer_)
  tryCatch({
    lines <- readLines("/proc/self/status")
    line  <- grep("^VmRSS:", lines, value = TRUE)
    if (length(line) == 0) return(NA_integer_)
    as.integer(sub(".*?([0-9]+).*", "\\1", line[1]))
  }, error = function(e) NA_integer_)
}

validate_forced_registration <- function(dynamic_lookup, label) {
  if (!isFALSE(dynamic_lookup)) stop(sprintf("dynamic symbol lookup is enabled for %s", label))
  invisible(TRUE)
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
  } else if (call_type == ".External") {
    as.call(c(list(quote(.External), cfun), args))
  } else {
    as.call(c(list(quote(.Call), cfun), args))
  }
}

timed_call <- function(prepare_call) {
  gc(full = TRUE)
  rss_before <- current_rss_kb()
  prepared <- tryCatch(list(ok = TRUE, expression = prepare_call()), error = function(error) {
    list(ok = FALSE, error = conditionMessage(error))
  })
  if (!isTRUE(prepared$ok)) {
    return(list(wall_ms = NA_real_, peak_rss_kb = NA_integer_, error = paste("cold input preparation failed:", prepared$error)))
  }
  error <- NA_character_
  wall_start <- get_nanotime()
  tryCatch(eval(prepared$expression), error = function(e) { error <<- conditionMessage(e) })
  wall_end <- get_nanotime()
  gc(full = TRUE)
  rss_after <- current_rss_kb()
  list(
    wall_ms     = (wall_end - wall_start) / 1e6,
    peak_rss_kb = max(0, rss_after - rss_before, na.rm = TRUE),
    error       = error
  )
}

get_nanotime <- function() {
  microbenchmark::get_nanotime()
}

benchmark_call <- function(prepare_warmup, prepare_timed, warmup = 10L, block_size = 10L,
                           max_iter = 500L, cv_threshold = 1.0, convergence_blocks = 5L,
                           timer_noise_floor_ms = 0.01,
                           rss_metric = "post_gc_endpoint_delta_kb") {

  gc(full = TRUE)
  rss_before <- current_rss_kb()

  for (i in seq_len(warmup)) {
    prepared <- tryCatch(list(ok = TRUE, expression = prepare_warmup()), error = function(error) {
      list(ok = FALSE, error = conditionMessage(error))
    })
    if (!isTRUE(prepared$ok)) return(list(error = paste("warmup input preparation failed:", prepared$error)))
    call_ok <- TRUE
    t0 <- get_nanotime()
    tryCatch(eval(prepared$expression), error = function(error) {
      call_ok <<- FALSE
    })
    t1 <- get_nanotime()
    if (!call_ok) return(list(error = "warmup failed"))
  }

  all_times <- numeric()
  n_blocks <- 0L
  convergence_cv_pct <- NA_real_
  stopping_condition <- "max_iterations"
  repeat {
    block_times <- numeric(block_size)
    for (sample_index in seq_len(block_size)) {
      prepared <- tryCatch(list(ok = TRUE, expression = prepare_timed()), error = function(error) {
        list(ok = FALSE, error = conditionMessage(error))
      })
      if (!isTRUE(prepared$ok)) return(list(error = paste("timed input preparation failed:", prepared$error)))
      call_error <- NULL
      t0 <- get_nanotime()
      tryCatch(eval(prepared$expression), error = function(error) {
        call_error <<- conditionMessage(error)
      })
      t1 <- get_nanotime()
      if (!is.null(call_error)) return(list(error = call_error))
      block_times[[sample_index]] <- (t1 - t0) / 1e6
    }
    all_times <- c(all_times, block_times)
    n_blocks <- n_blocks + 1L
    n <- length(all_times)

    if (n >= block_size * convergence_blocks) {
      window <- tail(all_times, block_size * convergence_blocks)
      window_mean <- mean(window)
      convergence_cv_pct <- if (window_mean > 0) sd(window) / window_mean * 100 else 0
      if (convergence_cv_pct < cv_threshold) {
        stopping_condition <- "rolling_cv"
        break
      }
    }

    if (n >= max_iter) {
      stopping_condition <- "max_iterations"
      break
    }
  }

  n <- length(all_times)
  mean_ms <- mean(all_times)
  median_ms <- median(all_times)
  min_ms <- min(all_times)
  max_ms <- max(all_times)
  sd_ms <- sd(all_times)
  cv_pct <- if (mean_ms > 0) sd_ms / mean_ms * 100 else 0

  gc(full = TRUE)
  rss_after <- current_rss_kb()
  rss_delta <- max(0, rss_after - rss_before, na.rm = TRUE)

  list(
    times      = all_times,
    converged  = identical(stopping_condition, "rolling_cv"),
    n_runs     = n,
    n_blocks   = n_blocks,
    warmup_iterations = warmup,
    block_size = block_size,
    max_iterations = max_iter,
    convergence_window_blocks = convergence_blocks,
    convergence_cv_threshold_pct = cv_threshold,
    convergence_cv_pct = convergence_cv_pct,
    stopping_condition = stopping_condition,
    mean_ms    = mean_ms,
    median_ms  = median_ms,
    min_ms     = min_ms,
    max_ms     = max_ms,
    sd_ms      = sd_ms,
    cv_pct     = cv_pct,
    timer_noise_floor_ms = timer_noise_floor_ms,
    timer_noise_status = if (median_ms < timer_noise_floor_ms) "below_floor" else "above_floor",
    rss_metric = rss_metric,
    peak_rss   = rss_delta,
    error      = NA_character_
  )
}

write_csv <- function(df, path, append = FALSE) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  write.table(df, path, sep = ",", row.names = FALSE, quote = TRUE, na = "",
              append = append, col.names = !append)
}

log_cold_start <- function(runner, task, wall_ms, run_id = NA_character_, dir = "results") {
  path <- file.path(dir, runner, "cold_start.csv")
  header <- !file.exists(path)
  df <- data.frame(runner = runner, task = task, wall_ms = round(wall_ms, 3), run_id = run_id,
                   stringsAsFactors = FALSE)
  write_csv(df, path, append = !header)
}

log_error <- function(runner, task, msg, dir = "results") {
  path <- file.path(dir, runner, "errors.csv")
  header <- !file.exists(path)
  msg <- gsub(",", " ", msg)
  df <- data.frame(runner = runner, task = task, error = msg,
                   stringsAsFactors = FALSE)
  write_csv(df, path, append = !header)
}
