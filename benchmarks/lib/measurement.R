# Deterministic input construction and mutation contracts.

benchmark_input_schema_version <- function() "benchmark-input-v2"

benchmark_master_seed <- function() 20260713L

benchmark_encoded_strings <- function() {
  byte_marked <- rawToChar(as.raw(c(0x66, 0x61, 0xe7, 0x61, 0x64, 0x65)))
  values <- c(
    enc2utf8("façade"),
    iconv("façade", from = "UTF-8", to = "latin1"),
    byte_marked,
    "",
    NA_character_
  )
  Encoding(values[[1L]]) <- "UTF-8"
  Encoding(values[[2L]]) <- "latin1"
  Encoding(values[[3L]]) <- "bytes"
  values
}

benchmark_string_input <- function(task_id) {
  cases <- benchmark_encoded_strings()
  if (identical(task_id, "17_string_concat")) {
    return(rep(cases, length.out = 10000L))
  }
  if (identical(task_id, "18_string_nchar")) {
    values <- rep(cases[seq_len(4L)], length.out = 10000L)
    values[seq.int(20L, 10000L, by = 20L)] <- NA_character_
    return(values)
  }
  if (identical(task_id, "19_string_encoding")) {
    return(rep(cases, length.out = 10000L))
  }
  stop(sprintf("no benchmark string input is declared for %s", task_id))
}

benchmark_factor_input <- function() {
  vocabulary <- sprintf("level_%03d", seq_len(100L))
  values <- rep(vocabulary, length.out = 10000L)
  values[[10000L]] <- NA_character_
  values
}

validate_special_task_input <- function(task_id, arguments) {
  if (task_id %in% c("17_string_concat", "18_string_nchar", "19_string_encoding")) {
    expected <- benchmark_string_input(task_id)
    if (length(arguments) != 1L || !identical(arguments[[1L]], expected) ||
        !identical(Encoding(arguments[[1L]]), Encoding(expected))) {
      stop(sprintf("string contract input differs for %s", task_id))
    }
  }
  if (identical(task_id, "20_factor_ops")) {
    if (length(arguments) != 1L) {
      stop("factor contract requires 100 deterministic levels and one explicit trailing missing value")
    }
    values <- arguments[[1L]]
    vocabulary <- sprintf("level_%03d", seq_len(100L))
    if (!is.character(values) || length(values) != 10000L ||
        !identical(sort(unique(values[!is.na(values)])), vocabulary) ||
        sum(is.na(values)) != 1L || !is.na(values[[10000L]])) {
      stop("factor contract requires 100 deterministic levels and one explicit trailing missing value")
    }
  }
  if (identical(task_id, "24_long_vector_idx")) {
    values <- arguments[[1L]]
    if (length(arguments) != 1L || typeof(values) != "integer" || length(values) != 10000000L ||
        values[[1L]] != 1L || values[[length(values)]] != 10000000L) {
      stop("compact ALTREP indexing contract input differs")
    }
  }
  if (identical(task_id, "43_rng_stress")) {
    if (length(arguments) != 1L || !identical(arguments[[1L]], 1000000L)) {
      stop("RNG contract requires exactly one million normal draws")
    }
  }
  invisible(arguments)
}

input_scalar_integer <- function(value, label) {
  numeric_value <- suppressWarnings(as.numeric(value))
  if (length(numeric_value) != 1L || is.na(numeric_value) || !is.finite(numeric_value) ||
      numeric_value < 1 || numeric_value > .Machine$integer.max || numeric_value != as.integer(numeric_value)) {
    stop(sprintf("%s must be one positive integer", label))
  }
  as.integer(numeric_value)
}

task_input_seed <- function(master_seed, task_id, fixture_version) {
  master_seed <- input_scalar_integer(master_seed, "master seed")
  identity <- paste(task_id, fixture_version, sep = "\r")
  bytes <- utf8ToInt(enc2utf8(identity))
  hash <- as.double(master_seed)
  if (length(bytes) > 0L) {
    for (index in seq_along(bytes)) {
      hash <- (hash * 131 + bytes[[index]] + index) %% 2147483646
    }
  }
  as.integer(hash + 1)
}

task_altrep_intent <- function(task_id) {
  if (task_id %in% c("24_long_vector_idx", "62_boundary_altrep_integer_generated", "63_boundary_altrep_integer_handwritten")) {
    return("compact_integer_input")
  }
  if (task_id %in% sprintf("%02d_%s", 30:37, c(
    "altrep_create", "altrep_materialize", "altrep_elt_walk", "altrep_region_read",
    "altrep_sum_via_R", "altrep_sum_native", "altrep_min_max", "altrep_no_na_query"
  ))) {
    return("runtime_compact_integer_recipe")
  }
  "ordinary_r_object"
}

with_preserved_rng <- function(seed, expression) {
  existed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  previous <- if (existed) get(".Random.seed", envir = .GlobalEnv, inherits = FALSE) else NULL
  on.exit({
    if (existed) {
      assign(".Random.seed", previous, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(input_scalar_integer(seed, "task seed"), kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
  force(expression)
}

materialize_task_input <- function(task, seed) {
  with_preserved_rng(seed, {
    arguments <- task$args()
    if (!is.list(arguments)) stop(sprintf("input factory for %s did not return a list", task$id))
    validate_special_task_input(task$id, arguments)
    list(arguments = arguments, post_factory_rng_state = get(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
  })
}

rng_state_snapshot <- function() {
  if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    stop("RNG contract expected .Random.seed to exist")
  }
  as.integer(get(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
}

assert_rng_state_equivalent <- function(expected, actual, task_id = "43_rng_stress") {
  if (!identical(as.integer(expected), as.integer(actual))) {
    stop(sprintf("post-call RNG state differs for %s", task_id))
  }
  invisible(actual)
}

canonical_input_value <- function(value) {
  if (identical(typeof(value), "externalptr") || inherits(value, "benchmark_external_state_recipe")) {
    return(list(type = "externalptr", recipe = "runner_registered_fixture_state"))
  }
  if (is.environment(value) || is.function(value) || identical(typeof(value), "weakref")) {
    stop(sprintf("input fingerprint does not support values of type %s", typeof(value)))
  }
  attributes_value <- attributes(value)
  if (is.list(value)) {
    payload <- lapply(value, canonical_input_value)
  } else {
    payload <- value
  }
  list(
    type = typeof(value),
    length = length(value),
    payload = payload,
    encodings = if (is.character(value)) Encoding(value) else NULL,
    attributes = if (is.null(attributes_value)) NULL else list(
      names = names(attributes_value),
      values = lapply(unname(attributes_value), canonical_input_value)
    )
  )
}

serialized_md5 <- function(value) {
  path <- tempfile("zigr-fingerprint-")
  on.exit(unlink(path), add = TRUE)
  saveRDS(value, path, version = 3, compress = FALSE)
  unname(as.character(tools::md5sum(path))[[1L]])
}

task_input_fingerprint <- function(task_id, arguments, post_factory_rng_state, altrep_intent) {
  serialized_md5(list(
    schema_version = benchmark_input_schema_version(),
    task = as.character(task_id),
    arguments = canonical_input_value(arguments),
    altrep_intent = as.character(altrep_intent),
    post_factory_rng_state = as.integer(post_factory_rng_state)
  ))
}

task_arguments_fingerprint <- function(task_id, arguments, altrep_intent) {
  serialized_md5(list(
    schema_version = benchmark_input_schema_version(),
    task = as.character(task_id),
    arguments = canonical_input_value(arguments),
    altrep_intent = as.character(altrep_intent)
  ))
}

materialize_runtime_task_arguments <- function(arguments, mutation_policy, method_receiver) {
  if (!identical(mutation_policy, "stateful_reset_required")) return(arguments)
  if (length(arguments) < 1L || !inherits(arguments[[1L]], "benchmark_external_state_recipe")) {
    stop("stateful task input is missing its external-state recipe")
  }
  if (!is.function(method_receiver)) stop("stateful task has no method receiver factory")
  arguments[[1L]] <- method_receiver()
  arguments
}

build_input_recipe_record <- function(task, master_seed, fixture_version, contract_version) {
  seed <- task_input_seed(master_seed, task$id, fixture_version)
  materialized <- materialize_task_input(task, seed)
  intent <- task_altrep_intent(task$id)
  list(
    task = task$id,
    master_seed = input_scalar_integer(master_seed, "master seed"),
    task_seed = seed,
    fixture_version = as.character(fixture_version),
    contract_version = as.character(contract_version),
    mutation_policy = task_mutation_policy(task$id),
    altrep_intent = intent,
    fingerprint = task_input_fingerprint(
      task$id,
      materialized$arguments,
      materialized$post_factory_rng_state,
      intent
    )
  )
}

write_input_recipe_manifest <- function(path, tasks, task_manifest, evidence, master_seed) {
  selected_ids <- vapply(tasks, function(task) task$id, character(1))
  task_rows <- match(selected_ids, task_manifest$task)
  if (anyNA(task_rows)) stop("input recipes contain a task absent from the task manifest")
  r_rows <- evidence$tasks[evidence$tasks$runner == "r", , drop = FALSE]
  evidence_indices <- match(selected_ids, r_rows$task)
  if (anyNA(evidence_indices)) stop("input recipes contain a task absent from normalized evidence")
  records <- vector("list", length(tasks))
  for (index in seq_along(tasks)) {
    records[[index]] <- build_input_recipe_record(
      tasks[[index]],
      master_seed,
      r_rows$fixture_version[[evidence_indices[[index]]]],
      r_rows$contract_version[[evidence_indices[[index]]]]
    )
    gc(verbose = FALSE)
  }
  payload <- list(
    schema_version = benchmark_input_schema_version(),
    master_seed = input_scalar_integer(master_seed, "master seed"),
    fingerprint_algorithm = "R serialization v3 structural MD5",
    tasks = records
  )
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(payload, path, auto_unbox = TRUE, pretty = TRUE)
  invisible(payload)
}

read_input_recipe_manifest <- function(path) {
  if (!file.exists(path)) stop(sprintf("canonical input manifest not found: %s", path))
  payload <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  if (!identical(as.character(payload$schema_version), benchmark_input_schema_version())) {
    stop("unsupported canonical input schema version")
  }
  records <- payload$tasks
  if (is.null(records) || length(records) == 0L) stop("canonical input manifest has no task recipes")
  ids <- vapply(records, function(record) as.character(record$task), character(1))
  if (anyDuplicated(ids)) stop("canonical input manifest has duplicate task recipes")
  names(records) <- ids
  payload$tasks <- records
  payload
}

validate_input_manifest_digest <- function(path, expected_digest) {
  if (length(expected_digest) != 1L || is.na(expected_digest) || !nzchar(expected_digest)) {
    stop("expected canonical input manifest digest is missing")
  }
  actual <- unname(as.character(tools::md5sum(path))[[1L]])
  if (!identical(actual, as.character(expected_digest))) {
    stop(sprintf("canonical input manifest digest differs: expected %s, got %s", expected_digest, actual))
  }
  invisible(actual)
}

validate_materialized_task_input <- function(task, record, master_seed, materialized) {
  if (is.null(record)) stop(sprintf("canonical input recipe is missing for %s", task$id))
  expected_master <- input_scalar_integer(master_seed, "master seed")
  recorded_master <- input_scalar_integer(record$master_seed, sprintf("recorded master seed for %s", task$id))
  if (!identical(expected_master, recorded_master)) stop(sprintf("master seed mismatch for %s", task$id))
  expected_seed <- task_input_seed(expected_master, task$id, as.character(record$fixture_version))
  recorded_seed <- input_scalar_integer(record$task_seed, sprintf("recorded task seed for %s", task$id))
  if (!identical(expected_seed, recorded_seed)) stop(sprintf("task seed mismatch for %s", task$id))
  expected_policy <- task_mutation_policy(task$id)
  if (!identical(as.character(record$mutation_policy), expected_policy)) {
    stop(sprintf("mutation policy mismatch for %s", task$id))
  }
  expected_intent <- task_altrep_intent(task$id)
  if (!identical(as.character(record$altrep_intent), expected_intent)) {
    stop(sprintf("ALTREP intent mismatch for %s", task$id))
  }
  actual <- task_input_fingerprint(
    task$id,
    materialized$arguments,
    materialized$post_factory_rng_state,
    expected_intent
  )
  if (!identical(actual, as.character(record$fingerprint))) {
    stop(sprintf("canonical input fingerprint mismatch for %s: expected %s, got %s", task$id, record$fingerprint, actual))
  }
  invisible(materialized)
}

assert_immutable_input <- function(task_id, arguments, before_fingerprint, altrep_intent) {
  after <- task_arguments_fingerprint(task_id, arguments, altrep_intent)
  if (!identical(after, before_fingerprint)) stop(sprintf("immutable input mutated for %s", task_id))
  invisible(after)
}

# Prepared-call timing, RSS observation, and result logging.

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

evaluate_prepared_call <- function(prepared) {
  if (is.function(prepared)) prepared() else eval(prepared, envir = parent.frame())
}

timed_call <- function(prepare_call) {
  gc(full = TRUE)
  rss_before <- current_rss_kb()
  prepared <- tryCatch(list(ok = TRUE, expression = prepare_call()), error = function(error) {
    list(ok = FALSE, error = conditionMessage(error))
  })
  if (!isTRUE(prepared$ok)) {
    return(list(wall_ms = NA_real_, rss_delta_kb = NA_integer_, error = paste("cold input preparation failed:", prepared$error)))
  }
  error <- NA_character_
  wall_start <- get_nanotime()
  tryCatch(evaluate_prepared_call(prepared$expression), error = function(e) { error <<- conditionMessage(e) })
  wall_end <- get_nanotime()
  gc(full = TRUE)
  rss_after <- current_rss_kb()
  list(
    wall_ms     = (wall_end - wall_start) / 1e6,
    rss_delta_kb = max(0, rss_after - rss_before, na.rm = TRUE),
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
    tryCatch(evaluate_prepared_call(prepared$expression), error = function(error) {
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
      tryCatch(evaluate_prepared_call(prepared$expression), error = function(error) {
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
    rss_delta_kb = rss_delta,
    error      = NA_character_
  )
}

write_csv <- function(df, path, append = FALSE) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  write.table(df, path, sep = ",", row.names = FALSE, quote = TRUE, na = "",
              append = append, col.names = !append)
}

write_csv_once <- function(df, path, label = "CSV output") {
  path <- normalizePath(path, mustWork = FALSE)
  if (file.exists(path)) stop(sprintf("%s already exists: %s", label, path))
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  staged <- paste0(path, ".tmp-", Sys.getpid())
  on.exit(unlink(staged), add = TRUE)
  write_csv(df, staged)
  if (!file.rename(staged, path)) stop(sprintf("cannot promote %s: %s", label, path))
  invisible(path)
}

combine_csv_files_once <- function(paths, output, label) {
  paths <- as.character(paths)
  if (length(paths) == 0L || any(!file.exists(paths))) {
    stop(sprintf("cannot combine incomplete %s", label))
  }
  tables <- lapply(paths, read.csv, stringsAsFactors = FALSE)
  columns <- lapply(tables, names)
  if (!all(vapply(columns, identical, logical(1), columns[[1L]]))) {
    stop(sprintf("cannot combine %s with different columns", label))
  }
  write_csv_once(do.call(rbind, tables), output, label)
  unlink(paths)
  invisible(output)
}

log_error <- function(runner, task, msg, dir = "results") {
  path <- file.path(dir, runner, "errors.csv")
  header <- !file.exists(path)
  msg <- gsub(",", " ", msg)
  df <- data.frame(runner = runner, task = task, error = msg,
                   stringsAsFactors = FALSE)
  write_csv(df, path, append = !header)
}

benchmark_artifact_layout <- function(metadata) {
  schema <- suppressWarnings(as.integer(metadata$schema_version))
  if (length(schema) != 1L || is.na(schema) || !(schema %in% c(2L, 3L))) {
    stop("unsupported run manifest schema version")
  }
  if (schema == 2L) return("per-cell-v1")
  layout <- as.character(metadata$artifact_layout)
  if (length(layout) != 1L || is.na(layout) || !identical(layout, "grouped-v1")) {
    stop("run manifest has no supported artifact layout")
  }
  layout
}

run_summary_artifact_paths <- function(run_dir, metadata, universe, runners = character(0)) {
  if (!(universe %in% c("task", "fixture"))) stop("summary universe must be task or fixture")
  rooted <- function(path) if (length(run_dir) == 1L && !is.na(run_dir) && nzchar(run_dir)) file.path(run_dir, path) else path
  if (identical(benchmark_artifact_layout(metadata), "grouped-v1")) {
    return(rooted(paste0(universe, "_summary.csv")))
  }
  prefix <- if (identical(universe, "fixture")) "fixture_" else ""
  rooted(paste0(prefix, as.character(runners), "_summary.csv"))
}

read_run_summary_table <- function(run_dir, metadata, universe, runners) {
  paths <- run_summary_artifact_paths(run_dir, metadata, universe, runners)
  missing <- paths[!file.exists(paths)]
  if (length(missing) > 0L) stop(sprintf("run %s summaries are missing", universe))
  do.call(rbind, lapply(paths, read.csv, stringsAsFactors = FALSE))
}

run_correctness_artifact_paths <- function(run_dir, metadata, universe, runners = character(0)) {
  if (!(universe %in% c("task", "fixture"))) stop("correctness universe must be task or fixture")
  rooted <- function(path) if (length(run_dir) == 1L && !is.na(run_dir) && nzchar(run_dir)) file.path(run_dir, path) else path
  if (identical(benchmark_artifact_layout(metadata), "grouped-v1")) {
    return(rooted(file.path("correctness", paste0(universe, "s.csv"))))
  }
  rooted(file.path("correctness", paste0(universe, "s"), paste0(as.character(runners), ".csv")))
}

run_sample_artifact_paths <- function(run_dir, metadata, universe, runner, ids = character(0)) {
  if (!(universe %in% c("task", "fixture"))) stop("sample universe must be task or fixture")
  ids <- as.character(ids)
  if (length(ids) == 0L) return(character(0))
  if (identical(benchmark_artifact_layout(metadata), "grouped-v1")) {
    path <- paste0(universe, "_samples.csv")
    return(if (length(run_dir) == 1L && !is.na(run_dir) && nzchar(run_dir)) file.path(run_dir, path) else path)
  }
  relative_base <- if (identical(universe, "task")) runner else file.path("fixtures", runner)
  base <- if (length(run_dir) == 1L && !is.na(run_dir) && nzchar(run_dir)) {
    file.path(run_dir, relative_base)
  } else relative_base
  names <- if (identical(universe, "task")) paste0("task_", ids, ".csv") else paste0(ids, ".csv")
  file.path(base, names)
}

read_run_sample_table <- function(run_dir, metadata, universe, runner, ids) {
  ids <- as.character(ids)
  paths <- run_sample_artifact_paths(run_dir, metadata, universe, runner, ids)
  if (length(ids) == 0L) return(data.frame())
  missing <- paths[!file.exists(paths)]
  if (length(missing) > 0L) {
    stop(sprintf("raw timing samples are missing for %s: %s", runner, paste(basename(missing), collapse = ", ")))
  }
  tables <- lapply(paths, read.csv, stringsAsFactors = FALSE)
  samples <- do.call(rbind, tables)
  if (identical(benchmark_artifact_layout(metadata), "grouped-v1")) {
    if (!("runner" %in% names(samples))) stop("grouped raw timing samples lack runner")
    samples <- samples[as.character(samples$runner) == runner, , drop = FALSE]
  }
  samples
}

read_run_wall_time_samples <- function(
    run_dir, metadata, universe, runner, id, expected_n = NULL, minimum_n = 2L) {
  paths <- run_sample_artifact_paths(run_dir, metadata, universe, runner, id)
  filters <- if (identical(benchmark_artifact_layout(metadata), "grouped-v1")) {
    if (identical(universe, "task")) list(runner = runner, task = id, phase = "timed") else list(runner = runner, row_id = id)
  } else NULL
  read_wall_time_samples(paths[[1L]], expected_n = expected_n, minimum_n = minimum_n, filters = filters)
}

read_wall_time_samples <- function(path, expected_n = NULL, minimum_n = 2L, filters = NULL) {
  if (length(path) != 1L || is.na(path) || !nzchar(path)) stop("raw timing sample path must be one non-empty value")
  if (!file.exists(path)) stop(sprintf("raw timing samples are missing: %s", path))
  samples <- read.csv(path, stringsAsFactors = FALSE)
  if (!is.null(filters)) {
    if (!is.list(filters) || is.null(names(filters)) || any(!nzchar(names(filters)))) {
      stop("raw timing sample filters must be a named list")
    }
    missing_filters <- setdiff(names(filters), names(samples))
    if (length(missing_filters) > 0L) {
      stop(sprintf("raw timing samples lack filter columns: %s", paste(missing_filters, collapse = ", ")))
    }
    keep <- rep(TRUE, nrow(samples))
    for (field in names(filters)) keep <- keep & as.character(samples[[field]]) == as.character(filters[[field]])
    samples <- samples[keep, , drop = FALSE]
  }
  if (!("wall_ms" %in% names(samples))) stop(sprintf("raw timing samples lack wall_ms: %s", path))
  values <- samples$wall_ms
  if (!is.numeric(values) || any(!is.finite(values)) || any(values < 0)) {
    stop(sprintf("raw timing samples contain invalid wall_ms values: %s", path))
  }
  minimum_n <- as.integer(minimum_n)
  if (length(minimum_n) != 1L || is.na(minimum_n) || minimum_n < 0L) stop("minimum sample count must be non-negative")
  if (length(values) < minimum_n) stop(sprintf("raw timing samples are incomplete: %s", path))
  if (!is.null(expected_n) && (length(expected_n) != 1L || is.na(expected_n) || length(values) != as.integer(expected_n))) {
    stop(sprintf("raw timing sample count differs from summary: %s", path))
  }
  values
}

median_confidence_interval <- function(values, level) {
  if (!is.numeric(values) || any(!is.finite(values))) stop("median confidence interval requires finite numeric samples")
  if (length(level) != 1L || is.na(level) || !is.finite(level) || level <= 0 || level >= 1) {
    stop("median confidence level must be between zero and one")
  }
  values <- sort(as.numeric(values))
  n <- length(values)
  if (n < 2L) return(c(low = NA_real_, high = NA_real_))
  alpha <- 1 - level
  low_rank <- max(1L, as.integer(qbinom(alpha / 2, n, 0.5)))
  high_rank <- min(n, as.integer(qbinom(1 - alpha / 2, n, 0.5)) + 1L)
  c(low = values[[low_rank]], high = values[[high_rank]])
}

# Normalized fixture measurement and retained-artifact validation.

fixture_measurement_specs <- function() {
  complex_values <- function() {
    values <- complex(
      real = as.double(seq_len(32768L)),
      imaginary = as.double(seq_len(32768L)) * 0.5
    )
    values[[1L]] <- NA_complex_
    values[[2L]] <- complex(real = NaN, imaginary = 3)
    values
  }
  list(
    F01 = list(function_name = "fixture_zero", arguments = function() list()),
    F02 = list(function_name = "fixture_scalar", arguments = function() list(2.5)),
    F03 = list(
      function_name = "fixture_numeric",
      arguments = function() list(c(as.double(seq_len(100000L)) + 0.0, NA_real_, NaN))
    ),
    F04 = list(
      function_name = "fixture_altrep_integer",
      arguments = function() list(seq_len(100000L)),
      altrep_intent = "compact_integer_altrep"
    ),
    F05 = list(
      function_name = "fixture_strings",
      arguments = function() list(rep(benchmark_encoded_strings(), 2000L))
    ),
    F06 = list(
      function_name = "fixture_raw",
      arguments = function() list(as.raw((seq_len(262144L) - 1L) %% 251L))
    ),
    F07 = list(function_name = "fixture_complex", arguments = function() list(complex_values())),
    F08 = list(
      function_name = "fixture_logical_counts",
      arguments = function() list(rep(c(FALSE, TRUE, NA), length.out = 100000L))
    ),
    F09 = list(function_name = "fixture_schema", arguments = function() list(fixture_schema_value())),
    F10 = list(
      function_name = c("fixture_new", "fixture_method", "fixture_read"),
      arguments = function() list(7L),
      stateful = TRUE
    ),
    F12 = list(function_name = "fixture_outputs", arguments = function() list())
  )
}

fixture_measurement_optimized_specs <- function() {
  list(
    F03 = list(function_name = "F03", implementation_class = "optimized_base_r"),
    F04 = list(function_name = "F04", implementation_class = "optimized_base_r")
  )
}

fixture_measurement_altrep_intent <- function(spec) {
  if (is.null(spec$altrep_intent)) "ordinary_r_object" else as.character(spec$altrep_intent)
}

fixture_measurement_requires_fresh_input <- function(spec) {
  isTRUE(spec$stateful) || !identical(fixture_measurement_altrep_intent(spec), "ordinary_r_object")
}

fixture_measurement_input_fingerprint <- function(fixture, spec) {
  task_arguments_fingerprint(
    paste0("fixture:", fixture),
    spec$arguments(),
    fixture_measurement_altrep_intent(spec)
  )
}

fixture_measurement_context <- function(root_dir, runner, evidence) {
  supported <- as.character(evidence$fixture_rows$fixture[
    evidence$fixture_rows$runner == runner & evidence$fixture_rows$executable
  ])
  if (runner %in% c("zigr", "rcpp", "cpp11", "extendr", "savvy")) {
    package <- fixture_package_map(root_dir)[[runner]]
    old_paths <- .libPaths()
    .libPaths(c(package$library, old_paths))
    if (package$package %in% loadedNamespaces()) unloadNamespace(package$package)
    loadNamespace(package$package, lib.loc = package$library)
    namespace <- asNamespace(package$package)
    public_names <- unique(unlist(fixture_function_map(), use.names = FALSE))
    functions <- lapply(public_names, function(name) {
      if (exists(name, envir = namespace, mode = "function", inherits = FALSE)) {
        get(name, envir = namespace, mode = "function", inherits = FALSE)
      } else {
        NULL
      }
    })
    names(functions) <- public_names
    close <- function() {
      gc(full = TRUE)
      if (package$package %in% loadedNamespaces()) unloadNamespace(package$package)
      .libPaths(old_paths)
      invisible(NULL)
    }
    return(list(functions = functions, optimized = list(), supported = supported, close = close))
  }
  if (identical(runner, "r")) {
    reference <- fixture_r_functions(root_dir)
    return(list(
      functions = reference$functions,
      optimized = reference$optimized,
      supported = supported,
      close = function() invisible(NULL)
    ))
  }
  if (identical(runner, "c_call")) {
    control <- fixture_c_context(root_dir)
    return(list(
      functions = control$functions,
      optimized = list(),
      supported = supported,
      close = control$close
    ))
  }
  stop(sprintf("no fixture measurement context for runner %s", runner))
}

fixture_measurement_prepare <- function(functions, fixture, spec, arguments = spec$arguments()) {
  force(arguments)
  if (isTRUE(spec$stateful)) {
    state <- functions$fixture_new()
    amount <- arguments[[1L]]
    return(function() functions$fixture_method(state, amount))
  }
  fn <- functions[[spec$function_name]]
  if (!is.function(fn)) stop(sprintf("fixture %s has no callable implementation", fixture))
  function() do.call(fn, arguments)
}

fixture_measurement_validate_case <- function(runner, functions, reference_functions, fixture, spec) {
  if (isTRUE(spec$stateful)) {
    state <- functions$fixture_new()
    amount <- spec$arguments()[[1L]]
    actual <- functions$fixture_method(state, amount)
    if (!identical(as.integer(actual), amount) ||
        !identical(as.integer(functions$fixture_read(state)), amount)) {
      stop(sprintf("fixture timing correctness failed for %s/%s", runner, fixture))
    }
    return(invisible(TRUE))
  }
  actual <- fixture_measurement_prepare(functions, fixture, spec)()
  expected <- do.call(reference_functions[[spec$function_name]], spec$arguments())
  fixture_assert_same(expected, actual, sprintf("fixture timing %s/%s", runner, fixture))
  invisible(TRUE)
}

fixture_measurement_validate_optimized <- function(optimized, reference_functions, fixture, spec) {
  fn <- optimized[[fixture]]
  if (!is.function(fn)) stop(sprintf("optimized R fixture %s is missing", fixture))
  arguments <- spec$arguments()
  actual <- do.call(fn, arguments)
  expected <- do.call(reference_functions[[spec$function_name]], spec$arguments())
  fixture_assert_same(expected, actual, sprintf("optimized R fixture timing %s", fixture))
  invisible(TRUE)
}

correctness_artifact_set_digest <- function(paths) {
  paths <- sort(normalizePath(paths, mustWork = TRUE))
  records <- lapply(paths, function(path) list(
    name = basename(path),
    size = unname(file.info(path)$size),
    md5 = unname(as.character(tools::md5sum(path))[[1L]])
  ))
  source_ledger_object_digest(records)
}

validate_correctness_artifacts <- function(run_dir, metadata, evidence) {
  expected_runners <- sort(run_manifest_values(metadata$runners))
  expected_tasks <- sort(run_manifest_values(metadata$tasks))
  task_files <- run_correctness_artifact_paths(run_dir, metadata, "task", expected_runners)
  fixture_files <- run_correctness_artifact_paths(run_dir, metadata, "fixture", expected_runners)
  if (any(!file.exists(task_files)) || any(!file.exists(fixture_files))) {
    stop("correctness evidence files are incomplete")
  }

  tasks <- do.call(rbind, lapply(task_files, read.csv, stringsAsFactors = FALSE))
  task_required <- c(
    "run_id", "runner", "task", "status", "correctness_status", "correctness_policy",
    "correctness_message", "source_tree_digest", "source_ledger_identity_digest",
    "artifact_digest", "input_manifest_digest"
  )
  if (length(setdiff(task_required, names(tasks))) > 0L) stop("task correctness evidence columns differ")
  task_keys <- paste(tasks$runner, tasks$task, sep = "\r")
  expected_task_keys <- unlist(lapply(expected_runners, function(runner) {
    paste(runner, expected_tasks, sep = "\r")
  }), use.names = FALSE)
  if (!setequal(task_keys, expected_task_keys) || anyDuplicated(task_keys)) {
    stop("task correctness evidence coverage differs from the run manifest")
  }
  for (index in seq_len(nrow(tasks))) {
    row <- tasks[index, , drop = FALSE]
    runner <- as.character(row$runner)
    task <- as.character(row$task)
    disposition <- run_manifest_disposition(metadata, runner, task)
    environment <- runner_environment_record(metadata$environment, runner)
    expected_status <- if (isTRUE(disposition$executable)) "PASS" else "N/A"
    expected_correctness <- if (isTRUE(disposition$executable)) c("PASS", "REFERENCE") else "NOT_APPLICABLE"
    if (!identical(as.character(row$status), expected_status) ||
        !(as.character(row$correctness_status) %in% expected_correctness) ||
        !nzchar(as.character(row$correctness_message))) {
      stop(sprintf("task correctness evidence failed for %s/%s", runner, task))
    }
    if (!isTRUE(disposition$executable) &&
        !identical(as.character(row$correctness_message), as.character(disposition$reason))) {
      stop(sprintf("task correctness gap reason differs for %s/%s", runner, task))
    }
    exact <- list(
      run_id = as.character(metadata$run_id),
      source_tree_digest = as.character(metadata$environment$source_tree$digest),
      source_ledger_identity_digest = as.character(environment$source_ledger_identity_digest),
      artifact_digest = as.character(environment$artifact_digest),
      input_manifest_digest = as.character(metadata$input_manifest$digest)
    )
    for (field in names(exact)) {
      if (!identical(as.character(row[[field]]), exact[[field]])) {
        stop(sprintf("task correctness identity field %s differs for %s/%s", field, runner, task))
      }
    }
  }

  fixtures <- do.call(rbind, lapply(fixture_files, read.csv, stringsAsFactors = FALSE))
  fixture_required <- c(
    "run_id", "runner", "fixture", "variant", "row_id", "status", "correctness_status",
    "correctness_message", "source_tree_digest", "source_ledger_identity_digest", "artifact_digest"
  )
  if (length(setdiff(fixture_required, names(fixtures))) > 0L) {
    stop("fixture correctness evidence columns differ")
  }
  fixture_keys <- paste(fixtures$runner, fixtures$row_id, sep = "\r")
  expected_fixture_keys <- unlist(lapply(expected_runners, function(runner) {
    base <- paste(runner, evidence$fixtures, sep = "\r")
    if (identical(runner, "r")) {
      c(base, paste(runner, paste0(c("F03", "F04"), "_optimized_base_r"), sep = "\r"))
    } else base
  }), use.names = FALSE)
  if (!setequal(fixture_keys, expected_fixture_keys) || anyDuplicated(fixture_keys)) {
    stop("fixture correctness evidence coverage differs from the normalized matrix")
  }
  for (index in seq_len(nrow(fixtures))) {
    row <- fixtures[index, , drop = FALSE]
    runner <- as.character(row$runner)
    fixture <- as.character(row$fixture)
    optimized <- identical(as.character(row$variant), "optimized_base_r")
    evidence_row <- evidence$fixture_rows[
      evidence$fixture_rows$runner == runner & evidence$fixture_rows$fixture == fixture,
      , drop = FALSE
    ]
    if (nrow(evidence_row) != 1L) stop(sprintf("fixture correctness evidence is missing for %s/%s", runner, fixture))
    environment <- runner_environment_record(metadata$environment, runner)
    executable <- optimized || isTRUE(evidence_row$executable)
    expected_status <- if (executable) "PASS" else "N/A"
    expected_correctness <- if (executable) c("PASS", "REFERENCE") else "NOT_APPLICABLE"
    expected_variant <- if (optimized) "optimized_base_r" else "public"
    expected_row_id <- if (optimized) paste0(fixture, "_optimized_base_r") else fixture
    if (!identical(as.character(row$variant), expected_variant) ||
        !identical(as.character(row$row_id), expected_row_id) ||
        !identical(as.character(row$status), expected_status) ||
        !(as.character(row$correctness_status) %in% expected_correctness) ||
        !nzchar(as.character(row$correctness_message))) {
      stop(sprintf("fixture correctness evidence failed for %s/%s", runner, row$row_id))
    }
    if (!executable &&
        !identical(as.character(row$correctness_message), as.character(evidence_row$reason))) {
      stop(sprintf("fixture correctness gap reason differs for %s/%s", runner, fixture))
    }
    exact <- list(
      run_id = as.character(metadata$run_id),
      source_tree_digest = as.character(metadata$environment$source_tree$digest),
      source_ledger_identity_digest = as.character(environment$source_ledger_identity_digest),
      artifact_digest = as.character(environment$fixture_artifact_digest)
    )
    for (field in names(exact)) {
      if (!identical(as.character(row[[field]]), exact[[field]])) {
        stop(sprintf("fixture correctness identity field %s differs for %s/%s", field, runner, row$row_id))
      }
    }
  }

  list(
    task_rows = nrow(tasks),
    fixture_rows = nrow(fixtures),
    task_artifact_digest = correctness_artifact_set_digest(task_files),
    fixture_artifact_digest = correctness_artifact_set_digest(fixture_files)
  )
}

validate_fixture_raw_statistics <- function(summary, raw, policy, label) {
  median_ms <- median(raw$wall_ms)
  mean_ms <- mean(raw$wall_ms)
  min_ms <- min(raw$wall_ms)
  max_ms <- max(raw$wall_ms)
  sd_ms <- sd(raw$wall_ms)
  cv_pct <- if (mean_ms > 0) sd_ms / mean_ms * 100 else 0
  if (!identical(as.numeric(summary$mean_ms), round(mean_ms, 4)) ||
      !identical(as.numeric(summary$median_ms), round(median_ms, 4)) ||
      !identical(as.numeric(summary$min_ms), round(min_ms, 4)) ||
      !identical(as.numeric(summary$max_ms), round(max_ms, 4)) ||
      !identical(as.numeric(summary$sd_ms), round(sd_ms, 4)) ||
      !identical(as.numeric(summary$cv_pct), round(cv_pct, 2))) {
    stop(sprintf("fixture raw statistics differ for %s", label))
  }
  expected_noise <- if (median_ms < as.numeric(policy$timer_noise_floor_ms)) "below_floor" else "above_floor"
  window_size <- as.integer(policy$block_size) * as.integer(policy$convergence_window_blocks)
  convergence_window <- tail(raw$wall_ms, window_size)
  convergence_mean <- mean(convergence_window)
  convergence_cv <- if (convergence_mean > 0) {
    sd(convergence_window) / convergence_mean * 100
  } else 0
  if (!identical(as.character(summary$timer_noise_status), expected_noise) ||
      !isTRUE(all.equal(as.numeric(summary$convergence_cv_pct), convergence_cv, tolerance = 1e-12))) {
    stop(sprintf("fixture convergence or timer-noise evidence differs for %s", label))
  }
  invisible(TRUE)
}

validate_fixture_measurement_artifacts <- function(run_dir, metadata, evidence) {
  expected_runners <- sort(run_manifest_values(metadata$runners))
  summaries <- read_run_summary_table(run_dir, metadata, "fixture", expected_runners)
  required <- c(
    "run_id", "runner", "fixture", "variant", "row_id", "status", "correctness_status",
    "correctness_message", "input_fingerprint", "implementation_role", "evidence_use",
    "path_kind", "representation_strategy", "comparison_tier", "setup_policy", "timing_eligible",
    "kernel_id", "contract_version", "fixture_version", "comparison_group", "tool_identity",
    "fixture_source_digest", "fixture_build_digest", "fixture_generated_glue_kind",
    "fixture_generated_glue_digest", "fixture_artifact_digest", "fixture_dependency_digest",
    "fixture_artifact_dependency_digest", "source_ledger_identity_digest", "mean_ms", "median_ms",
    "min_ms", "max_ms", "sd_ms", "cv_pct", "rss_kb", "cold_start_ms", "n_iterations",
    "warmup_iterations", "block_size", "max_iterations", "convergence_window_blocks",
    "convergence_cv_threshold_pct", "convergence_cv_pct", "stopping_condition", "converged",
    "timer_noise_floor_ms", "timer_noise_status", "rss_metric", "gc_policy"
  )
  missing <- setdiff(required, names(summaries))
  if (length(missing) > 0L) stop(sprintf("fixture summaries missing columns: %s", paste(missing, collapse = ", ")))
  if (!all(as.character(summaries$run_id) == as.character(metadata$run_id))) {
    stop("fixture summaries contain mixed run IDs")
  }
  expected_keys <- unlist(lapply(expected_runners, function(runner) {
    base <- paste(runner, evidence$fixtures, sep = "\r")
    if (identical(runner, "r")) c(base, paste(runner, paste0(c("F03", "F04"), "_optimized_base_r"), sep = "\r")) else base
  }), use.names = FALSE)
  actual_keys <- paste(summaries$runner, summaries$row_id, sep = "\r")
  if (!setequal(expected_keys, actual_keys) || anyDuplicated(actual_keys)) {
    stop("fixture summary coverage differs from the normalized matrix")
  }
  specs <- fixture_measurement_specs()
  for (index in seq_len(nrow(summaries))) {
    summary <- summaries[index, , drop = FALSE]
    runner <- as.character(summary$runner)
    fixture <- as.character(summary$fixture)
    variant <- as.character(summary$variant)
    optimized <- identical(variant, "optimized_base_r")
    evidence_row <- evidence$fixture_rows[
      evidence$fixture_rows$runner == runner & evidence$fixture_rows$fixture == fixture,
      , drop = FALSE
    ]
    if (nrow(evidence_row) != 1L) stop(sprintf("fixture evidence is missing for %s/%s", runner, fixture))
    environment <- runner_environment_record(metadata$environment, runner)
    validate_fixture_artifact_identity(environment)
    spec <- specs[[fixture]]
    expected_fingerprint <- if (is.null(spec)) {
      "not_applicable"
    } else {
      fixture_measurement_input_fingerprint(fixture, spec)
    }
    expected <- list(
      variant = if (optimized) "optimized_base_r" else "public",
      row_id = if (optimized) paste0(fixture, "_optimized_base_r") else fixture,
      input_fingerprint = expected_fingerprint,
      implementation_role = if (optimized) "optimized_base_r" else as.character(evidence_row$implementation_role),
      evidence_use = if (optimized) "timed_baseline" else as.character(evidence_row$evidence_use),
      path_kind = if (optimized) "optimized_base_r" else as.character(evidence_row$path_kind),
      representation_strategy = if (optimized) "runtime_service" else as.character(evidence_row$representation_strategy),
      comparison_tier = if (optimized) "tier_c" else as.character(evidence_row$comparison_tier),
      setup_policy = if (optimized) "setup_outside_timer" else as.character(evidence_row$setup_policy),
      kernel_id = if (optimized) paste0("normalized:", fixture, ":optimized-base-r-v1") else as.character(evidence_row$kernel_id),
      contract_version = as.character(evidence_row$contract_version),
      fixture_version = as.character(evidence_row$fixture_version),
      comparison_group = if (optimized) paste0("normalized:", fixture, ":optimized-base-r") else as.character(evidence_row$comparison_group),
      tool_identity = as.character(environment$tool_identity),
      fixture_source_digest = as.character(environment$fixture_source_digest),
      fixture_build_digest = as.character(environment$fixture_build_digest),
      fixture_generated_glue_kind = as.character(environment$fixture_generated_glue_kind),
      fixture_generated_glue_digest = as.character(environment$fixture_generated_glue_digest),
      fixture_artifact_digest = as.character(environment$fixture_artifact_digest),
      fixture_dependency_digest = as.character(environment$fixture_dependency_digest),
      fixture_artifact_dependency_digest = as.character(environment$fixture_artifact_dependency_digest),
      source_ledger_identity_digest = as.character(environment$source_ledger_identity_digest),
      timing_eligible = if (optimized) "TRUE" else as.character(isTRUE(evidence_row$timing_eligible))
    )
    for (field in names(expected)) {
      if (!identical(as.character(summary[[field]]), expected[[field]])) {
        stop(sprintf("fixture summary field %s differs for %s/%s", field, runner, summary$row_id))
      }
    }
    admitted <- optimized || isTRUE(evidence_row$timing_eligible)
    if (admitted) {
      if (!identical(as.character(summary$status), "PASS") ||
          !(as.character(summary$correctness_status) %in% c("PASS", "REFERENCE")) ||
          !nzchar(as.character(summary$correctness_message))) {
        stop(sprintf("timed fixture lacks passing correctness for %s/%s", runner, summary$row_id))
      }
    } else if (isTRUE(evidence_row$executable)) {
      if (!identical(as.character(summary$status), "CORRECTNESS_ONLY") ||
          !(as.character(summary$correctness_status) %in% c("PASS", "REFERENCE"))) {
        stop(sprintf("correctness-only fixture status differs for %s/%s", runner, fixture))
      }
    } else if (!identical(as.character(summary$status), "N/A") ||
               !identical(as.character(summary$correctness_status), "NOT_APPLICABLE")) {
      stop(sprintf("fixture gap status differs for %s/%s", runner, fixture))
    }
  }
  pass <- summaries[summaries$status == "PASS", , drop = FALSE]
  policy <- metadata$timing_policy
  if (any(as.integer(pass$warmup_iterations) != as.integer(policy$warmup_iterations)) ||
      any(as.integer(pass$block_size) != as.integer(policy$block_size)) ||
      any(as.integer(pass$max_iterations) != as.integer(policy$max_iterations)) ||
      any(as.integer(pass$convergence_window_blocks) != as.integer(policy$convergence_window_blocks)) ||
      any(as.numeric(pass$convergence_cv_threshold_pct) != as.numeric(policy$convergence_cv_threshold_pct)) ||
      any(as.numeric(pass$timer_noise_floor_ms) != as.numeric(policy$timer_noise_floor_ms)) ||
      any(as.character(pass$rss_metric) != as.character(policy$rss_metric)) ||
      any(as.character(pass$gc_policy) != as.character(policy$gc_policy))) {
    stop("fixture summaries disagree with the timing policy")
  }
  numeric_fields <- c(
    "mean_ms", "median_ms", "min_ms", "max_ms", "sd_ms", "cv_pct", "rss_kb", "cold_start_ms",
    "convergence_cv_pct"
  )
  if (any(!vapply(pass[numeric_fields], function(values) all(is.finite(values) & values >= 0), logical(1)))) {
    stop("fixture PASS summaries contain invalid timing statistics")
  }
  if (any(pass$n_iterations < 1L |
          pass$n_iterations > as.integer(policy$max_iterations) |
          pass$n_iterations %% as.integer(policy$block_size) != 0L)) {
    stop("fixture PASS summaries contain an invalid measured sample count")
  }
  if (any(!(pass$stopping_condition %in% c("rolling_cv", "max_iterations")))) {
    stop("fixture PASS summaries contain an invalid stopping condition")
  }
  max_rows <- pass$stopping_condition == "max_iterations"
  if (any(pass$n_iterations[max_rows] != as.integer(policy$max_iterations)) ||
      any(pass$converged[max_rows])) {
    stop("fixture max-iteration summaries disagree with the timing policy")
  }
  converged_rows <- pass$stopping_condition == "rolling_cv"
  if (any(!pass$converged[converged_rows])) {
    stop("fixture rolling-CV summaries are not marked converged")
  }
  not_measured <- summaries$status != "PASS"
  not_measured_numeric <- c(
    "mean_ms", "median_ms", "min_ms", "max_ms", "sd_ms", "cv_pct", "rss_kb", "cold_start_ms",
    "n_iterations", "convergence_cv_pct"
  )
  if (any(!vapply(summaries[not_measured, not_measured_numeric, drop = FALSE], function(values) {
    all(is.na(values))
  }, logical(1))) ||
      any(as.character(summaries$stopping_condition[not_measured]) != "not_measured") ||
      any(!is.na(summaries$converged[not_measured])) ||
      any(as.character(summaries$timer_noise_status[not_measured]) != "not_measured")) {
    stop("untimed fixture summaries contain measurement evidence")
  }
  layout <- benchmark_artifact_layout(metadata)
  if (identical(layout, "grouped-v1")) {
    shared_samples <- run_sample_artifact_paths(run_dir, metadata, "fixture", expected_runners[[1L]], ".")
    if (!identical(file.exists(shared_samples), nrow(pass) > 0L)) {
      stop("shared fixture timing artifact presence differs from PASS summaries")
    }
    if (dir.exists(file.path(run_dir, "fixtures"))) {
      stop("grouped run retains per-runner fixture artifact directories")
    }
  }
  for (runner in expected_runners) {
    runner_rows <- pass[pass$runner == runner, , drop = FALSE]
    raw_dir <- file.path(run_dir, "fixtures", runner)
    expected_raw <- sort(as.character(runner_rows$row_id))
    if (identical(layout, "per-cell-v1")) {
      expected_raw_files <- basename(run_sample_artifact_paths(run_dir, metadata, "fixture", runner, expected_raw))
      actual_raw_files <- if (dir.exists(raw_dir)) list.files(raw_dir, pattern = "^[^.].*\\.csv$") else character(0)
      if (!identical(sort(unique(expected_raw_files)), sort(actual_raw_files))) {
        stop(sprintf("fixture raw timing artifact set differs for %s", runner))
      }
    }
    raw_samples <- read_run_sample_table(run_dir, metadata, "fixture", runner, expected_raw)
    actual_raw <- sort(unique(as.character(raw_samples$row_id)))
    if (!identical(actual_raw, expected_raw)) {
      stop(sprintf("fixture raw timing coverage differs for %s", runner))
    }
    required_raw <- c("run_id", "runner", "fixture", "variant", "row_id", "iteration", "wall_ms")
    if (nrow(raw_samples) > 0L && length(setdiff(required_raw, names(raw_samples))) > 0L) {
      stop(sprintf("fixture raw timing columns differ for %s", runner))
    }
    for (row_id in expected_raw) {
      raw <- raw_samples[as.character(raw_samples$row_id) == row_id, , drop = FALSE]
      summary <- runner_rows[runner_rows$row_id == row_id, , drop = FALSE]
      identity_matches <- nrow(summary) == 1L &&
        all(as.character(raw$run_id) == as.character(metadata$run_id)) &&
        all(as.character(raw$runner) == runner) &&
        all(as.character(raw$fixture) == as.character(summary$fixture)) &&
        all(as.character(raw$variant) == as.character(summary$variant)) &&
        all(as.character(raw$row_id) == row_id)
      if (!identity_matches || nrow(raw) != as.integer(summary$n_iterations) ||
          !identical(as.integer(raw$iteration), seq_len(nrow(raw))) ||
          any(!is.finite(raw$wall_ms) | raw$wall_ms < 0)) {
        stop(sprintf("fixture raw sample count differs for %s/%s", runner, row_id))
      }
      validate_fixture_raw_statistics(summary, raw, policy, paste(runner, row_id, sep = "/"))
    }
  }
  invisible(TRUE)
}
