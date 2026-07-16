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

# Prepared-call timing, process-memory observation, and result logging.

library(microbenchmark)

current_rss_kb <- function() {
  if (.Platform$OS.type != "unix" || !file.exists("/proc/self/status")) return(NA_integer_)
  tryCatch({
    lines <- readLines("/proc/self/status")
    line  <- grep("^VmRSS:", lines, value = TRUE)
    if (length(line) == 0) return(NA_integer_)
    as.integer(sub(".*?([0-9]+).*", "\\1", line[1]))
  }, error = function(e) NA_integer_)
}

rss_endpoint_support_reason <- function() {
  if (.Platform$OS.type != "unix") return("current process RSS is unsupported outside Unix hosts")
  if (!file.exists("/proc/self/status")) return("current process RSS requires Linux /proc/self/status")
  "available"
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

prepared_call_function <- function(prepared, parent = parent.frame()) {
  if (is.function(prepared)) return(prepared)
  if (!is.call(prepared)) stop("prepared timing value must be a function or call")
  parts <- as.list(prepared)
  interface <- as.character(parts[[1L]])
  native_interface <- interface %in% c(".Call", ".C", ".External")
  target <- if (native_interface) {
    parts[[2L]]
  } else {
    get(interface, envir = parent, inherits = TRUE)
  }
  arguments <- if (native_interface) {
    if (length(parts) <= 2L) list() else parts[-c(1L, 2L)]
  } else if (length(parts) <= 1L) list() else parts[-1L]
  if (length(arguments) > 2L) stop("prepared timing calls support at most two arguments")
  arg1 <- if (length(arguments) >= 1L) arguments[[1L]] else NULL
  arg2 <- if (length(arguments) >= 2L) arguments[[2L]] else NULL
  if (identical(interface, ".Call")) {
    return(switch(as.character(length(arguments) + 1L),
      "1" = function() .Call(target),
      "2" = function() .Call(target, arg1),
      "3" = function() .Call(target, arg1, arg2)
    ))
  }
  if (identical(interface, ".C")) {
    return(switch(as.character(length(arguments) + 1L),
      "1" = function() .C(target),
      "2" = function() .C(target, arg1),
      "3" = function() .C(target, arg1, arg2)
    ))
  }
  if (identical(interface, ".External")) {
    return(switch(as.character(length(arguments) + 1L),
      "1" = function() .External(target),
      "2" = function() .External(target, arg1),
      "3" = function() .External(target, arg1, arg2)
    ))
  }
  switch(as.character(length(arguments) + 1L),
    "1" = function() target(),
    "2" = function() target(arg1),
    "3" = function() target(arg1, arg2)
  )
}

measure_first_call <- function(prepare_call) {
  gc(full = TRUE)
  prepared <- tryCatch(list(ok = TRUE, expression = prepare_call()), error = function(error) {
    list(ok = FALSE, error = conditionMessage(error))
  })
  if (!isTRUE(prepared$ok)) {
    return(list(wall_ms = NA_real_, error = paste("first-call input preparation failed:", prepared$error)))
  }
  error <- NA_character_
  wall_start <- get_nanotime()
  tryCatch(evaluate_prepared_call(prepared$expression), error = function(e) { error <<- conditionMessage(e) })
  wall_end <- get_nanotime()
  list(
    wall_ms = (wall_end - wall_start) / 1e6,
    error = error
  )
}

get_nanotime <- function() {
  microbenchmark::get_nanotime()
}

benchmark_timing_policy <- function() {
  list(
    policy_version = "bounded-pilot-confirmation-v2",
    warmup_iterations = 10L,
    pilot_iterations = 20L,
    confirmation_min_iterations = 20L,
    confirmation_max_iterations = 500L,
    confirmation_target_cv_pct = 5.0,
    group_time_cap_ms = 15000,
    batch_time_cap_ms = 60000,
    batch_group_cap = 8L,
    batch_timeout_seconds = 90L,
    total_run_budget_seconds = 7200L,
    timer_noise_floor_ms = 0.01,
    timer_noise_floor_method = "fixed 0.01 ms floor rounded above empty-eval p99 calibration",
    low_noise_cv_threshold_pct = 20.0,
    meaningful_margin_ratio = 1.05,
    median_ci_level = 0.95,
    median_ci_method = "exact order-statistic interval",
    rss_endpoint_metric = "post_gc_current_rss_endpoint_delta_kb",
    peak_rss_metric = "linux_proc_status_vmhwm_kb",
    peak_rss_repetitions = 3L,
    peak_rss_timeout_seconds = 60L,
    peak_rss_fixture_ids = c("F03", "F04", "F06"),
    gc_policy = "full before warmup and timed sequence; full after timed sequence before endpoint RSS; no forced GC between timed samples"
  )
}

benchmark_call <- function(prepare_warmup, prepare_timed, iterations, warmup = 10L,
                           fresh_each_iteration = FALSE,
                           timer_noise_floor_ms = 0.01,
                           rss_endpoint_metric = "post_gc_current_rss_endpoint_delta_kb") {

  iterations <- as.integer(iterations)
  if (length(iterations) != 1L || is.na(iterations) || iterations < 1L) {
    stop("fixed timing iteration count must be a positive integer")
  }

  gc(full = TRUE)

  fixed_call <- if (isTRUE(fresh_each_iteration)) NULL else {
    prepared_call_function(prepare_timed(), parent.frame())
  }
  for (i in seq_len(warmup)) {
    prepared <- tryCatch(list(
      ok = TRUE,
      expression = if (is.null(fixed_call)) {
        prepared_call_function(prepare_warmup(), parent.frame())
      } else fixed_call
    ), error = function(error) {
      list(ok = FALSE, error = conditionMessage(error))
    })
    if (!isTRUE(prepared$ok)) return(list(error = paste("warmup input preparation failed:", prepared$error)))
    call_ok <- TRUE
    t0 <- get_nanotime()
    tryCatch(prepared$expression(), error = function(error) {
      call_ok <<- FALSE
    })
    t1 <- get_nanotime()
    if (!call_ok) return(list(error = "warmup failed"))
  }

  gc(full = TRUE)
  rss_before <- current_rss_kb()

  planning_times <- numeric(iterations)
  measured <- tryCatch({
    if (isTRUE(fresh_each_iteration)) {
      times <- numeric(iterations)
      for (sample_index in seq_len(iterations)) {
        sample_started <- get_nanotime()
        prepared <- prepared_call_function(prepare_timed(), parent.frame())
        expression <- as.call(list(prepared))
        sample <- microbenchmark::microbenchmark(
          list = list(call = expression), times = 1L, unit = "ns"
        )
        times[[sample_index]] <- as.numeric(sample$time[[1L]]) / 1e6
        planning_times[[sample_index]] <- (get_nanotime() - sample_started) / 1e6
      }
      times
    } else {
      batch_started <- get_nanotime()
      expression <- as.call(list(fixed_call))
      sample <- microbenchmark::microbenchmark(
        list = list(call = expression), times = iterations, unit = "ns"
      )
      planning_times[] <- (get_nanotime() - batch_started) / 1e6 / iterations
      as.numeric(sample$time) / 1e6
    }
  }, error = function(error) error)
  if (inherits(measured, "error")) {
    return(list(error = paste("timed call failed:", conditionMessage(measured))))
  }
  all_times <- measured

  n <- length(all_times)
  mean_ms <- mean(all_times)
  median_ms <- median(all_times)
  min_ms <- min(all_times)
  max_ms <- max(all_times)
  sd_ms <- sd(all_times)
  cv_pct <- if (mean_ms > 0) sd_ms / mean_ms * 100 else 0

  gc(full = TRUE)
  rss_after <- current_rss_kb()
  rss_supported <- !is.na(rss_before) && !is.na(rss_after)
  rss_delta <- if (rss_supported) max(0L, rss_after - rss_before) else NA_integer_
  rss_reason <- if (rss_supported) {
    "available"
  } else {
    reason <- rss_endpoint_support_reason()
    if (identical(reason, "available")) "current process RSS reading failed" else reason
  }

  list(
    times      = all_times,
    planning_times = planning_times,
    n_runs     = n,
    warmup_iterations = warmup,
    fixed_iterations = iterations,
    mean_ms    = mean_ms,
    median_ms  = median_ms,
    min_ms     = min_ms,
    max_ms     = max_ms,
    sd_ms      = sd_ms,
    cv_pct     = cv_pct,
    timer_noise_floor_ms = timer_noise_floor_ms,
    timer_noise_status = if (median_ms < timer_noise_floor_ms) "below_floor" else "above_floor",
    rss_endpoint_metric = rss_endpoint_metric,
    rss_endpoint_delta_kb = rss_delta,
    rss_endpoint_support = if (rss_supported) "supported" else "unsupported",
    rss_endpoint_support_reason = rss_reason,
    error      = NA_character_
  )
}

validate_rss_endpoint_support <- function(rows, measured, policy, label) {
  required <- c(
    "rss_endpoint_delta_kb", "rss_endpoint_metric", "rss_endpoint_support",
    "rss_endpoint_support_reason"
  )
  missing <- setdiff(required, names(rows))
  if (length(missing) > 0L) stop(sprintf("%s lacks endpoint RSS columns", label))
  metrics <- as.character(rows$rss_endpoint_metric)
  if (anyNA(metrics) || any(metrics != as.character(policy$rss_endpoint_metric))) {
    stop(sprintf("%s endpoint RSS metric differs from policy", label))
  }
  measured <- as.logical(measured)
  states <- as.character(rows$rss_endpoint_support)
  if (length(measured) != nrow(rows) || anyNA(measured) || anyNA(states)) {
    stop(sprintf("%s has an invalid endpoint RSS support state", label))
  }
  supported <- measured & states == "supported"
  unsupported <- measured & states == "unsupported"
  if (any(measured & !(supported | unsupported)) ||
      any(!measured & states != "not_measured")) {
    stop(sprintf("%s has an invalid endpoint RSS support state", label))
  }
  values <- strict_metric_numeric(rows$rss_endpoint_delta_kb, paste(label, "endpoint RSS"))
  reasons <- as.character(rows$rss_endpoint_support_reason)
  if (anyNA(reasons) || any(!nzchar(reasons)) ||
      any(!is.finite(values[supported]) | values[supported] < 0) ||
      any(!is.na(values[unsupported | !measured])) ||
      any(reasons[supported] != "available") ||
      any(reasons[unsupported | !measured] == "available") ||
      any(!nzchar(reasons[unsupported | !measured]))) {
    stop(sprintf("%s endpoint RSS value disagrees with its support state", label))
  }
  invisible(rows)
}

strict_metric_numeric <- function(values, label) {
  missing <- is.na(values)
  numeric <- suppressWarnings(as.numeric(as.character(values)))
  if (any(!missing & is.na(numeric))) stop(sprintf("%s contains a non-numeric value", label))
  numeric
}

validate_rss_endpoint_raw <- function(summary, raw, label) {
  if (nrow(summary) != 1L || !("rss_endpoint_delta_kb" %in% names(raw))) {
    stop(sprintf("%s lacks endpoint RSS evidence", label))
  }
  raw_values <- strict_metric_numeric(raw$rss_endpoint_delta_kb, paste(label, "raw endpoint RSS"))
  summary_value <- strict_metric_numeric(summary$rss_endpoint_delta_kb, paste(label, "summary endpoint RSS"))
  supported <- identical(as.character(summary$rss_endpoint_support[[1L]]), "supported")
  unsupported <- identical(as.character(summary$rss_endpoint_support[[1L]]), "unsupported")
  observed <- which(!is.na(raw_values))
  valid <- if (supported) {
    length(observed) == 1L && observed[[1L]] == nrow(raw) &&
      identical(raw_values[[observed]], summary_value[[1L]])
  } else {
    unsupported && length(observed) == 0L && is.na(summary_value[[1L]])
  }
  if (!isTRUE(valid)) stop(sprintf("%s raw endpoint RSS differs from its summary", label))
  invisible(raw)
}

validate_first_call_raw <- function(summary, raw, label) {
  required <- c("iteration", "wall_ms")
  if (nrow(summary) != 1L || nrow(raw) != 1L || length(setdiff(required, names(raw))) > 0L ||
      as.integer(raw$iteration[[1L]]) != 1L || !is.finite(as.numeric(raw$wall_ms[[1L]])) ||
      round(as.numeric(raw$wall_ms[[1L]]), 3) != as.numeric(summary$first_call_ms[[1L]]) ||
      ("error" %in% names(raw) && !is.na(raw$error[[1L]]))) {
    stop(sprintf("%s raw first-call timing differs from its summary", label))
  }
  invisible(raw)
}

validate_first_call_metric <- function(rows, measured, label) {
  if (!("first_call_ms" %in% names(rows))) stop(sprintf("%s lacks first_call_ms", label))
  measured <- as.logical(measured)
  if (length(measured) != nrow(rows) || anyNA(measured)) {
    stop(sprintf("%s has an invalid first-call measurement", label))
  }
  values <- as.numeric(rows$first_call_ms)
  if (any(!is.finite(values[measured]) | values[measured] < 0) || any(!is.na(values[!measured]))) {
    stop(sprintf("%s has an invalid first-call measurement", label))
  }
  invisible(rows)
}

parse_proc_status_memory <- function(lines) {
  value <- function(name) {
    match <- grep(paste0("^", name, ":"), lines, value = TRUE)
    if (length(match) != 1L || !grepl("^[^:]+:[[:space:]]*[0-9]+[[:space:]]+kB[[:space:]]*$", match)) {
      return(NA_integer_)
    }
    as.integer(sub("^[^:]+:[[:space:]]*([0-9]+)[[:space:]]+kB[[:space:]]*$", "\\1", match))
  }
  list(loaded_process_rss_kb = value("VmRSS"), peak_rss_kb = value("VmHWM"))
}

peak_rss_host_support <- function(status_path = "/proc/self/status") {
  linux <- .Platform$OS.type == "unix" && identical(unname(Sys.info()[["sysname"]]), "Linux")
  if (!linux) {
    return(list(supported = FALSE, reason = "gross peak RSS is supported only on Linux /proc"))
  }
  if (!file.exists(status_path)) {
    return(list(supported = FALSE, reason = "gross peak RSS requires /proc/self/status"))
  }
  snapshot <- tryCatch(parse_proc_status_memory(readLines(status_path)), error = function(error) NULL)
  supported <- !is.null(snapshot) && !is.na(snapshot$loaded_process_rss_kb) && !is.na(snapshot$peak_rss_kb)
  list(
    supported = supported,
    reason = if (supported) "available" else "Linux /proc/self/status lacks VmRSS or VmHWM"
  )
}

measure_peak_process_rss <- function(prepare_call, repetitions, status_path = "/proc/self/status") {
  repetitions <- as.integer(repetitions)
  if (length(repetitions) != 1L || is.na(repetitions) || repetitions < 1L) {
    stop("peak RSS repetition count must be a positive integer")
  }
  support <- peak_rss_host_support(status_path)
  if (!isTRUE(support$supported)) {
    return(list(
      peak_rss_kb = NA_integer_, loaded_process_rss_kb = NA_integer_,
      peak_rss_support = "unsupported", peak_rss_support_reason = as.character(support$reason),
      peak_rss_repetitions = repetitions
    ))
  }
  gc(full = TRUE)
  loaded <- parse_proc_status_memory(readLines(status_path))$loaded_process_rss_kb
  retained <- vector("list", repetitions)
  for (index in seq_len(repetitions)) {
    prepared <- prepare_call()
    retained[[index]] <- evaluate_prepared_call(prepared)
  }
  peak <- parse_proc_status_memory(readLines(status_path))$peak_rss_kb
  if (is.na(loaded) || is.na(peak) || peak < loaded) {
    stop("peak RSS reading is invalid")
  }
  list(
    peak_rss_kb = peak, loaded_process_rss_kb = loaded,
    peak_rss_support = "supported", peak_rss_support_reason = "available",
    peak_rss_repetitions = repetitions
  )
}

peak_rss_fixture_eligible <- function(fixture, variant, status, policy) {
  fixture <- as.character(fixture)
  variant <- as.character(variant)
  eligible_variant <- variant == "public" |
    (variant == "optimized_base_r" & fixture %in% names(fixture_measurement_optimized_specs()))
  as.character(status) == "PASS" & fixture %in% as.character(policy$peak_rss_fixture_ids) & eligible_variant
}

apply_peak_rss_results <- function(summaries, results, policy, host_support) {
  required_summary <- c("run_id", "runner", "fixture", "variant", "row_id", "status")
  if (length(setdiff(required_summary, names(summaries))) > 0L) stop("fixture summaries lack peak RSS identity columns")
  eligible <- peak_rss_fixture_eligible(summaries$fixture, summaries$variant, summaries$status, policy)
  summaries$peak_rss_kb <- NA_integer_
  summaries$loaded_process_rss_kb <- NA_integer_
  summaries$peak_rss_metric <- as.character(policy$peak_rss_metric)
  summaries$peak_rss_support <- "not_eligible"
  summaries$peak_rss_support_reason <- "workload is not declared memory eligible"
  summaries$peak_rss_repetitions <- NA_integer_
  if (!isTRUE(host_support$supported)) {
    if (!is.null(results) && nrow(results) > 0L) stop("unsupported host produced peak RSS results")
    summaries$peak_rss_support[eligible] <- "unsupported"
    summaries$peak_rss_support_reason[eligible] <- as.character(host_support$reason)
    summaries$peak_rss_repetitions[eligible] <- as.integer(policy$peak_rss_repetitions)
    return(summaries)
  }
  expected_keys <- paste(summaries$runner[eligible], summaries$row_id[eligible], sep = "\r")
  if (length(expected_keys) == 0L) {
    if (!is.null(results) && nrow(results) > 0L) stop("peak RSS results exist without eligible rows")
    return(summaries)
  }
  required_results <- c(
    "run_id", "runner", "fixture", "variant", "row_id", "peak_rss_kb", "loaded_process_rss_kb",
    "peak_rss_metric", "peak_rss_support", "peak_rss_support_reason", "peak_rss_repetitions"
  )
  if (is.null(results) || length(setdiff(required_results, names(results))) > 0L) {
    stop("peak RSS results are missing required columns")
  }
  result_keys <- paste(results$runner, results$row_id, sep = "\r")
  if (!setequal(expected_keys, result_keys) || anyDuplicated(result_keys)) {
    stop("peak RSS result coverage differs from declared memory-eligible rows")
  }
  results <- results[match(expected_keys, result_keys), , drop = FALSE]
  identity_valid <- as.character(results$run_id) == as.character(summaries$run_id[eligible]) &
    as.character(results$fixture) == as.character(summaries$fixture[eligible]) &
    as.character(results$variant) == as.character(summaries$variant[eligible]) &
    as.character(results$peak_rss_metric) == as.character(policy$peak_rss_metric) &
    as.integer(results$peak_rss_repetitions) == as.integer(policy$peak_rss_repetitions)
  supported <- as.character(results$peak_rss_support) == "supported" &
    as.character(results$peak_rss_support_reason) == "available" &
    is.finite(as.numeric(results$peak_rss_kb)) & as.numeric(results$peak_rss_kb) > 0 &
    is.finite(as.numeric(results$loaded_process_rss_kb)) & as.numeric(results$loaded_process_rss_kb) > 0 &
    as.numeric(results$peak_rss_kb) >= as.numeric(results$loaded_process_rss_kb)
  unsupported <- as.character(results$peak_rss_support) == "unsupported" &
    is.na(results$peak_rss_kb) & is.na(results$loaded_process_rss_kb) &
    !is.na(results$peak_rss_support_reason) & nzchar(as.character(results$peak_rss_support_reason)) &
    as.character(results$peak_rss_support_reason) != "available"
  identity_valid[is.na(identity_valid)] <- FALSE
  supported[is.na(supported)] <- FALSE
  unsupported[is.na(unsupported)] <- FALSE
  if (any(!identity_valid | !(supported | unsupported))) {
    stop("peak RSS result value or identity is invalid")
  }
  summaries$peak_rss_kb[eligible] <- as.integer(results$peak_rss_kb)
  summaries$loaded_process_rss_kb[eligible] <- as.integer(results$loaded_process_rss_kb)
  summaries$peak_rss_support[eligible] <- as.character(results$peak_rss_support)
  summaries$peak_rss_support_reason[eligible] <- as.character(results$peak_rss_support_reason)
  summaries$peak_rss_repetitions[eligible] <- as.integer(results$peak_rss_repetitions)
  summaries
}

validate_peak_rss_support <- function(rows, policy, universe, label) {
  required <- c(
    "peak_rss_kb", "loaded_process_rss_kb", "peak_rss_metric", "peak_rss_support",
    "peak_rss_support_reason", "peak_rss_repetitions"
  )
  if (length(setdiff(required, names(rows))) > 0L) stop(sprintf("%s lacks peak RSS columns", label))
  metrics <- as.character(rows$peak_rss_metric)
  if (anyNA(metrics) || any(metrics != as.character(policy$peak_rss_metric))) {
    stop(sprintf("%s peak RSS metric differs from policy", label))
  }
  eligible <- if (identical(universe, "fixture")) {
    peak_rss_fixture_eligible(rows$fixture, rows$variant, rows$status, policy)
  } else {
    rep(FALSE, nrow(rows))
  }
  states <- as.character(rows$peak_rss_support)
  if (anyNA(states)) stop(sprintf("%s has an invalid peak RSS support state", label))
  supported <- eligible & states == "supported"
  unsupported <- eligible & states == "unsupported"
  not_eligible <- !eligible & states == "not_eligible"
  if (any(!(supported | unsupported | not_eligible))) {
    stop(sprintf("%s has an invalid peak RSS support state", label))
  }
  peak <- strict_metric_numeric(rows$peak_rss_kb, paste(label, "peak RSS"))
  loaded <- strict_metric_numeric(rows$loaded_process_rss_kb, paste(label, "loaded-process RSS"))
  repetitions <- strict_metric_numeric(rows$peak_rss_repetitions, paste(label, "peak RSS repetitions"))
  reasons <- as.character(rows$peak_rss_support_reason)
  if (anyNA(reasons) || any(!nzchar(reasons)) ||
      any(!is.finite(peak[supported]) | !is.finite(loaded[supported]) |
          peak[supported] < loaded[supported] | loaded[supported] <= 0) ||
      anyNA(repetitions[supported | unsupported]) ||
      any(repetitions[supported | unsupported] != as.integer(policy$peak_rss_repetitions)) ||
      any(!is.na(peak[unsupported | not_eligible]) | !is.na(loaded[unsupported | not_eligible])) ||
      any(!is.na(repetitions[not_eligible])) || any(reasons[supported] != "available") ||
      any(reasons[unsupported | not_eligible] == "available") ||
      any(!nzchar(reasons[unsupported | not_eligible]))) {
    stop(sprintf("%s peak RSS value disagrees with its support state", label))
  }
  invisible(rows)
}

ordered_selection <- function(available, selected, label) {
  indices <- match(as.character(selected), as.character(available))
  if (anyNA(indices) || anyDuplicated(indices)) stop(sprintf("%s differs from available IDs", label))
  indices
}

timing_group_schedule <- function(group_ids, seed) {
  group_ids <- as.character(group_ids)
  if (anyNA(group_ids) || any(!nzchar(group_ids)) || anyDuplicated(group_ids)) {
    stop("timing group IDs must be unique non-empty values")
  }
  seed <- input_scalar_integer(seed, "timing schedule seed")
  prior_seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  } else NULL
  on.exit({
    if (is.null(prior_seed)) {
      if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
    } else assign(".Random.seed", prior_seed, envir = .GlobalEnv)
  }, add = TRUE)
  set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
  realized <- if (length(group_ids) > 1L) sample(group_ids, length(group_ids)) else group_ids
  data.frame(group_id = realized, group_order = seq_along(realized), stringsAsFactors = FALSE)
}

timing_batch_schedule <- function(batches, runners) {
  runners <- as.character(runners)
  if (length(runners) < 1L || anyNA(runners) || any(!nzchar(runners)) || anyDuplicated(runners)) {
    stop("timing runners must be unique non-empty values")
  }
  do.call(rbind, lapply(seq_len(nrow(batches)), function(index) {
    batch <- as.integer(batches$batch[[index]])
    offset <- (batch - 1L) %% length(runners)
    order <- runners[((seq_along(runners) - 1L + offset) %% length(runners)) + 1L]
    data.frame(
      group_id = as.character(batches$group_id[[index]]),
      group_order = as.integer(batches$group_order[[index]]), batch = batch,
      runner = order, member_order = seq_along(order), stringsAsFactors = FALSE
    )
  }))
}

pilot_group_plan <- function(samples, policy = benchmark_timing_policy()) {
  required <- c("group_id", "member_id", "iteration", "wall_ms", "planning_ms")
  missing <- setdiff(required, names(samples))
  if (length(missing) > 0L) stop(sprintf("pilot samples missing fields: %s", paste(missing, collapse = ", ")))
  if (nrow(samples) == 0L) return(data.frame())
  samples$group_id <- as.character(samples$group_id)
  samples$member_id <- as.character(samples$member_id)
  samples$iteration <- as.integer(samples$iteration)
  samples$wall_ms <- as.numeric(samples$wall_ms)
  samples$planning_ms <- as.numeric(samples$planning_ms)
  if (anyNA(samples[c("group_id", "member_id", "iteration", "wall_ms", "planning_ms")]) ||
      any(!nzchar(samples$group_id)) || any(!nzchar(samples$member_id)) ||
      any(!is.finite(samples$wall_ms)) || any(samples$wall_ms < 0) ||
      any(!is.finite(samples$planning_ms)) || any(samples$planning_ms < samples$wall_ms)) {
    stop("pilot samples contain invalid values")
  }
  keys <- paste(samples$group_id, samples$member_id, sep = "\r")
  split_rows <- split(seq_len(nrow(samples)), keys)
  members <- do.call(rbind, lapply(split_rows, function(indices) {
    values <- samples$wall_ms[indices]
    iterations <- sort(samples$iteration[indices])
    data.frame(
      group_id = samples$group_id[indices[[1L]]], member_id = samples$member_id[indices[[1L]]],
      n = length(values),
      complete = identical(iterations, seq_len(as.integer(policy$pilot_iterations))),
      median_ms = median(values),
      planning_median_ms = median(samples$planning_ms[indices]),
      cv_pct = if (mean(values) > 0) sd(values) / mean(values) * 100 else 0,
      drift_pct = if (length(values) >= 4L) {
        half <- floor(length(values) / 2L)
        first <- median(values[seq_len(half)])
        second <- median(tail(values, half))
        if (first > 0) abs(second / first - 1) * 100 else 0
      } else NA_real_, stringsAsFactors = FALSE
    )
  }))
  groups <- split(seq_len(nrow(members)), members$group_id)
  do.call(rbind, lapply(groups, function(indices) {
    rows <- members[indices, , drop = FALSE]
    complete <- all(rows$complete)
    above_floor <- all(rows$median_ms >= as.numeric(policy$timer_noise_floor_ms))
    group_cost <- sum(rows$median_ms)
    planning_group_cost <- sum(rows$planning_median_ms)
    worst_cv <- max(rows$cv_pct, na.rm = TRUE)
    desired <- ceiling(as.integer(policy$confirmation_min_iterations) *
      max(1, (worst_cv / as.numeric(policy$confirmation_target_cv_pct)) ^ 2))
    affordable <- if (planning_group_cost > 0) {
      floor(as.numeric(policy$group_time_cap_ms) / planning_group_cost)
    } else 0L
    count <- min(as.integer(policy$confirmation_max_iterations), desired, affordable)
    status <- if (!complete) "incomplete" else if (!above_floor) "below_timer_floor" else if (
      count < as.integer(policy$confirmation_min_iterations)
    ) "incomplete" else "confirmation"
    data.frame(
      group_id = rows$group_id[[1L]], pilot_complete = complete,
      pilot_median_group_ms = group_cost, pilot_max_cv_pct = worst_cv,
      pilot_median_planning_group_ms = planning_group_cost,
      pilot_max_drift_pct = max(rows$drift_pct, na.rm = TRUE),
      confirmation_iterations = if (identical(status, "confirmation")) as.integer(count) else NA_integer_,
      estimated_confirmation_ms = if (identical(status, "confirmation")) count * planning_group_cost else NA_real_,
      status = status, stringsAsFactors = FALSE
    )
  }))
}

pack_timing_batches <- function(groups, policy = benchmark_timing_policy()) {
  required <- c("group_id", "group_order", "estimated_ms")
  missing <- setdiff(required, names(groups))
  if (length(missing) > 0L) stop(sprintf("timing groups missing fields: %s", paste(missing, collapse = ", ")))
  if (nrow(groups) == 0L) return(transform(groups, batch = integer()))
  groups <- groups[order(as.integer(groups$group_order)), , drop = FALSE]
  batch <- integer(nrow(groups))
  batch_id <- 1L
  batch_count <- 0L
  batch_ms <- 0
  for (index in seq_len(nrow(groups))) {
    cost <- as.numeric(groups$estimated_ms[[index]])
    if (!is.finite(cost) || cost < 0) stop("timing group estimate must be finite and non-negative")
    would_overflow <- batch_count > 0L && (
      batch_count >= as.integer(policy$batch_group_cap) ||
      batch_ms + cost > as.numeric(policy$batch_time_cap_ms)
    )
    if (would_overflow) {
      batch_id <- batch_id + 1L
      batch_count <- 0L
      batch_ms <- 0
    }
    batch[[index]] <- batch_id
    batch_count <- batch_count + 1L
    batch_ms <- batch_ms + cost
  }
  groups$batch <- batch
  groups
}

admit_timing_budget <- function(groups, budget_ms) {
  required <- c("universe", "group_id", "estimated_ms")
  missing <- setdiff(required, names(groups))
  if (length(missing) > 0L) stop(sprintf("timing budget groups missing fields: %s", paste(missing, collapse = ", ")))
  budget_ms <- as.numeric(budget_ms)
  if (length(budget_ms) != 1L || !is.finite(budget_ms) || budget_ms < 0) {
    stop("timing budget must be one finite non-negative value")
  }
  if (nrow(groups) == 0L) {
    groups$admitted <- logical()
    groups$remaining_after_ms <- numeric()
    return(groups)
  }
  estimates <- as.numeric(groups$estimated_ms)
  if (any(!is.finite(estimates)) || any(estimates < 0)) {
    stop("timing budget estimates must be finite and non-negative")
  }
  remaining <- budget_ms
  admitted <- logical(nrow(groups))
  remaining_after <- numeric(nrow(groups))
  for (index in seq_len(nrow(groups))) {
    admitted[[index]] <- estimates[[index]] <= remaining
    if (admitted[[index]]) remaining <- remaining - estimates[[index]]
    remaining_after[[index]] <- remaining
  }
  groups$admitted <- admitted
  groups$remaining_after_ms <- remaining_after
  groups
}

run_timing_batches <- function(batches, execute, timeout_seconds, total_budget_seconds,
                               started_at = proc.time()[["elapsed"]]) {
  required <- c("batch", "group_id")
  missing <- setdiff(required, names(batches))
  if (length(missing) > 0L) stop(sprintf("timing batches missing fields: %s", paste(missing, collapse = ", ")))
  outcomes <- list()
  batch_epoch <- 0L
  run_one <- function(rows, attempt) {
    batch_epoch <<- batch_epoch + 1L
    elapsed <- proc.time()[["elapsed"]] - started_at
    remaining <- total_budget_seconds - elapsed
    if (remaining <= 0) {
      return(list(ok = FALSE, timed_out = FALSE, budget_exhausted = TRUE, batch_epoch = batch_epoch))
    }
    result <- execute(rows, min(timeout_seconds, remaining), attempt, batch_epoch)
    if (!is.list(result) || is.null(result$ok) || is.null(result$timed_out)) {
      stop("timing batch executor returned an invalid result")
    }
    result$budget_exhausted <- FALSE
    result$batch_epoch <- batch_epoch
    result
  }
  for (batch_id in unique(as.integer(batches$batch))) {
    rows <- batches[as.integer(batches$batch) == batch_id, , drop = FALSE]
    first <- run_one(rows, 1L)
    if (isTRUE(first$ok)) {
      outcomes[[length(outcomes) + 1L]] <- data.frame(
        group_id = rows$group_id, batch = batch_id, attempt = 1L,
        batch_epoch = first$batch_epoch, status = "complete", stringsAsFactors = FALSE
      )
      next
    }
    if (!isTRUE(first$timed_out) || isTRUE(first$budget_exhausted)) {
      outcomes[[length(outcomes) + 1L]] <- data.frame(
        group_id = rows$group_id, batch = batch_id, attempt = 1L,
        batch_epoch = first$batch_epoch,
        status = if (isTRUE(first$budget_exhausted)) "incomplete_budget" else "failed",
        stringsAsFactors = FALSE
      )
      next
    }
    splits <- as.list(seq_len(nrow(rows)))
    for (indices in splits) {
      retry <- run_one(rows[indices, , drop = FALSE], 2L)
      outcomes[[length(outcomes) + 1L]] <- data.frame(
        group_id = rows$group_id[indices], batch = batch_id, attempt = 2L,
        batch_epoch = retry$batch_epoch,
        status = if (isTRUE(retry$ok)) "complete" else if (isTRUE(retry$budget_exhausted)) {
          "incomplete_budget"
        } else if (isTRUE(retry$timed_out)) "incomplete_timeout" else "failed",
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(outcomes) == 0L) data.frame() else do.call(rbind, outcomes)
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
  if (length(schema) != 1L || is.na(schema) || schema != 3L) {
    stop("unsupported run manifest schema version")
  }
  layout <- as.character(metadata$artifact_layout)
  if (length(layout) != 1L || is.na(layout) || !identical(layout, "grouped-v1")) {
    stop("run manifest has no supported artifact layout")
  }
  layout
}

run_summary_artifact_paths <- function(run_dir, metadata, universe, runners = character(0)) {
  if (!(universe %in% c("task", "fixture"))) stop("summary universe must be task or fixture")
  rooted <- function(path) if (length(run_dir) == 1L && !is.na(run_dir) && nzchar(run_dir)) file.path(run_dir, path) else path
  benchmark_artifact_layout(metadata)
  rooted(paste0(universe, "_summary.csv"))
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
  benchmark_artifact_layout(metadata)
  rooted(file.path("correctness", paste0(universe, "s.csv")))
}

run_sample_artifact_paths <- function(run_dir, metadata, universe, runner, ids = character(0)) {
  if (!(universe %in% c("task", "fixture"))) stop("sample universe must be task or fixture")
  ids <- as.character(ids)
  if (length(ids) == 0L) return(character(0))
  benchmark_artifact_layout(metadata)
  path <- paste0(universe, "_samples.csv")
  if (length(run_dir) == 1L && !is.na(run_dir) && nzchar(run_dir)) file.path(run_dir, path) else path
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
  if (!("runner" %in% names(samples))) stop("grouped raw timing samples lack runner")
  samples <- samples[as.character(samples$runner) == runner, , drop = FALSE]
  samples
}

read_run_wall_time_samples <- function(
    run_dir, metadata, universe, runner, id, expected_n = NULL, minimum_n = 2L, stage = NULL) {
  paths <- run_sample_artifact_paths(run_dir, metadata, universe, runner, id)
  filters <- if (identical(universe, "task")) list(runner = runner, task = id) else list(runner = runner, row_id = id)
  header_names <- names(read.csv(paths[[1L]], nrows = 0L, stringsAsFactors = FALSE))
  if ("phase" %in% header_names) filters$phase <- "timed"
  if (is.null(stage)) {
    if ("stage" %in% header_names) {
      header <- read.csv(paths[[1L]], stringsAsFactors = FALSE)
      keep <- rep(TRUE, nrow(header))
      for (field in names(filters)) keep <- keep & as.character(header[[field]]) == as.character(filters[[field]])
      stages <- unique(as.character(header$stage[keep]))
      stage <- if ("confirmation" %in% stages) "confirmation" else if ("pilot" %in% stages) "pilot" else NULL
    }
  }
  if (!is.null(stage) && "stage" %in% header_names) filters$stage <- stage
  if ("excluded" %in% header_names) filters$excluded <- FALSE
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
  task_selected <- benchmark_run_includes(metadata, "task")
  fixture_selected <- benchmark_run_includes(metadata, "fixture")
  task_files <- run_correctness_artifact_paths(run_dir, metadata, "task", expected_runners)
  fixture_files <- run_correctness_artifact_paths(run_dir, metadata, "fixture", expected_runners)
  if ((task_selected && any(!file.exists(task_files))) ||
      (fixture_selected && any(!file.exists(fixture_files)))) {
    stop("correctness evidence files are incomplete")
  }
  if ((!task_selected && any(file.exists(task_files))) ||
      (!fixture_selected && any(file.exists(fixture_files)))) {
    stop("correctness evidence exists for an unselected suite")
  }

  tasks <- data.frame()
  if (task_selected) {
    tasks <- do.call(rbind, lapply(task_files, read.csv, stringsAsFactors = FALSE))
    task_required <- c(
      "run_id", "runner", "task", "status", "correctness_status", "correctness_policy",
      "correctness_message", "source_tree_digest", "source_ledger_identity_digest",
      "artifact_digest", "input_manifest_digest", "contract_version", "timing_policy_digest"
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
        input_manifest_digest = as.character(metadata$input_manifest$digest),
        contract_version = as.character(disposition$contract_version),
        timing_policy_digest = run_manifest_object_digest(metadata$timing_policy)
      )
      for (field in names(exact)) {
        if (!identical(as.character(row[[field]]), exact[[field]])) {
          stop(sprintf("task correctness identity field %s differs for %s/%s", field, runner, task))
        }
      }
    }
  }

  fixtures <- data.frame()
  if (fixture_selected) {
    fixtures <- do.call(rbind, lapply(fixture_files, read.csv, stringsAsFactors = FALSE))
    fixture_required <- c(
      "run_id", "runner", "fixture", "variant", "row_id", "status", "correctness_status",
      "correctness_message", "source_tree_digest", "source_ledger_identity_digest", "artifact_digest",
      "input_fingerprint", "contract_version", "timing_policy_digest"
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
    fixture_specs <- fixture_measurement_specs()
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
      spec <- fixture_specs[[fixture]]
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
        artifact_digest = as.character(environment$fixture_artifact_digest),
        input_fingerprint = if (is.null(spec)) {
          "not_applicable"
        } else fixture_measurement_input_fingerprint(fixture, spec),
        contract_version = as.character(evidence_row$contract_version),
        timing_policy_digest = run_manifest_object_digest(metadata$timing_policy)
      )
      for (field in names(exact)) {
        if (!identical(as.character(row[[field]]), exact[[field]])) {
          stop(sprintf("fixture correctness identity field %s differs for %s/%s", field, runner, row$row_id))
        }
      }
    }
  }

  list(
    task_rows = nrow(tasks),
    fixture_rows = nrow(fixtures),
    task_artifact_digest = if (task_selected) correctness_artifact_set_digest(task_files) else "not_selected",
    fixture_artifact_digest = if (fixture_selected) correctness_artifact_set_digest(fixture_files) else "not_selected"
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
  if (!identical(as.character(summary$timer_noise_status), expected_noise)) {
    stop(sprintf("fixture timer-noise evidence differs for %s", label))
  }
  if (!is_bounded_timing_policy(policy)) {
    window <- tail(raw$wall_ms, as.integer(policy$block_size) * as.integer(policy$convergence_window_blocks))
    window_mean <- mean(window)
    expected_cv <- if (window_mean > 0) sd(window) / window_mean * 100 else 0
    if (!isTRUE(all.equal(as.numeric(summary$convergence_cv_pct), expected_cv, tolerance = 1e-12))) {
      stop(sprintf("fixture convergence evidence differs for %s", label))
    }
  }
  invisible(TRUE)
}

validate_fixture_measurement_artifacts <- function(run_dir, metadata, evidence) {
  if (!benchmark_run_includes(metadata, "fixture")) {
    stop("fixture measurement validation requires the fixture suite")
  }
  expected_runners <- sort(run_manifest_values(metadata$runners))
  summaries <- read_run_summary_table(run_dir, metadata, "fixture", expected_runners)
  bounded <- is_bounded_timing_policy(metadata$timing_policy)
  required <- c(
    "run_id", "runner", "fixture", "variant", "row_id", "status", "correctness_status",
    "correctness_message", "input_fingerprint", "implementation_role", "evidence_use",
    "path_kind", "representation_strategy", "comparison_tier", "setup_policy", "timing_eligible",
    "kernel_id", "contract_version", "fixture_version", "comparison_group", "tool_identity",
    "fixture_source_digest", "fixture_build_digest", "fixture_generated_glue_kind",
    "fixture_generated_glue_digest", "fixture_artifact_digest", "fixture_dependency_digest",
    "fixture_artifact_dependency_digest", "source_ledger_identity_digest", "mean_ms", "median_ms",
    "min_ms", "max_ms", "sd_ms", "cv_pct", "n_iterations"
  )
  timing_required <- if (bounded) c(
    "rss_endpoint_delta_kb", "first_call_ms", "peak_rss_kb", "loaded_process_rss_kb",
    "peak_rss_metric", "peak_rss_support", "peak_rss_support_reason", "peak_rss_repetitions",
    "warmup_iterations", "sample_stage", "fixed_iterations", "timer_noise_floor_ms",
    "timer_noise_status", "rss_endpoint_metric", "rss_endpoint_support",
    "rss_endpoint_support_reason", "gc_policy"
  ) else c(
    "rss_kb", "cold_start_ms",
    "warmup_iterations", "block_size", "max_iterations", "convergence_window_blocks",
    "convergence_cv_threshold_pct", "convergence_cv_pct", "stopping_condition", "converged",
    "timer_noise_floor_ms", "timer_noise_status", "rss_metric", "gc_policy"
  )
  required <- c(required, timing_required)
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
  policy_mismatch <- any(as.integer(pass$warmup_iterations) != as.integer(policy$warmup_iterations)) ||
      any(as.numeric(pass$timer_noise_floor_ms) != as.numeric(policy$timer_noise_floor_ms)) ||
      any(as.character(pass$gc_policy) != as.character(policy$gc_policy))
  if (!bounded) policy_mismatch <- policy_mismatch ||
    any(as.character(pass$rss_metric) != as.character(policy$rss_metric)) ||
    any(as.integer(pass$block_size) != as.integer(policy$block_size)) ||
    any(as.integer(pass$max_iterations) != as.integer(policy$max_iterations)) ||
    any(as.integer(pass$convergence_window_blocks) != as.integer(policy$convergence_window_blocks)) ||
    any(as.numeric(pass$convergence_cv_threshold_pct) != as.numeric(policy$convergence_cv_threshold_pct))
  if (policy_mismatch) {
    stop("fixture summaries disagree with the timing policy")
  }
  numeric_fields <- c("mean_ms", "median_ms", "min_ms", "max_ms", "sd_ms", "cv_pct")
  if (!bounded) numeric_fields <- c(numeric_fields, "rss_kb", "cold_start_ms")
  if (any(!vapply(pass[numeric_fields], function(values) all(is.finite(values) & values >= 0), logical(1)))) {
    stop("fixture PASS summaries contain invalid timing statistics")
  }
  if (bounded) {
    validate_rss_endpoint_support(summaries, summaries$status == "PASS", policy, "fixture summaries")
    validate_first_call_metric(summaries, summaries$status == "PASS", "fixture summaries")
    validate_peak_rss_support(summaries, policy, "fixture", "fixture summaries")
    if (any(!(pass$sample_stage %in% c("pilot", "confirmation"))) ||
        any(as.integer(pass$n_iterations) != as.integer(pass$fixed_iterations))) {
      stop("fixture PASS summaries contain an invalid measured sample count")
    }
    pilot_rows <- pass$sample_stage == "pilot"
    confirmation_rows <- pass$sample_stage == "confirmation"
    if (any(pass$n_iterations[pilot_rows] != as.integer(policy$pilot_iterations)) ||
        any(pass$n_iterations[confirmation_rows] < as.integer(policy$confirmation_min_iterations)) ||
        any(pass$n_iterations[confirmation_rows] > as.integer(policy$confirmation_max_iterations))) {
      stop("fixture PASS summaries disagree with bounded timing policy")
    }
  } else if (any(!(pass$stopping_condition %in% c("rolling_cv", "max_iterations"))) ||
             any(pass$n_iterations < 1L | pass$n_iterations > as.integer(policy$max_iterations) |
                 pass$n_iterations %% as.integer(policy$block_size) != 0L)) {
    stop("legacy fixture PASS summaries contain invalid adaptive timing evidence")
  }
  not_measured <- summaries$status != "PASS"
  not_measured_numeric <- c("mean_ms", "median_ms", "min_ms", "max_ms", "sd_ms", "cv_pct")
  not_measured_numeric <- c(
    not_measured_numeric,
    if (bounded) c(
      "rss_endpoint_delta_kb", "peak_rss_kb", "loaded_process_rss_kb", "peak_rss_repetitions",
      "n_iterations", "fixed_iterations"
    ) else c("rss_kb", "cold_start_ms", "n_iterations", "convergence_cv_pct")
  )
  if (any(!vapply(summaries[not_measured, not_measured_numeric, drop = FALSE], function(values) {
    all(is.na(values))
  }, logical(1))) ||
      any(as.character(summaries[[if (bounded) "sample_stage" else "stopping_condition"]][not_measured]) != "not_measured") ||
      any(as.character(summaries$timer_noise_status[not_measured]) != "not_measured")) {
    stop("untimed fixture summaries contain measurement evidence")
  }
  benchmark_artifact_layout(metadata)
  shared_samples <- run_sample_artifact_paths(run_dir, metadata, "fixture", expected_runners[[1L]], ".")
  if (!identical(file.exists(shared_samples), nrow(pass) > 0L)) {
    stop("shared fixture timing artifact presence differs from PASS summaries")
  }
  if (dir.exists(file.path(run_dir, "fixtures"))) {
    stop("grouped run retains per-runner fixture artifact directories")
  }
  for (runner in expected_runners) {
    runner_rows <- pass[pass$runner == runner, , drop = FALSE]
    expected_raw <- sort(as.character(runner_rows$row_id))
    raw_samples <- read_run_sample_table(run_dir, metadata, "fixture", runner, expected_raw)
    actual_raw <- sort(unique(as.character(raw_samples$row_id)))
    if (!identical(actual_raw, expected_raw)) {
      stop(sprintf("fixture raw timing coverage differs for %s", runner))
    }
    required_raw <- c("run_id", "runner", "fixture", "variant", "row_id", "iteration", "wall_ms")
    if (bounded) required_raw <- c(
      required_raw, "stage", "process_epoch", "batch", "attempt", "group_order", "member_order",
      "excluded", "exclusion_reason", "phase", "rss_endpoint_delta_kb"
    )
    if (nrow(raw_samples) > 0L && length(setdiff(required_raw, names(raw_samples))) > 0L) {
      stop(sprintf("fixture raw timing columns differ for %s", runner))
    }
    if (bounded && nrow(raw_samples) > 0L && (
        any(!(as.character(raw_samples$stage) %in% c("pilot", "confirmation"))) ||
        any(as.integer(raw_samples$process_epoch) < 1L) || any(as.integer(raw_samples$batch) < 1L) ||
        any(!(as.integer(raw_samples$attempt) %in% 1:2)) ||
        any(as.integer(raw_samples$group_order) < 1L) || any(as.integer(raw_samples$member_order) < 1L))) {
      stop(sprintf("fixture raw timing metadata differs for %s", runner))
    }
    if (bounded) {
      excluded <- as.logical(raw_samples$excluded) %in% TRUE
      if (anyNA(as.logical(raw_samples$excluded)) ||
          any(excluded & (is.na(raw_samples$exclusion_reason) | !nzchar(as.character(raw_samples$exclusion_reason)))) ||
          any(!excluded & !is.na(raw_samples$exclusion_reason) & nzchar(as.character(raw_samples$exclusion_reason)))) {
        stop(sprintf("fixture raw timing exclusions differ for %s", runner))
      }
      if (nrow(raw_samples) > 0L &&
          !setequal(unique(as.character(raw_samples$phase)), c("first_call", "timed"))) {
        stop(sprintf("fixture raw timing phases differ for %s", runner))
      }
    }
    for (row_id in expected_raw) {
      summary <- runner_rows[runner_rows$row_id == row_id, , drop = FALSE]
      raw <- raw_samples[
        as.character(raw_samples$row_id) == row_id,
        , drop = FALSE
      ]
      if (bounded) raw <- raw[
        !(as.logical(raw$excluded) %in% TRUE) &
          as.character(raw$stage) == as.character(summary$sample_stage[[1L]]), , drop = FALSE
      ]
      first_call_raw <- if (bounded) raw[as.character(raw$phase) == "first_call", , drop = FALSE] else NULL
      if (bounded) raw <- raw[as.character(raw$phase) == "timed", , drop = FALSE]
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
      if (bounded) {
        label <- paste(runner, row_id, sep = "/")
        validate_first_call_raw(summary, first_call_raw, label)
        validate_rss_endpoint_raw(summary, raw, label)
      }
      validate_fixture_raw_statistics(summary, raw, policy, paste(runner, row_id, sep = "/"))
    }
  }
  confirmation <- if (bounded) pass[pass$sample_stage == "confirmation", , drop = FALSE] else pass[0, , drop = FALSE]
  if (nrow(confirmation) > 0L) {
    counts <- split(as.integer(confirmation$n_iterations), as.character(confirmation$fixture))
    if (any(vapply(counts, function(value) length(unique(value)) != 1L, logical(1)))) {
      stop("confirmation fixture groups do not use symmetric frozen sample counts")
    }
  }
  invisible(TRUE)
}
