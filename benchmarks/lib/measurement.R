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

# Each factory builds one task input on demand so correctness checks keep only
# the current event's input live.
benchmark_revision_task_specs <- function() {
  list(
    list(id = "vector_sum", function_name = "bench_vector_sum", arguments = function() list(runif(1000000L)), tolerance = TRUE),
    list(id = "numeric_transform", function_name = "bench_numeric_transform", arguments = function() list(c(seq_len(99998L), NA_real_, NaN))),
    list(id = "broadcast", function_name = "bench_broadcast", arguments = function() list(runif(1000000L), 3.14), tolerance = TRUE),
    list(id = "sort", function_name = "bench_sort", arguments = function() list(runif(100000L))),
    list(id = "missing_mean", function_name = "bench_missing_mean", arguments = function() {
      x <- runif(1000000L)
      positions <- sample.int(length(x), 50000L)
      x[positions[seq_len(25000L)]] <- NA_real_
      x[positions[25000L + seq_len(25000L)]] <- NaN
      list(x)
    }, tolerance = TRUE),
    list(id = "transpose", function_name = "bench_transpose", arguments = function() list(matrix(runif(512L * 512L), 512L, 512L))),
    list(id = "rowcol", function_name = "bench_rowcol", arguments = function() list(matrix(runif(500L * 1000L), 500L, 1000L)), tolerance = TRUE),
    list(id = "matmul", function_name = "bench_matmul", arguments = function() list(matrix(runif(256L * 256L), 256L), matrix(runif(256L * 256L), 256L)), tolerance = TRUE),
    list(id = "dataframe", function_name = "bench_dataframe", arguments = function() {
      x <- rnorm(100000L)
      y <- abs(rnorm(100000L)) + 0.1
      missing <- sample.int(length(x), 5000L)
      x[missing] <- NA_real_
      groups <- sprintf("grp_%02d", 1:10)
      list(data.frame(x = x, y = y, grp = factor(rep(groups, length.out = length(x)), levels = groups)))
    }, tolerance = TRUE),
    list(id = "list_sum", function_name = "bench_list_sum", arguments = function() list(replicate(1000L, runif(100L), simplify = FALSE)), tolerance = TRUE),
    list(id = "string_concat", function_name = "bench_string_concat", arguments = function() list(rep(benchmark_encoded_strings(), 2000L))),
    list(id = "string_metadata", function_name = "bench_string_metadata", arguments = function() list(rep(benchmark_encoded_strings(), 2000L))),
    list(id = "factor", function_name = "bench_factor", arguments = function() list(benchmark_factor_input())),
    list(id = "attributes", function_name = "bench_attributes", arguments = function() list(runif(100000L))),
    list(id = "s4", function_name = "bench_s4", arguments = function() list(runif(100000L))),
    list(id = "logical_counts", function_name = "bench_logical_counts", arguments = function() list(rep(c(FALSE, TRUE, NA), length.out = 100000L))),
    list(id = "raw_copy", function_name = "bench_raw_copy", arguments = function() list(as.raw((0:262143) %% 251L))),
    list(id = "complex_conjugate", function_name = "bench_complex_conjugate", arguments = function() {
      x <- complex(real = seq_len(32768L), imaginary = seq_len(32768L) / 2)
      x[[1L]] <- NA_complex_
      x[[2L]] <- complex(real = NaN, imaginary = 3)
      list(x)
    }),
    list(id = "schema", function_name = "bench_schema", arguments = function() list(list(id = 1L, count = 2L, ratio = 0.5, enabled = TRUE))),
    list(id = "altrep_sum", function_name = "bench_altrep_sum", arguments = function() list(seq_len(1000000L)), altrep = TRUE),
    list(id = "altrep_index", function_name = "bench_altrep_index", arguments = function() list(seq_len(10000000L)), altrep = TRUE),
    list(id = "altrep_materialize", function_name = "bench_altrep_materialize", arguments = function() list(seq_len(1000000L)), altrep = TRUE),
    list(id = "external_state", function_name = "bench_external_state", arguments = function() list()),
    list(id = "eval", function_name = "bench_eval", arguments = function() list(runif(100000L)), tolerance = TRUE),
    list(id = "serialize", function_name = "bench_serialize", arguments = function() list(runif(100000L))),
    list(id = "rng", function_name = "bench_rng", arguments = function() list(100000L), rng = TRUE),
    list(id = "outputs", function_name = "bench_outputs", arguments = function() list())
  )
}

# One event is one invocation with freshly prepared deterministic arguments.
# These tasks cannot repeat that event against one prepared object: they mutate
# it, consume RNG state, or may change ALTREP representation.
direct_task_batchability <- function(task_id) {
  if (task_id %in% c(
    "altrep_sum", "altrep_index", "altrep_materialize",
    "external_state", "rng"
  )) {
    return("one")
  }
  "repeat"
}

direct_batch_repetition_map <- function(tasks, repetitions = 1L) {
  tasks <- as.character(tasks)
  if (length(tasks) == 0L || any(!nzchar(tasks)) || anyDuplicated(tasks)) {
    stop("batch repetition tasks are invalid")
  }
  repetitions <- input_scalar_integer(repetitions, "default batch repetitions")
  result <- setNames(rep.int(repetitions, length(tasks)), tasks)
  result[vapply(tasks, direct_task_batchability, character(1)) == "one"] <- 1L
  result
}

format_named_integer_map <- function(values, label) {
  if (!is.numeric(values) || is.null(names(values)) || any(!nzchar(names(values))) ||
      anyDuplicated(names(values))) {
    stop(sprintf("%s must be a named integer map", label))
  }
  checked <- vapply(values, input_scalar_integer, integer(1), label = label)
  paste(paste(names(checked), checked, sep = ":"), collapse = ",")
}

parse_named_integer_map <- function(value, expected_names, label) {
  entries <- strsplit(as.character(value), ",", fixed = TRUE)[[1L]]
  fields <- strsplit(entries, ":", fixed = TRUE)
  if (length(entries) == 0L || any(lengths(fields) != 2L)) {
    stop(sprintf("%s must be task:integer entries", label))
  }
  names <- vapply(fields, `[[`, character(1), 1L)
  values <- vapply(fields, `[[`, character(1), 2L)
  if (any(!nzchar(names)) || anyDuplicated(names) || !setequal(names, expected_names)) {
    stop(sprintf("%s task coverage differs", label))
  }
  parsed <- setNames(vapply(values, input_scalar_integer, integer(1), label = label), names)
  parsed[expected_names]
}

direct_sizing_policy <- function() {
  list(
    policy_version = "shared-ladder-v1",
    ladder = as.list(c(1L, 8L, 64L)),
    minimum_batch_ms = 1,
    timer_floor_multiplier = 20L,
    target_batch_ms = 5,
    maximum_batch_ms = 250
  )
}

validate_direct_sizing_policy <- function(policy) {
  fields <- c(
    "policy_version", "ladder", "minimum_batch_ms", "timer_floor_multiplier",
    "target_batch_ms", "maximum_batch_ms"
  )
  if (!is.list(policy) || !identical(names(policy), fields) ||
      !identical(as.character(policy$policy_version), "shared-ladder-v1") ||
      !identical(as.integer(unlist(policy$ladder, use.names = FALSE)), c(1L, 8L, 64L))) {
    stop("direct sizing policy is invalid")
  }
  multiplier <- input_scalar_integer(policy$timer_floor_multiplier, "timer floor multiplier")
  numeric <- unlist(policy[c("minimum_batch_ms", "target_batch_ms", "maximum_batch_ms")], use.names = FALSE)
  if (length(numeric) != 3L || any(!is.finite(numeric)) || any(numeric <= 0) ||
      numeric[[1L]] > numeric[[2L]] || numeric[[2L]] > numeric[[3L]]) {
    stop("direct sizing policy has invalid batch bounds")
  }
  list(
    ladder = as.integer(unlist(policy$ladder, use.names = FALSE)),
    minimum_batch_ms = as.numeric(numeric[[1L]]),
    timer_floor_multiplier = multiplier,
    target_batch_ms = as.numeric(numeric[[2L]]),
    maximum_batch_ms = as.numeric(numeric[[3L]])
  )
}

direct_sizing_target_ms <- function(timer_floors, policy = direct_sizing_policy()) {
  policy <- validate_direct_sizing_policy(policy)
  if (length(timer_floors) == 0L || any(!is.finite(timer_floors)) || any(timer_floors < 0)) {
    stop("batch sizing timer floors are invalid")
  }
  max(policy$minimum_batch_ms, policy$target_batch_ms,
      max(as.numeric(timer_floors)) * policy$timer_floor_multiplier)
}

remaining_direct_run_seconds <- function(started_elapsed, current_elapsed, timeout_seconds) {
  if (length(started_elapsed) != 1L || length(current_elapsed) != 1L ||
      !is.finite(started_elapsed) || !is.finite(current_elapsed) ||
      current_elapsed < started_elapsed) {
    stop("direct run elapsed times are invalid")
  }
  timeout_seconds <- input_scalar_integer(timeout_seconds, "total run timeout")
  floor(timeout_seconds - (current_elapsed - started_elapsed))
}

direct_sizing_count_accepted <- function(rows, timer_floors, runners,
                                         policy = direct_sizing_policy()) {
  required <- c("runner", "task", "batch_repetitions", "batch_elapsed_ms", "gc_elapsed_ms")
  sizing_policy <- validate_direct_sizing_policy(policy)
  if (!is.data.frame(rows) || !identical(names(rows), required) ||
      nrow(rows) != length(runners) || !identical(as.character(rows$runner), runners) ||
      anyNA(rows) || any(!vapply(rows[c("batch_repetitions", "batch_elapsed_ms", "gc_elapsed_ms")], is.numeric, logical(1))) ||
      any(!is.finite(as.matrix(rows[c("batch_repetitions", "batch_elapsed_ms", "gc_elapsed_ms")]))) ||
      any(rows$batch_repetitions < 1L) || any(rows$batch_repetitions != as.integer(rows$batch_repetitions)) ||
      any(!rows$batch_repetitions %in% sizing_policy$ladder) ||
      any(rows$batch_elapsed_ms < 0) || any(rows$gc_elapsed_ms < 0) ||
      any(rows$gc_elapsed_ms > rows$batch_elapsed_ms) ||
      any(rows$batch_elapsed_ms > sizing_policy$maximum_batch_ms)) {
    return(FALSE)
  }
  target_ms <- max(
    sizing_policy$minimum_batch_ms, sizing_policy$target_batch_ms,
    max(as.numeric(timer_floors)) * sizing_policy$timer_floor_multiplier
  )
  all(rows$batch_elapsed_ms > target_ms)
}

select_direct_batch_repetitions <- function(sizing, timer_floors, runners, tasks,
                                            policy = direct_sizing_policy()) {
  required <- c("runner", "task", "batch_repetitions", "batch_elapsed_ms", "gc_elapsed_ms")
  if (!is.data.frame(sizing) || !identical(names(sizing), required) ||
      anyNA(sizing) || any(!is.finite(as.matrix(sizing[c("batch_repetitions", "batch_elapsed_ms", "gc_elapsed_ms")]))) ||
      any(sizing$batch_repetitions < 1L) || any(sizing$batch_elapsed_ms < 0) ||
      any(sizing$gc_elapsed_ms < 0) || any(sizing$gc_elapsed_ms > sizing$batch_elapsed_ms)) {
    stop("batch sizing rows are invalid")
  }
  if (!identical(names(timer_floors), runners) || any(!is.finite(timer_floors)) || any(timer_floors < 0)) {
    stop("batch sizing timer floors are invalid")
  }
  target_ms <- direct_sizing_target_ms(timer_floors, policy)
  policy <- validate_direct_sizing_policy(policy)
  if (any(sizing$batch_repetitions != as.integer(sizing$batch_repetitions)) ||
      any(!sizing$batch_repetitions %in% policy$ladder) ||
      !setequal(as.character(sizing$runner), runners) || !setequal(as.character(sizing$task), tasks)) {
    stop("batch sizing rows have invalid coverage or repetitions")
  }
  selected <- integer(length(tasks))
  names(selected) <- tasks
  for (task in tasks) {
    candidates <- if (identical(direct_task_batchability(task), "one")) 1L else policy$ladder
    rows <- sizing[sizing$task == task, , drop = FALSE]
    if (anyDuplicated(paste(rows$runner, rows$batch_repetitions, sep = "\r"))) {
      stop(sprintf("%s has duplicate batch sizing observations", task))
    }
    observed <- lapply(runners, function(runner) {
      as.integer(rows$batch_repetitions[rows$runner == runner])
    })
    if (any(vapply(observed, length, integer(1)) == 0L) ||
        any(!vapply(observed, identical, logical(1), observed[[1L]]))) {
      stop(sprintf("%s batch sizing coverage differs across runners", task))
    }
    chosen <- NA_integer_
    for (count in candidates) {
      candidate <- rows[rows$batch_repetitions == count, , drop = FALSE]
      if (nrow(candidate) == length(runners) && identical(as.character(candidate$runner), runners) &&
          all(candidate$batch_elapsed_ms > target_ms) &&
          all(candidate$batch_elapsed_ms <= policy$maximum_batch_ms)) {
        chosen <- count
        break
      }
    }
    if (is.na(chosen)) {
      stop(sprintf("%s cannot meet the shared sizing target within the batch cap", task))
    }
    expected <- candidates[seq_len(match(chosen, candidates))]
    if (!identical(observed[[1L]], expected)) {
      stop(sprintf("%s batch sizing ladder contains an unnecessary or undeclared step", task))
    }
    selected[[task]] <- chosen
  }
  selected
}

benchmark_revision_arguments <- function(spec, master_seed = benchmark_master_seed()) {
  seed <- task_input_seed(master_seed, spec$id, "revision-v1")
  arguments <- with_preserved_rng(seed, spec$arguments())
  if (!is.list(arguments)) stop(sprintf("input factory for %s did not return a list", spec$id))
  arguments
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

# One direct timing path: calls are prepared before the clock, and only the
# benchmark event is evaluated between the two timer reads.

if (!requireNamespace("microbenchmark", quietly = TRUE)) {
  stop("direct timing requires the microbenchmark package")
}

direct_function_call <- function(function_object, arguments = list()) {
  if (!is.function(function_object)) stop("direct R call requires a function")
  if (!is.list(arguments)) stop("direct call arguments must be a list")
  as.call(c(list(function_object), arguments))
}

direct_native_call <- function(interface, symbol, arguments = list()) {
  if (!(interface %in% c(".Call", ".External"))) {
    stop("direct native interface must be .Call or .External")
  }
  if (!inherits(symbol, "NativeSymbolInfo") && !inherits(symbol, "NativeSymbol")) {
    stop("direct native call requires a resolved symbol")
  }
  if (!is.list(arguments)) stop("direct call arguments must be a list")
  as.call(c(list(as.name(interface), symbol), arguments))
}

direct_batch_expression <- function(call, repetitions) {
  repetitions <- input_scalar_integer(repetitions, "batch repetitions")
  if (repetitions < 1L) stop("batch repetitions must be positive")
  if (!is.call(call)) stop("direct batch requires one prepared call expression")
  if (repetitions == 1L) return(call)
  as.call(c(list(as.name("{")), rep(list(call), repetitions)))
}

measure_direct_batch <- function(
    call, environment, repetitions,
    clock = function() microbenchmark::get_nanotime() / 1e9,
    gc_clock = function() gc.time()[[3L]]) {
  batch <- direct_batch_expression(call, repetitions)
  read_elapsed <- function() {
    value <- clock()
    if (length(value) != 1L) value <- value[["elapsed"]]
    value <- as.numeric(value)
    if (length(value) != 1L || !is.finite(value)) stop("timer returned an invalid elapsed value")
    value
  }
  gc_started <- as.numeric(gc_clock())
  started <- read_elapsed()
  result <- eval(batch, envir = environment)
  finished <- read_elapsed()
  gc_finished <- as.numeric(gc_clock())
  elapsed_ms <- (finished - started) * 1000
  gc_elapsed_ms <- (gc_finished - gc_started) * 1000
  if (!is.finite(elapsed_ms) || elapsed_ms < 0 || !is.finite(gc_elapsed_ms) ||
      gc_elapsed_ms < 0) {
    stop("timer moved backwards")
  }
  list(
    result = result,
    batch_elapsed_ms = elapsed_ms,
    batch_repetitions = as.integer(repetitions),
    elapsed_per_event_ms = elapsed_ms / repetitions,
    gc_elapsed_ms = gc_elapsed_ms
  )
}

validate_direct_timing_samples <- function(
    samples, runners = NULL, tasks = NULL, measurement_samples = NULL) {
  required <- c(
    "runner", "task", "phase", "measurement_sample", "batch_repetitions",
    "batch_elapsed_ms", "elapsed_per_event_ms", "gc_elapsed_ms"
  )
  if (!is.data.frame(samples) || !identical(names(samples), required)) {
    stop("direct timing samples have the wrong columns")
  }
  if (nrow(samples) == 0L) stop("direct timing samples are empty")
  identity_fields <- c("runner", "task", "phase")
  if (anyNA(samples[identity_fields]) ||
      any(!vapply(samples[identity_fields], is.character, logical(1))) ||
      any(!nzchar(samples$runner)) || any(!nzchar(samples$task)) ||
      any(samples$phase != "measurement")) {
    stop("only measurement rows may enter timing samples")
  }
  numeric_fields <- c(
    "measurement_sample", "batch_repetitions", "batch_elapsed_ms", "elapsed_per_event_ms",
    "gc_elapsed_ms"
  )
  if (any(!vapply(samples[numeric_fields], is.numeric, logical(1))) ||
      anyNA(samples[numeric_fields]) ||
      any(!is.finite(as.matrix(samples[numeric_fields]))) ||
      any(samples$measurement_sample < 1L) ||
      any(samples$measurement_sample != as.integer(samples$measurement_sample)) ||
      any(samples$batch_repetitions < 1L) ||
      any(samples$batch_repetitions != as.integer(samples$batch_repetitions)) ||
      any(samples$batch_elapsed_ms < 0) || any(samples$elapsed_per_event_ms < 0) ||
      any(samples$gc_elapsed_ms < 0) || any(samples$gc_elapsed_ms > samples$batch_elapsed_ms)) {
    stop("direct timing samples contain invalid measurements")
  }
  expected_elapsed <- samples$batch_elapsed_ms / samples$batch_repetitions
  if (!isTRUE(all.equal(
    samples$elapsed_per_event_ms, expected_elapsed,
    tolerance = 1e-12, check.attributes = FALSE
  ))) {
    stop("direct timing elapsed-per-event values differ from their batches")
  }
  keys <- paste(samples$runner, samples$task, samples$measurement_sample, sep = "\r")
  if (anyDuplicated(keys)) stop("direct timing samples contain duplicate identities")
  repetitions <- split(samples$batch_repetitions, samples$task)
  if (any(vapply(repetitions, function(value) length(unique(value)) != 1L, logical(1)))) {
    stop("a task does not use one shared batch repetition count")
  }
  if (!is.null(runners)) {
    runners <- as.character(runners)
    coverage <- split(as.character(samples$runner), samples$task)
    if (any(vapply(coverage, function(value) !setequal(unique(value), runners), logical(1)))) {
      stop("direct timing runner coverage is incomplete")
    }
  }
  if (!is.null(tasks)) {
    tasks <- as.character(tasks)
    if (!setequal(unique(samples$task), tasks)) stop("direct timing task coverage is incomplete")
  }
  if (!is.null(measurement_samples)) {
    measurement_samples <- input_scalar_integer(measurement_samples, "measurement samples")
    groups <- split(samples$measurement_sample, paste(samples$runner, samples$task, sep = "\r"))
    expected <- seq_len(measurement_samples)
    if (any(vapply(groups, function(value) !identical(sort(as.integer(value)), expected), logical(1)))) {
      stop("direct timing measurement-sample coverage is incomplete")
    }
    if (!is.null(runners) && !is.null(tasks)) {
      expected_groups <- as.vector(outer(runners, tasks, paste, sep = "\r"))
      if (!setequal(names(groups), expected_groups)) {
        stop("direct timing runner-task coverage is incomplete")
      }
      expected_order <- unlist(lapply(runners, function(runner) {
        unlist(lapply(tasks, function(task) {
          paste(runner, task, seq_len(measurement_samples), sep = "\r")
        }), use.names = FALSE)
      }), use.names = FALSE)
      observed_order <- paste(samples$runner, samples$task, samples$measurement_sample, sep = "\r")
      if (!identical(observed_order, expected_order)) {
        stop("direct timing raw sample order is invalid")
      }
    }
  }
  invisible(samples)
}

direct_distribution_metrics <- function(values) {
  values <- as.numeric(values)
  if (length(values) < 1L || anyNA(values) || any(!is.finite(values)) || any(values < 0)) {
    stop("distribution values are invalid")
  }
  median_ms <- stats::median(values)
  mean_ms <- mean(values)
  denominator <- max(median_ms, .Machine$double.eps)
  regime_change <- FALSE
  if (length(values) >= 6L) {
    for (split in 3L:(length(values) - 3L)) {
      left <- stats::median(values[seq_len(split)])
      right <- stats::median(values[(split + 1L):length(values)])
      ratio <- max(left, right) / max(min(left, right), .Machine$double.eps)
      if (ratio > 3) {
        regime_change <- TRUE
        break
      }
    }
  }
  list(
    median_ms = median_ms,
    mean_ms = mean_ms,
    q1_ms = unname(stats::quantile(values, 0.25, names = FALSE)),
    q3_ms = unname(stats::quantile(values, 0.75, names = FALSE)),
    p95_ms = unname(stats::quantile(values, 0.95, names = FALSE)),
    p99_ms = unname(stats::quantile(values, 0.99, names = FALSE)),
    min_ms = min(values),
    max_ms = max(values),
    sd_ms = if (length(values) > 1L) stats::sd(values) else 0,
    cv_pct = if (mean_ms > 0 && length(values) > 1L) stats::sd(values) / mean_ms * 100 else 0,
    max_over_median = max(values) / denominator,
    p99_over_median = unname(stats::quantile(values, 0.99, names = FALSE)) / denominator,
    regime_change = regime_change
  )
}

direct_distribution_needs_explanation <- function(metrics) {
  metrics$max_over_median > 20 || metrics$p99_over_median > 5 ||
    metrics$cv_pct > 50 || isTRUE(metrics$regime_change)
}

classify_direct_distribution <- function(values, batch_elapsed_ms, gc_elapsed_ms, timer_floor_ms) {
  metrics <- direct_distribution_metrics(values)
  batch_metrics <- direct_distribution_metrics(batch_elapsed_ms)
  if (batch_metrics$median_ms <= timer_floor_ms) {
    return(list(status = "BLOCK", reason = "median is at or below the measured timer floor", metrics = metrics))
  }
  if (!direct_distribution_needs_explanation(metrics)) {
    return(list(status = "PASS", reason = "ordered samples pass the distribution gates", metrics = metrics))
  }
  gc_samples <- which(as.numeric(gc_elapsed_ms) > 0)
  if (length(gc_samples) > 0L && length(gc_samples) <= length(values) - 3L) {
    without_gc <- direct_distribution_metrics(values[-gc_samples])
    if (!direct_distribution_needs_explanation(without_gc)) {
      return(list(
        status = "PASS_GC", reason = "distribution spread is explained by measured R GC time",
        metrics = metrics
      ))
    }
  }
  list(status = "BLOCK", reason = "ordered samples contain unexplained spread or a regime change", metrics = metrics)
}

summarize_direct_timing <- function(samples, first_calls, timer_floors = NULL) {
  validate_direct_timing_samples(samples)
  first_required <- c("runner", "task", "first_call_ms")
  if (!is.data.frame(first_calls) || !identical(names(first_calls), first_required) ||
      anyNA(first_calls) || !is.numeric(first_calls$first_call_ms) ||
      any(!is.finite(first_calls$first_call_ms)) || any(first_calls$first_call_ms < 0) ||
      any(!nzchar(first_calls$runner)) || any(!nzchar(first_calls$task))) {
    stop("direct first-call rows are invalid")
  }
  first_keys <- paste(first_calls$runner, first_calls$task, sep = "\r")
  if (anyDuplicated(first_keys)) stop("direct first-call rows contain duplicate identities")
  groups <- split(seq_len(nrow(samples)), paste(samples$runner, samples$task, sep = "\r"))
  if (!setequal(first_keys, names(groups))) stop("direct first-call coverage differs from timing samples")
  rows <- lapply(groups, function(index) {
    values <- samples$elapsed_per_event_ms[index]
    key <- paste(samples$runner[index[[1L]]], samples$task[index[[1L]]], sep = "\r")
    first_index <- match(key, first_keys)
    if (is.na(first_index)) stop("direct timing summary is missing first-call evidence")
    timer_floor <- if (is.null(timer_floors)) 0 else as.numeric(timer_floors[[samples$runner[index[[1L]]]]])
    if (length(timer_floor) != 1L || !is.finite(timer_floor) || timer_floor < 0) {
      stop("direct timing summary is missing a valid runner timer floor")
    }
    classification <- classify_direct_distribution(
      values, samples$batch_elapsed_ms[index], samples$gc_elapsed_ms[index], timer_floor
    )
    metrics <- classification$metrics
    data.frame(
      runner = samples$runner[index[[1L]]], task = samples$task[index[[1L]]],
      measurement_samples = length(values),
      batch_repetitions = samples$batch_repetitions[index[[1L]]],
      median_ms = metrics$median_ms, mean_ms = metrics$mean_ms,
      q1_ms = metrics$q1_ms, q3_ms = metrics$q3_ms, p95_ms = metrics$p95_ms,
      p99_ms = metrics$p99_ms, min_ms = metrics$min_ms, max_ms = metrics$max_ms,
      sd_ms = metrics$sd_ms, cv_pct = metrics$cv_pct,
      max_over_median = metrics$max_over_median,
      p99_over_median = metrics$p99_over_median,
      regime_change = metrics$regime_change,
      timer_floor_ms = timer_floor,
      distribution_status = classification$status,
      distribution_reason = classification$reason,
      first_call_ms = first_calls$first_call_ms[first_index],
      stringsAsFactors = FALSE
    )
  })
  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  result[order(result$task, result$runner), , drop = FALSE]
}

measurement_probe_names <- function() c("noop_native", "noop_r", "cpu", "allocate")

measurement_probe_timer_floor <- function(samples) {
  values <- lapply(c("noop_native", "noop_r"), function(name) {
    samples$elapsed_per_event_ms[samples$probe == name]
  })
  noops <- unlist(values, use.names = FALSE)
  if (any(lengths(values) == 0L) || anyNA(noops) ||
      any(!is.finite(noops)) || any(noops < 0)) {
    stop("no-op probe samples are invalid")
  }
  max(vapply(values, stats::quantile, numeric(1), probs = 0.99, names = FALSE))
}

measurement_unit_minimum_ms <- function() 10

measurement_unit_tolerance_ms <- function(independent_elapsed_ms) {
  max(2, as.numeric(independent_elapsed_ms) * 0.20)
}

run_direct_measurement_probes <- function(c_dll, samples = 101L) {
  samples <- input_scalar_integer(samples, "probe samples")
  symbol <- function(name) getNativeSymbolInfo(name, c_dll)
  same_sexp <- function(left, right) {
    isTRUE(eval(direct_native_call(
      ".Call", symbol("c_benchmark_same_sexp"), list(left, right)
    ), envir = environment()))
  }
  cpu_input <- seq(0, 1, length.out = 200000L)
  allocating_expected <- as.double(seq.int(0L, 32767L))
  calls <- list(
    noop_native = direct_native_call(".Call", symbol("c_measurement_probe_noop")),
    noop_r = direct_function_call(compiler::cmpfun(function() NULL)),
    cpu = direct_native_call(
      ".Call", symbol("c_measurement_probe_cpu"), list(cpu_input)
    ),
    allocate = direct_native_call(
      ".Call", symbol("c_measurement_probe_allocate"), list(32768L)
    )
  )
  gc(full = TRUE)
  gc.time(TRUE)
  rows <- list()
  for (name in names(calls)) {
    for (sample in seq_len(samples)) {
      measured <- measure_direct_batch(calls[[name]], environment(), 1L)
      valid <- switch(name,
        noop_native = is.null(measured$result),
        noop_r = is.null(measured$result),
        cpu = same_sexp(cpu_input, measured$result),
        allocate = identical(measured$result, allocating_expected)
      )
      if (!isTRUE(valid)) stop(sprintf("%s measurement probe result was not evaluated", name))
      rows[[length(rows) + 1L]] <- data.frame(
        probe = name, probe_sample = sample, batch_repetitions = 1L,
        batch_elapsed_ms = measured$batch_elapsed_ms,
        elapsed_per_event_ms = measured$elapsed_per_event_ms,
        gc_elapsed_ms = measured$gc_elapsed_ms,
        stringsAsFactors = FALSE
      )
    }
  }
  rows <- do.call(rbind, rows)
  rownames(rows) <- NULL

  first_allocated <- eval(calls$allocate, envir = environment())
  second_allocated <- eval(calls$allocate, envir = environment())
  if (same_sexp(first_allocated, second_allocated)) {
    stop("allocation probe returned the same R object twice")
  }
  first_allocated[[1L]] <- -1
  if (!identical(second_allocated[[1L]], 0)) {
    stop("allocation probe results alias each other")
  }

  unit_batch <- direct_batch_expression(calls$cpu, 64L)
  proc_started <- proc.time()[["elapsed"]]
  nano_started <- microbenchmark::get_nanotime() / 1e9
  unit_result <- eval(unit_batch, envir = environment())
  nano_finished <- microbenchmark::get_nanotime() / 1e9
  proc_finished <- proc.time()[["elapsed"]]
  nano_ms <- (nano_finished - nano_started) * 1000
  proc_ms <- (proc_finished - proc_started) * 1000
  tolerance_ms <- measurement_unit_tolerance_ms(proc_ms)
  if (!same_sexp(cpu_input, unit_result) ||
      proc_ms < measurement_unit_minimum_ms() ||
      abs(nano_ms - proc_ms) > tolerance_ms) {
    stop("nanotime units disagree with the independent elapsed-time check")
  }
  list(
    samples = rows,
    timer_floor_ms = unname(measurement_probe_timer_floor(rows)),
    nanotime_elapsed_ms = nano_ms,
    independent_elapsed_ms = proc_ms
  )
}
benchmark_timing_policy <- function() {
  tasks <- vapply(benchmark_revision_task_specs(), `[[`, character(1), "id")
  list(
    policy_version = "direct-batch-v4",
    warmup_iterations = 1L,
    local_calibration_batches = 1L,
    measurement_samples = 11L,
    measurement_probe_samples = 101L,
    sizing_policy = direct_sizing_policy(),
    batch_repetitions = as.list(direct_batch_repetition_map(tasks)),
    worker_timeout_seconds = 600L,
    total_run_timeout_seconds = 5400L,
    r_jit_policy = "disabled-before-runner-load",
    gc_policy = paste(
      "full before first call, warmup, and calibration; no forced collection between",
      "measurement samples; completed task state released before the next task"
    )
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
