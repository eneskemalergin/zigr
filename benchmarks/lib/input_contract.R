benchmark_input_schema_version <- function() "p4.5-input-v2"

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

task_mutation_policy <- function(task_id) {
  if (task_id %in% c("04_sort", "21_attrib_ops")) return("fresh_input_required")
  if (task_id %in% c("72_boundary_external_method_generated", "73_boundary_external_method_handwritten")) {
    return("stateful_reset_required")
  }
  if (identical(task_id, "43_rng_stress")) return("rng_reset_required")
  "immutable"
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

validate_task_input_recipe <- function(task, record, master_seed) {
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
  materialized <- materialize_task_input(task, recorded_seed)
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
