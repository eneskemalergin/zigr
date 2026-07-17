direct_runner_packages <- function(root_dir) {
  fixture_library <- file.path(root_dir, "tmp", "fixture-library")
  list(
    zigr = list(package = "zigrFixture", library = fixture_library, dll = "zigrFixture"),
    rcpp = list(package = "zigrRcpp", library = fixture_library, dll = "zigrRcpp"),
    cpp11 = list(
      package = "zigrCpp11", library = file.path(root_dir, "tmp", "cpp11-library"),
      dll = "zigrCpp11"
    ),
    extendr = list(package = "zigrExtendr", library = fixture_library, dll = "zigrExtendr"),
    savvy = list(package = "zigrSavvy", library = fixture_library, dll = "zigrSavvy")
  )
}

direct_runner_package <- function(root_dir, runner) {
  if (length(runner) != 1L || is.na(runner) || !nzchar(runner)) {
    stop("direct runner package requires one runner")
  }
  package <- direct_runner_packages(root_dir)[[runner]]
  if (is.null(package)) stop(sprintf("no direct runner package is declared for %s", runner))
  package
}

direct_runner_artifact_path <- function(root_dir, runner) {
  if (identical(runner, "r")) {
    return(file.path(root_dir, "src", "r", "run_all.R"))
  }
  if (identical(runner, "c_call")) {
    return(file.path(root_dir, "src", "c_call", "bench.so"))
  }
  package <- direct_runner_package(root_dir, runner)
  file.path(
    package$library, package$package, "libs",
    paste0(package$dll, .Platform$dynlib.ext)
  )
}

direct_runner_environment <- function(root_dir, runner) {
  if (runner %in% c("r", "c_call")) return(.GlobalEnv)
  package <- direct_runner_package(root_dir, runner)
  loadNamespace(package$package, lib.loc = package$library)
}

revision_native_call <- function(dll, symbol, arguments = list()) {
  address <- getNativeSymbolInfo(symbol, dll)$address
  do.call(.Call, c(list(address), arguments))
}

revision_assert_same <- function(expected, actual, task_id, tolerance = FALSE, path = "result") {
  if (!identical(typeof(expected), typeof(actual)) ||
      !identical(length(expected), length(actual)) ||
      !identical(attributes(expected), attributes(actual))) {
    stop(sprintf("%s %s type, length, or attributes differ", task_id, path))
  }
  if (is.list(expected)) {
    for (index in seq_along(expected)) {
      revision_assert_same(
        expected[[index]], actual[[index]], task_id, tolerance,
        sprintf("%s[[%d]]", path, index)
      )
    }
    return(invisible(actual))
  }
  if (!tolerance) {
    if (!identical(expected, actual)) stop(sprintf("%s %s values differ", task_id, path))
    return(invisible(actual))
  }
  expected_na <- is.na(expected) & !is.nan(expected)
  actual_na <- is.na(actual) & !is.nan(actual)
  expected_nan <- is.nan(expected)
  actual_nan <- is.nan(actual)
  expected_infinite <- is.infinite(expected)
  actual_infinite <- is.infinite(actual)
  if (!identical(expected_na, actual_na) || !identical(expected_nan, actual_nan) ||
      !identical(expected_infinite, actual_infinite) ||
      !identical(expected[expected_infinite], actual[actual_infinite])) {
    stop(sprintf("%s %s special-value positions differ", task_id, path))
  }
  finite <- is.finite(expected)
  if (any(finite)) {
    scale <- pmax(1, abs(expected[finite]))
    if (any(abs(expected[finite] - actual[finite]) / scale > 1e-12)) {
      stop(sprintf("%s %s finite values exceed relative tolerance", task_id, path))
    }
  }
  invisible(actual)
}

direct_revision_native_checks <- function(dll) {
  list(
    same_sexp = function(x, y) {
      isTRUE(revision_native_call(dll, "c_benchmark_same_sexp", list(x, y)))
    },
    is_altrep = function(x) {
      isTRUE(revision_native_call(dll, "c_revision_is_altrep", list(x)))
    },
    is_unmaterialized_altrep = function(x) {
      isTRUE(revision_native_call(dll, "c_revision_altrep_unmaterialized", list(x)))
    }
  )
}

direct_assert_result_parity <- function(expected, actual, spec, label) {
  revision_assert_same(expected, actual, label, isTRUE(spec$tolerance))
  invisible(actual)
}

direct_assert_runner_parity <- function(spec, r_result, c_result, runner_result, runner,
                                        r_rng = NULL, c_rng = NULL, runner_rng = NULL) {
  direct_assert_result_parity(r_result, c_result, spec, paste0(spec$id, " R/C"))
  direct_assert_result_parity(r_result, runner_result, spec, paste0(spec$id, " R/", runner))
  direct_assert_result_parity(c_result, runner_result, spec, paste0(spec$id, " C/", runner))
  if (isTRUE(spec$rng)) {
    assert_rng_state_equivalent(r_rng, c_rng, spec$id)
    assert_rng_state_equivalent(r_rng, runner_rng, spec$id)
    assert_rng_state_equivalent(c_rng, runner_rng, spec$id)
  }
  invisible(runner_result)
}

direct_assert_fresh_result <- function(spec, runner, input, result, repeat_result,
                                       same_sexp) {
  allocating <- validate_direct_task_suitability()
  allocating <- allocating$task[allocating$large_output | allocating$small_output]
  if (!is.null(repeat_result)) {
    direct_assert_result_parity(
      result, repeat_result, spec, paste0(spec$id, " repeated ", runner)
    )
    if (spec$id %in% allocating && same_sexp(result, repeat_result)) {
      stop(sprintf("%s/%s reused its prior allocating result", runner, spec$id))
    }
  }
  if (spec$id %in% allocating && length(input) > 0L && same_sexp(input[[1L]], result)) {
    stop(sprintf("%s/%s returned its input instead of a fresh result", runner, spec$id))
  }
  invisible(result)
}

direct_assert_altrep_phase <- function(spec, value, is_unmaterialized, label,
                                       phase = c("before", "after")) {
  phase <- match.arg(phase)
  if (!direct_task_is_altrep(spec$id)) return(invisible(value))
  observed <- isTRUE(is_unmaterialized(value))
  if (identical(phase, "before")) {
    if (!observed) {
      stop(sprintf("%s phase input is not an unmaterialized compact ALTREP", label))
    }
  } else {
    assert_direct_task_altrep_input(spec, observed, label)
  }
  invisible(value)
}

direct_assert_altrep_result <- function(spec, result, is_altrep, label) {
  if (identical(spec$id, "altrep_materialize") && isTRUE(is_altrep(result))) {
    stop(sprintf("%s/altrep_materialize returned an ALTREP result", label))
  }
  invisible(result)
}

direct_assert_fresh_tree <- function(left, right, same_sexp, label) {
  if ((is.list(left) || length(left) != 1L) && isTRUE(same_sexp(left, right))) {
    stop(sprintf("%s reused an output SEXP", label))
  }
  if (is.list(left) && is.list(right)) {
    if (length(left) != length(right)) {
      stop(sprintf("%s output trees differ", label))
    }
    for (index in seq_along(left)) {
      direct_assert_fresh_tree(
        left[[index]], right[[index]], same_sexp,
        sprintf("%s/child-%d", label, index)
      )
    }
  }
  invisible(TRUE)
}

direct_assert_state_reset <- function(spec, runner, runner_invoke, runner_result, same_sexp) {
  if (identical(spec$id, "external_state")) {
    second <- runner_invoke(list())
    if (!identical(second, 700L)) stop(sprintf("%s external state did not reset", runner))
  }
  if (identical(spec$id, "outputs")) {
    second <- runner_invoke(list())
    direct_assert_fresh_tree(
      runner_result, second, same_sexp, sprintf("%s/outputs", runner)
    )
  }
  invisible(runner_result)
}

run_benchmark_revision_gate <- function(
    root_dir, runner, task_ids = NULL, master_seed = benchmark_master_seed()) {
  if (!runner %in% c("r", "c_call", "zigr", "rcpp", "cpp11", "extendr", "savvy")) {
    stop(sprintf("unknown revision runner: %s", runner))
  }
  source(file.path(root_dir, "src", "r", "run_all.R"), local = .GlobalEnv)
  c_dll <- dyn.load(file.path(root_dir, "src", "c_call", "bench.so"), local = TRUE, now = TRUE)
  on.exit(try(dyn.unload(c_dll[["path"]]), silent = TRUE), add = TRUE)

  runner_environment <- .GlobalEnv
  if (!runner %in% c("r", "c_call")) {
    runner_environment <- direct_runner_environment(root_dir, runner)
  }

  native_checks <- direct_revision_native_checks(c_dll)
  suitability <- validate_direct_task_suitability()
  altrep_tasks <- suitability$task[suitability$representation_changing]
  master_seed <- input_scalar_integer(master_seed, "revision correctness master seed")
  rng_seed <- task_input_seed(master_seed, "rng", "direct-timing-v1")

  specs <- benchmark_revision_task_specs()
  if (!is.null(task_ids)) {
    available <- vapply(specs, `[[`, character(1), "id")
    if (any(!nzchar(task_ids)) || anyDuplicated(task_ids) || !all(task_ids %in% available)) {
      stop("revision correctness selection contains an unknown or duplicate task")
    }
    specs <- specs[match(task_ids, available)]
  }

  for (spec in specs) {
    runner_invoke <- function(arguments) {
      if (identical(runner, "r")) {
        return(do.call(get(spec$function_name, envir = .GlobalEnv), arguments))
      }
      if (identical(runner, "c_call")) {
        return(revision_native_call(c_dll, paste0("c_revision_", spec$id), arguments))
      }
      do.call(get(spec$function_name, envir = runner_environment), arguments)
    }
    r_arguments <- benchmark_revision_arguments(spec, master_seed)
    r_input_fingerprint <- if (length(r_arguments) && !spec$id %in% altrep_tasks) {
      task_arguments_fingerprint(spec$id, r_arguments, "ordinary_r_object")
    } else NULL
    if (spec$id %in% altrep_tasks) {
      direct_assert_altrep_phase(
        spec, r_arguments[[1L]], native_checks$is_unmaterialized_altrep,
        sprintf("R truth for %s", spec$id), "before"
      )
    }

    if (isTRUE(spec$rng)) {
      set.seed(rng_seed, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
    }
    r_result <- do.call(get(spec$function_name, envir = .GlobalEnv), r_arguments)
    if (!is.null(r_input_fingerprint)) {
      assert_immutable_input(spec$id, r_arguments, r_input_fingerprint, "ordinary_r_object")
    }
    r_rng <- if (isTRUE(spec$rng)) rng_state_snapshot() else NULL
    rm(r_arguments)

    c_arguments <- benchmark_revision_arguments(spec, master_seed)
    c_input_fingerprint <- if (length(c_arguments) && !spec$id %in% altrep_tasks) {
      task_arguments_fingerprint(spec$id, c_arguments, "ordinary_r_object")
    } else NULL
    if (spec$id %in% altrep_tasks) {
      direct_assert_altrep_phase(
        spec, c_arguments[[1L]], native_checks$is_unmaterialized_altrep,
        sprintf("C truth for %s", spec$id), "before"
      )
    }
    if (isTRUE(spec$rng)) {
      set.seed(rng_seed, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
    }
    c_result <- revision_native_call(c_dll, paste0("c_revision_", spec$id), c_arguments)
    if (!is.null(c_input_fingerprint)) {
      assert_immutable_input(spec$id, c_arguments, c_input_fingerprint, "ordinary_r_object")
    }
    c_rng <- if (isTRUE(spec$rng)) rng_state_snapshot() else NULL
    direct_assert_result_parity(r_result, c_result, spec, paste0(spec$id, " R/C"))
    direct_assert_altrep_result(spec, r_result, native_checks$is_altrep, "R truth")
    direct_assert_altrep_result(spec, c_result, native_checks$is_altrep, "C truth")
    if (isTRUE(spec$rng)) assert_rng_state_equivalent(r_rng, c_rng, spec$id)
    rm(c_arguments)

    runner_arguments <- benchmark_revision_arguments(spec, master_seed)
    runner_input_fingerprint <- if (length(runner_arguments) && !spec$id %in% altrep_tasks) {
      task_arguments_fingerprint(spec$id, runner_arguments, "ordinary_r_object")
    } else NULL
    if (spec$id %in% altrep_tasks) {
      direct_assert_altrep_phase(
        spec, runner_arguments[[1L]], native_checks$is_unmaterialized_altrep,
        sprintf("%s/%s", runner, spec$id), "before"
      )
    }
    if (isTRUE(spec$rng)) {
      set.seed(rng_seed, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
    }
    runner_result <- runner_invoke(runner_arguments)
    if (!is.null(runner_input_fingerprint)) {
      assert_immutable_input(spec$id, runner_arguments, runner_input_fingerprint, "ordinary_r_object")
    }
    runner_rng <- if (isTRUE(spec$rng)) rng_state_snapshot() else NULL
    direct_assert_runner_parity(
      spec, r_result, c_result, runner_result, runner, r_rng, c_rng, runner_rng
    )

    runner_repeat_result <- NULL
    if (identical(direct_task_batchability(spec$id), "repeat")) {
      runner_repeat_result <- runner_invoke(runner_arguments)
      if (!is.null(runner_input_fingerprint)) {
        assert_immutable_input(spec$id, runner_arguments, runner_input_fingerprint, "ordinary_r_object")
      }
    }
    direct_assert_fresh_result(
      spec, runner, runner_arguments, runner_result, runner_repeat_result,
      native_checks$same_sexp
    )
    direct_assert_altrep_result(spec, runner_result, native_checks$is_altrep, runner)
    direct_assert_state_reset(
      spec, runner, runner_invoke, runner_result, native_checks$same_sexp
    )

    rm(runner_arguments, r_result, c_result, runner_result, runner_repeat_result)
    gc(FALSE)
  }
  invisible(length(specs))
}
