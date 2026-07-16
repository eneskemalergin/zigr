r_bench_fib_recursive <- function(n) {
  if (n <= 1) return(as.numeric(n))
  as.numeric(Recall(n - 1)) + as.numeric(Recall(n - 2))
}

r_bench_vectorsum <- function(x) sum(x)

r_bench_transpose <- function(m) t(m)

r_bench_strings <- function(x) paste0(x, collapse = ", ")

r_bench_dataframe <- function(df) {
  sub <- df[df$x > 0 & !is.na(df$x), , drop = FALSE]
  sub$z <- sub$x / sub$y
  if (nrow(sub) == 0) return(data.frame(grp = integer(), z_sum = numeric()))
  agg <- aggregate(z ~ grp, data = sub, FUN = sum)
  names(agg) <- c("grp", "z_sum")
  agg$grp <- as.integer(agg$grp)
  agg
}

r_bench_na_prop <- function(x) mean(x, na.rm = TRUE)

r_bench_parallel <- function(x) sum(x)

r_bench_protect_stress <- function(n) 0L
r_bench_protect_shallow <- function(x) 0L
r_bench_protect_scaling <- function(x) 0L

r_bench_blas_matmul <- function(A, B) A %*% B

r_bench_crossprod <- function(X) crossprod(X)

r_bench_cholesky <- function(A) chol(A)

r_bench_lm <- function(X, y) {
  as.numeric(lm.fit(X, y)$coefficients)
}

r_bench_rowsums <- function(X) rowSums(X)

r_bench_elem_ops <- function(x) {
  cbind(abs(x), log(ifelse(x > 0, x, 1)), exp(x), sqrt(ifelse(x >= 0, x, 0)))
}

r_bench_rowcol_means <- function(X) {
  list(rowMeans(X), colSums(X))
}

r_bench_broadcast <- function(x, s) sum(x + s)

r_bench_sort <- function(x) sort(x)

r_bench_cumsum <- function(x) cumsum(x)

r_bench_rnorm <- function(n) rnorm(n)

r_bench_string_nchar <- function(x) sum(nchar(x, type = "bytes"), na.rm = TRUE)

r_bench_string_encoding <- function(x) {
  sum(Encoding(x) == "UTF-8")
}

r_bench_factor_ops <- function(x) {
  sum(as.integer(factor(x)), na.rm = TRUE)
}

r_bench_attrib_ops <- function(x) {
  class(x) <- "bench_class"
  attr(x, "creator") <- "zigr_bench"
  sum(nchar(class(x))) + sum(nchar(attr(x, "creator")))
}

ensure_bench_s4_class <- function() {
  if (!methods::isClass("BenchS4")) {
    methods::setClass("BenchS4", representation(slot_x = "numeric"))
  }
  invisible(NULL)
}
ensure_bench_s4_class()
r_bench_s4_slot_access <- function(x) {
  obj <- methods::new("BenchS4", slot_x = x)
  methods::slot(obj, "slot_x")
}
r_bench_which_na <- function(x) which(is.na(x))

r_bench_long_vector_idx <- function(x) {
  total <- 0
  n <- length(x)
  for (i in seq(1L, n, by = 10000L)) {
    total <- total + x[i]
  }
  total
}

r_bench_l1_arithmetic <- function(x) {
  total <- 0.0
  for (rep in 1:2500) {
    for (v in x) {
      total <- total + v * 0.5 + 0.5
    }
  }
  total
}

r_bench_altrep_sum <- function(n) {
  x <- seq_len(n)
  sum(x)
}

r_bench_altrep_read <- function(x) c(x[1], x[length(x)])

r_bench_altrep_create <- function(n) seq_len(n)

r_bench_altrep_materialize <- function(n) {
  x <- seq_len(n)
  x[]  # force materialization
  x[1] + x[n]
}

r_bench_altrep_elt_walk <- function(n) {
  x <- seq_len(n)
  total <- 0
  for (i in seq_len(n)) total <- total + x[i]
  total
}

r_bench_altrep_region_read <- function(n) {
  x <- seq_len(n)
  total <- 0
  for (v in x) total <- total + v
  total
}

r_bench_altrep_sum_native <- r_bench_altrep_elt_walk

r_bench_altrep_min_max <- function(n) {
  x <- seq_len(n)
  max(x) - min(x)
}

r_bench_altrep_no_na_query <- function(n) {
  x <- seq_len(n)
  as.integer(any(is.na(x)))
}

r_bench_owned_altrep_real_sum <- function(n) {
  x <- as.double(((seq_len(n) - 1L) %% 1024L) + 1L)
  sum(x)
}

r_bench_owned_altrep_int_sum <- function(n) {
  x <- ((seq_len(n) - 1L) %% 1024L) + 1L
  sum(x)
}

r_bench_owned_altrep_logical_sum <- function(n) {
  x <- rep(c(TRUE, FALSE), length.out = n)
  sum(x)
}

r_bench_comptime_dispatch <- function(xs) {
  total <- 0L
  for (i in 1:2048) {
    for (x in xs) {
      t <- typeof(x)
      total <- total + switch(t, double = 1L, integer = 2L, character = 3L, 0L)
    }
  }
  total
}

.type_code <- function(x) {
  switch(typeof(x),
    double = 14L, integer = 13L, character = 16L,
    logical = 10L, list = 19L,
    0L)
}

r_bench_sexp_inspect <- function(xs) {
  total <- 0L
  for (iter in 1:10000) {
    for (x in xs) {
      total <- total + .type_code(x) + is.vector(x) + is.double(x)
    }
  }
  total
}

r_bench_struct_convert <- function(x) {
  list(
    id = as.integer(x$id),
    count = as.integer(x$count),
    level = as.integer(x$level),
    flag = as.logical(x$flag),
    enabled = as.logical(x$enabled),
    ratio = as.numeric(x$ratio),
    offset = as.numeric(x$offset),
    scale = as.numeric(x$scale),
    weights = as.numeric(x$weights),
    indices = as.integer(x$indices)
  )
}

r_bench_r_eval <- function(x) {
  sum(x) + mean(x)
}

r_bench_r_tryeval <- function(x) {
  count <- 0L
  for (i in 1:512) {
    tryCatch(stop("task40"), error = function(e) count <<- count + 1L)
  }
  count
}

r_bench_serialize_roundtrip <- function(x) {
  sum(unserialize(serialize(x, NULL)))
}

r_bench_external_ptr <- function(x) {
  x
}

r_bench_rng_stress <- function(n) {
  rnorm(n)
}

# Authored semantic oracles used for correctness where the task can be
# expressed without delegating its kernel to a compiled base primitive. The
# timed R runner remains independently classified in the evidence manifest.
r_oracle_vectorsum <- function(x) {
  total <- 0.0
  for (value in x) total <- total + value
  total
}

r_oracle_broadcast <- function(x, scalar) {
  total <- 0.0
  for (value in x) {
    increment <- value + scalar
    total <- total + increment
  }
  total
}

r_oracle_transpose <- function(x) {
  dimensions <- dim(x)
  result <- matrix(0.0, dimensions[[2L]], dimensions[[1L]])
  for (column in seq_len(dimensions[[2L]])) {
    for (row in seq_len(dimensions[[1L]])) result[[column, row]] <- x[[row, column]]
  }
  result
}

r_oracle_rowsums <- function(x) {
  dimensions <- dim(x)
  result <- numeric(dimensions[[1L]])
  for (column in seq_len(dimensions[[2L]])) {
    for (row in seq_len(dimensions[[1L]])) result[[row]] <- result[[row]] + x[[row, column]]
  }
  result
}

r_oracle_rowcol_means <- function(x) {
  dimensions <- dim(x)
  row_means <- numeric(dimensions[[1L]])
  column_sums <- numeric(dimensions[[2L]])
  for (column in seq_len(dimensions[[2L]])) {
    for (row in seq_len(dimensions[[1L]])) {
      value <- x[[row, column]]
      row_means[[row]] <- row_means[[row]] + value
      column_sums[[column]] <- column_sums[[column]] + value
    }
  }
  for (row in seq_len(dimensions[[1L]])) row_means[[row]] <- row_means[[row]] / dimensions[[2L]]
  list(row_means, column_sums)
}

r_oracle_na_mean <- function(x) {
  total <- 0.0
  count <- 0L
  for (value in x) {
    if (!is.na(value)) {
      total <- total + value
      count <- count + 1L
    }
  }
  if (count == 0L) return(NaN)
  total / count
}

r_oracle_altrep_min_max <- function(n) {
  x <- seq_len(n)
  minimum <- x[[1L]]
  maximum <- minimum
  for (index in seq_len(n)) {
    value <- x[[index]]
    if (value < minimum) minimum <- value
    if (value > maximum) maximum <- value
  }
  maximum - minimum
}

r_oracle_altrep_no_na <- function(n) {
  x <- seq_len(n)
  for (index in seq_len(n)) {
    if (is.na(x[[index]])) return(1L)
  }
  0L
}

r_oracle_struct_convert <- function(x) {
  list(
    id = x[[1L]], count = x[[2L]], level = x[[3L]], flag = x[[4L]],
    enabled = x[[5L]], ratio = x[[6L]], offset = x[[7L]], scale = x[[8L]],
    weights = x[[9L]], indices = x[[10L]]
  )
}

# These do the smallest useful work so fixture cost stays visible.
r_boundary_zero <- function() 1.0
r_boundary_scalar <- function(x) {
  if (typeof(x) != "double" || length(x) != 1L) stop("scalar expected one REAL value")
  if (is.na(x) && !is.nan(x)) stop("scalar expected a non-missing REAL value")
  x
}
r_boundary_optional <- function(x) {
  if (is.null(x)) return(0L)
  if (typeof(x) != "double" || length(x) != 1L) stop("optional expected NULL or one REAL value")
  if (is.na(x) && !is.nan(x)) 0L else 1L
}
r_boundary_numeric <- function(x) sum(x)
r_boundary_altrep_integer <- function(x) as.double(sum(x))
r_boundary_string_view <- function(x) as.integer(sum(!is.na(x)))
r_boundary_raw <- function(x) as.integer(sum(as.integer(x)))
r_boundary_complex <- function(x) sum(Re(x))
r_boundary_schema <- function(x) {
  fields <- c("id", "count", "ratio", "enabled")
  if (typeof(x) != "list" || length(x) != 4L || !identical(attributes(x), list(names = fields))) stop("fixed schema expected names only")
  if (!is.integer(x[[1L]]) || length(x[[1L]]) != 1L || is.na(x[[1L]])) stop("fixed schema expected a non-missing integer id")
  if (!is.integer(x[[2L]]) || length(x[[2L]]) != 1L || is.na(x[[2L]])) stop("fixed schema expected a non-missing integer count")
  if (!is.double(x[[3L]]) || length(x[[3L]]) != 1L || (is.na(x[[3L]]) && !is.nan(x[[3L]]))) stop("fixed schema expected a non-missing real ratio")
  if (!is.logical(x[[4L]]) || length(x[[4L]]) != 1L || is.na(x[[4L]])) stop("fixed schema expected a non-missing logical enabled")
  x
}
r_boundary_external_method <- function(receiver, amount) as.integer(amount)
r_boundary_external <- function(x) as.double(x + 1.0)

# Normalized fixtures. These functions are authored R implementations of
# the declared kernels. Vectorized alternatives are kept under separate names.
r_fixture_zero <- function() {
  result <- integer(1L)
  result[[1L]] <- 1L
  result
}

r_fixture_scalar <- function(x) {
  if (typeof(x) != "double" || length(x) != 1L) stop("scalar expected one REAL value")
  if (is.na(x) && !is.nan(x)) stop("scalar expected a non-missing REAL value")
  result <- numeric(1L)
  result[[1L]] <- x[[1L]]
  result
}

r_fixture_numeric <- function(x) {
  if (typeof(x) != "double") stop("numeric fixture expected a double vector")
  result <- numeric(length(x))
  for (index in seq_len(length(x))) result[[index]] <- x[[index]] * 2.0
  result
}

r_fixture_altrep_integer <- function(x) {
  if (typeof(x) != "integer") stop("ALTREP fixture expected an integer vector")
  total <- 0.0
  for (index in seq_len(length(x))) total <- total + x[[index]]
  total
}

r_fixture_strings <- function(x) {
  if (typeof(x) != "character") stop("string fixture expected a character vector")
  count <- 0L
  for (index in seq_len(length(x))) {
    if (!is.na(x[[index]])) count <- count + 1L
  }
  count
}

r_fixture_raw <- function(x) {
  if (typeof(x) != "raw") stop("raw fixture expected a raw vector")
  result <- raw(length(x))
  for (index in seq_len(length(x))) result[[index]] <- x[[index]]
  result
}

r_fixture_complex <- function(x) {
  if (typeof(x) != "complex") stop("complex fixture expected a complex vector")
  result <- complex(length(x))
  for (index in seq_len(length(x))) result[[index]] <- x[[index]]
  result
}

r_fixture_logical_counts <- function(x) {
  if (typeof(x) != "logical") stop("logical fixture expected a logical vector")
  result <- integer(3L)
  for (index in seq_len(length(x))) {
    value <- x[[index]]
    if (is.na(value)) {
      result[[3L]] <- result[[3L]] + 1L
    } else if (value) {
      result[[2L]] <- result[[2L]] + 1L
    } else {
      result[[1L]] <- result[[1L]] + 1L
    }
  }
  names(result) <- c("false", "true", "missing")
  result
}

r_fixture_schema <- function(x) {
  fields <- c("id", "count", "ratio", "enabled")
  if (typeof(x) != "list" || length(x) != 4L || !identical(attributes(x), list(names = fields))) stop("fixed schema expected names only")
  if (!is.integer(x[[1L]]) || length(x[[1L]]) != 1L || is.na(x[[1L]])) stop("fixed schema expected a non-missing integer id")
  if (!is.integer(x[[2L]]) || length(x[[2L]]) != 1L || is.na(x[[2L]])) stop("fixed schema expected a non-missing integer count")
  if (!is.double(x[[3L]]) || length(x[[3L]]) != 1L || (is.na(x[[3L]]) && !is.nan(x[[3L]]))) stop("fixed schema expected a non-missing real ratio")
  if (!is.logical(x[[4L]]) || length(x[[4L]]) != 1L || is.na(x[[4L]])) stop("fixed schema expected a non-missing logical enabled")
  x
}

r_fixture_error <- function(trigger) {
  if (typeof(trigger) != "double" || length(trigger) != 1L) stop("error trigger expected one REAL value")
  stop("fixture error")
}

r_fixture_outputs <- function() {
  numeric_output <- numeric(2L)
  numeric_output[[1L]] <- 1.5
  numeric_output[[2L]] <- NA_real_
  string_output <- character(1L)
  string_output[[1L]] <- "fixture"
  raw_output <- raw(3L)
  raw_output[[1L]] <- as.raw(1L)
  raw_output[[2L]] <- as.raw(2L)
  raw_output[[3L]] <- as.raw(3L)
  complex_output <- complex(2L)
  complex_output[[1L]] <- 1.0 + 2.0i
  complex_output[[2L]] <- NA_complex_
  logical_output <- logical(3L)
  logical_output[[1L]] <- FALSE
  logical_output[[2L]] <- TRUE
  logical_output[[3L]] <- NA
  nested_output <- vector("list", 1L)
  nested_output[[1L]] <- 7L
  names(nested_output) <- "value"
  result <- vector("list", 6L)
  result[[1L]] <- numeric_output
  result[[2L]] <- string_output
  result[[3L]] <- raw_output
  result[[4L]] <- complex_output
  result[[5L]] <- logical_output
  result[[6L]] <- nested_output
  names(result) <- c("numeric", "string", "raw", "complex", "logical", "list")
  result
}

r_optimized_fixture_numeric <- function(x) {
  result <- x * 2.0
  attributes(result) <- NULL
  result
}
r_optimized_fixture_altrep_integer <- function(x) as.double(sum(x))

# Four passes keep cache setup visible instead of hiding it in repetition.
r_string_total <- function(x) as.integer(sum(nchar(x, type = "bytes"), na.rm = TRUE))
r_string_cache_build <- function(x) as.integer(length(x))
r_string_repeated_total <- function(x) {
  total <- 0L
  for (pass in seq_len(4L)) total <- total + r_string_total(x)
  total
}
r_raw <- function(x) as.integer(sum(as.integer(x)))
r_complex_view <- function(x) sum(Re(x) + Im(x))
r_complex_return <- function(x) x + (0 + 0i)

r_bench_sexp_create <- function(n) {
  for (i in seq_len(n)) {
    x <- numeric(1)
  }
  0L
}

r_bench_list_access <- function(x) {
  total <- 0.0
  for (index in seq_len(length(x))) total <- total + x[[index]][[1L]]
  total
}

r_bench_na_prop_vary <- function(xs) {
  setNames(vapply(xs, function(x) mean(x, na.rm = TRUE), numeric(1)), names(xs))
}

r_bench_scale_law <- function(xs) {
  setNames(vapply(xs, sum, numeric(1)), names(xs))
}

r_bench_arena_vs_rmalloc <- function(x) {
  total <- 0
  for (i in seq_len(100L)) {
    temp <- x + (i * 0.001)
    total <- total + sum(temp)
  }
  total
}

r_bench_prot_overhead <- function(x) {
  repeats <- 4096
  total <- 0
  for (i in seq_len(repeats)) {
    temp <- x + (i * 0.001)
    total <- total + sum(temp)
  }
  setNames(rep(total, 5L), c("unsafe", "manual", "batch", "preserve", "reprotect"))
}

r_bench_longjmp_safety <- function(x) {
  repeats <- 512L
  direct_total <- 0
  try_ok_total <- 0
  try_err_total <- 0
  unwind_ok_total <- 0

  for (i in seq_len(repeats)) {
    bias <- i * 0.001
    direct_total <- direct_total + sum(x + bias)
    try_ok_total <- try_ok_total + tryCatch(sum(x + bias), error = function(e) stop(e))
    try_err_total <- try_err_total + tryCatch({ stop("task32"); 0 }, error = function(e) 1)
    unwind_ok_total <- unwind_ok_total + tryCatch(sum(x + bias), error = function(e) stop(e))
  }

  setNames(c(direct_total, try_ok_total, try_err_total, unwind_ok_total),
           c("direct", "try_ok", "try_err", "unwind_ok"))
}

r_bench_translate_c_cost <- function(x) {
  repeats <- 512L
  abs_total <- 0
  log_total <- 0
  exp_total <- 0
  sqrt_total <- 0

  for (i in seq_len(repeats)) {
    shifted <- x + (i * 0.001)
    abs_total <- abs_total + sum(abs(shifted - 0.75))
    log_total <- log_total + sum(log(shifted))
    exp_total <- exp_total + sum(exp(shifted))
    sqrt_total <- sqrt_total + sum(sqrt(shifted))
  }

  setNames(c(abs_total, log_total, exp_total, sqrt_total),
           c("abs", "log", "exp", "sqrt"))
}

r_bench_string_variants <- function(x) {
  valid <- !is.na(x)
  setNames(
    list(
      paste0(x[valid], collapse = ","),
      as.integer(sum(nchar(x[valid]))),
      as.integer(sum(startsWith(x[valid], "abc"))),
      substring(x, 1L, 3L),
      toupper(x)
    ),
    c("concat", "nchar_sum", "prefix_match", "extract_substr", "to_upper")
  )
}

r_bench_parallel_scaling <- function(x) {
  setNames(
    c(sum(x), sum(x), sum(x), sum(x), sum(x)),
    c("threads_1", "threads_2", "threads_4", "threads_8", "threads_16")
  )
}

r_bench_memory_bandwidth <- function(x) {
  n <- length(x)
  copy_temp_total <- 0
  copy_out_total <- 0
  fill_out_total <- 0

  for (i in 1:2) {
    tmp <- x[]
    copy_temp_total <- copy_temp_total + sum(tmp)

    out <- numeric(n)
    out[] <- x
    copy_out_total <- copy_out_total + sum(out)

    filled <- numeric(n)
    filled[] <- x + 0.5
    fill_out_total <- fill_out_total + sum(filled)
  }

  setNames(
    c(copy_temp_total, copy_out_total, fill_out_total),
    c("copy_temp", "copy_out", "fill_out")
  )
}

# These event names are shared by every benchmark runner.
bench_vector_sum <- function(x) sum(x)
bench_numeric_transform <- r_fixture_numeric
bench_broadcast <- function(x, scalar) sum(x + scalar)
bench_sort <- function(x) sort(x)
bench_missing_mean <- function(x) mean(x, na.rm = TRUE)
bench_transpose <- function(x) t(x)
bench_rowcol <- function(x) list(row_means = rowMeans(x), column_sums = colSums(x))
bench_matmul <- function(x, y) x %*% y
bench_dataframe <- function(x) {
  keep <- !is.na(x$x) & x$x > 0
  z <- x$x[keep] / x$y[keep]
  grp <- as.integer(x$grp[keep])
  data.frame(grp = 1:10, z_sum = vapply(1:10, function(group) sum(z[grp == group]), numeric(1)))
}
bench_list_sum <- function(x) sum(vapply(x, sum, numeric(1)))
bench_string_concat <- function(x) {
  result <- paste(x, collapse = ", ")
  Encoding(result) <- "bytes"
  result
}
bench_string_metadata <- function(x) {
  marks <- Encoding(x)
  c(bytes = sum(nchar(x, type = "bytes"), na.rm = TRUE), utf8 = sum(marks == "UTF-8"),
    latin1 = sum(marks == "latin1"), bytes_marked = sum(marks == "bytes"), missing = sum(is.na(x)))
}
bench_factor <- function(x) factor(x, levels = sprintf("level_%03d", 1:100))
bench_attributes <- function(x) {
  x <- x[]
  class(x) <- "bench_class"
  attr(x, "creator") <- "zigr_bench"
  class(x)
  attr(x, "creator")
  x
}
bench_s4 <- function(x) methods::slot(methods::new("BenchS4", slot_x = x), "slot_x")
bench_logical_counts <- r_fixture_logical_counts
bench_raw_copy <- r_fixture_raw
bench_complex_conjugate <- function(x) Conj(x)
bench_schema <- r_fixture_schema
bench_altrep_sum <- r_fixture_altrep_integer
bench_altrep_index <- function(x) {
  total <- 0
  for (index in seq.int(1L, length(x), by = 10000L)) total <- total + x[[index]]
  total
}
bench_altrep_materialize <- function(x) as.integer(x) + 0L
bench_external_state <- function() {
  state <- new.env(parent = emptyenv())
  state$value <- 0L
  for (i in seq_len(100L)) state$value <- state$value + 7L
  state$value
}
bench_eval <- function(x) eval(quote(sum(x) + mean(x)), envir = list2env(list(x = x), parent = baseenv()))
bench_serialize <- function(x) unserialize(serialize(x, NULL, version = 3L))
bench_rng <- function(n) rnorm(n)
bench_outputs <- r_fixture_outputs
