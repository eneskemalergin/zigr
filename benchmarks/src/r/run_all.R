# Pure R baseline implementations for all benchmark tasks.
# Each function matches the interface expected by the .Call wrapper:
# first arg is the R object, returns an R object.

# Task 1: Fibonacci (recursive, to match compiled backends)
r_bench_fib_recursive <- function(n) {
  if (n <= 1) return(as.numeric(n))
  as.numeric(Recall(n - 1)) + as.numeric(Recall(n - 2))
}

# Task 2: Vector Sum
r_bench_vectorsum <- function(x) sum(x)

# Task 3: Naive Matrix Multiply
r_bench_transpose <- function(m) t(m)

# Task 4: String Concatenation
r_bench_strings <- function(x) paste0(x, collapse = ", ")

# Task 5: Data Frame Filtering
r_bench_dataframe <- function(df) {
  sub <- df[df$x > 0 & !is.na(df$x), , drop = FALSE]
  sub$z <- sub$x / sub$y
  if (nrow(sub) == 0) return(data.frame(grp = integer(), z_sum = numeric()))
  agg <- aggregate(z ~ grp, data = sub, FUN = sum)
  names(agg) <- c("grp", "z_sum")
  agg$grp <- as.integer(agg$grp)
  agg
}

# Task 6: NA-safe Mean
r_bench_na_prop <- function(x) mean(x, na.rm = TRUE)

# Task 7: Parallel Sum (just sum() in R because R is single-threaded)
r_bench_parallel <- function(x) sum(x)

# Task 9: PROTECT Stress
r_bench_protect_stress <- function(n) 0L
r_bench_protect_shallow <- function(x) 0L
r_bench_protect_scaling <- function(x) 0L

# Task 10: BLAS Matmul
r_bench_blas_matmul <- function(A, B) A %*% B

# Task 11: Cross-product
r_bench_crossprod <- function(X) crossprod(X)

# Task 12: Cholesky
r_bench_cholesky <- function(A) chol(A)

# Task 13: Linear Model
r_bench_lm <- function(X, y) {
  as.numeric(lm.fit(X, y)$coefficients)
}

# Task 14: Row Sums
r_bench_rowsums <- function(X) rowSums(X)

# Task 15: Element-wise ops (abs, log, exp, sqrt)
r_bench_elem_ops <- function(x) {
  cbind(abs(x), log(ifelse(x > 0, x, 1)), exp(x), sqrt(ifelse(x >= 0, x, 0)))
}

# Task 16: Row means and column sums
r_bench_rowcol_means <- function(X) {
  list(rowMeans(X), colSums(X))
}

# Task 17: Vector + scalar broadcast
r_bench_broadcast <- function(x, s) sum(x + s)

# Task 18: Sort
r_bench_sort <- function(x) sort(x)

# Task 19: Cumulative sum
r_bench_cumsum <- function(x) cumsum(x)

# Task 20: Random normal
r_bench_rnorm <- function(n) rnorm(n)

# Task 21: String nchar
r_bench_string_nchar <- function(x) sum(nchar(x), na.rm = TRUE)

# Task 22: String encoding (count UTF-8 encoded strings)
r_bench_string_encoding <- function(x) {
  sum(Encoding(x) == "UTF-8")
}

# Task 20: Factor ops
r_bench_factor_ops <- function(x) {
  sum(as.integer(factor(x)), na.rm = TRUE)
}

# Task 21: Attribute ops
r_bench_attrib_ops <- function(x) {
  class(x) <- "bench_class"
  attr(x, "creator") <- "zigr_bench"
  sum(nchar(class(x))) + sum(nchar(attr(x, "creator")))
}

# Task 22: S4 slot access
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

# Task 24: Long vector index (ALTREP-aware element access)
r_bench_long_vector_idx <- function(x) {
  total <- 0
  n <- length(x)
  for (i in seq(1L, n, by = 10000L)) {
    total <- total + x[i]
  }
  total
}

# Task 25: L1 arithmetic (4000 x 2500 passes)
r_bench_l1_arithmetic <- function(x) {
  total <- 0.0
  for (rep in 1:2500) {
    for (v in x) {
      total <- total + v * 0.5 + 0.5
    }
  }
  total
}

# Task 34: ALTREP sum via R (create seq_len, sum via R)
r_bench_altrep_sum <- function(n) {
  x <- seq_len(n)
  sum(x)
}

# Task 24: ALTREP read: R accesses directly, no materialization
r_bench_altrep_read <- function(x) c(x[1], x[length(x)])

# Task 25: ALTREP create: pure R can create built-in compact intseq ALTREP
r_bench_altrep_create <- function(n) seq_len(n)

# Task 31: ALTREP materialize
r_bench_altrep_materialize <- function(n) {
  x <- seq_len(n)
  x[]  # force materialization
  x[1] + x[n]
}

# Task 32: ALTREP element walk
r_bench_altrep_elt_walk <- function(n) {
  x <- seq_len(n)
  total <- 0
  for (i in seq_len(n)) total <- total + x[i]
  total
}

# Task 33: ALTREP region read
r_bench_altrep_region_read <- function(n) {
  x <- seq_len(n)
  total <- 0
  for (v in x) total <- total + v
  total
}

# Task 34: ALTREP sum via R (uses ALTREP method dispatch)
# Already defined as r_bench_altrep_sum <- function(x) sum(x)
# Mapped in runner JSON as "sum"

# Task 35: ALTREP sum native (same as elt_walk)
r_bench_altrep_sum_native <- r_bench_altrep_elt_walk

# Task 36: ALTREP min/max
r_bench_altrep_min_max <- function(n) {
  x <- seq_len(n)
  max(x) - min(x)
}

# Task 37: ALTREP no-NA query
r_bench_altrep_no_na_query <- function(n) {
  x <- seq_len(n)
  as.integer(any(is.na(x)))
}

# Task 38: Real create + sum over a numeric vector payload
r_bench_owned_altrep_real_sum <- function(n) {
  x <- as.double(((seq_len(n) - 1L) %% 1024L) + 1L)
  sum(x)
}

# Task 39: Integer create + sum over a recycled integer payload
r_bench_owned_altrep_int_sum <- function(n) {
  x <- ((seq_len(n) - 1L) %% 1024L) + 1L
  sum(x)
}

# Task 40: Logical create + sum over alternating flags
r_bench_owned_altrep_logical_sum <- function(n) {
  x <- rep(c(TRUE, FALSE), length.out = n)
  sum(x)
}

# Task 26: Type dispatch over mixed atomic vectors
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

# Task 27: Struct convert
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

# Task 39: R eval (sum + mean via eval)
r_bench_r_eval <- function(x) {
  sum(x) + mean(x)
}

# Task 40: R tryEval (stop error catch)
r_bench_r_tryeval <- function(x) {
  count <- 0L
  for (i in 1:512) {
    tryCatch(stop("task40"), error = function(e) count <<- count + 1L)
  }
  count
}

# Task 41: Serialize roundtrip
r_bench_serialize_roundtrip <- function(x) {
  sum(unserialize(serialize(x, NULL)))
}

# Task 42: External pointer
r_bench_external_ptr <- function(x) {
  x
}

# Task 43: RNG stress
r_bench_rng_stress <- function(n) {
  rnorm(n)
}

# Task 10: SEXP create (100k small vectors)
r_bench_sexp_create <- function(n) {
  for (i in seq_len(n)) {
    x <- numeric(1)
  }
  0L
}

# Task 16: List access (sum first element of each list item)
r_bench_list_access <- function(lst) {
  total <- 0.0
  for (i in seq_along(lst)) {
    total <- total + lst[[i]][1]
  }
  total
}

# Task 28: NA proportion sweep
r_bench_na_prop_vary <- function(xs) {
  setNames(vapply(xs, function(x) mean(x, na.rm = TRUE), numeric(1)), names(xs))
}

# Task 29: Scale law mixed-size vector sums
r_bench_scale_law <- function(xs) {
  setNames(vapply(xs, sum, numeric(1)), names(xs))
}

# Task 30: Allocation strategy benchmark
r_bench_arena_vs_rmalloc <- function(x) {
  total <- 0
  for (i in seq_len(100L)) {
    temp <- x + (i * 0.001)
    total <- total + sum(temp)
  }
  total
}

# Task 31: Protection strategy comparison
r_bench_prot_overhead <- function(x) {
  repeats <- 4096
  total <- 0
  for (i in seq_len(repeats)) {
    temp <- x + (i * 0.001)
    total <- total + sum(temp)
  }
  setNames(rep(total, 5L), c("unsafe", "manual", "batch", "preserve", "reprotect"))
}

# Task 32: Longjmp safety comparison
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

# Task 34: Math call cost comparison
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

# Task 35: String operation suite
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
