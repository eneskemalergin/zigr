fixture_zero <- function() .Call(wrap__fixture_zero)

fixture_scalar <- function(value) .Call(wrap__fixture_scalar, value)

fixture_numeric <- function(value) .Call(wrap__fixture_numeric, value)

fixture_altrep_integer <- function(value) .Call(wrap__fixture_altrep_integer, value)

fixture_strings <- function(value) .Call(wrap__fixture_strings, value)

fixture_raw <- function(value) .Call(wrap__fixture_raw, value)

fixture_complex <- function(value) .Call(wrap__fixture_complex, value)

fixture_logical_counts <- function(value) .Call(wrap__fixture_logical_counts, value)

fixture_schema <- function(value) .Call(wrap__fixture_schema, value)

fixture_error <- function(`_trigger`) .Call(wrap__fixture_error, `_trigger`)

fixture_lifecycle_reset <- function() .Call(wrap__fixture_lifecycle_reset)

fixture_lifecycle_counts <- function() .Call(wrap__fixture_lifecycle_counts)

fixture_outputs <- function() .Call(wrap__fixture_outputs)

bench_vector_sum <- function(x) .Call(wrap__bench_vector_sum, x)
bench_numeric_transform <- function(x) .Call(wrap__bench_numeric_transform, x)
bench_broadcast <- function(x, scalar) .Call(wrap__bench_broadcast, x, scalar)
bench_sort <- function(x) .Call(wrap__bench_sort, x)
bench_missing_mean <- function(x) .Call(wrap__bench_missing_mean, x)
bench_transpose <- function(x) .Call(wrap__bench_transpose, x)
bench_rowcol <- function(x) .Call(wrap__bench_rowcol, x)
bench_matmul <- function(x, y) .Call(wrap__bench_matmul, x, y)
bench_dataframe <- function(x) .Call(wrap__bench_dataframe, x)
bench_list_sum <- function(x) .Call(wrap__bench_list_sum, x)
bench_string_concat <- function(x) .Call(wrap__bench_string_concat, x)
bench_string_metadata <- function(x) .Call(wrap__bench_string_metadata, x)
bench_factor <- function(x) .Call(wrap__bench_factor, x)
bench_attributes <- function(x) .Call(wrap__bench_attributes, x)
bench_s4 <- function(x) .Call(wrap__bench_s4, x)
bench_logical_counts <- function(x) .Call(wrap__bench_logical_counts, x)
bench_raw_copy <- function(x) .Call(wrap__bench_raw_copy, x)
bench_complex_conjugate <- function(x) .Call(wrap__bench_complex_conjugate, x)
bench_schema <- function(x) .Call(wrap__bench_schema, x)
bench_altrep_sum <- function(x) .Call(wrap__bench_altrep_sum, x)
bench_altrep_index <- function(x) .Call(wrap__bench_altrep_index, x)
bench_altrep_materialize <- function(x) .Call(wrap__bench_altrep_materialize, x)
bench_eval <- function(x) .Call(wrap__bench_eval, x)
bench_serialize <- function(x) .Call(wrap__bench_serialize, x)
bench_rng <- function(n) .Call(wrap__bench_rng, n)
bench_outputs <- function() .Call(wrap__bench_outputs)

FixtureState <- new.env(parent = emptyenv())

FixtureState$new <- function() .Call(wrap__FixtureState__new)

FixtureState$increment <- function(amount) .Call(wrap__FixtureState__increment, self, amount)

FixtureState$read <- function() .Call(wrap__FixtureState__read, self)

#' @export
`$.FixtureState` <- function (self, name) { func <- FixtureState[[name]]; environment(func) <- environment(); func }

#' @export
`[[.FixtureState` <- `$.FixtureState`
