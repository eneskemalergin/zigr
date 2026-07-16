fixture_zero <- function() .Call(C_fixture_zero)

fixture_scalar <- function(value) .Call(C_fixture_scalar, value)

fixture_numeric <- function(value) .Call(C_fixture_numeric, value)

fixture_altrep_integer <- function(value) .Call(C_fixture_altrep_integer, value)

fixture_strings <- function(value) .Call(C_fixture_strings, value)

fixture_raw <- function(value) .Call(C_fixture_raw, value)

fixture_complex <- function(value) .Call(C_fixture_complex, value)

fixture_schema <- function(value) .Call(C_fixture_schema, value)

fixture_new <- function() .Call(C_fixture_new)

fixture_method <- function(state, amount) .Call(C_fixture_FixtureState__increment, state, amount)

fixture_read <- function(state) .Call(C_fixture_FixtureState__read, state)

fixture_error <- function(trigger) invisible(.Call(C_fixture_error, trigger))

fixture_lifecycle_reset <- function() invisible(.Call(C_fixture_lifecycle_reset))

fixture_lifecycle_counts <- function() .Call(C_fixture_lifecycle_counts)

fixture_outputs <- function() .Call(C_fixture_outputs)

bench_vector_sum <- function(x) .Call(C_bench_vector_sum, x)
bench_numeric_transform <- function(x) .Call(C_bench_numeric_transform, x)
bench_broadcast <- function(x, scalar) .Call(C_bench_broadcast, x, scalar)
bench_sort <- function(x) .Call(C_bench_sort, x)
bench_missing_mean <- function(x) .Call(C_bench_missing_mean, x)
bench_transpose <- function(x) .Call(C_bench_transpose, x)
bench_rowcol <- function(x) .Call(C_bench_rowcol, x)
bench_matmul <- function(x, y) .Call(C_bench_matmul, x, y)
bench_dataframe <- function(x) .Call(C_bench_dataframe, x)
bench_list_sum <- function(x) .Call(C_bench_list_sum, x)
bench_string_concat <- function(x) .Call(C_bench_string_concat, x)
bench_string_metadata <- function(x) .Call(C_bench_string_metadata, x)
bench_factor <- function(x) .Call(C_bench_factor, x)
bench_attributes <- function(x) .Call(C_bench_attributes, x)
bench_s4 <- function(x) .Call(C_bench_s4, x)
bench_logical_counts <- function(x) .Call(C_bench_logical_counts, x)
bench_raw_copy <- function(x) .Call(C_bench_raw_copy, x)
bench_complex_conjugate <- function(x) .Call(C_bench_complex_conjugate, x)
bench_schema <- function(x) .Call(C_bench_schema, x)
bench_altrep_sum <- function(x) .Call(C_bench_altrep_sum, x)
bench_altrep_index <- function(x) .Call(C_bench_altrep_index, x)
bench_altrep_materialize <- function(x) .Call(C_bench_altrep_materialize, x)
bench_eval <- function(x) .Call(C_bench_eval, x)
bench_serialize <- function(x) .Call(C_bench_serialize, x)
bench_rng <- function(n) .Call(C_bench_rng, n)
bench_outputs <- function() .Call(C_bench_outputs)

bench_external_state <- function() {
  state <- fixture_new()
  for (i in seq_len(100L)) fixture_method(state, 7L)
  fixture_read(state)
}
