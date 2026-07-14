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

fixture_outputs <- function() .Call(C_fixture_outputs)
