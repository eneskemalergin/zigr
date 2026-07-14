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

fixture_outputs <- function() .Call(wrap__fixture_outputs)

FixtureState <- new.env(parent = emptyenv())

FixtureState$new <- function() .Call(wrap__FixtureState__new)

FixtureState$increment <- function(amount) .Call(wrap__FixtureState__increment, self, amount)

FixtureState$read <- function() .Call(wrap__FixtureState__read, self)

#' @export
`$.FixtureState` <- function (self, name) { func <- FixtureState[[name]]; environment(func) <- environment(); func }

#' @export
`[[.FixtureState` <- `$.FixtureState`


