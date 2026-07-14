fixture_new <- function() FixtureState$new()

fixture_method <- function(state, amount) state$increment(amount)

fixture_read <- function(state) state$read()

