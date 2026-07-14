Rcpp::loadModule("zigr_fixture_module", TRUE)

fixture_new <- function() new(FixtureState)

fixture_method <- function(state, amount) state$increment(amount)

fixture_read <- function(state) state$read()

