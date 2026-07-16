fixture_new <- function() FixtureState$new()

fixture_method <- function(state, amount) state$increment(amount)

fixture_read <- function(state) state$read()

bench_external_state <- function() {
  state <- fixture_new()
  for (i in seq_len(100L)) fixture_method(state, 7L)
  fixture_read(state)
}
