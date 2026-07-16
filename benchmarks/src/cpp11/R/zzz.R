.onUnload <- function(libpath) {
  library.dynam.unload("zigrCpp11", libpath)
}

bench_external_state <- function() {
  state <- diagnostic_state_new()
  for (i in seq_len(100L)) diagnostic_state_method(state, 7L)
  diagnostic_state_read(state)
}
