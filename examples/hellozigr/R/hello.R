#' Sum a numeric vector using Zig
#' @param x a numeric vector
#' @return the sum
#' @export
vector_sum <- function(x) {
  .Call("C_vector_sum", as.numeric(x))
}


#' Generate normal variates using R's rnorm from Zig
#' @param n number of observations
#' @return invisibly calls rnorm, results printed by R
#' @export
r_norm <- function(n) {
  invisible(.Call("r_norm", as.integer(n)))
}
