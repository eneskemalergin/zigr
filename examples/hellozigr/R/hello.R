#' Sum a numeric vector using Zig
#' @param x a numeric vector
#' @return the sum
#' @export
vector_sum <- function(x) {
  .Call("C_vector_sum", as.numeric(x))
}


#' Sum string byte lengths using Zig
#' @param x a character vector
#' @return the total byte length of all elements
#' @export
string_total_bytes <- function(x) {
  .Call("C_string_total_bytes", as.character(x))
}


