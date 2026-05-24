\name{string_total_bytes}
\alias{string_total_bytes}
\title{Sum String Byte Lengths}
\description{
  Computes the total byte length of a character vector using Zig.
}
\usage{
string_total_bytes(x)
}
\arguments{
  \item{x}{a character vector}
}
\value{
  The total byte length of all elements in \code{x}.
}
\examples{
string_total_bytes(c("a", "bc", "def"))
}