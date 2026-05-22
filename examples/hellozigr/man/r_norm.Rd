\name{r_norm}
\alias{r_norm}
\title{Generate Normal Variates}
\description{
  Calls R's \code{rnorm()} from Zig.
}
\usage{
r_norm(n)
}
\arguments{
  \item{n}{number of observations}
}
\value{
  Invisibly calls \code{rnorm(n)}.
}
\examples{
r_norm(5)
}
