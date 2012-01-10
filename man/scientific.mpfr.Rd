\name{scientific.mpfr}
\alias{scientific.mpfr}
\title{
A more exact version of \code{scientific()}.
}
\description{
An R function using Rmpfr package, which is a more exact version
of the C function \code{scientific()} from \code{R/src/main/format.c}.
}
\usage{
scientific.mpfr(x, digits)
}
\arguments{
  \item{x}{ Numeric value. }
  \item{digits}{ The required number of digits for rounding. }
}
\value{
A named numeric vector with components
\code{"sgn", "kpower", "nsig", "left", "right", "sleft"}.
}
\examples{
scientific.mpfr(1.25, 3)
}
