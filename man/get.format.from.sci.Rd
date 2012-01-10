\name{get.format.from.sci}
\alias{get.format.from.sci}
\title{
Conversion of the format information.
}
\description{
Convert the output of \code{scientific.mpfr()} to the format
of \code{format.info()}.
}
\usage{
get.format.from.sci(sci)
}
\arguments{
  \item{sci}{ A named numeric vector as produced by \code{scientific.mpfr()}.}
}
\value{
A numeric vector with components \code{"width", "decimal", "exptype"}.
}

\seealso{
\code{\link{scientific.mpfr}}
}
\examples{
get.format.from.sci(scientific.mpfr(1.25, 3))
}

