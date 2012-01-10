\name{check.random}
\alias{check.random}
\title{
Call \code{check.format()} on random numbers.
}
\description{
Generates random mantissas and calls \code{check.format()} on them
with a range of exponents.
}
\usage{
check.random(n, dig, show, stop, verbose = 1)
}
\arguments{
  \item{n}{ The number of generated random mantissas. }
  \item{dig}{ The number of digits of the mantissa. }
  \item{show}{ Minimum number of errors to print a report. }
  \item{stop}{ Minimum number of errors to stop search. }
  \item{verbose}{ Verbosity to be passed to \code{check.format()}}
}
\details{
The function generates \code{n} random mantissas with
\code{dig+1} digits, so the tested rounding is to \code{dig}
digits. Each mantissa is tested with exponents from \code{-150:150}.

The number of errors is determined as the sum of the output
vector of \code{check.format()}. See the help for this
function for details.
}
\value{
No output value. The function relies on the reports
generated in \code{check.format()}.
}
\seealso{
\code{\link{check.format}}
}
\examples{
# Print a report for all errors found.
\dontrun{
check.random(n=1, dig=3, show=1, stop=Inf)
}
}
