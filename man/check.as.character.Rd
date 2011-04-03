\name{check.as.character}
\alias{check.as.character}
\alias{check.all}
\title{
Test of accuracy of \code{as.character()} for a numeric input using the standard arithmetic.
}
\description{
The function tests, whether small differences of the input
numbers to \code{as.character()} yield different output.
}
\usage{
check.as.character(eps)
check.all()
}
\arguments{
  \item{eps}{Real number between \code{0} and \code{0.5}.}
}
\details{
The function \code{check.as.character()}  creates pairs of close numbers
and checks, whether they are converted to different values by \code{as.character()}.
The parameter \code{eps} controls, how close the tested numbers are.
For \code{eps <= 0.125} the function should not find any pairs of numbers
with equal character value on any platform. On platforms with long double,
no such pairs should be found for \code{eps <= 0.25}. For values between
\code{0.3} and \code{0.5} pairs with equal character values occur on all
platforms also due to inaccuracy of double precision itself. The number
of exponents, for which such pairs occur, increases as \code{eps} increases.

The function \code{check.all()} runs \code{check.as.character()} with
several values of \code{eps}.
}
\value{
The function \code{check.as.character()} returns a vector of length 2. The
first component is the number of exponents, for which some of generated 
pairs of numbers have equal character values. The second component is the
number of exponents, for which some of the generated test numbers
are equal. The test is meaningful only if the second component is zero.

The function \code{check.all()} returns the table of the used values
of \code{eps} and the corresponding number of exponents with equality
among the character and the numeric values.
}

\examples{
if (.Machine$sizeof.longdouble >= 10) {
    eps <- 0.25
} else {
    eps <- 0.125
}
errors <- check.as.character(eps)[1]
if (errors != 0) {
    stop(errors, " errors found with eps = ", eps)
}
}

