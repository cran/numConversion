\name{check.format}
\alias{check.format}
\title{
Verifies \code{format.info()} using extended arithmetic.
}
\description{
Verifies \code{format.info()} using extended arithmetic in Rmpfr package
for a number specified by its mantissa and exponents. Also, a few close
numbers, which differ only in the last bits are checked.
}
\usage{
check.format(mantissa, kpower, verbose = 2)
}
\arguments{
  \item{mantissa}{ Character, the required mantissa.}
  \item{kpower}{ Numeric, the decimal exponent.}
  \item{verbose}{ Specify verbosity level 0, 1 or 2.}
}
\details{
The \code{digits} argument for \code{format.info()} is one
digit less than the number of digits in \code{mantissa}.
The result of the test is platform dependent.
The expected errors occur on the boundary between rounding
to different number of digits, so the last two digits of the
mantissa should be "05" or "95". The input number \code{x} is
converted to numeric type from mpfr type, so it is rounded
towards zero in the last bit. The test is performed for
components of \code{x + (-3:4)*eps}, where \code{eps} is 1 ulp
(unit in the last place).

Function \code{format.info(x)} makes errors in the sense that its
output may not be correct for \code{x}, but for some \code{x'},
which differs slightly from \code{x}. The magnitude of the error
can be measured by the distance of \code{x'} from \code{x} in ulp
(units in the last place). This magnitude is approximated as the
maximum length of a sequence of consecutive numbers, which differ
by 1 ulp, which all lead to an error in \code{format.info(x)}.
}
\value{
The maximum number of consecutive components of \code{x + (-3:4)*eps},
which lead to an error.
}

\examples{
# the output is platform dependent
\dontrun{
check.format("7.805", 27)
check.format("2.7034305", -17)
check.format("8.497230006983295", -12)
}
}

