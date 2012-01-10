check.format <- function(mantissa, kpower, verbose=2)
{
	# rounding mantissa to one digit less
	digits <- nchar(gsub("[-\\.]", "", mantissa)) - 1
	stopifnot(digits <= 15)
	stopifnot(kpower >= -290)
	x <- mpfr(mantissa, precBits=200)*mpfr(10, precBits=200)^kpower
	eps <- 2^(floor(log2(x)) - 52)
	shift <-  -3:4
	num <- as.numeric(x + shift*eps)
	err <- rep(0, times=length(shift))
	names(err) <- shift
	for (i in seq.int(along.with=num)) {
		fmt.base <- format.info(num[i], digits)
		fmt.mpfr <- get.format.from.sci(scientific.mpfr(num[i], digits))
		if (any(fmt.base != fmt.mpfr)) {
			err[i] <- 1
		}
		if (any(fmt.base != fmt.mpfr) & verbose >= 1) {
			cat("***** Incorrect format.info() found with\n")
			cat("mantissa  =", mantissa, "\n")
			cat("kpower    =", kpower, "\n")
			cat("ulp shift =", shift[i], "\n")
			cat("***** mpfr number and output of print(,digits=",digits,")\n", sep="")
			print(mpfr(num[i], precBits=100))
			print(num[i], digits=digits)
			cat("***** mpfr and R base computed format info\n")
			print(rbind(mpfr=fmt.mpfr, base=fmt.base))
		}
	}
	if (any(err != 0) & verbose >= 2) {
		cat("***** ulp shifts with errors\n")
		print(err)
		cat("\n")
	}
	r <- rle(err)
	max(r$lengths[r$values == 1], 0)
}

check.random <- function(n, dig, show, stop, verbose=1)
{
	stopifnot(2 <= dig & dig <= 15)
	for (i in seq.int(length.out=n)) {
		x <- as.character(1+floor(9*runif(1)))
		y <- as.character(floor(10*runif(dig - 2)))
		z <- if (runif(1) < 0.5) "0" else "9"
		mantissa <- paste(c(x, ".", y, z, "5"), collapse="")
		for (kpower in -150:150) {
			consecutive.err <- check.format(mantissa, kpower, verbose=0)
			if (consecutive.err >= show)
				check.format(mantissa, kpower, verbose=verbose)
			if (consecutive.err >= stop)
				stop("Error in format found for ", consecutive.err, " consecutive numbers.")
		}
	}
}

