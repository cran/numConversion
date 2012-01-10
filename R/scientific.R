# rounding middle cases to an even digit
nearbyint <- function(x)
{
	y <- floor(x + 0.5)
	if (y/2 != floor(y/2)) {
		y <-  - floor( - x + 0.5)
	}
	y
}

# Rmpfr version of C function scientific() from R/src/main/format.c
scientific.mpfr <- function(x, digits)
{
	if (x == 0) {
		kpower <- 0
		nsig <- 1
		sgn <- 0
	} else {
		if (x < 0) {
			sgn <- 1
			r <-  -x
		} else {
			sgn <- 0
			r <- x
		}
		r <- mpfr(r, precBits=200)
		kpower <- floor(log10(r))
		a <- nearbyint(r/10^(kpower - digits + 1))
		nsig <- digits
		for (j in 1:nsig) {
			a <- a / 10
			if (a == floor(a)) {
				nsig <- nsig - 1
			} else {
				break
			}
		}
		if (nsig == 0) {
			nsig <- 1
			kpower <- kpower + 1
		}
	}
	kpower <- as.numeric(kpower)
	left <- kpower + 1
	c(sgn = sgn, kpower = kpower, nsig = nsig,
		left = left, right = nsig - left,
		sleft = sgn + max(1, left))
}

# convert the output of scientific.mpfr() to the format used in format.info()
get.format.from.sci <- function(sci)
{
	right <- max(0, sci["right"])
	fixed <- sci["sleft"] + right + (right >= 1)
	sgn <- sci["sgn"]
	kpower <- sci["kpower"]
	nsig <- sci["nsig"]
	expon <- sgn + nsig + (nsig >= 2) + 4 + (abs(kpower) >= 100)
	if (fixed <= expon) {
		width <- unname(fixed)
		decimal <- unname(right)
		exptype <- 0
	} else {
		width <- unname(expon)
		decimal <- unname(nsig - 1)
		exptype <- unname(1 + (abs(kpower) >= 100))
	}
	c(width = width, decimal = decimal, exptype = exptype)
}

