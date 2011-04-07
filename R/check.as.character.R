check.as.character <- function(eps)
{
	kpower <-  -300:308
	base.power <- 10^kpower
	ulp.power <- 10^(kpower - 15)
	n <- 100
	shift <- sort(c(1 + 10*(0:n), 10*(1:n)))
	char <- rep(0, times=length(kpower))
	num <- rep(0, times=length(kpower))
	for (i in seq.int(along.with=shift)) {
		x <- base.power - (shift[i] - eps)*ulp.power
		y <- base.power - (shift[i] - 1 + eps)*ulp.power
		char <- char + (as.character(x) == as.character(y))
		num <- num + (x == y)
	}
	c(sum(char != 0), sum(num != 0))
}

