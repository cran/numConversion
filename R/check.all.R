check.all <- function()
{
	eps <- (5:16)/40
	out <- cbind(eps, char.eq=NA, num.eq=NA)
	for (i in seq.int(along.with=eps)) {
		out[i, 2:3] <- check.as.character(eps[i])
	}
	out
}

