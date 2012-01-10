library(numConversion)

# Function check.random() searches for errors in format.info(x) and
# an approximation of their magnitude. With "stop=2", the test fails,
# if an error of format.info() of magnitude at least 2 ulp is found.

# On platforms, which have a proper long double type, the error of
# format.info() should be at most 1 ulp, so the test is expected
# to pass.

# On platforms with .Machine$sizeof.longdouble = 8, the error of
# format.info() is at least 2 ulp, so the test is expected to fail.

set.seed(12345)

if (Sys.getenv("INTENSIVE_TESTS") != "") {
	check.random(n=1, dig=2,  show=2, stop=2)
	check.random(n=1, dig=3,  show=2, stop=2)
	check.random(n=1, dig=6,  show=2, stop=2)
	check.random(n=1, dig=7,  show=2, stop=2)
	check.random(n=1, dig=8,  show=2, stop=2)
	check.random(n=1, dig=14, show=2, stop=2)
	check.random(n=1, dig=15, show=2, stop=2)
}


