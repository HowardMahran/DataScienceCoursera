myfunction <- function(x) {
	y <- rnorm(100)
	mean(y)
}

newfunction <- function(x) {
	x * rnorm(length(x))
	
}