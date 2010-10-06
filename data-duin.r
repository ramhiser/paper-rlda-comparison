duin.data <- function(n1 = 100, n2 = 100, p = 100) {
	# This paper only deals with binary data.
	num.groups <- 2
	
	# The first mean is a p-dimensional vector of 0's.
	mean1 <- replicate(p, 0)
	
	# The second mean is filled with mostly 0's. We set 10 percent to 1/2.
	mean2 <- c(replicate(round(0.1 * p), 0.5), replicate(round(0.9 * p), 0))
	
	# The standard deviations for the covariance matrix are 0.01 and 40 for the first two dimensions.
	# The rest of the standard deviations are 1.
	# We square these to obtain the variances.
	sigma <- diag(c(0.01, 40, replicate(p - 2, 1))^2)

	# X contains the generated random variates.
	X <- rbind(
		rmvnorm(n1, mean = mean1, sigma = sigma),
		rmvnorm(n2, mean = mean2, sigma = sigma)
	)
	colnames(X) <- paste("X", seq_len(p), sep = "")
	labels <- as.factor(c(replicate(n1, 1), replicate(n2, 2)))
	
	data <- cbind.data.frame(labels, X)
	
	return(data)
}