library(mvtnorm)
library(plyr)

# Returns a p-dimensional matrix with an autocorrelation (autoregressive) structure.
# The value of rho determines the amount of autocorrelation.
autocorr.mat <- function(p = 100, rho = 0.9) {
  mat <- diag(p)
  return(rho^abs(row(mat) - col(mat)))
}

# Returns a data frame of two multivariate normal data sets with their population labels.
# The data sets share a common covariance matrix defined in Guo (2007).
guo.data <- function(n1 = 100, n2 = 100, p = 100, rho = 0.9, block.size = 100, .seed = 42) {
	stopifnot(p %% block.size == 0)
	
	set.seed(.seed)
	
	# This paper only deals with binary data.
	num.groups <- 2
	num.blocks <- p / block.size
	
	# The first mean is a p-dimensional vector of 0's.
	mean1 <- replicate(p, 0)
	
	# The second mean is filled with mostly 0's. We set 10 percent to 1/2.
	mean2 <- c(replicate(round(0.1 * p), 0.5), replicate(round(0.9 * p), 0))
	
	# The common covariance matrix is a block-matrix filled with autocorrelation matrices.
	# See paper for more details.
	sigma <- matrix(0, nrow = p, ncol = p)
	for(i in seq_len(num.blocks)) {
		sigma[seq.int((i - 1) * block.size + 1,i * block.size),] <-
			cbind(matrix(0, nrow = block.size, ncol = (i - 1) * block.size),
				autocorr.mat(block.size, (-1)^(i+1) * rho),
				matrix(0, nrow = block.size, ncol = (num.blocks - i) * block.size))
	}

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