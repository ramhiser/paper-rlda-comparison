library(mvtnorm)

# Function to generate mixture (non-Gaussian) data in the format described in Skurichina and Duin (1998).
duin.data <- function(n1 = 100, n2 = 100, p = 100, .seed = 42) {
	set.seed(.seed)
	
	# This paper only deals with binary data.
	num.groups <- 2
	
	# First we generate two multivariate normal samples that share a common covariance matrix but have
	# different means.
	
	# The first mean is a p-dimensional vector of 0's.
	mean1 <- replicate(p, 0)
	
	# The mean of the second class is 0.3 for the first feature,
	# 3 for the second feature,
	# and zero for the remaining features.
	mean2 <- c(0.3, 3, replicate(p - 2, 0))
	
	# The variance for the covariance matrix are 0.01 and 40 for the first two dimensions.
	# The rest of the variances are 1.
	sigma <- diag(c(0.01, 40, replicate(p - 2, 1)))

	# X contains the generated random variates.
	X <- rbind(
		rmvnorm(n1 / 2, mean = mean1, sigma = sigma),
		rmvnorm(n2 / 2, mean = mean2, sigma = sigma)
	)
	
	colnames(X) <- paste("X", seq_len(p), sep = "")
	labels <- as.factor(c(replicate(n1 / 2, 1), replicate(n2 / 2, 2)))
	
	data1 <- cbind.data.frame(labels, X)
	
	# Next we generate two additional multivariate normal samples to obtain
	# a mixture of highly correlated non-Gaussian classes.
	
	# The first mean is a p-dimensional vector with the first two features having mean 10
	# and the remaining features having mean 0.
	mean1 <- c(10, 10, replicate(p - 2, 0))
	
	# The mean of the second class is -9.7 for the first feature,
	# -7 for the second feature,
	# and zero for the remaining features.
	mean2 <- c(-9.7, -7, replicate(p - 2, 0))
	
	# The variance for the covariance matrix are 0.01 and 40 for the first two dimensions.
	# The rest of the variances are 1.
	sigma <- diag(c(0.01, replicate(p - 1, 1)))
	
	# X contains the generated random variates.
	X <- rbind(
		rmvnorm(n1 / 2, mean = mean1, sigma = sigma),
		rmvnorm(n2 / 2, mean = mean2, sigma = sigma)
	)
	
	colnames(X) <- paste("X", seq_len(p), sep = "")
	labels <- as.factor(c(replicate(n1 / 2, 1), replicate(n2 / 2, 2)))
	
	data2 <- cbind.data.frame(labels, X)
	
	# The data.frame contains two classes each with a mixture of Gaussian populations,
	# such that the resulting classes are highly correlated and non-Gaussian.
	data <- rbind(data1, data2)
	
	return(data)
}