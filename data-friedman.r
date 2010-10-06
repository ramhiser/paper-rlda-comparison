library(mvtnorm)
# TODO: Describe all the data sets more elaborately.

# Function to generate data in the format described in Friedman (1989)
friedman.data <- function(.n = 40, .p = 6, .experiment = 1, .seed = 42) {
	set.seed(.seed)
	# The number of groups is 3 in Friedman (1989).
	num.groups <- 3
	
	stopifnot(is.element(.experiment, 1:6), .n >= 2 * num.groups)
	
	# We use Friedman's original setup except that we ensure that at least 2 observations
	# from each population are observed.
	class.size <- table(c(rep(1:num.groups, 3), sample(num.groups, .n - 3 * num.groups, replace = T)))
	
	mean.1 <- mean.2 <- mean.3 <- NULL
	cov.1 <- cov.2 <- cov.3 <- NULL
	
	params <- NULL
	
	experiment1 <- function() {
		mean.1 <- rep(0, .p)
		mean.2 <- c(3, rep(0, .p - 1))
		mean.3 <- c(0, 3, rep(0, .p - 2))

		cov.1 <- diag(.p)
		cov.2 <- diag(.p)
		cov.3 <- diag(.p)
		
		list(mean.1 = mean.1, mean.2 = mean.2, mean.3 = mean.3,
			cov.1 = cov.1, cov.2 = cov.2, cov.3 = cov.3)
	}
	
	experiment2 <- function() {
		mean.1 <- rep(0, .p)
		mean.2 <- c(3, rep(0, .p - 1))
		mean.3 <- c(0, 4, rep(0, .p - 2))

		cov.1 <- diag(.p)
		cov.2 <- 2 * diag(.p)
		cov.3 <- 3 * diag(.p)
		
		list(mean.1 = mean.1, mean.2 = mean.2, mean.3 = mean.3,
			cov.1 = cov.1, cov.2 = cov.2, cov.3 = cov.3)
	}
	
	experiment3 <- function() {
		eigenvals <- (9 * ((seq_len(.p) - 1)/(.p - 1)) + 1)^2
		
		mean.1 <- rep(0, .p)
		mean.2 <- 2.5 * sqrt(eigenvals / .p) * (.p - seq_len(.p)) / (.p / 2 - 1)
		mean.3 <- (rep(-1, .p) ^ seq_len(.p)) * mean.2

		cov.1 <- diag(eigenvals)
		cov.2 <- cov.1
		cov.3 <- cov.1
		
		list(mean.1 = mean.1, mean.2 = mean.2, mean.3 = mean.3,
			cov.1 = cov.1, cov.2 = cov.2, cov.3 = cov.3)
	}
	
	experiment4 <- function() {
		eigenvals <- (9 * ((seq_len(.p) - 1)/(.p - 1)) + 1)^2
		
		mean.1 <- rep(0, .p)
		mean.2 <- 2.5 * sqrt(eigenvals/.p) * (seq_len(.p) - 1) / (.p/2 - 1)
		mean.3 <- (rep(-1, .p)^seq_len(.p)) * mean.2
		
		cov.1 <- diag(eigenvals)
		cov.2 <- cov.1
		cov.3 <- cov.1
		list(mean.1 = mean.1, mean.2 = mean.2, mean.3 = mean.3, 
			cov.1 = cov.1, cov.2 = cov.2, cov.3 = cov.3)
	}

	experiment5 <- function() {
		eigenvals.1 <- (9 * ((seq_len(.p) - 1)/(.p - 1)) + 1)^2
		eigenvals.2 <- (9 * (.p - seq_len(.p))/(.p - 1) + 1)^2
		eigenvals.3 <- (9 * (seq_len(.p) - (.p - 1)/2) / (.p - 1))^2
		
		mean.1 <- rep(0, .p)
		mean.2 <- mean.1
		mean.3 <- mean.1
		
		cov.1 <- diag(eigenvals.1)
		cov.2 <- diag(eigenvals.2)
		cov.3 <- diag(eigenvals.3)
		list(mean.1 = mean.1, mean.2 = mean.2, mean.3 = mean.3, 
			cov.1 = cov.1, cov.2 = cov.2, cov.3 = cov.3)
	}

	experiment6 <- function() {
		eigenvals.1 <- (9 * ((seq_len(.p) - 1)/(.p - 1)) + 1)^2
		eigenvals.2 <- (9 * (.p - seq_len(.p))/(.p - 1) + 1)^2
		eigenvals.3 <- (9 * (seq_len(.p) - (.p - 1)/2) / (.p - 1))^2
		
		mean.1 <- rep(0, .p)
		mean.2 <- rep(14 / sqrt(.p), .p)
		mean.3 <- (rep(-1, .p)^seq_len(.p)) * mean.2
		
		cov.1 <- diag(eigenvals.1)
		cov.2 <- diag(eigenvals.2)
		cov.3 <- diag(eigenvals.3)
		list(mean.1 = mean.1, mean.2 = mean.2, mean.3 = mean.3, 
			cov.1 = cov.1, cov.2 = cov.2, cov.3 = cov.3)
	}
	
	switch(.experiment,
		params <- experiment1(),
		params <- experiment2(),
		params <- experiment3(),
		params <- experiment4(),
		params <- experiment5(),
		params <- experiment6()
	)
	
	data <- rbind(
		rmvnorm(class.size[1], mean = params$mean.1, sigma = params$cov.1),
		rmvnorm(class.size[2], mean = params$mean.2, sigma = params$cov.2),
		rmvnorm(class.size[3], mean = params$mean.3, sigma = params$cov.3)
		)
	colnames(data) <- paste("X", seq_len(.p), sep = "")
	labels <- factor(unlist(lapply(seq_len(num.groups), function(k) rep(k, class.size[k]))))
	
	data.df <- cbind.data.frame(labels, data)

	return(data.df)
}
