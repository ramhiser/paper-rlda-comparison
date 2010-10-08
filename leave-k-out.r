run.example.code <- FALSE

# Returns a list of mutually exclusive folds utilizing leave-k-out crossvalidation.
leave.k.out <- function(n, k = 1) {
	obs <- seq_len(n)
	folds <- list()
	fold.i <- 1
	
	while(length(obs) > 0) {
		fold <- numeric(0)
		if(k > length(obs)) {
			fold <- obs
			obs <- numeric(0)
		}
		else {
			fold <- sample(obs, k)
			obs <- obs[-which(obs %in% fold)]
		}
		folds[[paste("fold", fold.i, sep = "")]] <- fold
		fold.i <- fold.i + 1
	}
	folds
}

if(run.example.code) {
	library(plyr)
	n <- 10
	obs <- seq_len(n)
	leave.k.out(n, k = 1)
	folds <- leave.k.out(n, 1)
	folds
	laply(folds, function(fold) obs[-fold])
}