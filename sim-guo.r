library(mvtnorm)
library(corpcor) # for cov.shrink and inv.cov.shrink for MLDA
library(plyr)
library(MASS)
source("data-guo.r")
source("~/Dropbox/R/rlda/R/rlda.r")
source("~/Dropbox/R/rlda/R/mkhadri.r")
source("~/Dropbox/R/rlda/R/predict.r")
source("~/Dropbox/R/rlda/R/summary.r")

guo.error.rates <- function(N, p, rlda.method, num.replications, rho, block.size, parallel.flag = FALSE) {
	cat("N:", N, "\tp:", p, "\tMethod:", rlda.method, "\n")
	
	error.rates <- aaply(seq_len(num.replications), 1, function(rep) {
		# For each simulation replication, we use a different seed to generate the
		# random variates. We arbitrarily choose the training seed to be the current
		# replication number and the test data seed to be the same with 1000 added to it.
		training.seed <- rep
		test.seed <- 1000 + rep

		training <- guo.data(n1 = N/2, n2 = N/2, p = p, rho = rho, block.size = block.size, .seed = training.seed)
		test.data <- guo.data(n1 = test.size/2, n2 = test.size/2, p = p, rho = rho, block.size = block.size, .seed = test.seed)

		classifier <- rlda(training, .method = rlda.method)
		predicted.classes <- predict(classifier, test.data[,-1])$group
		mean(test.data[,1] != predicted.classes)
	}, .parallel = parallel.flag, .progress = "text")
	data.frame(N = N, p = p, error = error.rates)
}

guo.sim <- function(experiment, rlda.method, num.replications, rho, block.size, parallel.flag = FALSE) {
	sim.results <- adply(experiment, 1, function(exper) {
		guo.error.rates(N = exper$N,
				p = exper$p,
				rlda.method = rlda.method,
				num.replications = num.replications,
				rho = rho,
				block.size = block.size,
				parallel.flag = parallel.flag
		)
	})
}

# Number of Replications for each classifier
num.replications <- 100

# N = num of observations
# p = dimension of feature space
# test.size = number of replications of each experiment
#sample.sizes <- c(25, 50, 100)
#dim.features <- c(250, 500, 1000)
#test.size <- 1000
#experiment.num <- 3

sample.sizes <- c(30)
dim.features <- c(6)
test.size <- 100
rho = 0.9
block.size = 2

experiment <- expand.grid(sample.sizes, dim.features)
names(experiment) <- c("N", "p")

lda.results <- guo.sim(experiment, "lda", num.replications, rho = rho, block.size = block.size)
nlda.results <- guo.sim(experiment, "nlda", num.replications, rho = rho, block.size = block.size)
mlda.results <- guo.sim(experiment, "mlda", num.replications, rho = rho, block.size = block.size)
mkhadri.results <- guo.sim(experiment, "mkhadri", num.replications, rho = rho, block.size = block.size)

#save(experiment.results, file = paste("experiment-results-", experiment.num, ".RData", sep = ""))