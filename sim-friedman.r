library(mvtnorm)
library(corpcor) # for cov.shrink and inv.cov.shrink for MLDA
library(plyr)
library(MASS)
source("data-friedman.r")
source("~/Dropbox/R/rlda/R/rlda.r")
source("~/Dropbox/R/rlda/R/mkhadri.r")
source("~/Dropbox/R/rlda/R/predict.r")
source("~/Dropbox/R/rlda/R/summary.r")

friedman.error.rates <- function(N, p, rlda.method, num.replications, experiment.num, parallel.flag = FALSE) {
	cat("N:", N, "\tp:", p, "\tMethod:", rlda.method, "\n")
	
	error.rates <- aaply(seq_len(num.replications), 1, function(rep) {
		# For each simulation replication, we use a different seed to generate the
		# random variates. We arbitrarily choose the training seed to be the current
		# replication number and the test data seed to be the same with 1000 added to it.
		training.seed <- rep
		test.seed <- 1000 + rep

		training <- friedman.data(.n = N, .p = p, .experiment = experiment.num, .seed = training.seed)
		test.data <- friedman.data(.n = test.size, .p = p, .experiment = experiment.num, .seed = test.seed)

		classifier <- rlda(training, .method = rlda.method)
		predicted.classes <- predict(classifier, test.data[,-1])$group
		mean(test.data[,1] != predicted.classes)
	}, .parallel = parallel.flag, .progress = "text")
	data.frame(N = N, p = p, error = error.rates)
}

friedman.sim <- function(experiment, rlda.method, num.replications, friedman.experiment.num) {
	sim.results <- adply(experiment, 1, function(exper) {
		friedman.error.rates(N = exper$N,
				p = exper$p,
				rlda.method = rlda.method,
				num.replications = num.replications,
				experiment.num = friedman.experiment.num
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

sample.sizes <- c(40)
dim.features <- c(100)
test.size <- 1000
friedman.experiment.num <- 3

experiment <- expand.grid(sample.sizes, dim.features)
names(experiment) <- c("N", "p")

lda.results <- friedman.sim(experiment, "lda", num.replications, friedman.experiment.num)
nlda.results <- friedman.sim(experiment, "nlda", num.replications, friedman.experiment.num)
mlda.results <- friedman.sim(experiment, "mlda", num.replications, friedman.experiment.num)
mkhadri.results <- friedman.sim(experiment, "mkhadri", num.replications, friedman.experiment.num)

#save(experiment.results, file = paste("experiment-results-", experiment.num, ".RData", sep = ""))