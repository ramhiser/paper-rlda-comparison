library(mvtnorm)
library(corpcor) # for cov.shrink and inv.cov.shrink for MLDA
library(plyr)
library(MASS)
source("data-guo.r")

# Temporarily we have to manually run the source code from the RLDA project.
# We are using a rlda.dir because the directory on my Mac differs from the RLDA directory on the Baylor cluster.
rlda.dir <- "~/rlda.git/"
#rlda.dir <- "~/Dropbox/R/rlda/R/"
source(paste(rlda.dir, "rlda.r", sep = ""))
source(paste(rlda.dir, "mkhadri.r", sep = ""))
source(paste(rlda.dir, "predict.r", sep = ""))
source(paste(rlda.dir, "summary.r", sep = ""))

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
	data.frame(N = N, p = p, method = rlda.method, error = error.rates)
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
sample.sizes <- c(50)
dim.features <- c(250, 500, 1000)
test.size <- 100

rho = 0.9
block.size = 50

parallel.flag <- TRUE

experiment <- expand.grid(sample.sizes, dim.features)
names(experiment) <- c("N", "p")

lda.results <- guo.sim(experiment, "lda", num.replications, rho = rho, block.size = block.size, parallel.flag = parallel.flag)
nlda.results <- guo.sim(experiment, "nlda", num.replications, rho = rho, block.size = block.size, parallel.flag = parallel.flag)
mlda.results <- guo.sim(experiment, "mlda", num.replications, rho = rho, block.size = block.size, parallel.flag = parallel.flag)
mkhadri.results <- guo.sim(experiment, "mkhadri", num.replications, rho = rho, block.size = block.size, parallel.flag = parallel.flag)

sim.results <- rbind(lda.results, nlda.results, mlda.results, mkhadri.results)

save(sim.results, file = "rlda-guo-sim-results.RData", sep = ""))