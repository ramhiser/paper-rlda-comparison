library(mvtnorm)
library(corpcor) # for cov.shrink and inv.cov.shrink for MLDA
library(plyr)
library(MASS)
source("data-friedman.r")
source("~/Dropbox/R/rlda/R/rlda.r")
source("~/Dropbox/R/rlda/R/mkhadri.r")
source("~/Dropbox/R/rlda/R/predict.r")
source("~/Dropbox/R/rlda/R/summary.r")

set.seed(42)

seeds <- 

# N = num of observations
# p = dimension of feature space
# test.size = number of replications of each experiment
sample.sizes <- c(25, 50, 100)
dim.features <- c(250, 500, 1000)
test.size <- 1000
experiment.num <- 3

experiment <- expand.grid(sample.sizes, dim.features)
names(experiment) <- c("N", "p")


experiment.fcn <- function(N, p) {
	cat("N:", N, "\tp:", p, "\n")
	
	training <- friedman.data(.n = N, .p = p, .experiment = experiment.num)
	test.data <- friedman.data(.n = test.size, .p = p, .experiment = experiment.num)
	
	lda.classifier <- rlda(training, .method = "lda")
	nlda.classifier <- rlda(training, .method = "nlda")
	mlda.classifier <- rlda(training, .method = "mlda")
	mkhadri.classifier <- rlda(training, .method = "mkhadri")
	
	predicted.classes.lda <- predict(lda.classifier, test.data[,-1])$group
	predicted.classes.nlda <- predict(nlda.classifier, test.data[,-1])$group
	predicted.classes.mlda <- predict(mlda.classifier, test.data[,-1])$group
	predicted.classes.mkhadri <- predict(mkhadri.classifier, test.data[,-1])$group
	
	error.rate.lda <- mean(test.data[,1] != predicted.classes.lda)
	error.rate.nlda <- mean(test.data[,1] != predicted.classes.nlda)
	error.rate.mlda <- mean(test.data[,1] != predicted.classes.mlda)
	error.rate.mkhadri <- mean(test.data[,1] != predicted.classes.mkhadri)
	
	
	error.se.lda <- sqrt(error.rate.lda * (1 - error.rate.lda) / test.size)
	error.se.nlda <- sqrt(error.rate.nlda * (1 - error.rate.nlda) / test.size)
	error.se.mlda <- sqrt(error.rate.mlda * (1 - error.rate.mlda) / test.size)
	error.se.mkhadri <- sqrt(error.rate.mkhadri * (1 - error.rate.mkhadri) / test.size)

	
	error.rates <- c(lda = error.rate.lda, nlda = error.rate.nlda, mlda = error.rate.mlda, mkhadri = error.rate.mkhadri)
	error.rates.se <- c(lda = error.se.lda, nlda = error.se.nlda, mlda = error.se.mlda, mkhadri = error.se.mkhadri)
	methods <- c("lda", "nlda", "mlda", "mkhadri")
	error.df <- data.frame(N = N, p = p, method = methods, error = error.rates, std.error = error.rates.se)
}

sim.results <- adply(experiment, 1, function(exper) {experiment.fcn(exper$N, exper$p)}, .progress = "text")

save(experiment.results, file = paste("experiment-results-", experiment.num, ".RData", sep = ""))