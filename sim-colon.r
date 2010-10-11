library(mvtnorm)
library(corpcor) # for cov.shrink and inv.cov.shrink for MLDA
library(plyr)
library(MASS)

# For running parallel
library(foreach)
library(doMC)

# Registers the "multicore" parallel backend with the "foreach" package.
registerDoMC()

# Returns the number of parallel execution workers assigned.
getDoParWorkers()

source("leave-k-out.r")
source("variable-selection.r")

# Temporarily we have to manually run the source code from the RLDA project.
# We are using a rlda.dir because the directory on my Mac differs from the RLDA directory on the Baylor cluster.
#rlda.dir <- "~/rlda.git/"
rlda.dir <- "~/Dropbox/R/rlda/R/"
source(paste(rlda.dir, "rlda.r", sep = ""))
source(paste(rlda.dir, "mkhadri.r", sep = ""))
source(paste(rlda.dir, "predict.r", sep = ""))
source(paste(rlda.dir, "summary.r", sep = ""))

# Colon Cancer Data Set from Alon et al. (1999)
colon.df <- read.csv("data/colon-cancer.csv")

# n is the number of observations
# p is the feature dimension
# We subtract 1 from the number of columns to correct for the column of labels.
n <- nrow(colon.df)
p <- ncol(colon.df) - 1

# TODO: Need to use same folds for each classifier.
colon.error.rates <- function(rlda.method, k = 5, variable.selection = FALSE, alpha = 0.01, parallel.flag = FALSE) {
	folds <- leave.k.out(n, k)
	error.rates <- laply(folds, function(fold) {
		training.df <- colon.df[-fold,]
		test.df <- colon.df[fold,]

		if(variable.selection) {
			var.select.out <- variable.selection.t.test(training.df, alpha = alpha)
			training.df <- training.df[, c(1, var.select.out$kept.variables)]
			test.df <- test.df[, c(1, var.select.out$kept.variables)]
		}
		classifier <- rlda(training.df, .method = rlda.method)
		predicted.classes <- predict(classifier, test.df[,-1])$group
		mean(test.df[,1] != predicted.classes)
	}, .progress = "text", .parallel = parallel.flag)
	
	data.frame(method = rlda.method, error = error.rates)
}

set.seed(42)

parallel <- TRUE

# Leave-10-Out Crossvalidation Error Rates for Colon Cancer Data Set
# with variable selection (t-test) and alpha = 0.01
k <- 10
lda.results <- colon.error.rates("lda", k, variable.selection = TRUE, alpha = 0.01, parallel.flag = parallel)
nlda.results <- colon.error.rates("nlda", k, variable.selection = TRUE, alpha = 0.01, parallel.flag = parallel)
mlda.results <- colon.error.rates("mlda", k, variable.selection = TRUE, alpha = 0.01, parallel.flag = parallel)
mkhadri.results <- colon.error.rates("mkhadri", k, variable.selection = TRUE, alpha = 0.01, parallel.flag = parallel)

save(lda.results, nlda.results, mlda.results, mkhadri.results, file = "colon-CV10-varselect-alpha-001.RData")

# Leave-5-Out Crossvalidation Error Rates for Colon Cancer Data Set
# with variable selection (t-test)
k <- 5
lda.results <- colon.error.rates("lda", k, variable.selection = TRUE, alpha = 0.01, parallel.flag = parallel)
nlda.results <- colon.error.rates("nlda", k, variable.selection = TRUE, alpha = 0.01, parallel.flag = parallel)
mlda.results <- colon.error.rates("mlda", k, variable.selection = TRUE, alpha = 0.01, parallel.flag = parallel)
mkhadri.results <- colon.error.rates("mkhadri", k, variable.selection = TRUE, alpha = 0.01, parallel.flag = parallel)

save(lda.results, nlda.results, mlda.results, mkhadri.results, file = "colon-CV5-varselect-alpha-001.RData")

# Leave-1-Out Crossvalidation Error Rates for Colon Cancer Data Set
# with variable selection (t-test)
k <- 1
lda.results <- colon.error.rates("lda", k, variable.selection = TRUE, alpha = 0.01, parallel.flag = parallel)
nlda.results <- colon.error.rates("nlda", k, variable.selection = TRUE, alpha = 0.01, parallel.flag = parallel)
mlda.results <- colon.error.rates("mlda", k, variable.selection = TRUE, alpha = 0.01, parallel.flag = parallel)
mkhadri.results <- colon.error.rates("mkhadri", k, variable.selection = TRUE, alpha = 0.01, parallel.flag = parallel)

save(lda.results, nlda.results, mlda.results, mkhadri.results, file = "colon-CV1-varselect-alpha-001.RData")

# Leave-10-Out Crossvalidation Error Rates for Colon Cancer Data Set
# with variable selection (t-test) and alpha = 0.05
k <- 10
lda.results <- colon.error.rates("lda", k, variable.selection = TRUE, alpha = 0.05, parallel.flag = parallel)
nlda.results <- colon.error.rates("nlda", k, variable.selection = TRUE, alpha = 0.05, parallel.flag = parallel)
mlda.results <- colon.error.rates("mlda", k, variable.selection = TRUE, alpha = 0.05, parallel.flag = parallel)
mkhadri.results <- colon.error.rates("mkhadri", k, variable.selection = TRUE, alpha = 0.05, parallel.flag = parallel)

save(lda.results, nlda.results, mlda.results, mkhadri.results, file = "colon-CV10-varselect-alpha-005.RData")

# Leave-5-Out Crossvalidation Error Rates for Colon Cancer Data Set
# with variable selection (t-test) and alpha = 0.05
k <- 5
lda.results <- colon.error.rates("lda", k, variable.selection = TRUE, alpha = 0.05, parallel.flag = parallel)
nlda.results <- colon.error.rates("nlda", k, variable.selection = TRUE, alpha = 0.05, parallel.flag = parallel)
mlda.results <- colon.error.rates("mlda", k, variable.selection = TRUE, alpha = 0.05, parallel.flag = parallel)
mkhadri.results <- colon.error.rates("mkhadri", k, variable.selection = TRUE, alpha = 0.05, parallel.flag = parallel)

save(lda.results, nlda.results, mlda.results, mkhadri.results, file = "colon-CV5-varselect-alpha-005.RData")

# Leave-1-Out Crossvalidation Error Rates for Colon Cancer Data Set
# with variable selection (t-test) and alpha = 0.05
k <- 1
lda.results <- colon.error.rates("lda", k, variable.selection = TRUE, alpha = 0.05, parallel.flag = parallel)
nlda.results <- colon.error.rates("nlda", k, variable.selection = TRUE, alpha = 0.05, parallel.flag = parallel)
mlda.results <- colon.error.rates("mlda", k, variable.selection = TRUE, alpha = 0.05, parallel.flag = parallel)
mkhadri.results <- colon.error.rates("mkhadri", k, variable.selection = TRUE, alpha = 0.05, parallel.flag = parallel)

save(lda.results, nlda.results, mlda.results, mkhadri.results, file = "colon-CV1-varselect-alpha-005.RData")

# Leave-10-Out Crossvalidation Error Rates for Colon Cancer Data Set
k <- 10
lda.results <- colon.error.rates("lda", k, parallel.flag = parallel)
nlda.results <- colon.error.rates("nlda", k, parallel.flag = parallel)
mlda.results <- colon.error.rates("mlda", k, parallel.flag = parallel)
mkhadri.results <- colon.error.rates("mkhadri", k, parallel.flag = parallel)

save(lda.results, nlda.results, mlda.results, mkhadri.results, file = "colon-CV10.RData")
# Leave-5-Out Crossvalidation Error Rates for Colon Cancer Data Set
k <- 5
lda.results <- colon.error.rates("lda", k, parallel.flag = parallel)
nlda.results <- colon.error.rates("nlda", k, parallel.flag = parallel)
mlda.results <- colon.error.rates("mlda", k, parallel.flag = parallel)
mkhadri.results <- colon.error.rates("mkhadri", k, parallel.flag = parallel)

save(lda.results, nlda.results, mlda.results, mkhadri.results, file = "colon-CV5.RData")

# Leave-1-Out Crossvalidation Error Rates for Colon Cancer Data Set
k <- 1
lda.results <- colon.error.rates("lda", k, parallel.flag = parallel)
nlda.results <- colon.error.rates("nlda", k, parallel.flag = parallel)
mlda.results <- colon.error.rates("mlda", k, parallel.flag = parallel)
mkhadri.results <- colon.error.rates("mkhadri", k, parallel.flag = parallel)

save(lda.results, nlda.results, mlda.results, mkhadri.results, file = "colon-CV1.RData")