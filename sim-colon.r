library(mvtnorm)
library(corpcor) # for cov.shrink and inv.cov.shrink for MLDA
library(plyr)
library(MASS)

source("leave-k-out.r")
source("variable-selection.r")

# Temporarily we have to manually run the source code from the RLDA project.
# We are using a rlda.dir because the directory on my Mac differs from the RLDA directory on the Baylor cluster.
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
	}, .progress = "text")
	
	data.frame(method = rlda.method, error = error.rates)
}

set.seed(42)

# Leave-10-Out Crossvalidation Error Rates for Colon Cancer Data Set
k <- 10
lda.results <- colon.error.rates("lda", k)
nlda.results <- colon.error.rates("nlda", k)
mlda.results <- colon.error.rates("mlda", k)
mkhadri.results <- colon.error.rates("mkhadri", k)

# Leave-5-Out Crossvalidation Error Rates for Colon Cancer Data Set
k <- 5
lda.results <- colon.error.rates("lda", k)
nlda.results <- colon.error.rates("nlda", k)
mlda.results <- colon.error.rates("mlda", k)
mkhadri.results <- colon.error.rates("mkhadri", k)

# Leave-1-Out Crossvalidation Error Rates for Colon Cancer Data Set
k <- 1
lda.results <- colon.error.rates("lda", k)
nlda.results <- colon.error.rates("nlda", k)
mlda.results <- colon.error.rates("mlda", k)
mkhadri.results <- colon.error.rates("mkhadri", k)


# Leave-10-Out Crossvalidation Error Rates for Colon Cancer Data Set
# with variable selection (t-test) and alpha = 0.01
k <- 10
lda.results <- colon.error.rates("lda", k, variable.selection = TRUE, alpha = 0.01)
nlda.results <- colon.error.rates("nlda", k, variable.selection = TRUE, alpha = 0.01)
mlda.results <- colon.error.rates("mlda", k, variable.selection = TRUE, alpha = 0.01)
mkhadri.results <- colon.error.rates("mkhadri", k, variable.selection = TRUE, alpha = 0.01)

# Leave-5-Out Crossvalidation Error Rates for Colon Cancer Data Set
# with variable selection (t-test)
k <- 5
lda.results <- colon.error.rates("lda", k, variable.selection = TRUE, alpha = 0.01)
nlda.results <- colon.error.rates("nlda", k, variable.selection = TRUE, alpha = 0.01)
mlda.results <- colon.error.rates("mlda", k, variable.selection = TRUE, alpha = 0.01)
mkhadri.results <- colon.error.rates("mkhadri", k, variable.selection = TRUE, alpha = 0.01)

# Leave-1-Out Crossvalidation Error Rates for Colon Cancer Data Set
# with variable selection (t-test)
k <- 1
lda.results <- colon.error.rates("lda", k, variable.selection = TRUE, alpha = 0.01)
nlda.results <- colon.error.rates("nlda", k, variable.selection = TRUE, alpha = 0.01)
mlda.results <- colon.error.rates("mlda", k, variable.selection = TRUE, alpha = 0.01)
mkhadri.results <- colon.error.rates("mkhadri", k, variable.selection = TRUE, alpha = 0.01)

# Leave-10-Out Crossvalidation Error Rates for Colon Cancer Data Set
# with variable selection (t-test) and alpha = 0.05
k <- 10
lda.results <- colon.error.rates("lda", k, variable.selection = TRUE, alpha = 0.05)
nlda.results <- colon.error.rates("nlda", k, variable.selection = TRUE, alpha = 0.05)
mlda.results <- colon.error.rates("mlda", k, variable.selection = TRUE, alpha = 0.05)
mkhadri.results <- colon.error.rates("mkhadri", k, variable.selection = TRUE, alpha = 0.05)

# Leave-5-Out Crossvalidation Error Rates for Colon Cancer Data Set
# with variable selection (t-test) and alpha = 0.05
k <- 5
lda.results <- colon.error.rates("lda", k, variable.selection = TRUE, alpha = 0.05)
nlda.results <- colon.error.rates("nlda", k, variable.selection = TRUE, alpha = 0.05)
mlda.results <- colon.error.rates("mlda", k, variable.selection = TRUE, alpha = 0.05)
mkhadri.results <- colon.error.rates("mkhadri", k, variable.selection = TRUE, alpha = 0.05)

# Leave-1-Out Crossvalidation Error Rates for Colon Cancer Data Set
# with variable selection (t-test) and alpha = 0.05
k <- 1
lda.results <- colon.error.rates("lda", k, variable.selection = TRUE, alpha = 0.05)
nlda.results <- colon.error.rates("nlda", k, variable.selection = TRUE, alpha = 0.05)
mlda.results <- colon.error.rates("mlda", k, variable.selection = TRUE, alpha = 0.05)
mkhadri.results <- colon.error.rates("mkhadri", k, variable.selection = TRUE, alpha = 0.05)

#save(experiment.results, file = paste("experiment-results-", experiment.num, ".RData", sep = ""))