library(ProjectTemplate)
run.locally <- FALSE
load.project()

# n is the number of observations
# p is the feature dimension
# We subtract 1 from the number of columns to correct for the column of labels.
n <- nrow(colon.cancer)
p <- ncol(colon.cancer) - 1

set.seed(42)

parallel <- FALSE

# Leave-10-Out Crossvalidation Error Rates for Colon Cancer Data Set
# with variable selection (t-test) and alpha = 0.01
k <- 10
lda.results <- colon.error.rates("lda", k, variable.selection = TRUE, alpha = 0.01, parallel.flag = parallel)
nlda.results <- colon.error.rates("nlda", k, variable.selection = TRUE, alpha = 0.01, parallel.flag = parallel)
mlda.results <- colon.error.rates("mlda", k, variable.selection = TRUE, alpha = 0.01, parallel.flag = parallel)


save(lda.results, nlda.results, mlda.results, mkhadri.results, file = "colon-CV10-varselect-alpha-001.RData")

# Leave-5-Out Crossvalidation Error Rates for Colon Cancer Data Set
# with variable selection (t-test)
k <- 5
lda.results <- colon.error.rates("lda", k, variable.selection = TRUE, alpha = 0.01, parallel.flag = parallel)
nlda.results <- colon.error.rates("nlda", k, variable.selection = TRUE, alpha = 0.01, parallel.flag = parallel)
mlda.results <- colon.error.rates("mlda", k, variable.selection = TRUE, alpha = 0.01, parallel.flag = parallel)


save(lda.results, nlda.results, mlda.results, mkhadri.results, file = "colon-CV5-varselect-alpha-001.RData")

# Leave-1-Out Crossvalidation Error Rates for Colon Cancer Data Set
# with variable selection (t-test)
k <- 1
lda.results <- colon.error.rates("lda", k, variable.selection = TRUE, alpha = 0.01, parallel.flag = parallel)
nlda.results <- colon.error.rates("nlda", k, variable.selection = TRUE, alpha = 0.01, parallel.flag = parallel)
mlda.results <- colon.error.rates("mlda", k, variable.selection = TRUE, alpha = 0.01, parallel.flag = parallel)


save(lda.results, nlda.results, mlda.results, mkhadri.results, file = "colon-CV1-varselect-alpha-001.RData")

# Leave-10-Out Crossvalidation Error Rates for Colon Cancer Data Set
# with variable selection (t-test) and alpha = 0.05
k <- 10
lda.results <- colon.error.rates("lda", k, variable.selection = TRUE, alpha = 0.05, parallel.flag = parallel)
nlda.results <- colon.error.rates("nlda", k, variable.selection = TRUE, alpha = 0.05, parallel.flag = parallel)
mlda.results <- colon.error.rates("mlda", k, variable.selection = TRUE, alpha = 0.05, parallel.flag = parallel)


save(lda.results, nlda.results, mlda.results, mkhadri.results, file = "colon-CV10-varselect-alpha-005.RData")

# Leave-5-Out Crossvalidation Error Rates for Colon Cancer Data Set
# with variable selection (t-test) and alpha = 0.05
k <- 5
lda.results <- colon.error.rates("lda", k, variable.selection = TRUE, alpha = 0.05, parallel.flag = parallel)
nlda.results <- colon.error.rates("nlda", k, variable.selection = TRUE, alpha = 0.05, parallel.flag = parallel)
mlda.results <- colon.error.rates("mlda", k, variable.selection = TRUE, alpha = 0.05, parallel.flag = parallel)


save(lda.results, nlda.results, mlda.results, mkhadri.results, file = "colon-CV5-varselect-alpha-005.RData")

# Leave-1-Out Crossvalidation Error Rates for Colon Cancer Data Set
# with variable selection (t-test) and alpha = 0.05
k <- 1
lda.results <- colon.error.rates("lda", k, variable.selection = TRUE, alpha = 0.05, parallel.flag = parallel)
nlda.results <- colon.error.rates("nlda", k, variable.selection = TRUE, alpha = 0.05, parallel.flag = parallel)
mlda.results <- colon.error.rates("mlda", k, variable.selection = TRUE, alpha = 0.05, parallel.flag = parallel)


save(lda.results, nlda.results, mlda.results, mkhadri.results, file = "colon-CV1-varselect-alpha-005.RData")

# Leave-10-Out Crossvalidation Error Rates for Colon Cancer Data Set
k <- 10
lda.results <- colon.error.rates("lda", k, parallel.flag = parallel)
nlda.results <- colon.error.rates("nlda", k, parallel.flag = parallel)
mlda.results <- colon.error.rates("mlda", k, parallel.flag = parallel)


save(lda.results, nlda.results, mlda.results, mkhadri.results, file = "colon-CV10.RData")
# Leave-5-Out Crossvalidation Error Rates for Colon Cancer Data Set
k <- 5
lda.results <- colon.error.rates("lda", k, parallel.flag = parallel)
nlda.results <- colon.error.rates("nlda", k, parallel.flag = parallel)
mlda.results <- colon.error.rates("mlda", k, parallel.flag = parallel)


save(lda.results, nlda.results, mlda.results, mkhadri.results, file = "colon-CV5.RData")

# Leave-1-Out Crossvalidation Error Rates for Colon Cancer Data Set
k <- 1
lda.results <- colon.error.rates("lda", k, parallel.flag = parallel)
nlda.results <- colon.error.rates("nlda", k, parallel.flag = parallel)
mlda.results <- colon.error.rates("mlda", k, parallel.flag = parallel)
mkhadri.results <- colon.error.rates("mkhadri", k, parallel.flag = parallel)

save(lda.results, nlda.results, mlda.results, mkhadri.results, file = "colon-CV1.RData")
