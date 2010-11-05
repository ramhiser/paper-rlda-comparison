library(ProjectTemplate)
run.locally <- FALSE
load.project()

set.seed(42)

parallel <- TRUE

# Leave-10-Out Crossvalidation Error Rates for Khan Cancer Data Set
# with variable selection (t-test) and alpha = 0.01
k <- 10
lda.results <- khan.error.rates("lda", k, variable.selection = TRUE, alpha = 0.01, parallel.flag = parallel)
nlda.results <- khan.error.rates("nlda", k, variable.selection = TRUE, alpha = 0.01, parallel.flag = parallel)
mlda.results <- khan.error.rates("mlda", k, variable.selection = TRUE, alpha = 0.01, parallel.flag = parallel)
save(lda.results, nlda.results, mlda.results, file = "khan-CV10-varselect-alpha-001.RData")

# Leave-5-Out Crossvalidation Error Rates for Khan Cancer Data Set
# with variable selection (t-test)
k <- 5
lda.results <- khan.error.rates("lda", k, variable.selection = TRUE, alpha = 0.01, parallel.flag = parallel)
nlda.results <- khan.error.rates("nlda", k, variable.selection = TRUE, alpha = 0.01, parallel.flag = parallel)
mlda.results <- khan.error.rates("mlda", k, variable.selection = TRUE, alpha = 0.01, parallel.flag = parallel)
save(lda.results, nlda.results, mlda.results, file = "khan-CV5-varselect-alpha-001.RData")

# Leave-1-Out Crossvalidation Error Rates for Khan Cancer Data Set
# with variable selection (t-test)
k <- 1
lda.results <- khan.error.rates("lda", k, variable.selection = TRUE, alpha = 0.01, parallel.flag = parallel)
nlda.results <- khan.error.rates("nlda", k, variable.selection = TRUE, alpha = 0.01, parallel.flag = parallel)
mlda.results <- khan.error.rates("mlda", k, variable.selection = TRUE, alpha = 0.01, parallel.flag = parallel)
save(lda.results, nlda.results, mlda.results, file = "khan-CV1-varselect-alpha-001.RData")

# Leave-10-Out Crossvalidation Error Rates for Khan Cancer Data Set
# with variable selection (t-test) and alpha = 0.05
k <- 10
lda.results <- khan.error.rates("lda", k, variable.selection = TRUE, alpha = 0.05, parallel.flag = parallel)
nlda.results <- khan.error.rates("nlda", k, variable.selection = TRUE, alpha = 0.05, parallel.flag = parallel)
mlda.results <- khan.error.rates("mlda", k, variable.selection = TRUE, alpha = 0.05, parallel.flag = parallel)
save(lda.results, nlda.results, mlda.results, file = "khan-CV10-varselect-alpha-005.RData")

# Leave-5-Out Crossvalidation Error Rates for Khan Cancer Data Set
# with variable selection (t-test) and alpha = 0.05
k <- 5
lda.results <- khan.error.rates("lda", k, variable.selection = TRUE, alpha = 0.05, parallel.flag = parallel)
nlda.results <- khan.error.rates("nlda", k, variable.selection = TRUE, alpha = 0.05, parallel.flag = parallel)
mlda.results <- khan.error.rates("mlda", k, variable.selection = TRUE, alpha = 0.05, parallel.flag = parallel)
save(lda.results, nlda.results, mlda.results, file = "khan-CV5-varselect-alpha-005.RData")

# Leave-1-Out Crossvalidation Error Rates for Khan Cancer Data Set
# with variable selection (t-test) and alpha = 0.05
k <- 1
lda.results <- khan.error.rates("lda", k, variable.selection = TRUE, alpha = 0.05, parallel.flag = parallel)
nlda.results <- khan.error.rates("nlda", k, variable.selection = TRUE, alpha = 0.05, parallel.flag = parallel)
mlda.results <- khan.error.rates("mlda", k, variable.selection = TRUE, alpha = 0.05, parallel.flag = parallel)
save(lda.results, nlda.results, mlda.results, file = "khan-CV1-varselect-alpha-005.RData")

# Leave-10-Out Crossvalidation Error Rates for Khan Cancer Data Set
k <- 10
lda.results <- khan.error.rates("lda", k, parallel.flag = parallel)
nlda.results <- khan.error.rates("nlda", k, parallel.flag = parallel)
mlda.results <- khan.error.rates("mlda", k, parallel.flag = parallel)
save(lda.results, nlda.results, mlda.results, file = "khan-CV10.RData")

# Leave-5-Out Crossvalidation Error Rates for Khan Cancer Data Set
k <- 5
lda.results <- khan.error.rates("lda", k, parallel.flag = parallel)
nlda.results <- khan.error.rates("nlda", k, parallel.flag = parallel)
mlda.results <- khan.error.rates("mlda", k, parallel.flag = parallel)
save(lda.results, nlda.results, mlda.results, file = "khan-CV5.RData")

# Leave-1-Out Crossvalidation Error Rates for Khan Cancer Data Set
k <- 1
lda.results <- khan.error.rates("lda", k, parallel.flag = parallel)
nlda.results <- khan.error.rates("nlda", k, parallel.flag = parallel)
mlda.results <- khan.error.rates("mlda", k, parallel.flag = parallel)
save(lda.results, nlda.results, mlda.results, file = "khan-CV1.RData")

#
# Mkhadri Simulations (with and without grids)
#

# Leave-10-Out Crossvalidation Error Rates for Khan Cancer Data Set
# with variable selection (t-test) and alpha = 0.01
k <- 10
mkhadri.results <- khan.error.rates("mkhadri", k, variable.selection = TRUE, alpha = 0.01, parallel.flag = parallel)
mkhadri.grid.results <- khan.error.rates("mkhadri-grid", k, variable.selection = TRUE, alpha = 0.01, parallel.flag = parallel)
load("khan-CV10-varselect-alpha-001.RData")
save(lda.results, nlda.results, mlda.results, mkhadri.results, mkhadri.grid.results, file = "khan-CV10-varselect-alpha-001.RData")

# Leave-5-Out Crossvalidation Error Rates for Khan Cancer Data Set
# with variable selection (t-test)
k <- 5
mkhadri.results <- khan.error.rates("mkhadri", k, variable.selection = TRUE, alpha = 0.01, parallel.flag = parallel)
mkhadri.grid.results <- khan.error.rates("mkhadri-grid", k, variable.selection = TRUE, alpha = 0.01, parallel.flag = parallel)
load("khan-CV5-varselect-alpha-001.RData")
save(lda.results, nlda.results, mlda.results, mkhadri.results, mkhadri.grid.results, file = "khan-CV5-varselect-alpha-001.RData")

# Leave-1-Out Crossvalidation Error Rates for Khan Cancer Data Set
# with variable selection (t-test)
k <- 1
mkhadri.results <- khan.error.rates("mkhadri", k, variable.selection = TRUE, alpha = 0.01, parallel.flag = parallel)
mkhadri.grid.results <- khan.error.rates("mkhadri-grid", k, variable.selection = TRUE, alpha = 0.01, parallel.flag = parallel)
load("khan-CV1-varselect-alpha-001.RData")
save(lda.results, nlda.results, mlda.results, mkhadri.results, mkhadri.grid.results, file = "khan-CV1-varselect-alpha-001.RData")

# Leave-10-Out Crossvalidation Error Rates for Khan Cancer Data Set
# with variable selection (t-test) and alpha = 0.05
k <- 10
mkhadri.results <- khan.error.rates("mkhadri", k, variable.selection = TRUE, alpha = 0.05, parallel.flag = parallel)
mkhadri.grid.results <- khan.error.rates("mkhadri-grid", k, variable.selection = TRUE, alpha = 0.05, parallel.flag = parallel)
load("khan-CV10-varselect-alpha-005.RData")
save(lda.results, nlda.results, mlda.results, mkhadri.results, mkhadri.grid.results, file = "khan-CV10-varselect-alpha-005.RData")

# Leave-5-Out Crossvalidation Error Rates for Khan Cancer Data Set
# with variable selection (t-test) and alpha = 0.05
k <- 5
mkhadri.results <- khan.error.rates("mkhadri", k, variable.selection = TRUE, alpha = 0.05, parallel.flag = parallel)
mkhadri.grid.results <- khan.error.rates("mkhadri-grid", k, variable.selection = TRUE, alpha = 0.05, parallel.flag = parallel)
load("khan-CV5-varselect-alpha-005.RData")
save(lda.results, nlda.results, mlda.results, mkhadri.results, mkhadri.grid.results, file = "khan-CV5-varselect-alpha-005.RData")

# Leave-1-Out Crossvalidation Error Rates for Khan Cancer Data Set
# with variable selection (t-test) and alpha = 0.05
k <- 1
mkhadri.results <- khan.error.rates("mkhadri", k, variable.selection = TRUE, alpha = 0.05, parallel.flag = parallel)
mkhadri.grid.results <- khan.error.rates("mkhadri-grid", k, variable.selection = TRUE, alpha = 0.05, parallel.flag = parallel)
load("khan-CV1-varselect-alpha-005.RData")
save(lda.results, nlda.results, mlda.results, mkhadri.results, mkhadri.grid.results, file = "khan-CV1-varselect-alpha-005.RData")

# Leave-10-Out Crossvalidation Error Rates for Khan Cancer Data Set
k <- 10
mkhadri.results <- khan.error.rates("mkhadri", k, parallel.flag = parallel)
mkhadri.grid.results <- khan.error.rates("mkhadri-grid", k, parallel.flag = parallel)
load("khan-CV10.RData")
save(lda.results, nlda.results, mlda.results, mkhadri.results, mkhadri.grid.results, file = "khan-CV10.RData")

# Leave-5-Out Crossvalidation Error Rates for Khan Cancer Data Set
k <- 5
mkhadri.results <- khan.error.rates("mkhadri", k, parallel.flag = parallel)
mkhadri.grid.results <- khan.error.rates("mkhadri-grid", k, parallel.flag = parallel)
load("khan-CV5.RData")
save(lda.results, nlda.results, mlda.results, mkhadri.results, mkhadri.grid.results, file = "khan-CV5.RData")

# Leave-1-Out Crossvalidation Error Rates for Khan Cancer Data Set
k <- 1
mkhadri.results <- khan.error.rates("mkhadri", k, parallel.flag = parallel)
mkhadri.grid.results <- khan.error.rates("mkhadri-grid", k, parallel.flag = parallel)
load("khan-CV1.RData")
save(lda.results, nlda.results, mlda.results, mkhadri.results, mkhadri.grid.results, file = "khan-CV1.RData")
