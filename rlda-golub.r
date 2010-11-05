library(ProjectTemplate)
run.locally <- FALSE
load.project()

set.seed(42)

# Leave-10-Out Crossvalidation Error Rates for Golub Cancer Data Set
# with variable selection (t-test) and alpha = 0.01
k <- 10
lda.results <- golub.error.rates("lda", k, variable.selection = TRUE, alpha = 0.01)
nlda.results <- golub.error.rates("nlda", k, variable.selection = TRUE, alpha = 0.01)
mlda.results <- golub.error.rates("mlda", k, variable.selection = TRUE, alpha = 0.01)
save(lda.results, nlda.results, mlda.results, file = "golub-CV10-varselect-alpha-001.RData")

# Leave-5-Out Crossvalidation Error Rates for Golub Cancer Data Set
# with variable selection (t-test)
k <- 5
lda.results <- golub.error.rates("lda", k, variable.selection = TRUE, alpha = 0.01)
nlda.results <- golub.error.rates("nlda", k, variable.selection = TRUE, alpha = 0.01)
mlda.results <- golub.error.rates("mlda", k, variable.selection = TRUE, alpha = 0.01)
save(lda.results, nlda.results, mlda.results, file = "golub-CV5-varselect-alpha-001.RData")

# Leave-1-Out Crossvalidation Error Rates for Golub Cancer Data Set
# with variable selection (t-test)
k <- 1
lda.results <- golub.error.rates("lda", k, variable.selection = TRUE, alpha = 0.01)
nlda.results <- golub.error.rates("nlda", k, variable.selection = TRUE, alpha = 0.01)
mlda.results <- golub.error.rates("mlda", k, variable.selection = TRUE, alpha = 0.01)
save(lda.results, nlda.results, mlda.results, file = "golub-CV1-varselect-alpha-001.RData")

# Leave-10-Out Crossvalidation Error Rates for Golub Cancer Data Set
# with variable selection (t-test) and alpha = 0.05
k <- 10
lda.results <- golub.error.rates("lda", k, variable.selection = TRUE, alpha = 0.05)
nlda.results <- golub.error.rates("nlda", k, variable.selection = TRUE, alpha = 0.05)
mlda.results <- golub.error.rates("mlda", k, variable.selection = TRUE, alpha = 0.05)
save(lda.results, nlda.results, mlda.results, file = "golub-CV10-varselect-alpha-005.RData")

# Leave-5-Out Crossvalidation Error Rates for Golub Cancer Data Set
# with variable selection (t-test) and alpha = 0.05
k <- 5
lda.results <- golub.error.rates("lda", k, variable.selection = TRUE, alpha = 0.05)
nlda.results <- golub.error.rates("nlda", k, variable.selection = TRUE, alpha = 0.05)
mlda.results <- golub.error.rates("mlda", k, variable.selection = TRUE, alpha = 0.05)
save(lda.results, nlda.results, mlda.results, file = "golub-CV5-varselect-alpha-005.RData")

# Leave-1-Out Crossvalidation Error Rates for Golub Cancer Data Set
# with variable selection (t-test) and alpha = 0.05
k <- 1
lda.results <- golub.error.rates("lda", k, variable.selection = TRUE, alpha = 0.05)
nlda.results <- golub.error.rates("nlda", k, variable.selection = TRUE, alpha = 0.05)
mlda.results <- golub.error.rates("mlda", k, variable.selection = TRUE, alpha = 0.05)
save(lda.results, nlda.results, mlda.results, file = "golub-CV1-varselect-alpha-005.RData")

# Leave-10-Out Crossvalidation Error Rates for Golub Cancer Data Set
k <- 10
lda.results <- golub.error.rates("lda", k)
nlda.results <- golub.error.rates("nlda", k)
mlda.results <- golub.error.rates("mlda", k)
save(lda.results, nlda.results, mlda.results, file = "golub-CV10.RData")

# Leave-5-Out Crossvalidation Error Rates for Golub Cancer Data Set
k <- 5
lda.results <- golub.error.rates("lda", k)
nlda.results <- golub.error.rates("nlda", k)
mlda.results <- golub.error.rates("mlda", k)
save(lda.results, nlda.results, mlda.results, file = "golub-CV5.RData")

# Leave-1-Out Crossvalidation Error Rates for Golub Cancer Data Set
k <- 1
lda.results <- golub.error.rates("lda", k)
nlda.results <- golub.error.rates("nlda", k)
mlda.results <- golub.error.rates("mlda", k)
save(lda.results, nlda.results, mlda.results, file = "golub-CV1.RData")

#
# Mkhadri Simulations (with and without grids)
#

# Leave-10-Out Crossvalidation Error Rates for Golub Cancer Data Set
# with variable selection (t-test) and alpha = 0.01
k <- 10
mkhadri.results <- golub.error.rates("mkhadri", k, variable.selection = TRUE, alpha = 0.01)
mkhadri.grid.results <- golub.error.rates("mkhadri-grid", k, variable.selection = TRUE, alpha = 0.01)
load("golub-CV10-varselect-alpha-001.RData")
save(lda.results, nlda.results, mlda.results, mkhadri.results, mkhadri.grid.results, file = "golub-CV10-varselect-alpha-001.RData")

# Leave-5-Out Crossvalidation Error Rates for Golub Cancer Data Set
# with variable selection (t-test)
k <- 5
mkhadri.results <- golub.error.rates("mkhadri", k, variable.selection = TRUE, alpha = 0.01)
mkhadri.grid.results <- golub.error.rates("mkhadri-grid", k, variable.selection = TRUE, alpha = 0.01)
load("golub-CV5-varselect-alpha-001.RData")
save(lda.results, nlda.results, mlda.results, mkhadri.results, mkhadri.grid.results, file = "golub-CV5-varselect-alpha-001.RData")

# Leave-1-Out Crossvalidation Error Rates for Golub Cancer Data Set
# with variable selection (t-test)
k <- 1
mkhadri.results <- golub.error.rates("mkhadri", k, variable.selection = TRUE, alpha = 0.01)
mkhadri.grid.results <- golub.error.rates("mkhadri-grid", k, variable.selection = TRUE, alpha = 0.01)
load("golub-CV1-varselect-alpha-001.RData")
save(lda.results, nlda.results, mlda.results, mkhadri.results, mkhadri.grid.results, file = "golub-CV1-varselect-alpha-001.RData")

# Leave-10-Out Crossvalidation Error Rates for Golub Cancer Data Set
# with variable selection (t-test) and alpha = 0.05
k <- 10
mkhadri.results <- golub.error.rates("mkhadri", k, variable.selection = TRUE, alpha = 0.05)
mkhadri.grid.results <- golub.error.rates("mkhadri-grid", k, variable.selection = TRUE, alpha = 0.05)
load("golub-CV10-varselect-alpha-005.RData")
save(lda.results, nlda.results, mlda.results, mkhadri.results, mkhadri.grid.results, file = "golub-CV10-varselect-alpha-005.RData")

# Leave-5-Out Crossvalidation Error Rates for Golub Cancer Data Set
# with variable selection (t-test) and alpha = 0.05
k <- 5
mkhadri.results <- golub.error.rates("mkhadri", k, variable.selection = TRUE, alpha = 0.05)
mkhadri.grid.results <- golub.error.rates("mkhadri-grid", k, variable.selection = TRUE, alpha = 0.05)
load("golub-CV5-varselect-alpha-005.RData")
save(lda.results, nlda.results, mlda.results, mkhadri.results, mkhadri.grid.results, file = "golub-CV5-varselect-alpha-005.RData")

# Leave-1-Out Crossvalidation Error Rates for Golub Cancer Data Set
# with variable selection (t-test) and alpha = 0.05
k <- 1
mkhadri.results <- golub.error.rates("mkhadri", k, variable.selection = TRUE, alpha = 0.05)
mkhadri.grid.results <- golub.error.rates("mkhadri-grid", k, variable.selection = TRUE, alpha = 0.05)
load("golub-CV1-varselect-alpha-005.RData")
save(lda.results, nlda.results, mlda.results, mkhadri.results, mkhadri.grid.results, file = "golub-CV1-varselect-alpha-005.RData")

# Leave-10-Out Crossvalidation Error Rates for Golub Cancer Data Set
k <- 10
mkhadri.results <- golub.error.rates("mkhadri", k)
mkhadri.grid.results <- golub.error.rates("mkhadri-grid", k)
load("golub-CV10.RData")
save(lda.results, nlda.results, mlda.results, mkhadri.results, mkhadri.grid.results, file = "golub-CV10.RData")

# Leave-5-Out Crossvalidation Error Rates for Golub Cancer Data Set
k <- 5
mkhadri.results <- golub.error.rates("mkhadri", k)
mkhadri.grid.results <- golub.error.rates("mkhadri-grid", k)
load("golub-CV5.RData")
save(lda.results, nlda.results, mlda.results, mkhadri.results, mkhadri.grid.results, file = "golub-CV5.RData")

# Leave-1-Out Crossvalidation Error Rates for Golub Cancer Data Set
k <- 1
mkhadri.results <- golub.error.rates("mkhadri", k)
mkhadri.grid.results <- golub.error.rates("mkhadri-grid", k)
load("golub-CV1.RData")
save(lda.results, nlda.results, mlda.results, mkhadri.results, mkhadri.grid.results, file = "golub-CV1.RData")
