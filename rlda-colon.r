library(ProjectTemplate)
run.locally <- TRUE
load.project()

set.seed(42)

parallel <- TRUE

# Leave-10-Out Crossvalidation Error Rates for Colon Cancer Data Set
# with variable selection (t-test) and alpha = 0.01
k <- 10
cat("Leave-10-Out\tVariable Selection with alpha = 0.01\n")
lda.results <- colon.error.rates("lda", k, variable.selection = TRUE, alpha = 0.01, parallel.flag = parallel)
nlda.results <- colon.error.rates("nlda", k, variable.selection = TRUE, alpha = 0.01, parallel.flag = parallel)
mlda.results <- colon.error.rates("mlda", k, variable.selection = TRUE, alpha = 0.01, parallel.flag = parallel)
save(lda.results, nlda.results, mlda.results, file = "colon-CV10-varselect-alpha-001.RData")

# Leave-5-Out Crossvalidation Error Rates for Colon Cancer Data Set
# with variable selection (t-test)
k <- 5
cat("Leave-5-Out\tVariable Selection with alpha = 0.01\n")
lda.results <- colon.error.rates("lda", k, variable.selection = TRUE, alpha = 0.01, parallel.flag = parallel)
nlda.results <- colon.error.rates("nlda", k, variable.selection = TRUE, alpha = 0.01, parallel.flag = parallel)
mlda.results <- colon.error.rates("mlda", k, variable.selection = TRUE, alpha = 0.01, parallel.flag = parallel)
save(lda.results, nlda.results, mlda.results, file = "colon-CV5-varselect-alpha-001.RData")

# Leave-1-Out Crossvalidation Error Rates for Colon Cancer Data Set
# with variable selection (t-test)
k <- 1
cat("Leave-1-Out\tVariable Selection with alpha = 0.01\n")
lda.results <- colon.error.rates("lda", k, variable.selection = TRUE, alpha = 0.01, parallel.flag = parallel)
nlda.results <- colon.error.rates("nlda", k, variable.selection = TRUE, alpha = 0.01, parallel.flag = parallel)
mlda.results <- colon.error.rates("mlda", k, variable.selection = TRUE, alpha = 0.01, parallel.flag = parallel)
save(lda.results, nlda.results, mlda.results, file = "colon-CV1-varselect-alpha-001.RData")

# Leave-10-Out Crossvalidation Error Rates for Colon Cancer Data Set
# with variable selection (t-test) and alpha = 0.05
k <- 10
cat("Leave-10-Out\tVariable Selection with alpha = 0.05\n")
cat("Now it's time for LDA\n")
lda.results <- colon.error.rates("lda", k, variable.selection = TRUE, alpha = 0.05, parallel.flag = parallel)
cat("Now it's time for NLDA\n")
nlda.results <- colon.error.rates("nlda", k, variable.selection = TRUE, alpha = 0.05, parallel.flag = parallel)
cat("Now it's time for MLDA\n")
mlda.results <- colon.error.rates("mlda", k, variable.selection = TRUE, alpha = 0.05, parallel.flag = parallel)
cat("Done!\n")
save(lda.results, nlda.results, mlda.results, file = "colon-CV10-varselect-alpha-005.RData")

# Leave-5-Out Crossvalidation Error Rates for Colon Cancer Data Set
# with variable selection (t-test) and alpha = 0.05
k <- 5
cat("Leave-5-Out\tVariable Selection with alpha = 0.05\n")
lda.results <- colon.error.rates("lda", k, variable.selection = TRUE, alpha = 0.05, parallel.flag = parallel)
nlda.results <- colon.error.rates("nlda", k, variable.selection = TRUE, alpha = 0.05, parallel.flag = parallel)
mlda.results <- colon.error.rates("mlda", k, variable.selection = TRUE, alpha = 0.05, parallel.flag = parallel)
save(lda.results, nlda.results, mlda.results, file = "colon-CV5-varselect-alpha-005.RData")

# Leave-1-Out Crossvalidation Error Rates for Colon Cancer Data Set
# with variable selection (t-test) and alpha = 0.05
k <- 1
cat("Leave-1-Out\tVariable Selection with alpha = 0.05\n")
lda.results <- colon.error.rates("lda", k, variable.selection = TRUE, alpha = 0.05, parallel.flag = parallel)
nlda.results <- colon.error.rates("nlda", k, variable.selection = TRUE, alpha = 0.05, parallel.flag = parallel)
mlda.results <- colon.error.rates("mlda", k, variable.selection = TRUE, alpha = 0.05, parallel.flag = parallel)
save(lda.results, nlda.results, mlda.results, file = "colon-CV1-varselect-alpha-005.RData")

# Leave-10-Out Crossvalidation Error Rates for Colon Cancer Data Set
k <- 10
cat("Leave-10-Out\tNo Variable Selection\n")
lda.results <- colon.error.rates("lda", k, parallel.flag = parallel)
nlda.results <- colon.error.rates("nlda", k, parallel.flag = parallel)
mlda.results <- colon.error.rates("mlda", k, parallel.flag = parallel)
save(lda.results, nlda.results, mlda.results, file = "colon-CV10.RData")

# Leave-5-Out Crossvalidation Error Rates for Colon Cancer Data Set
k <- 5
cat("Leave-5-Out\tNo Variable Selection\n")
lda.results <- colon.error.rates("lda", k, parallel.flag = parallel)
nlda.results <- colon.error.rates("nlda", k, parallel.flag = parallel)
mlda.results <- colon.error.rates("mlda", k, parallel.flag = parallel)
save(lda.results, nlda.results, mlda.results, file = "colon-CV5.RData")

# Leave-1-Out Crossvalidation Error Rates for Colon Cancer Data Set
k <- 1
cat("Leave-1-Out\tNo Variable Selection\n")
lda.results <- colon.error.rates("lda", k, parallel.flag = parallel)
nlda.results <- colon.error.rates("nlda", k, parallel.flag = parallel)
mlda.results <- colon.error.rates("mlda", k, parallel.flag = parallel)
save(lda.results, nlda.results, mlda.results, file = "colon-CV1.RData")

#
# Mkhadri Simulations (with and without grids)
#

# Leave-10-Out Crossvalidation Error Rates for Colon Cancer Data Set
# with variable selection (t-test) and alpha = 0.01
k <- 10
cat("Leave-10-Out\tVariable Selection with alpha = 0.01\t(Mkhadri)\n")
mkhadri.results <- colon.error.rates("mkhadri", k, variable.selection = TRUE, alpha = 0.01, parallel.flag = parallel)
mkhadri.grid.results <- colon.error.rates("mkhadri-grid", k, variable.selection = TRUE, alpha = 0.01, parallel.flag = parallel)
load("colon-CV10-varselect-alpha-001.RData")
save(lda.results, nlda.results, mlda.results, mkhadri.results, mkhadri.grid.results, file = "colon-CV10-varselect-alpha-001.RData")

# Leave-5-Out Crossvalidation Error Rates for Colon Cancer Data Set
# with variable selection (t-test)
k <- 5
cat("Leave-5-Out\tVariable Selection with alpha = 0.01\t(Mkhadri)\n")
mkhadri.results <- colon.error.rates("mkhadri", k, variable.selection = TRUE, alpha = 0.01, parallel.flag = parallel)
mkhadri.grid.results <- colon.error.rates("mkhadri-grid", k, variable.selection = TRUE, alpha = 0.01, parallel.flag = parallel)
load("colon-CV5-varselect-alpha-001.RData")
save(lda.results, nlda.results, mlda.results, mkhadri.results, mkhadri.grid.results, file = "colon-CV5-varselect-alpha-001.RData")

# Leave-1-Out Crossvalidation Error Rates for Colon Cancer Data Set
# with variable selection (t-test)
k <- 1
cat("Leave-1-Out\tVariable Selection with alpha = 0.01\t(Mkhadri)\n")
mkhadri.results <- colon.error.rates("mkhadri", k, variable.selection = TRUE, alpha = 0.01, parallel.flag = parallel)
mkhadri.grid.results <- colon.error.rates("mkhadri-grid", k, variable.selection = TRUE, alpha = 0.01, parallel.flag = parallel)
load("colon-CV1-varselect-alpha-001.RData")
save(lda.results, nlda.results, mlda.results, mkhadri.results, mkhadri.grid.results, file = "colon-CV1-varselect-alpha-001.RData")

# Leave-10-Out Crossvalidation Error Rates for Colon Cancer Data Set
# with variable selection (t-test) and alpha = 0.05
k <- 10
cat("Leave-10-Out\tVariable Selection with alpha = 0.05\t(Mkhadri)\n")
mkhadri.results <- colon.error.rates("mkhadri", k, variable.selection = TRUE, alpha = 0.05, parallel.flag = parallel)
mkhadri.grid.results <- colon.error.rates("mkhadri-grid", k, variable.selection = TRUE, alpha = 0.05, parallel.flag = parallel)
load("colon-CV10-varselect-alpha-005.RData")
save(lda.results, nlda.results, mlda.results, mkhadri.results, mkhadri.grid.results, file = "colon-CV10-varselect-alpha-005.RData")

# Leave-5-Out Crossvalidation Error Rates for Colon Cancer Data Set
# with variable selection (t-test) and alpha = 0.05
k <- 5
cat("Leave-5-Out\tVariable Selection with alpha = 0.05\t(Mkhadri)\n")
mkhadri.results <- colon.error.rates("mkhadri", k, variable.selection = TRUE, alpha = 0.05, parallel.flag = parallel)
mkhadri.grid.results <- colon.error.rates("mkhadri-grid", k, variable.selection = TRUE, alpha = 0.05, parallel.flag = parallel)
load("colon-CV5-varselect-alpha-005.RData")
save(lda.results, nlda.results, mlda.results, mkhadri.results, mkhadri.grid.results, file = "colon-CV5-varselect-alpha-005.RData")

# Leave-1-Out Crossvalidation Error Rates for Colon Cancer Data Set
# with variable selection (t-test) and alpha = 0.05
k <- 1
cat("Leave-1-Out\tVariable Selection with alpha = 0.05\t(Mkhadri)\n")
mkhadri.results <- colon.error.rates("mkhadri", k, variable.selection = TRUE, alpha = 0.05, parallel.flag = parallel)
mkhadri.grid.results <- colon.error.rates("mkhadri-grid", k, variable.selection = TRUE, alpha = 0.05, parallel.flag = parallel)
load("colon-CV1-varselect-alpha-005.RData")
save(lda.results, nlda.results, mlda.results, mkhadri.results, mkhadri.grid.results, file = "colon-CV1-varselect-alpha-005.RData")

# Leave-10-Out Crossvalidation Error Rates for Colon Cancer Data Set
k <- 10
cat("Leave-10-Out\tNo Variable Selection\t(Mkhadri)\n")
mkhadri.results <- colon.error.rates("mkhadri", k, parallel.flag = parallel)
mkhadri.grid.results <- colon.error.rates("mkhadri-grid", k, parallel.flag = parallel)
load("colon-CV10.RData")
save(lda.results, nlda.results, mlda.results, mkhadri.results, mkhadri.grid.results, file = "colon-CV10.RData")

# Leave-5-Out Crossvalidation Error Rates for Colon Cancer Data Set
k <- 5
cat("Leave-5-Out\tNo Variable Selection\t(Mkhadri)\n")
mkhadri.results <- colon.error.rates("mkhadri", k, parallel.flag = parallel)
mkhadri.grid.results <- colon.error.rates("mkhadri-grid", k, parallel.flag = parallel)
load("colon-CV5.RData")
save(lda.results, nlda.results, mlda.results, mkhadri.results, mkhadri.grid.results, file = "colon-CV5.RData")

# Leave-1-Out Crossvalidation Error Rates for Colon Cancer Data Set
k <- 1
cat("Leave-1-Out\tNo Variable Selection\t(Mkhadri)\n")
mkhadri.results <- colon.error.rates("mkhadri", k, parallel.flag = parallel)
mkhadri.grid.results <- colon.error.rates("mkhadri-grid", k, parallel.flag = parallel)
load("colon-CV1.RData")
save(lda.results, nlda.results, mlda.results, mkhadri.results, mkhadri.grid.results, file = "colon-CV1.RData")
