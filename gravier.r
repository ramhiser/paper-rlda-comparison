library('ProjectTemplate')
load.project()
registerDoMC()

seed <- 42
set.seed(seed)
B <- 100
q_vec <- c(30, 50, 100, 200, 500)

data('gravier')

gravier_results <- foreach(q = q_vec, .combine=rbind, .verbose = TRUE) %:%
  foreach(icount(B), .combine=rbind, .verbose = TRUE) %do% {
    rlda_data(x = gravier$x, y = gravier$y, q = q)
  }
gravier_results <- data.frame(gravier_results)

gravier_summary <- ddply(gravier_results, .(method, q), summarize, error = mean(error), se = sd(error))

gravier_results <- list(
  results = gravier_results,
  summary = gravier_summary
)

save(gravier_results, file = "data/gravier.RData")