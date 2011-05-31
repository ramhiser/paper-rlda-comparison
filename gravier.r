library('ProjectTemplate')
load.project()

seed <- 42
set.seed(seed)
B <- 1000
hold_out <- 5
q_vec <- c(30, 50, 100)

data('gravier')

gravier_results <- foreach(q = q_vec, .combine=rbind) %:%
  foreach(icount(B), .combine=rbind) %dopar% {
    rlda_data(x = gravier$x, y = gravier$y, q = q, hold_out = hold_out)
  }
gravier_results <- data.frame(gravier_results)

gravier_summary <- ddply(gravier_results, .(method, q), summarize, error = mean(error), se = sd(error))

gravier_results <- list(
  results = gravier_results,
  summary = gravier_summary,
  hold_out = hold_out
)

save(gravier_results, file = "data/gravier.RData")
