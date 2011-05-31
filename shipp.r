library('ProjectTemplate')
load.project()

seed <- 42
set.seed(seed)
B <- 1000
hold_out <- 5
q_vec <- c(30, 50, 100)

data('shipp')

shipp_results <- foreach(q = q_vec, .combine=rbind) %:%
  foreach(icount(B), .combine=rbind) %dopar% {
    rlda_data(x = shipp$x, y = shipp$y, q = q, hold_out = hold_out)
  }
shipp_results <- data.frame(shipp_results)

shipp_summary <- ddply(shipp_results, .(method, q), summarize, error = mean(error), se = sd(error))

shipp_results <- list(
  results = shipp_results,
  summary = shipp_summary,
  hold_out = hold_out
)

save(shipp_results, file = "data/shipp.RData")
