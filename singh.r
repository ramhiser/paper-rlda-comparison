library('ProjectTemplate')
load.project()

seed <- 42
set.seed(seed)
B <- 1000
q_vec <- c(30, 50, 100)

data('singh')

singh_results <- foreach(q = q_vec, .combine=rbind) %:%
  foreach(icount(B), .combine=rbind) %dopar% {
    rlda_data(x = singh$x, y = singh$y, q = q)
  }
singh_results <- data.frame(singh_results)

singh_summary <- ddply(singh_results, .(method, q), summarize, error = mean(error), se = sd(error))

singh_results <- list(
  results = singh_results,
  summary = singh_summary
)

save(singh_results, file = "data/singh.RData")
