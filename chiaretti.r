library('ProjectTemplate')
load.project()

seed <- 42
set.seed(seed)
B <- 1000
q_vec <- c(30, 50, 100)

data('chiaretti')

chiaretti_results <- foreach(q = q_vec, .combine=rbind) %:%
  foreach(icount(B), .combine=rbind) %dopar% {
    rlda_data(x = chiaretti$x, y = chiaretti$y, q = q)
  }
chiaretti_results <- data.frame(chiaretti_results)

chiaretti_summary <- ddply(chiaretti_results, .(method, q), summarize, error = mean(error), se = sd(error))

chiaretti_results <- list(
  results = chiaretti_results,
  summary = chiaretti_summary
)

save(chiaretti_results, file = "data/chiaretti.RData")
