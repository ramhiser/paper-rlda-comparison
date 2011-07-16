library('ProjectTemplate')
load.project()
registerDoMC()

seed <- 42
set.seed(seed)
B <- 100
q_vec <- c(30, 50, 100, 200, 500)

data('alon')

alon_results <- foreach(q = q_vec, .combine=rbind) %:%
  foreach(icount(B), .combine=rbind) %dopar% {
    rlda_data(x = alon$x, y = alon$y, q = q)
  }
alon_results <- data.frame(alon_results)

alon_summary <- ddply(alon_results, .(method, q), summarize, error = mean(error), se = sd(error))

alon_results <- list(
  results = alon_results,
  summary = alon_summary
)

save(alon_results, file = "data/alon.RData")
