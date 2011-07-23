library('ProjectTemplate')
load.project()
registerDoMC()

seed <- 42
set.seed(seed)
B <- 250

p <- 1000
test_size <- 500

n <- seq.int(10, 100, by = 10)
q <- c(30, 50, 100, 200, 500)
sim_config <- expand.grid(n = n, q = q)

duin_results <- foreach(conf=t(sim_config), .combine=rbind) %:% 
  foreach(icount(B), .combine=rbind) %dopar% {
    rlda_sim(generate = "generate_duin", n = conf[1], p = p, q = conf[2],
		test_size = test_size)
  }

duin_results <- data.frame(duin_results)

duin_summary <- ddply(duin_results, .(method, n, q), summarize, error = mean(error), se = sd(error))

duin_results <- list(
  results = duin_results,
  summary = duin_summary,
  p = p,
  seed = seed
)

save(duin_results, file = "data/duin.RData")
