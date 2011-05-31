library('ProjectTemplate')
load.project()

seed <- 42
set.seed(seed)
B <- 1000

p <- 1000
block_size <- 25
test_size <- 500

n <- seq.int(10, 100, by = 10)
q <- c(30, 50, 100)
sim_config <- expand.grid(n = n, q = q)

guo_results <- foreach(conf=t(sim_config), .combine=rbind) %:% 
  foreach(icount(B), .combine=rbind) %dopar% {
    rlda_sim(generate = "generate_guo", n = conf[1], p = p, q = conf[2],
    test_size = test_size, block_size = block_size)
  }

guo_results <- data.frame(guo_results)

guo_summary <- ddply(guo_results, .(method, n, q), summarize, error = mean(error), se = sd(error))

guo_results <- list(
  results = guo_results,
  summary = guo_summary,
  p = p,
  seed = seed
)

save(guo_results, file = "data/guo.RData")
