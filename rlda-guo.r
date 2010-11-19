library(ProjectTemplate)
run.locally <- FALSE
load.project()

# Number of Replications for each classifier
num.replications <- 1000

# n.k = num of observations per class
# p = dimension of feature space
# test.size = number of replications of each experiment
sample.sizes <- seq.int(5, 50, by = 5)
dim.features <- seq.int(25, 100, by = 25)
test.size <- 500

rho = 0.9
block.size = 25

parallel.flag <- TRUE

experiment <- expand.grid(sample.sizes, dim.features)
names(experiment) <- c("n.k", "p")

lda.results <- guo.sim(experiment, "lda", num.replications, rho = rho, block.size = block.size, parallel.flag = parallel.flag)
sim.results <- lda.results
save(sim.results, file = "rlda-guo-sim-results.RData")

nlda.results <- guo.sim(experiment, "nlda", num.replications, rho = rho, block.size = block.size, parallel.flag = parallel.flag)
sim.results <- rbind(sim.results, nlda.results)
save(sim.results, file = "rlda-guo-sim-results.RData")

mlda.results <- guo.sim(experiment, "mlda", num.replications, rho = rho, block.size = block.size, parallel.flag = parallel.flag)
sim.results <- rbind(sim.results, mlda.results)
save(sim.results, file = "rlda-guo-sim-results.RData")

grid.results <- guo.sim(experiment, "grid", num.replications, rho = rho, block.size = block.size, parallel.flag = parallel.flag)
sim.results <- rbind(sim.results, grid.results)
save(sim.results, file = "rlda-guo-sim-results.RData")

mkhadri.results <- guo.sim(experiment, "mkhadri", num.replications, rho = rho, block.size = block.size, parallel.flag = parallel.flag)
sim.results <- rbind(sim.results, mkhadri.results)
save(sim.results, file = "rlda-guo-sim-results.RData")

mkhadri.grid.results <- guo.sim(experiment, "mkhadri-grid", num.replications, rho = rho, block.size = block.size, parallel.flag = parallel.flag)
sim.results <- rbind(sim.results, mkhadri.grid.results)
save(sim.results, file = "rlda-guo-sim-results.RData")