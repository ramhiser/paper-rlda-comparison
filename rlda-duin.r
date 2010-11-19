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

parallel.flag <- TRUE

experiment <- expand.grid(sample.sizes, dim.features)
names(experiment) <- c("n.k", "p")

lda.results <- duin.sim(experiment, "lda", num.replications, parallel.flag = parallel.flag)
sim.results <- lda.results
save(sim.results, file = "rlda-duin-n5-50-sim-results.RData")

nlda.results <- duin.sim(experiment, "nlda", num.replications, parallel.flag = parallel.flag)
sim.results <- rbind(sim.results, nlda.results)
save(sim.results, file = "rlda-duin-n5-50-sim-results.RData")

mlda.results <- duin.sim(experiment, "mlda", num.replications, parallel.flag = parallel.flag)
sim.results <- rbind(sim.results, mlda.results)
save(sim.results, file = "rlda-duin-n5-50-sim-results.RData")

grid.results <- duin.sim(experiment, "grid", num.replications, parallel.flag = parallel.flag)
sim.results <- rbind(sim.results, grid.results)
save(sim.results, file = "rlda-duin-n5-50-sim-results.RData")

mkhadri.results <- duin.sim(experiment, "mkhadri", num.replications, parallel.flag = parallel.flag)
sim.results <- rbind(sim.results, mkhadri.results)
save(sim.results, file = "rlda-duin-n5-50-sim-results.RData")

mkhadri.grid.results <- duin.sim(experiment, "mkhadri-grid", num.replications, parallel.flag = parallel.flag)
sim.results <- rbind(sim.results, mkhadri.grid.results)
save(sim.results, file = "rlda-duin-n5-50-sim-results.RData")