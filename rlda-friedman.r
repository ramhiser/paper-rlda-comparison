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

friedman.experiment.num <- 4

parallel.flag <- TRUE

experiment <- expand.grid(sample.sizes, dim.features)
names(experiment) <- c("n.k", "p")

lda.results <- friedman.sim(experiment, "lda", num.replications, friedman.experiment.num, parallel.flag = parallel.flag)
sim.results <- lda.results
save(sim.results, file = "rlda-friedman-n5-50-sim-results.RData")

nlda.results <- friedman.sim(experiment, "nlda", num.replications, friedman.experiment.num, parallel.flag = parallel.flag)
sim.results <- rbind(sim.results, nlda.results)
save(sim.results, file = "rlda-friedman-n5-50-sim-results.RData")

mlda.results <- friedman.sim(experiment, "mlda", num.replications, friedman.experiment.num, parallel.flag = parallel.flag)
sim.results <- rbind(sim.results, mlda.results)
save(sim.results, file = "rlda-friedman-n5-50-sim-results.RData")

grid.results <- friedman.sim(experiment, "grid", num.replications, friedman.experiment.num, parallel.flag = parallel.flag)
sim.results <- rbind(sim.results, grid.results)
save(sim.results, file = "rlda-friedman-n5-50-sim-results.RData")

mkhadri.results <- friedman.sim(experiment, "mkhadri", num.replications, friedman.experiment.num, parallel.flag = parallel.flag)
sim.results <- rbind(sim.results, mkhadri.results)
save(sim.results, file = "rlda-friedman-n5-50-sim-results.RData")

mkhadri.grid.results <- friedman.sim(experiment, "mkhadri-grid", num.replications, friedman.experiment.num, parallel.flag = parallel.flag)
sim.results <- rbind(sim.results, mkhadri.grid.results)
save(sim.results, file = "rlda-friedman-n5-50-sim-results.RData")
