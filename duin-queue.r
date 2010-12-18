library('ProjectTemplate')
load.project()

verbose <- FALSE
cleanup <- TRUE

num.iterations <- 1000

sample.sizes <- seq.int(5, 25, by = 5)
dim.features <- 1000
q <- 30
test.size <- 500

grid.size <- 11

sim.configurations <- expand.grid(sample.sizes, dim.features, q, grid.size, test.size, num.iterations)
names(sim.configurations) <- c("n.k", "p", "q", "grid.size", "test.size", "num.iterations")


# Queue a simulation for each simulation configuration
d_ply(sim.configurations, .(n.k, p, q, grid.size, test.size, num.iterations), queue.sim, cleanup = cleanup, verbose = verbose)