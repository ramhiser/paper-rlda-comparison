library('ProjectTemplate')
run.locally <- FALSE
load.project()

# Kodiak arguments
sim.type <- "rlda-duin"
np <- 8
npn <- 8
verbose <- FALSE
cleanup <- FALSE

# Simulation Configuration Design Points
num.iterations <- 1000
sample.sizes <- seq.int(30, 50, by = 5)
dim.features <- 1000
q <- c(30, 50, 100)
test.size <- 500
grid.size <- 11

sim.configurations <- expand.grid(sample.sizes, dim.features, q, grid.size, test.size, num.iterations)
names(sim.configurations) <- c("n.k", "p", "q", "grid.size", "test.size", "num.iterations")

# Queue a simulation for each simulation configuration
d_ply(sim.configurations, .(n.k, p, q, grid.size, test.size, num.iterations), queue.sim, sim.type = sim.type, np = np, npn = npn, cleanup = cleanup, verbose = verbose)