# Kodiak arguments
sim.type <- "rlda-duin"
np <- 8
npn <- 8
verbose <- FALSE
cleanup <- TRUE

# Simulation Configuration Design Points
num.iterations <- 1000
sample.sizes <- seq.int(5, 25, by = 5)
dim.features <- 1000
q <- 30
test.size <- 500
grid.size <- 11

sim.configurations <- expand.grid(sample.sizes, dim.features, q, grid.size, test.size, num.iterations)
names(sim.configurations) <- c("n.k", "p", "q", "grid.size", "test.size", "num.iterations")

# Queue a simulation for each simulation configuration
d_ply(sim.configurations, .(n.k, p, q, grid.size, test.size, num.iterations), queue.sim, sim.type = sim.type, np = np, npn = npn, cleanup = cleanup, verbose = verbose)