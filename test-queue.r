library('ProjectTemplate')
run.locally <- TRUE
verbose <- TRUE
parallel <- FALSE
load.project()


queue.duin.sim <- function(sim.config.df) {
	cat("New Duin Simulation\n")
	#print()
	cat("New Duin Simulation...queued!\n")
}

if(run.locally) {
	num.iterations <- 1000

	sample.sizes <- seq.int(5, 25, by = 5)
	dim.features <- 1000
	q <- 30
	test.size <- 500

	grid.size <- 11
} else {
	num.iterations <- 250

	sample.sizes <- seq.int(10, 30, by = 20)
	#dim.features <- seq.int(25, 250, by = 25)
	dim.features <- c(25, 50, 100, 150)
	q <- 50
	test.size <- 500

	grid.size <- 11
}

sim.configurations <- expand.grid(sample.sizes, dim.features, q)
names(sim.configurations) <- c("n.k", "p", "q")


# Queue a simulation for each simulation configuration
d_ply(sim.configurations, .(n.k, p, q), queue.duin.sim)