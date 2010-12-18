library('ProjectTemplate')
run.locally <- TRUE
verbose <- TRUE
parallel <- FALSE
load.project()

guo.sim <- function(n.k, test.size, p, variable.selection = TRUE, q, blocksize, rho, data.seed, verbose = FALSE) {
	if(verbose) cat("Generating training and test data\n")
	generated.data.df <- guo.data(n1 = n.k + test.size, n2 = n.k + test.size, p = p, rho = rho, block.size = blocksize, .seed = data.seed)
	if(verbose) cat("Generating training and test data...done!\n")
	
	# We are generating the training and test data at the same time.
	# From the generated data, we will randomly choose a subset as the training data.
	if(verbose) cat("Partitioning generated data into training and test data sets\n")
	which.are.training <- sapply(levels(generated.data.df$labels), function(label) sample(which(generated.data.df$labels == label), n.k))
	which.are.training <- as.vector(which.are.training)

	train.df <- generated.data.df[which.are.training,]
	test.df <- generated.data.df[-which.are.training,]
	if(verbose) cat("Partitioning generated data into training and test data sets...done!\n")

	if(verbose) cat("Building classifiers\n")
	mlda.out <- mlda(train.df)
	nlda.out <- nlda(train.df)
	lda.pseudo.out <- lda.pseudo(train.df)
	mdeb.out <- mdeb(train.df)
	rlda.grid.out <- rlda.grid(train.df)
	if(verbose) cat("Building classifiers...done!\n")

	if(verbose) cat("Performing model selection\n")
	rlda.grid.out <- model.select.rlda.grid(train.df, rlda.grid.out, grid.size = grid.size)
	if(verbose) cat("Performing model selection...done!\n")

	if(verbose) cat("Classifying validation data\n")
	test.x <- as.matrix(test.df[,-1])
	dimnames(test.x) <- NULL

	predictions.mlda <- predict.mlda(mlda.out, test.x)
	predictions.nlda <- predict.nlda(nlda.out, test.x)
	predictions.lda.pseudo <- predict.lda.pseudo(lda.pseudo.out, test.x)
	predictions.mdeb <- predict.mdeb(mdeb.out, test.x)
	predictions.rlda.grid <- predict.rlda.grid(rlda.grid.out, test.x)
	if(verbose) cat("Classifying validation data...done!\n")

	error.rate.mlda <- mean(test.df$labels != predictions.mlda)
	error.rate.nlda <- mean(test.df$labels != predictions.nlda)
	error.rate.lda.pseudo <- mean(test.df$labels != predictions.lda.pseudo)
	error.rate.mdeb <- mean(test.df$labels != predictions.mdeb)
	error.rate.rlda.grid <- mean(test.df$labels != predictions.rlda.grid)

	if(verbose) cat("MLDA Error Rate:", error.rate.mlda, "\n")
	if(verbose) cat("NLDA Error Rate:", error.rate.nlda, "\n")
	if(verbose) cat("LDA (Pseudo) Error Rate:", error.rate.lda.pseudo, "\n")
	if(verbose) cat("MDEB Error Rate:", error.rate.mdeb, "\n")
	if(verbose) cat("Grid Error Rate:", error.rate.rlda.grid, "\n")
	
	c(error.rate.mlda, error.rate.nlda, error.rate.lda.pseudo, error.rate.mdeb, error.rate.rlda.grid)
}

if(run.locally) {
	num.iterations <- 10

	sample.sizes <- c(10, 15)
	dim.features <- 25
	test.size <- 10
	
	autocorrelations <- 0.9
	block.size <- 25
	
	q <- 10

	grid.size <- 11
} else {
	num.iterations <- 250

	sample.sizes <- seq.int(10, 30, by = 20)
	#dim.features <- seq.int(25, 250, by = 25)
	dim.features <- c(25, 50, 100, 150)

	test.size <- 500

	autocorrelations <- 0.9
	block.size <- 25

	grid.size <- 11
}

sim.configurations <- expand.grid(sample.sizes, dim.features, autocorrelations, q)
names(sim.configurations) <- c("n.k", "p", "rho", "q")

guo.error.rates <- ddply(sim.configurations, .(n.k, p, rho, q), function(sim.config) {
	cat("n.k:", sim.config$n.k, "\tp:", sim.config$p, "\trho:", sim.config$rho, "\tq:", sim.config$q, "\n")
	error.rates <- laply(seq_len(num.iterations), function(i) {
		guo.sim(n.k = sim.config$n.k,
			test.size = test.size,
			p = sim.config$p,
			variable.selection = TRUE,
			q = sim.config$q,
			blocksize = block.size,
			rho = sim.config$rho,
			data.seed = i,
			verbose = verbose)
	}, .progress = "text", .parallel = parallel)
	error.rates.df <- data.frame(error.rates)
	names(error.rates.df) <- c("mlda", "nlda", "lda-pseudo", "mdeb", "rlda-grid")
	error.rates.df
})

save(guo.error.rates, file = "rlda-guo-sim-results.RData")