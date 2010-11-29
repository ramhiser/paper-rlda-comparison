library(ProjectTemplate)
library(jointDiag)
run.locally <- FALSE
verbose <- FALSE
parallel <- TRUE
load.project()

guo.sim <- function(n.k, test.size, p, blocksize, rho, training.seed, test.seed, verbose = FALSE) {
	if(verbose) cat("Generating training data\n")
	train.df <- guo.data(n1 = n.k, n2 = n.k, p = p, rho = rho, block.size = blocksize, .seed = training.seed)
	if(verbose) cat("Generating training data...done!\n")

	if(verbose) cat("Building classifiers\n")
	mlda.out <- mlda(train.df)
	nlda.out <- nlda(train.df)
	lda.pseudo.out <- lda.pseudo(train.df)
	mdeb.out <- mdeb(train.df)
	mkhadri.out <- mkhadri(train.df)
	rlda.grid.out <- rlda.grid(train.df)
	if(verbose) cat("Building classifiers...done!\n")

	if(verbose) cat("Performing model selection\n")
	mkhadri.out <- model.select.mkhadri(train.df, mkhadri.out)
	rlda.grid.out <- model.select.rlda.grid(train.df, rlda.grid.out, grid.size = grid.size)
	if(verbose) cat("Performing model selection...done!\n")

	if(verbose) cat("Generating validation data\n")
	test.df <- guo.data(n1 = test.size, n2 = test.size, p = p, rho = rho, block.size = blocksize, .seed = test.seed)
	if(verbose) cat("Generating validation data...done!\n")

	if(verbose) cat("Classifying validation data\n")
	test.x <- as.matrix(test.df[,-1])
	dimnames(test.x) <- NULL

	predictions.mlda <- predict.mlda(mlda.out, test.x)
	predictions.nlda <- predict.nlda(nlda.out, test.x)
	predictions.lda.pseudo <- predict.lda.pseudo(lda.pseudo.out, test.x)
	predictions.mdeb <- predict.mdeb(mdeb.out, test.x)
	predictions.mkhadri <- predict.mkhadri(mkhadri.out, test.x)
	predictions.rlda.grid <- predict.rlda.grid(rlda.grid.out, test.x)
	if(verbose) cat("Classifying validation data...done!\n")

	error.rate.mlda <- mean(test.df$labels != predictions.mlda)
	error.rate.nlda <- mean(test.df$labels != predictions.nlda)
	error.rate.lda.pseudo <- mean(test.df$labels != predictions.lda.pseudo)
	error.rate.mdeb <- mean(test.df$labels != predictions.mdeb)
	error.rate.mkhadri <- mean(test.df$labels != predictions.mkhadri)
	error.rate.rlda.grid <- mean(test.df$labels != predictions.rlda.grid)

	if(verbose) cat("MLDA Error Rate:", error.rate.mlda, "\n")
	if(verbose) cat("NLDA Error Rate:", error.rate.nlda, "\n")
	if(verbose) cat("LDA (Pseudo) Error Rate:", error.rate.lda.pseudo, "\n")
	if(verbose) cat("MDEB Error Rate:", error.rate.mdeb, "\n")
	if(verbose) cat("Mkhadri Error Rate:", error.rate.mkhadri, "\n")
	if(verbose) cat("Grid Error Rate:", error.rate.rlda.grid, "\n")
	
	c(error.rate.mlda, error.rate.nlda, error.rate.lda.pseudo, error.rate.mdeb, error.rate.mkhadri, error.rate.rlda.grid)
}


# Number of Replications for each simulated error rate
num.replications <- 1000

# n.k = num of observations per class
# p = dimension of feature space
# test.size = number of replications of each experiment
sample.sizes <- seq.int(10, 50, by = 10)
dim.features <- seq.int(25, 250, by = 25)
test.size <- 500

rho = 0.9
block.size = 25

sim.configurations <- expand.grid(sample.sizes, feature.dimensions, autocorrelations)
names(sim.configurations) <- c("n.k", "p", "rho")

sim.error.rates <- adply(sim.configurations, 1, function(sim.config) {
	cat("n.k:", sim.config$n.k, "\tp:", sim.config$p, "\trho:", sim.config$rho, "\n")
	error.rates <- laply(seq_len(num.replications), function(i) {
		guo.sim(n.k = sim.config$n.k,
			test.size = test.size,
			p = sim.config$p,
			blocksize = block.size,
			rho = sim.config$rho,
			training.seed = i,
			test.seed = 1000 + i)
	}, .progress = "text", .parallel = parallel)
	data.frame(mlda = error.rates[,1], nlda = error.rates[,2], lda.pseudo = error.rates[,3], mdeb = error.rates[,4], mkhadri = error.rates[,5], rlda.grid = error.rates[,6])
})

save(sim.error.rates, file = "rlda-guo-sim-results.RData")