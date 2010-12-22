library('ProjectTemplate')
run.locally <- FALSE
verbose <- FALSE
parallel <- TRUE
load.project()

duin.sim <- function(n.k, test.size, p, variable.selection = TRUE, q, data.seed, verbose = FALSE) {
	if(verbose) cat("Generating training and test data\n")
	generated.data.df <- duin.data(n1 = n.k + test.size, n2 = n.k + test.size, p = p, .seed = data.seed)
	if(verbose) cat("Generating training and test data...done!\n")
	
	# We are generating the training and test data at the same time.
	# From the generated data, we will randomly choose a subset as the training data.
	if(verbose) cat("Partitioning generated data into training and test data sets\n")
	which.are.training <- sapply(levels(generated.data.df$labels), function(label) sample(which(generated.data.df$labels == label), n.k))
	which.are.training <- as.vector(which.are.training)

	train.df <- generated.data.df[which.are.training,]
	test.df <- generated.data.df[-which.are.training,]
	if(verbose) cat("Partitioning generated data into training and test data sets...done!\n")

	if(verbose) cat("Dimension of data before variable selection:", ncol(train.df) - 1, "\n")

	if(variable.selection) {
		var.select.out <- variable.selection.anova(train.df, q = q)
		train.df <- train.df[, c(1, var.select.out$kept.variables)]
		test.df <- test.df[, c(1, var.select.out$kept.variables)]
	}
	
	if(verbose) cat("Dimension of data after variable selection:", ncol(train.df) - 1, "\n")

	if(verbose) cat("Building classifiers\n")
	mlda.out <- mlda(train.df)
	nlda.out <- nlda(train.df)
	lda.pseudo.out <- lda.pseudo(train.df)
	mdeb.out <- mdeb(train.df)
	mdeb.pool.out <- mdeb.pool(train.df)
	rlda.grid.out <- rlda.grid(train.df)
	if(verbose) cat("Building classifiers...done!\n")

	if(verbose) cat("Performing model selection\n")
	mdeb.pool.out <- model.select.mdeb.pool(train.df, mdeb.pool.out, grid.size = grid.size)
	rlda.grid.out <- model.select.rlda.grid(train.df, rlda.grid.out, grid.size = grid.size)
	if(verbose) cat("Performing model selection...done!\n")

	if(verbose) cat("Classifying validation data\n")
	test.x <- as.matrix(test.df[,-1])
	dimnames(test.x) <- NULL

	predictions.mlda <- predict.mlda(mlda.out, test.x)
	predictions.nlda <- predict.nlda(nlda.out, test.x)
	predictions.lda.pseudo <- predict.lda.pseudo(lda.pseudo.out, test.x)
	predictions.mdeb <- predict.mdeb(mdeb.out, test.x)
	predictions.mdeb.pool <- predict.mdeb.pool(mdeb.pool.out, test.x)
	predictions.rlda.grid <- predict.rlda.grid(rlda.grid.out, test.x)
	if(verbose) cat("Classifying validation data...done!\n")

	error.rate.mlda <- mean(test.df$labels != predictions.mlda)
	error.rate.nlda <- mean(test.df$labels != predictions.nlda)
	error.rate.lda.pseudo <- mean(test.df$labels != predictions.lda.pseudo)
	error.rate.mdeb <- mean(test.df$labels != predictions.mdeb)
	error.rate.mdeb.pool <- mean(test.df$labels != predictions.mdeb.pool)
	error.rate.rlda.grid <- mean(test.df$labels != predictions.rlda.grid)

	if(verbose) cat("MLDA Error Rate:", error.rate.mlda, "\n")
	if(verbose) cat("NLDA Error Rate:", error.rate.nlda, "\n")
	if(verbose) cat("LDA (Pseudo) Error Rate:", error.rate.lda.pseudo, "\n")
	if(verbose) cat("MDEB Error Rate:", error.rate.mdeb, "\n")
	if(verbose) cat("MDEB-pool Error Rate:", error.rate.mdeb.pool, "\n")
	if(verbose) cat("Grid Error Rate:", error.rate.rlda.grid, "\n")
	
	c(error.rate.mlda, error.rate.nlda, error.rate.lda.pseudo, error.rate.mdeb, error.rate.mdeb.pool, error.rate.rlda.grid)
}

if(run.locally) {
	num.iterations <- 100

	n.k <- 10
	p <- 50
	q <- 10
	grid.size <- 11
	test.size <- 50
} else {
	# R --no-restore --no-save --args 5 10 5 3 0.9 0.01 < sim-guo.r > sim-guo.out 2>&1
	commandline.args <- commandArgs(trailingOnly = TRUE)
	cat("n: p: q: grid.size: test.size: num.iterations:\n")
	print(commandline.args)

	n.k <- as.integer(commandline.args[1])
	p <- as.integer(commandline.args[2])
	q <- as.integer(commandline.args[3])
	grid.size <- as.integer(commandline.args[4])
	test.size <- as.integer(commandline.args[5])
	num.iterations <- as.integer(commandline.args[6])
}

duin.error.rates <- laply(seq_len(num.iterations), function(i) {
	duin.sim(n.k = n.k,
		test.size = test.size,
		p = p,
		variable.selection = TRUE,
		q = q,
		data.seed = i,
		verbose = verbose)
}, .parallel = parallel, .progress = "text")
duin.error.rates <- data.frame(duin.error.rates)
names(duin.error.rates) <- c("mlda", "nlda", "lda-pseudo", "mdeb", "mdeb-pool", "rlda-grid")
duin.error.rates <- cbind.data.frame(n.k = n.k, p = p, q = q, duin.error.rates)

results.file <- "rlda-duin-sim-results.RData"

if(file.exists(results.file)) {
	duin.error.rates.current <- duin.error.rates
	load(results.file)
	duin.error.rates <- rbind(duin.error.rates, duin.error.rates.current)
}

save(duin.error.rates, file = results.file)
