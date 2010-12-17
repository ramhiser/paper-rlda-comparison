library(ProjectTemplate)
run.locally <- FALSE
parallel <- TRUE
verbose <- FALSE
load.project()

khan.error.rates <- function(k = 5, variable.selection = TRUE, q = 30, verbose = FALSE, seed) {
	set.seed(seed)
	n <- nrow(tibshirani.khan)
	hold.out <- sample(seq_len(n), k)
	
	train.df <- tibshirani.khan[-hold.out,]
	test.df <- tibshirani.khan[hold.out,]
	
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
	
	c(error.rate.mlda, error.rate.nlda, error.rate.lda.pseudo, error.rate.mdeb, error.rate.rlda.grid, k, q)	
}

if(run.locally) {
	num.iterations <- 4

	hold.out.sizes <- c(3, 5)
	q <- c(30, 50)
	
	grid.size <- 2
} else {
	num.iterations <- 1000

	hold.out.sizes <- c(3, 5)
	q <- c(30, 50, 100)
	
	grid.size <- 11
}
sim.configurations <- expand.grid(hold.out.sizes, q)
names(sim.configurations) <- c("k", "q")

khan.error.rates <- ddply(sim.configurations, .(k, q), function(sim.config) {
	cat("Leaving Out:", sim.config$k, "\tq:", sim.config$q, "\n")
	error.rates <- laply(seq_len(num.iterations), function(i) {
		khan.error.rates(k = sim.config$k, variable.selection = TRUE, q = sim.config$q, verbose = verbose, seed = i)
	}, .parallel = parallel, .progress = "text")
	error.rates.df <- data.frame(error.rates)
	names(error.rates.df) <- c("mlda", "nlda", "lda-pseudo", "mdeb", "rlda-grid", "hold-out", "q")
	error.rates.df
})

save(khan.error.rates, file = "rlda-khan-sim-results.RData")