library(ProjectTemplate)
run.locally <- FALSE
load.project()

golub.df <- rbind(golub.train, golub.test)

golub.error.rates <- function(k = 5, variable.selection = TRUE, alpha = 0.01, verbose = FALSE) {
	n <- nrow(golub.df)
	hold.out <- sample(seq_len(n), k)
	
	train.df <- golub.df[-hold.out,]
	test.df <- golub.df[hold.out,]

	if(variable.selection) {
		var.select.out <- variable.selection.t.test(train.df, alpha = alpha)
		train.df <- train.df[, c(1, var.select.out$kept.variables)]
		test.df <- test.df[, c(1, var.select.out$kept.variables)]
	}
		
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
	
	c(error.rate.mlda, error.rate.nlda, error.rate.lda.pseudo, error.rate.mdeb, error.rate.mkhadri, error.rate.rlda.grid, k, alpha)	
}

set.seed(13)

grid.size <- 11
num.iterations <- 1000

hold.out.sizes <- c(2, 3, 4, 5)
alphas <- c(0.01, 0.05, 0.1)

sim.configurations <- expand.grid(hold.out.sizes, alphas)
names(sim.configurations) <- c("k", "alpha")

sim.error.rates <- adply(sim.configurations, 1, function(sim.config) {
	cat("Leaving Out:", sim.config$k, "\talpha:", sim.config$alpha, "\n")
	error.rates <- replicate(num.iterations, golub.error.rates(k = sim.config$k, variable.selection = TRUE, alpha = sim.config$alpha, verbose = TRUE))
	error.rates.df <- data.frame(t(error.rates))
	names(error.rates.df) <- c("mlda", "nlda", "lda-pseudo", "mdeb", "mkhadri", "rlda-grid", "hold-out", "alpha")
	error.rates.df
})

save(sim.error.rates, file = "rlda-golub-sim-results.RData")