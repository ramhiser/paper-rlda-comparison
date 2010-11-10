clean.variable.name <- function(variable.name)
{
  variable.name <- gsub('_', '.', variable.name, perl = TRUE)
  variable.name <- gsub('-', '.', variable.name, perl = TRUE)
  variable.name <- gsub('\\s+', '.', variable.name, perl = TRUE)
  return(variable.name)
}

# For each variable in the data frame, we test the hypothesis that the means are
# equal for each class and calculate the p-value.
# The variables that yield p-values < alpha are kept.
# The variables that yield p-values >= alpha are dropped.
# Assumes the first column contains the class (population) labels.
variable.selection.t.test <- function(df, alpha = 0.01) {
	x <- as.matrix(df[,-1])
	dimnames(x) <- NULL
	var.select.pvals <- aaply(x, 2, 
		function(col) {
			col.by.class <- split(col, df[,1])
			p.val <- t.test(col.by.class[[1]], col.by.class[[2]])$p.value
		})
	
	# We compute the column index of the variables that will be selected and dropped.
	# NOTE: The first column contains the class labels, so we must correct for it
	#	when we determine which variables have been kept and dropped.
	kept.variables <- which(var.select.pvals < alpha) + 1
	dropped.variables <- which(var.select.pvals >= alpha) + 1
	
	names(kept.variables) <- NULL
	names(dropped.variables) <- NULL
	
	list(kept.variables = kept.variables, dropped.variables = dropped.variables, p.vals = var.select.pvals)
}

# For each variable in the data frame, we test the hypothesis that the means are
# equal for each class and calculate the p-value.
# The variables that yield p-values < alpha are kept.
# The variables that yield p-values >= alpha are dropped.
# Assumes the first column contains the class (population) labels.
variable.selection.anova <- function(df, alpha = 0.01) {
	x <- as.matrix(df[,-1])
	dimnames(x) <- NULL
	var.select.pvals <- aaply(x, 2, 
		function(col) {
			aov.out <- aov(as.matrix(col) ~ df[,1])
			p.val <- summary(aov.out)[[1]][,5][1]
		})
	
	# We compute the column index of the variables that will be selected and dropped.
	# NOTE: The first column contains the class labels, so we must correct for it
	#	when we determine which variables have been kept and dropped.
	kept.variables <- which(var.select.pvals < alpha) + 1
	dropped.variables <- which(var.select.pvals >= alpha) + 1
	
	names(kept.variables) <- NULL
	names(dropped.variables) <- NULL
	
	list(kept.variables = kept.variables, dropped.variables = dropped.variables, p.vals = var.select.pvals)
}

# Returns a list of mutually exclusive folds utilizing leave-k-out crossvalidation.
leave.k.out <- function(n, k = 1) {
	obs <- seq_len(n)
	folds <- list()
	fold.i <- 1
	
	while(length(obs) > 0) {
		fold <- numeric(0)
		if(k > length(obs)) {
			fold <- obs
			obs <- numeric(0)
		}
		else {
			fold <- sample(obs, k)
			obs <- obs[-which(obs %in% fold)]
		}
		folds[[paste("fold", fold.i, sep = "")]] <- fold
		fold.i <- fold.i + 1
	}
	folds
}

guo.error.rates <- function(N, p, rlda.method, num.replications, rho, block.size, parallel.flag = FALSE) {
	cat("N:", N, "\tp:", p, "\tMethod:", rlda.method, "\n")
	
	error.rates <- aaply(seq_len(num.replications), 1, function(rep) {
		# For each simulation replication, we use a different seed to generate the
		# random variates. We arbitrarily choose the training seed to be the current
		# replication number and the test data seed to be the same with 1000 added to it.
		training <- guo.data(n1 = N/2, n2 = N/2, p = p, rho = rho, block.size = block.size, .seed = rep)
		test.data <- guo.data(n1 = test.size/2, n2 = test.size/2, p = p, rho = rho, block.size = block.size, .seed = 1000 + rep)

		classifier <- rlda(training, .method = rlda.method)
		predicted.classes <- predict(classifier, test.data[,-1], pseudo.inv = TRUE)$group
		mean(test.data[,1] != predicted.classes)
	}, .parallel = parallel.flag, .progress = "text")
	data.frame(N = N, p = p, method = rlda.method, error = error.rates)
}

guo.sim <- function(experiment, rlda.method, num.replications, rho, block.size, parallel.flag = FALSE) {
	sim.results <- adply(experiment, 1, function(exper) {
		guo.error.rates(N = exper$N,
				p = exper$p,
				rlda.method = rlda.method,
				num.replications = num.replications,
				rho = rho,
				block.size = block.size,
				parallel.flag = parallel.flag
		)
	})
}

duin.error.rates <- function(N, p, rlda.method, num.replications, parallel.flag = FALSE) {
	cat("N:", N, "\tp:", p, "\tMethod:", rlda.method, "\n")
	
	error.rates <- aaply(seq_len(num.replications), 1, function(rep) {
		# For each simulation replication, we use a different seed to generate the
		# random variates. We arbitrarily choose the training seed to be the current
		# replication number and the test data seed to be the same with 1000 added to it.
		training <- duin.data(n1 = N/2, n2 = N/2, p = p, .seed = rep)
		test.data <- duin.data(n1 = test.size/2, n2 = test.size/2, p = p, .seed = 1000 + rep)

		classifier <- rlda(training, .method = rlda.method)
		predicted.classes <- predict(classifier, test.data[,-1], pseudo.inv = TRUE)$group
		mean(test.data[,1] != predicted.classes)
	}, .parallel = parallel.flag, .progress = "text")
	data.frame(N = N, p = p, method = rlda.method, error = error.rates)
}

duin.sim <- function(experiment, rlda.method, num.replications, rho, block.size, parallel.flag = FALSE) {
	sim.results <- adply(experiment, 1, function(exper) {
		duin.error.rates(N = exper$N,
				p = exper$p,
				rlda.method = rlda.method,
				num.replications = num.replications,
				parallel.flag = parallel.flag
		)
	})
}

friedman.error.rates <- function(N, p, rlda.method, num.replications, experiment.num, parallel.flag = FALSE) {
	cat("N:", N, "\tp:", p, "\tMethod:", rlda.method, "\n")
	
	error.rates <- aaply(seq_len(num.replications), 1, function(rep) {
		# For each simulation replication, we use a different seed to generate the
		# random variates. We arbitrarily choose the training seed to be the current
		# replication number and the test data seed to be the same with 1000 added to it.
		training <- friedman.data(.n = N, .p = p, .experiment = experiment.num, .seed = rep)
		test.data <- friedman.data(.n = test.size, .p = p, .experiment = experiment.num, .seed = 1000 + rep)

		classifier <- rlda(training, .method = rlda.method)
		predicted.classes <- predict(classifier, test.data[,-1], pseudo.inv = TRUE)$group
		mean(test.data[,1] != predicted.classes)
	}, .parallel = parallel.flag, .progress = "text")
	data.frame(N = N, p = p, method = rlda.method, error = error.rates)
}

friedman.sim <- function(experiment, rlda.method, num.replications, friedman.experiment.num, parallel.flag = FALSE) {
	sim.results <- adply(experiment, 1, function(exper) {
		friedman.error.rates(N = exper$N,
				p = exper$p,
				rlda.method = rlda.method,
				num.replications = num.replications,
				experiment.num = friedman.experiment.num,
				parallel.flag = parallel.flag
		)
	})
}

# TODO: Need to use same folds for each classifier.
colon.error.rates <- function(rlda.method, k = 5, variable.selection = FALSE, alpha = 0.01, parallel.flag = FALSE) {
	n <- nrow(colon.cancer)
	folds <- leave.k.out(n, k)
	error.rates <- laply(folds, function(fold) {
		training.df <- colon.cancer[-fold,]
		test.df <- colon.cancer[fold,]

		if(variable.selection) {
			var.select.out <- variable.selection.t.test(training.df, alpha = alpha)
			training.df <- training.df[, c(1, var.select.out$kept.variables)]
			test.df <- test.df[, c(1, var.select.out$kept.variables)]
		}
		classifier <- rlda(training.df, .method = rlda.method)
		predicted.classes <- predict(classifier, test.df[,-1], pseudo.inv = TRUE)$group
		mean(test.df[,1] != predicted.classes)
	}, .progress = "text", .parallel = parallel.flag)
	
	data.frame(method = rlda.method, error = error.rates)
}

# TODO: Need to use same folds for each classifier.
khan.error.rates <- function(rlda.method, k = 5, variable.selection = FALSE, alpha = 0.01, parallel.flag = FALSE) {
	n <- nrow(tibshirani.khan)
	folds <- leave.k.out(n, k)
	error.rates <- laply(folds, function(fold) {
		training.df <- tibshirani.khan[-fold,]
		test.df <- tibshirani.khan[fold,]

		if(variable.selection) {
			var.select.out <- variable.selection.anova(training.df, alpha = alpha)
			training.df <- training.df[, c(1, var.select.out$kept.variables)]
			test.df <- test.df[, c(1, var.select.out$kept.variables)]
		}
		classifier <- rlda(training.df, .method = rlda.method)
		predicted.classes <- predict(classifier, test.df[,-1], pseudo.inv = TRUE)$group
		mean(test.df[,1] != predicted.classes)
	}, .progress = "text", .parallel = parallel.flag)
	
	data.frame(method = rlda.method, error = error.rates)
}

golub.error.rates <- function(rlda.method, k = 5, variable.selection = FALSE, alpha = 0.01) {
	training.df <- golub.train
	test.df <- golub.test
	
	if(variable.selection) {
		var.select.out <- variable.selection.anova(golub.train, alpha = alpha)
		training.df <- golub.train[, c(1, var.select.out$kept.variables)]
		test.df <- golub.test[, c(1, var.select.out$kept.variables)]
	}
	
	classifier <- rlda(training.df, .method = rlda.method)
	predicted.classes <- predict(classifier, test.df[,-1], pseudo.inv = TRUE)$group
	error.rate <- mean(test.df[,1] != predicted.classes)
	
	data.frame(method = rlda.method, error = error.rate)
}