rlda_data <- function(x, y, q, training_pct = 0.8) {
  N <- nrow(x)
  train <- sample(seq_len(N), training_pct * N)
  train_x <- x[train, ]
  test_x <- x[-train, ]
  train_y <- y[train]
  test_y <- y[-train]

	cat("N:", N, "\n")
	cat("train_x:", dim(train_x), "\n")
	cat("train_y:", length(train_y), "\n")
	cat("test_x:", dim(test_x), "\n")
	cat("test_y:", length(test_y), "\n")

  # Perform variable selection with ANOVA
  var_select_out <- var_select_anova(x = train_x, y = train_y, q = q)
  train_x <- var_select_out$data
  test_x <- test_x[, var_select_out$kept]

	cat("var-selected train_x:", dim(train_x), "\n")
	cat("var-selected test_x:", dim(test_x), "\n")
	cat("Building classifiers\n")
  # Build classifiers
  mlda_out <- mlda(x = train_x, y = train_y)
  nlda_out <- nlda(x = train_x, y = train_y)
  mdeb_out <- mdeb(x = train_x, y = train_y)
  pseudo_out <- lda_pseudo(x = train_x, y = train_y)
  rda_out <- rda(x = train_x, y = train_y)
	cat("Building classifiers...done!\n")
  # Find the optimal value of gamma in the RDA model
  # The lambda parameter is fixed to be 1 because
  # we are assuming equal covariance matrices.
	# Using 10-fold cross-validation...
	folds <- 10
  hold_out <- floor(length(train_y) / folds)
  grid_size <- 11
  grid <- rbind(1, seq(0, 1, length = grid_size))
	cat("Making RLDA grid\n")
  grid_error <- foreach(j = grid, .combine=c) %do% {
    rda_cv(x = train_x, y = train_y, lambda = j[1], gamma = j[2], k = hold_out)$error
  }
	cat("Making RLDA grid...done!\n")
  grid  <- cbind.data.frame(t(grid), grid_error)
  names(grid) <- c("lambda", "gamma", "error")
	cat("grid:\n")
	print(grid)
  friedman_grid <- rda_friedman(training_error = grid)
	cat("friedman_grid\n")
	print(friedman_grid)
  # Make predictions
	cat("making predictions\n")
  pred_mlda <- predict_mlda(mlda_out, test_x)
  pred_nlda <- predict_nlda(nlda_out, test_x)
  pred_mdeb <- predict_mdeb(mdeb_out, test_x)
  pred_pseudo <- predict_lda_pseudo(pseudo_out, test_x)
  pred_rda <- predict_rda(rda_out, test_x, friedman_grid$lambda, friedman_grid$gamma)
	cat("making predictions...done!\n")
  list_mlda <- list(
    method = "MLDA",
    q = q,
    error = mean(pred_mlda$predicted != test_y)
  )
  list_nlda <- list(
    method = "NLDA",
    q = q,
    error = mean(pred_nlda$predicted != test_y)
  )
  list_mdeb <- list(
    method = "MDEB",
    q = q,
    error = mean(pred_mdeb$predicted != test_y)
  )
  list_pseudo <- list(
    method = "PFLD",
    q = q,
    error = mean(pred_pseudo$predicted != test_y)
  )
  list_rda <- list(
    method = "RDA",
    q = q,
    error = mean(pred_rda$predicted != test_y)
  )

  list_results <- list(
    list_mlda,
    list_nlda,
    list_mdeb,
    list_pseudo,
    list_rda
  )
  
  do.call(rbind, lapply(list_results, data.frame))		
}

rlda_sim <- function(generate, n, p, q, test_size, ...) {
  # Generates the training and test data simultaneously
  data <- get(generate)(n = n + test_size, p = p, ...)

  # Randomly partition the generated data into training/test
  which_train <- c(sample(which(data$y == 1), n), sample(which(data$y == 2), n))
  train_x <- data$x[which_train,]
  train_y <- data$y[which_train]
  test_x <- data$x[-which_train,]
  test_y <- data$y[-which_train]

  # Perform variable selection with ANOVA
  var_select_out <- var_select_anova(x = train_x, y = train_y, q = q)
  train_x <- var_select_out$data
  test_x <- test_x[, var_select_out$kept]

  # Build classifiers
  mlda_out <- mlda(x = train_x, y = train_y)

  # Make predictions
  pred_mlda <- predict_mlda(mlda_out, test_x)

  list_mlda <- list(
    method = "MLDA",
    n = n,
    q = q,
    error = mean(pred_mlda$predicted != test_y)
  )

  list_results <- list(
    list_mlda
  )
  
  do.call(rbind, lapply(list_results, data.frame))		
}

# For each variable in the data frame, we calculate the F statistic with the hypothesis
# that the means are equal for each class.
# The q variables with largest values of the F statistic are kept.
# The remaining p - qvariables are dropped.
# Assumes the first column contains the class (population) labels.
var_select_anova <- function(x, y, q = 30) {
	p <- ncol(x)
	F_stats <-  apply(x, 2, function(col) {
		summary(aov(col ~ y))[[1]][["F value"]][1]
	})
	F_stat_ranks <- rank(F_stats, ties = "random")

	kept_vars <- which(F_stat_ranks > p - q)
	dropped_vars <- which(F_stat_ranks <= p - q)
	
	list(
		data = x[, kept_vars],
		kept = kept_vars,
		dropped = dropped_vars,
		F_stats = F_stats
	)
}
