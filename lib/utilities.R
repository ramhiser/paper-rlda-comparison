clean.variable.name <- function(variable.name)
{
  variable.name <- gsub('_', '.', variable.name, perl = TRUE)
  variable.name <- gsub('-', '.', variable.name, perl = TRUE)
  variable.name <- gsub('\\s+', '.', variable.name, perl = TRUE)
  return(variable.name)
}

rlda_data <- function(x, y, q, hold_out = 5) {
  N <- nrow(x)
  test <- sample(seq_len(N), hold_out)
  train_x <- x[-test, ]
  test_x <- x[test, ]
  train_y <- y[-test]
  test_y <- y[test]

  # Perform variable selection with ANOVA
  var_select_out <- var_select_anova(x = train_x, y = train_y, q = q)
  train_x <- var_select_out$data
  test_x <- test_x[, var_select_out$kept]

  # Build classifiers
  mlda_out <- mlda(x = train_x, y = train_y)
  nlda_out <- nlda(x = train_x, y = train_y)
  mdeb_out <- mdeb(x = train_x, y = train_y)
  pseudo_out <- lda_pseudo(x = train_x, y = train_y)
  rda_out <- rda(x = train_x, y = train_y)

  # Find the optimal value of gamma in the RDA model
  # The lambda parameter is fixed to be 1 because
  # we are assuming equal covariance matrices.
  hold_out <- 5
  grid_size <- 11
  grid <- rbind(1, seq(0, 1, length = grid_size))
  grid_error <- foreach(j = grid, .combine=c) %do% {
    rda_cv(x = train_x, y = train_y, lambda = j[1], gamma = j[2], k = hold_out)$error
  }
  grid  <- cbind.data.frame(t(grid), grid_error)
  names(grid) <- c("lambda", "gamma", "error")
  friedman_grid <- rda_friedman(training_error = grid)

  # Make predictions
  pred_mlda <- predict_mlda(mlda_out, test_x)
  pred_nlda <- predict_nlda(nlda_out, test_x)
  pred_mdeb <- predict_mdeb(mdeb_out, test_x)
  pred_pseudo <- predict_lda_pseudo(pseudo_out, test_x)
  pred_rda <- predict_rda(rda_out, test_x, friedman_grid$lambda, friedman_grid$gamma)

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
  nlda_out <- nlda(x = train_x, y = train_y)
  mdeb_out <- mdeb(x = train_x, y = train_y)
  pseudo_out <- lda_pseudo(x = train_x, y = train_y)
  rda_out <- rda(x = train_x, y = train_y)

  # Find the optimal value of gamma in the RDA model
  # The lambda parameter is fixed to be 1 because
  # we are assuming equal covariance matrices.
  hold_out <- 5
  grid_size <- 11
  grid <- rbind(1, seq(0, 1, length = grid_size))
  grid_error <- foreach(j = grid, .combine=c) %do% {
    rda_cv(x = train_x, y = train_y, lambda = j[1], gamma = j[2], k = hold_out)$error
  }
  grid  <- cbind.data.frame(t(grid), grid_error)
  names(grid) <- c("lambda", "gamma", "error")
  friedman_grid <- rda_friedman(training_error = grid)

  # Make predictions
  pred_mlda <- predict_mlda(mlda_out, test_x)
  pred_nlda <- predict_nlda(nlda_out, test_x)
  pred_mdeb <- predict_mdeb(mdeb_out, test_x)
  pred_pseudo <- predict_lda_pseudo(pseudo_out, test_x)
  pred_rda <- predict_rda(rda_out, test_x, friedman_grid$lambda, friedman_grid$gamma)

  list_mlda <- list(
    method = "MLDA",
    n = n,
    q = q,
    error = mean(pred_mlda$predicted != test_y)
  )
  list_nlda <- list(
    method = "NLDA",
    n = n,
    q = q,
    error = mean(pred_nlda$predicted != test_y)
  )
  list_mdeb <- list(
    method = "MDEB",
    n = n,
    q = q,
    error = mean(pred_mdeb$predicted != test_y)
  )
  list_pseudo <- list(
    method = "PFLD",
    n = n,
    q = q,
    error = mean(pred_pseudo$predicted != test_y)
  )
  list_rda <- list(
    method = "RDA",
    n = n,
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
