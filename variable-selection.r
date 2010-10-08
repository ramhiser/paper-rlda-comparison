library(plyr)

variable.selection.t.test <- function(training.df, alpha = 0.01) {
	var.select.pvals <- daply(training.df, 2, 
		function(col) {
			col.by.class <- split(col, temp.colon.labels)
			p.val <- t.test(col.by.class[[1]], col.by.class[[2]])$p.value
		})
}


filtered.x <- colon.x[, var.select.pvals < alpha]
transformed.x <- transformed.colon.train[, var.select.pvals < alpha]