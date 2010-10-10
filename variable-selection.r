library(plyr)

# For each variable in the data frame, we test the hypothesis that the means are
# equal for each class and calculate the p-value.
# The variables that yield p-values < alpha are kept.
# The variables that yield p-values >= alpha are dropped.
# Assumes the first column contains the class (population) labels.
variable.selection.t.test <- function(df, alpha = 0.01) {
	var.select.pvals <- aaply(df[,-1], 2, 
		function(col) {
			col.by.class <- split(col, df[,1])
			p.val <- t.test(col.by.class[[1]], col.by.class[[2]])$p.value
		})
		
	kept.variables <- which(var.select.pvals < alpha)
	dropped.variables <- which(var.select.pvals >= alpha)
	
	names(kept.variables) <- NULL
	names(dropped.variables) <- NULL
	
	list(kept.variables = kept.variables, dropped.variables = dropped.variables, p.vals = var.select.pvals)
}