library('testthat')
library('yaml')
library('foreign')

#library('reshape')
library('plyr')
library('mvtnorm')
library('corpcor') # for cov.shrink and inv.cov.shrink for MLDA
library('MASS')
#library('stringr')
#library('ggplot2')
#library('log4r')

# For running parallel
library('foreach')
library('doMC')

# Registers the "multicore" parallel backend with the "foreach" package.
registerDoMC()

# Returns the number of parallel execution workers assigned.
getDoParWorkers()

if(run.locally) {
	# Loads Duin simulation configuration
	source("~/Dropbox/R/data-sets/data-simulated/R/data-duin.r")

	# Loads Friedman simulation configuration
	source("~/Dropbox/R/data-sets/data-simulated/R/data-friedman.r")

	# Loads Guo simulation configuration
	source("~/Dropbox/R/data-sets/data-simulated/R/data-guo.r")
	
	# Loads RLDA code
	source("~/Dropbox/R/rlda/R/rlda.r")
	source("~/Dropbox/R/rlda/R/mkhadri.r")
	source("~/Dropbox/R/rlda/R/predict.r")
	source("~/Dropbox/R/rlda/R/summary.r")

} else {
	# Loads Duin simulation configuration
	source("~/RameyPackagesR/data-simulated.git/R/data-duin.r")

	# Loads Friedman simulation configuration
	source("~/RameyPackagesR/data-simulated.git/R/data-friedman.r")

	# Loads Guo simulation configuration
	source("~/RameyPackagesR/data-simulated.git/R/data-guo.r")
	
	# Loads RLDA code
	source("~/Dropbox/R/rlda/R/rlda.r")
	source("~/rlda.git/mkhadri.r")
	source("~/rlda.git/predict.r")
	source("~/rlda.git/summary.r")
}