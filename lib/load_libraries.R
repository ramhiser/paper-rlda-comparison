library('testthat')
library('yaml')
library('foreign')

library('plyr')
library('mvtnorm')
library('corpcor') # for cov.shrink and inv.cov.shrink for MLDA
library('MASS')

# For running parallel
library('foreach')
library('doMC')

# For microarray data sets
library('datamicroarray')

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
	source("~/Dropbox/R/rlda/R/rlda-helper.r")
	source("~/Dropbox/R/rlda/R/grid.r")
	source("~/Dropbox/R/rlda/R/lda-pseudo.r")
	source("~/Dropbox/R/rlda/R/mdeb.r")
	source("~/Dropbox/R/rlda/R/mdeb-pool.r")
	source("~/Dropbox/R/rlda/R/mlda.r")
	source("~/Dropbox/R/rlda/R/nlda.r")

} else {
	# Loads Duin simulation configuration
	source("~/RameyPackagesR/data-simulated/R/data-duin.r")

	# Loads Friedman simulation configuration
	source("~/RameyPackagesR/data-simulated/R/data-friedman.r")

	# Loads Guo simulation configuration
	source("~/RameyPackagesR/data-simulated/R/data-guo.r")
	
	# Loads RLDA code
	source("~/RameyPackagesR/rlda/rlda-helper.r")
	source("~/RameyPackagesR/rlda/grid.r")
	source("~/RameyPackagesR/rlda/lda-pseudo.r")
	source("~/RameyPackagesR/rlda/mdeb.r")
	source("~/RameyPackagesR/rlda/mdeb-pool.r")
	source("~/RameyPackagesR/rlda/mlda.r")
	source("~/RameyPackagesR/rlda/nlda.r")
}