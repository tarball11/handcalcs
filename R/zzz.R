.onLoad <- function(libname, pkgname) {
	op <- options()

	# Fetch the defaults
	op.handcalcs<- handcalcs_defaults()

	# Create the list with defaults if it does not exist
	if(is.null(op$handcalcs)) options(handcalcs = op.handcalcs)

	invisible()
}
