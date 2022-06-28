.onLoad <- function(libname, pkgname) {
	op <- options()

	# Create the list with defaults if it does not exist
	if(is.null(op$handcalcs)) options(handcalcs = handcalcs_defaults())

	invisible()
}
