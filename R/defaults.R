#' Default behaviors for handcalcs functions
#'
#' Sets default behaviors for handcalcs functions related to rounding for
#' interim/final calculations and handling of LaTeX solution string.
#'
#' The default behavior creates a self-contained LaTeX solution string (enclosed
#' in an aligned display math environment). If you are writing the LaTeX code
#' manually, (e.g., if you wish to present multiple equations within the same
#' environment), you can provide the named value as an argument to any solution
#' function. If you wish to change the global defaults (applied to all
#' functions), set the corresponding value of the named list ("handcalcs") in
#' \code{options()}.
#'
#'
#' \describe{
#'
#' \item{\code{round_to}}{Round based on the number of digits past the decimal
#' ('decimal'), or based on the number of significant figures ('sigfig'). Note:
#' this parameter can only be set as a global default, not within individual
#' functions.}
#'
#' \item{\code{round_interim}}{Number of digits to round for interim
#' calculations (default = 4). Note: It is strongly recommended that you change
#' this value if you are using sigfigs instead of decimals for rounding.}
#'
#' \item{\code{round_final}}{Number of digits to round for final result (default
#' = 4). Note: It is strongly recommended that you change this value if you are
#' using sigfigs instead of decimals for rounding.}
#'
#' \item{\code{add_math}}{Enclose solution string in LaTeX math block (`$$`;
#' default = TRUE)? If set to `FALSE`, must manually include the math block tags
#' in order for the solution to render properly.}
#'
#' \item{\code{add_aligned}}{Enclose solution in LaTeX aligned block
#' (`\begin{aligned}`) (default = TRUE)? If set to `FALSE`, allows for multiple
#' solutions to be set in the same aligned environment.}
#'
#' \item{\code{use_aligned}}{Use "&=" instead of "=" in solution string to
#' generate aligned equations (default = TRUE).}
#'
#' }
#'
#' @return Named list of default values
#' @export
#'
#' @examples
#' handcalcs_defaults()
#'
handcalcs_defaults<- function() {
	list(
		round_to = 'decimals',
		round_interim = 4,
		round_final = 4,
		add_math = TRUE,
		add_aligned = TRUE,
		use_aligned = TRUE
	)
}

# # Checks to see if a value is set in the list. If so, returns that value. If
# # not, returns current global option. Note: does not confirm correct class/type
# # for values!
# check_defaults<- function(x, l) {
# 	op<- getOption('handcalcs')
# 	stopifnot(x %in% names(op))
#
# 	val <- ifelse(!is.null(l[[x]]), l[[x]], op[[x]])
# 	return(val)
# }


#' Generates list of options, overriding the global options if specified.
#'
#' get_handcalcs_opts() is a convenience function for setting options(handcalcs = .). (See
#' the examples for usage.) This is also used internally for parameters to be
#' passed to individual solution functions to temporarily override the global
#' default.
#'
#' @param ... Default options to override
#'
#' @return
#' @export
#'
#' @examples
#' # With no arguments, simply returns a complete list of the current global options
#' get_handcalcs_opts()
#'
#' # When arguments are specified, modifies the list accordingly
#' get_handcalcs_opts(round_interim = 5)
#'
#' # Most common usage will be to update the package defaults in global usage:
#' options(handcalcs = get_handcalcs_opts(round_interim = 5))
#'
get_handcalcs_opts<- function(...) {
	args <- list(...)
	opts <- options()$handcalcs

	# If for some reason this isn't set in the global options, set it.
	if(length(opts) == 0) opts <- options(handcalcs = handcalcs_defaults())

	# No arguments, return current global values
	if(length(args) == 0) return(opts)

	# Warn if an invalid argument is supplied
	for(i in names(args)) {
		if(!i %in% names(opts)) {
			warning(paste0('Named argument provided to get_handcalcs_opts is invalid: ', i))
		}
	}

	for(i in names(args)) {
		opts[[i]] = args[[i]]
	}

	return(opts)
}

