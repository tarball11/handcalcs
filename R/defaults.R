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
#' [options()].
#'
#'
#' \describe{
#'
#' \item{`round_to`}{Round based on the number of digits past the decimal
#' ('decimal'), or based on the number of significant figures ('sigfig'). Note:
#' this parameter can only be set as a global default, not within individual
#' functions.}
#'
#' \item{`round_interim`}{Number of digits to round for interim calculations
#' (default = 4). Note: It is strongly recommended that you change this value if
#' you are using sigfigs instead of decimals for rounding.}
#'
#' \item{`round_final`}{Number of digits to round for final result (default =
#' 4). Note: It is strongly recommended that you change this value if you are
#' using sigfigs instead of decimals for rounding.}
#'
#' \item{`round_z`}{Number of digits to round values of `z`, such as for
#' *z*-scores (default = 2). This is primarily useful to ensure that the *z*
#' values being reported conform to the values presented in a z-table. Note that
#' this overrides `round_interim` and `round_final`.}
#'
#' \item{`round_t`}{Number of digits to round values of `t`, such as for
#' critical values of *t* (default = 3). This is primarily useful to ensure that
#' the *t* values being reported conform to the values presented in a t-table.
#' Note that this overrides `round_interim` and `round_final`.}
#'
#' \item{`add_math`}{Enclose solution string in LaTeX math block (`$$`; default
#' = TRUE)? If set to `FALSE`, must manually include the math block tags in
#' order for the solution to render properly.}
#'
#' \item{`add_aligned`}{Enclose solution in LaTeX aligned block
#' (`\begin{aligned}`) (default = TRUE)? If set to `FALSE`, allows for multiple
#' solutions to be set in the same aligned environment.}
#'
#' \item{`use_aligned`}{Use "&=" instead of "=" in solution string to generate
#' aligned equations (default = TRUE).}
#'
#' \item{`show_summation`}{Logical. Should explicit summation steps (e.g., for
#' sum of squares) be made explicit? If true, will include a step after a
#' formula shows \eqn{\sum{X}} that makes the summation explicit (e.g.,
#' \eqn{X_{1} + X_{2} + ... + X_{n}}). If false, will skip that step. (See
#' [summation()].)}
#'
#' \item{`abbrev_sum`}{Numeric scalar (default = 6). If `show_summation` is
#' TRUE, at what length of `x` should it abbreviate the explicit summation
#' within the solution using an ellipsis? If the length of `x` is greater than
#' or equal to `abbrev_sum`, it will abbreviate the summation sequence (e.g., "1
#' + 2 + ... + 6"). Otherwise, will show the entire sequence (e.g., "1 + 2 + 3 +
#' 4 + 5 + 6").}
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
		round_z = 2,
		round_t = 3,
		add_math = TRUE,
		add_aligned = TRUE,
		use_aligned = TRUE,
		show_summation = TRUE,
		abbrev_sum = 6
	)
}


#' Generates list of options, overriding the global options if specified.
#'
#' get_handcalcs_opts() is a convenience function for setting options(handcalcs = .). (See
#' the examples for usage.) This is also used internally for parameters to be
#' passed to individual solution functions to temporarily override the global
#' default.
#'
#' @param ... Default options to override
#'
#' @return A named list of the options.
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

