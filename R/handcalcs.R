#' handcalcs: A package for mimicking hand calculations and generating solutions
#' when teaching statistics
#'
#' The handcalcs package covers numerous basic statistical functions, including
#' measures of central tendency, variability, z-tests, t-tests, ANOVA,
#' regression, and correlation.
#'
#'
#' @docType package
#'
#' @importFrom magrittr %>%
#'
#' @name handcalcs
NULL


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
#' \item{\code{round_interim}}{Number of digits past the decimal to round
#' interim calculations. (Default = 4.)}
#'
#' \item{\code{round_final}}{Number of digits past the decimal to report final
#' result. (Default = 4.)}
#'
#' \item{\code{add_math}}{Enclose solution string in LaTeX math block (`$$`)? If
#' set to `FALSE`, must manually include the math block tags in order for the
#' solution to render properly. (Default = TRUE.)}
#'
#' \item{\code{add_aligned}}{Enclose solution in LaTeX aligned block
#' (`\begin{aligned}`)? If set to `FALSE`, allows for multiple solutions to be
#' set in the same aligned environment. (Default = TRUE.)}
#'
#' \item{\code{use_aligned}}{Use "&=" instead of "=" in solution string to
#' generate aligned equations. (Default = TRUE.)}
#'
#' }
#'
#'
#'
#' @return Named list of default values
#' @export
#'
#' @examples
#' handcalcs_defaults()
#'
handcalcs_defaults<- function() {
	list(
		round_interim = 4,
		round_final = 4,
		add_math = TRUE,
		add_aligned = TRUE,
		use_aligned = TRUE
	)
}

# Checks to see if a value is set in the list. If so, returns that value. If
# not, returns current global option. Note: does not confirm correct class/type
# for values!
check_defaults<- function(x, l) {
	op<- getOption('handcalcs')
	stopifnot(x %in% names(op))

	val <- ifelse(!is.null(l[[x]]), l[[x]], op[[x]])
	return(val)
}
