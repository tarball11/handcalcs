#' Critical values of Z
#'
#' Calculates the critical value of *z* for a given \eqn{\alpha} and the
#' direction of the hypothesis, along with the solution string in LaTeX format.
#'
#' Note that the returned critical value of z (`z_crit`) is rounded to the value
#' of `round_z` instead of the value of `round_interim` or `round_final` (see
#' [handcalcs_defaults()]).
#'
#' @param alpha Value of alpha (probability of a Type I error). Defaults to
#'   0.05.
#' @param direction Which tail of the distribution should the area equivalent to
#'   `alpha` be located: pos (z_obt > z_crit), neg (z_obt < z_crit), both
#'   (|z_obt| > |z_crit|).
#' @param ... Additional arguments to override default behaviors (see
#'   [handcalcs_defaults()]).
#'
#' @return Returns a list with the given values (`alpha`, `direction`), the
#'   final value (`z_crit`), and the solution string (`solution`).
#' @export
#'
#' @examples
#'
#' solve_z_crit(alpha = 0.01, direction = 'pos')
#' solve_z_crit(alpha = 0.01, direction = 'neg')
#' solve_z_crit(alpha = 0.01, direction = 'both')
#'
#' # In some cases, you may wish to change the default value for round_z:
#' solve_z_crit(alpha = 0.05, direction = 'pos', round_z = 3)
#'
solve_z_crit <- function(alpha = 0.05,
												 direction = c('pos', 'neg', 'both'),
												 ...) {
	# Check argument validity
	stopifnot(is.numeric(alpha) & dplyr::between(alpha, 0.00001, 0.5))
	stopifnot(!missing(direction))
	direction <- match.arg(direction)

	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts<- get_handcalcs_opts(...)

	# Use the appropriate equals sign for an aligned environment
	equals <- ifelse(opts$use_aligned, "&=", "=")

	# Get the critical value/solution string:
	if(direction == 'both') {
		z_crit <- rnd(qnorm(alpha/2, lower.tail=FALSE), opts$round_z)

		solution <- glue_solution(
			"\\alpha_{\\text{two-tailed}} <<equals>> <<alpha>>",
			"z_{crit} <<equals>> \\mathbf{\\pm<<z_crit>>}",
			# Print value of 'z' to the precision of opts$round_z unless round_to is
			# set to 'sigfigs', in which case just present the final rounded value as is.
			z_crit = ifelse(opts$round_to == 'sigfigs', z_crit, fmt(z_crit, opts$round_z)))

	} else {
		z_crit <- qnorm(alpha, lower.tail = ifelse(direction == 'pos', FALSE, TRUE))
		z_crit <- rnd(z_crit, opts$round_z)

		solution <- glue_solution(
			"\\alpha_{\\text{one-tailed}} <<equals>> <<alpha>>",
			"z_{crit} <<equals>> \\mathbf{<<z_crit>>}",
			# Print value of 'z' to the precision of opts$round_z unless round_to is
			# set to 'sigfigs', in which case just present the final rounded value as is.
			z_crit = ifelse(opts$round_to == 'sigfigs', z_crit, fmt(z_crit, opts$round_z)))
	}

	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	# Return list containing both values and solution string
	list(alpha = alpha,
			 direction = direction,
			 z_crit = z_crit,
			 solution = solution)

}
