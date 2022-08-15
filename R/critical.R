#' Critical values of *z*
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
			"z_{\\text{crit}} <<equals>> \\mathbf{\\pm<<z_crit>>}",
			# Print value of 'z' to the precision of opts$round_z unless round_to is
			# set to 'sigfigs', in which case just present the final rounded value as is.
			z_crit = ifelse(opts$round_to == 'sigfigs', z_crit, fmt(z_crit, opts$round_z)))

	} else {
		z_crit <- qnorm(alpha, lower.tail = ifelse(direction == 'pos', FALSE, TRUE))
		z_crit <- rnd(z_crit, opts$round_z)

		solution <- glue_solution(
			"\\alpha_{\\text{one-tailed}} <<equals>> <<alpha>>",
			"z_{\\text{crit}} <<equals>> \\mathbf{<<z_crit>>}",
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


#' Critical values of *t*
#'
#' Calculates the critical value of *t* for a given \eqn{\alpha}, degrees of
#' freedom (`df`), and the direction of the hypothesis, along with the solution
#' string in LaTeX format.
#'
#' Note that the returned critical value of *t* (`t_crit`) is rounded to the value
#' of `round_t` instead of the value of `round_interim` or `round_final` (see
#' [handcalcs_defaults()]).
#'
#' @param alpha Numeric scalar. Value of alpha (probability of a Type I error).
#'   Defaults to 0.05.
#' @param df Numeric scalar. Degrees of freedom.
#' @param direction Which tail of the distribution should the area equivalent to
#'   `alpha` be located: pos (t_obt > t_crit), neg (t_obt < t_crit), both
#'   (|t_obt| > |t_crit|).
#' @param ... Additional arguments to override default behaviors (see
#'   [handcalcs_defaults()]).
#'
#' @return Returns a list with the given values (`alpha`, `df`, `direction`),
#'   the final value (`t_crit`), and the solution string (`solution`).
#' @export
#'
#' @examples
#'
#' solve_t_crit(alpha = 0.01, df = 100, direction = 'pos')
#' solve_t_crit(alpha = 0.01, df = 100, direction = 'neg')
#' solve_t_crit(alpha = 0.01, df = 100, direction = 'both')
#'
#' # In some cases, you may wish to change the default value for round_final:
#' solve_t_crit(alpha = 0.05, df = 100, direction = 'pos', round_final = 3)
#'
solve_t_crit <- function(alpha = 0.05,
												 df,
												 direction = c('pos', 'neg', 'both'),
												 ...) {
	# Check argument validity
	stopifnot(is.numeric(alpha) & dplyr::between(alpha, 0.00001, 0.5))
	stopifnot(is.numeric(df) & df >= 1)
	stopifnot(!missing(direction))
	direction <- match.arg(direction)

	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts<- get_handcalcs_opts(...)

	# Use the appropriate equals sign for an aligned environment
	equals <- ifelse(opts$use_aligned, "&=", "=")

	# Get the critical value/solution string:
	if(direction == 'both') {
		t_crit <- rnd(qt(alpha/2, df = df, lower.tail=FALSE), opts$round_t)

		solution <- glue_solution(
			"\\alpha_{\\text{two-tailed}} <<equals>> <<alpha>>",
			"t_{\\text{crit}(\\mathit{df} = <<df>>)} <<equals>> \\mathbf{\\pm<<t_crit>>}",
			# Print value of 't' to the precision of opts$round_final unless round_to is
			# set to 'sigfigs', in which case just present the final rounded value as is.
			t_crit = ifelse(opts$round_to == 'sigfigs', t_crit, fmt(t_crit, opts$round_t)))

	} else {
		t_crit <- qt(alpha, df = df, lower.tail = ifelse(direction == 'pos', FALSE, TRUE))
		t_crit <- rnd(t_crit, opts$round_t)

		solution <- glue_solution(
			"\\alpha_{\\text{one-tailed}} <<equals>> <<alpha>>",
			"t_{\\text{crit}(\\mathit{df} = <<df>>)} <<equals>> \\mathbf{<<t_crit>>}",
			# Print value of 't' to the precision of opts$round_final unless round_to is
			# set to 'sigfigs', in which case just present the final rounded value as is.
			t_crit = ifelse(opts$round_to == 'sigfigs', t_crit, fmt(t_crit, opts$round_t)))
	}

	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	# Return list containing both values and solution string
	list(alpha = alpha,
			 df = df,
			 direction = direction,
			 t_crit = t_crit,
			 solution = solution)
}
