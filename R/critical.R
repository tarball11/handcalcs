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
#' # In some cases, you may wish to change the default value for round_t:
#' solve_t_crit(alpha = 0.05, df = 100, direction = 'pos', round_t = 3)
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


#' Critical values of *Q* (Studentized Range Statistic)
#'
#' Calculates the critical value of *Q* (the Studentized Range Statistic for
#' multiple comparisons) for a given \eqn{\alpha}, degrees of freedom for error
#' (`df_error`) from the ANOVA, and number of groups being compared (`k`), along
#' with the solution string in LaTeX format.
#'
#' This is used when conducting *post hoc* tests after a significant ANOVA,
#' considering each mean difference, taking into consideration the total number
#' of comparisons to maintain an overall Type I error rate (`alpha`). The
#' critical value would be compared against the value of Q_obt from the
#' `[solve_Q_obt()]` function for each comparison.
#'
#'
#' @param alpha Numeric scalar. Value of alpha (probability of a Type I error).
#'   Defaults to 0.05.
#' @param df_error Numeric scalar. Degrees of freedom.
#' @param k Numeric scalar. Number of groups in design.
#' @param ... Additional arguments to override default behaviors (see
#'   [handcalcs_defaults()]).
#'
#' @return Returns a list with the given values (`alpha`, `df`, `k`), the final
#'   value (`Q_crit`), and the solution string (`solution`).
#' @export
#'
#' @examples
#'
#' solve_Q_crit(alpha = 0.01, df_error = 100, k = 3)
#' solve_Q_crit(alpha = 0.01, df_error = 100, k = 4)
#' solve_Q_crit(alpha = 0.01, df_error = 100, k = 5)
#'
#' # In some cases, you may wish to change the default value for round_final:
#' solve_Q_crit(alpha = 0.05, df = 100, k = 3, round_final = 4)
#'
solve_Q_crit <- function(alpha = 0.05,
												 df_error,
												 k,
												 ...) {
	# Check argument validity
	stopifnot(is.numeric(alpha) & dplyr::between(alpha, 0.00001, 0.5))
	stopifnot(is.numeric(df_error) & df_error >= 1)
	stopifnot(is.numeric(k) & k >= 2)

	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts<- get_handcalcs_opts(...)

	# Use the appropriate equals sign for an aligned environment
	equals <- ifelse(opts$use_aligned, "&=", "=")

	# Get the critical value/solution string:
	Q_crit <- rnd(qtukey(alpha, nmeans = k, df = df_error, lower.tail = FALSE), opts$round_final)

	solution <- glue_solution(
		"\\alpha_{\\text{two-tailed}} <<equals>> <<alpha>>",
		"Q_{\\text{crit}(\\mathit{df_{\\text{error}} = <<df_error>>, k = <<k>>)} <<equals>> \\mathbf{<<Q_crit>>}",
		# Print value of 'Q' to the precision of opts$round_final unless round_to is
		# set to 'sigfigs', in which case just present the final rounded value as is.
		Q_crit = ifelse(opts$round_to == 'sigfigs', Q_crit, fmt(Q_crit, opts$round_final)))

	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	# Return list containing both values and solution string
	list(alpha = alpha,
			 df_error = df_error,
			 k = k,
			 Q_crit = Q_crit,
			 solution = solution)
}
