#' Calculate *p* values from the standard normal (*z*) distribution.
#'
#' Note that values of `z` are rounded to the value of `round_z` instead of the
#' value of `round_interim` or `round_final` (see [handcalcs_defaults()]).
#'
#' @param z Numeric scalar. Value of z.
#' @param direction For which tail(s) of the distribution should the p-value be
#'   located: pos (area under the curve above `z`), neg (area under the curve
#'   below `z`), both (area under the curve above +`z` and below -`z`).
#' @param ... Additional arguments to override default behaviors (see
#'   [handcalcs_defaults()]
#'
#' @return Returns a list with the provided values (`z`, `direction`), the final
#'   value (`p`), and the solution string (`solution`) in LaTeX format.
#'
#' @export
#'
#' @examples
#' solve_z_to_p(1.96, direction = 'both')
#'
#' # In some cases, you may need to change the default value of round_z
#' solve_z_to_p(1.645, direction = 'pos', round_z = 3)
#'
solve_z_to_p <- function(z,
												 direction = c('pos', 'neg', 'both'),
												 ...) {

	stopifnot(is.numeric(z), length(z) == 1)
	stopifnot(!missing(direction))
	direction <- match.arg(direction)

	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts<- get_handcalcs_opts(...)

	# Use the appropriate equals sign for an aligned environment
	equals <- ifelse(opts$use_aligned, "&=", "=")

	# Round the initial value
	z <- rnd(z, opts$round_z)

	# Get the p-value/solution string:
	if(direction == 'pos') {
		p <- rnd(pnorm(q = z, lower.tail=FALSE), opts$round_final)
		sol.str <- "p(z > <<z>>) <<equals>> \\mathbf{<<p>>}"
	} else if(direction == 'neg') {
		p <- rnd(pnorm(q = z, lower.tail=FALSE), opts$round_final)
		sol.str <- "p(z < <<z>>) <<equals>> \\mathbf{<<p>>}"
	} else if(direction == 'both') {
		p <- rnd(pnorm(q = z, lower.tail=FALSE), opts$round_final)
		sol.str <- "p(z > <<abs(z)>>  & z < <<-abs(z)>>) <<equals>> \\mathbf{<<p>>}"
	}

	solution <- glue_solution(
		sol.str,
		# Print values to the appropriate precision unless round_to is set to
		# 'sigfigs', in which case just present the final rounded value as is.
		z = ifelse(opts$round_to == 'sigfigs', z, fmt(z, opts$round_z)),
		p = ifelse(opts$round_to == 'sigfigs', p, fmt(p, opts$round_final)))


	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	# Return list containing both values and solution string
	list(z = z,
			 direction = direction,
			 p = p,
			 solution = solution)
}

#' Calculate *p* values from the *t* distribution.
#'
#' @param t Numeric scalar. Value of t.
#' @param df Numeric scalar. Degrees of freedom.
#' @param direction For which tail(s) of the distribution should the p-value be
#'   located: pos (area under the curve above `t`), neg (area under the curve
#'   below `t`), both (area under the curve above +`t` and below -`t`).
#' @param ... Additional arguments to override default behaviors (see
#'   [handcalcs_defaults()]
#'
#' @return Returns a list with the provided values (`t`, `df`, `direction`), the
#'   final value (`p`), and the solution string (`solution`) in LaTeX format.
#'
#' @export
#'
#' @examples
#' solve_t_to_p(1.96, direction = 'both')
#'
#' # In some cases, you may need to change the default value of round_final
#' solve_t_to_p(1.645, direction = 'pos', round_final = 3)
#'
solve_t_to_p <- function(t,
												 df,
												 direction = c('pos', 'neg', 'both'),
												 ...) {

	stopifnot(is.numeric(t), length(t) == 1)
	stopifnot(is.numeric(df) & df >= 1)
	stopifnot(!missing(direction))
	direction <- match.arg(direction)

	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts<- get_handcalcs_opts(...)

	# Use the appropriate equals sign for an aligned environment
	equals <- ifelse(opts$use_aligned, "&=", "=")

	# Round the initial value
	t <- rnd(t, opts$round_interim)

	# Get the p-value/solution string:
	if(direction == 'pos') {
		p <- rnd(pt(q = t, df = df, lower.tail=FALSE), opts$round_final)
		sol.str <- "p(t > <<t>>) <<equals>> \\mathbf{<<p>>}"
	} else if(direction == 'neg') {
		p <- rnd(pt(q = t, df = df, lower.tail=FALSE), opts$round_final)
		sol.str <- "p(t < <<t>>) <<equals>> \\mathbf{<<p>>}"
	} else if(direction == 'both') {
		p <- rnd(pt(q = t, df = df, lower.tail=FALSE), opts$round_final)
		sol.str <- "p(t > <<abs(t)>>  & t < <<-abs(t)>>) <<equals>> \\mathbf{<<p>>}"
	}

	solution <- glue_solution(
		sol.str,
		# Print values to the appropriate precision unless round_to is set to
		# 'sigfigs', in which case just present the final rounded value as is.
		t = ifelse(opts$round_to == 'sigfigs', t, fmt(t, opts$round_interim)),
		p = ifelse(opts$round_to == 'sigfigs', p, fmt(p, opts$round_final)))

	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	# Return list containing both values and solution string
	list(t = t,
			 df = df,
			 direction = direction,
			 p = p,
			 solution = solution)
}
