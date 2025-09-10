#' Calculate *p* values from the standard normal (*z*) distribution.
#'
#' Note that values of `z_val` are rounded to the value of `round_z` instead of
#' the value of `round_interim` or `round_final` (see [handcalcs_defaults()]).
#'
#' @param z_val Numeric scalar. Value of z statistic.
#' @param direction For which tail(s) of the distribution should the p-value be
#'   located: pos (area under the curve above `z_val`), neg (area under the
#'   curve below `z_val`), both (area under the curve above +`z_val` and below
#'   -`z_val`).
#' @param ... Additional arguments to override default behaviors (see
#'   [handcalcs_defaults()]
#'
#' @return Returns a list with the provided values (`z_val`, `direction`), the
#'   final value (`p_val`), and the solution string (`solution`) in LaTeX
#'   format. (`z` and `p` are also provided for ease of use and backwards
#'   compatibility.)
#'
#' @export
#'
#' @examples
#' solve_z_to_p(z_val = 1.96, direction = 'both')
#'
#' # In some cases, you may need to change the default value of round_z
#' solve_z_to_p(z_val = 1.645, direction = 'pos', round_z = 3)
#'
solve_z_to_p <- function(z_val,
												 direction = c('pos', 'neg', 'both'),
												 ...) {

	stopifnot(is.numeric(z_val), length(z_val) == 1)
	stopifnot(!missing(direction))
	direction <- match.arg(direction)

	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts <- get_handcalcs_opts(...)

	# Use the appropriate equals sign for an aligned environment
	equals <- ifelse(opts$use_aligned, "&=", "=")

	# Round the initial value
	z_val <- rnd(z_val, opts$round_z)
	z_val_abs <- abs(z_val)

	# Get the p-value/solution string:
	if(direction == 'pos') {
		p_val <- rnd(pnorm(q = z_val, lower.tail=FALSE), opts$round_final)
		sol.str <- "p(Z > <<z_val>>) <<equals>> \\mathbf{<<p_val>>}"
	} else if(direction == 'neg') {
		p_val <- rnd(pnorm(q = z_val, lower.tail=TRUE), opts$round_final)
		sol.str <- "p(Z < <<z_val>>) <<equals>> \\mathbf{<<p_val>>}"
	} else if(direction == 'both') {
		p_val <- rnd(pnorm(q = abs(z_val), lower.tail=FALSE)*2, opts$round_final)
		sol.str <- "p(|Z| > <<z_val_abs>>) <<equals>> \\mathbf{<<p_val>>}"
	}

	solution <- glue_solution(
		sol.str,
		# Print values to the appropriate precision unless round_to is set to
		# 'sigfigs', in which case just present the final rounded value as is.
		z_val = ifelse(opts$round_to == 'sigfigs', z_val, fmt(z_val, opts$round_z)),
		p_val = ifelse(opts$round_to == 'sigfigs', p_val, fmt(p_val, opts$round_final)))


	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	# Return list containing both values and solution string
	list(z_val = z_val,
			 z = z_val,
			 direction = direction,
			 p_val = p_val,
			 p = p_val,
			 solution = solution)
}

#' Calculate *p* values from the *t* distribution.
#'
#' @param t_val Numeric scalar. Value of t statistic.
#' @param df Numeric scalar. Degrees of freedom.
#' @param direction For which tail(s) of the distribution should the p-value be
#'   located: pos (area under the curve above `t_val`), neg (area under the
#'   curve below `t_val`), both (area under the curve above +`t_val` and below
#'   -`t_val`).
#' @param ... Additional arguments to override default behaviors (see
#'   [handcalcs_defaults()]
#'
#' @return Returns a list with the provided values (`t_val`, `df`, `direction`),
#'   the final value (`p_val`), and the solution string (`solution`) in LaTeX
#'   format. (`t` and `p` are also provided for ease of use and backwards
#'   compatibility.)
#'
#' @export
#'
#' @examples
#' solve_t_to_p(t_val = 1.96, df = 100, direction = 'both')
#'
#' # In some cases, you may need to change the default value of round_final
#' solve_t_to_p(t_valu = 1.645, df = 100, direction = 'pos', round_final = 3)
#'
solve_t_to_p <- function(t_val,
												 df,
												 direction = c('pos', 'neg', 'both'),
												 ...) {

	stopifnot(is.numeric(t_val), length(t_val) == 1)
	stopifnot(is.numeric(df) & df >= 1)
	stopifnot(!missing(direction))
	direction <- match.arg(direction)

	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts<- get_handcalcs_opts(...)

	# Use the appropriate equals sign for an aligned environment
	equals <- ifelse(opts$use_aligned, "&=", "=")

	# Round the initial value
	t_val <- rnd(t_val, opts$round_interim)

	# Get the p-value/solution string:
	if(direction == 'pos') {
		p_val <- rnd(pt(q = t_val, df = df, lower.tail=FALSE), opts$round_final)
		sol.str <- "p(t > <<t_val>>) <<equals>> \\mathbf{<<p>>}"
	} else if(direction == 'neg') {
		p_val <- rnd(pt(q = t_val, df = df, lower.tail=FALSE), opts$round_final)
		sol.str <- "p(t < <<t_val>>) <<equals>> \\mathbf{<<p_val>>}"
	} else if(direction == 'both') {
		p_val <- rnd(pt(q = t_val, df = df, lower.tail=FALSE), opts$round_final)
		sol.str <- "p(t > <<abs(t_val)>>  & t < <<-abs(t_val)>>) <<equals>> \\mathbf{<<p_val>>}"
	}

	solution <- glue_solution(
		sol.str,
		# Print values to the appropriate precision unless round_to is set to
		# 'sigfigs', in which case just present the final rounded value as is.
		t_val = ifelse(opts$round_to == 'sigfigs', t_val, fmt(t_val, opts$round_interim)),
		p_val = ifelse(opts$round_to == 'sigfigs', p_val, fmt(p_val, opts$round_final)))

	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	# Return list containing both values and solution string
	list(t_val = t_val,
			 t = t_val,
			 df = df,
			 direction = direction,
			 p_val = p_val,
			 p = p_val,
			 solution = solution)
}
