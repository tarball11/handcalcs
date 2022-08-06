#' Standard Error of the Mean (Unknown Population Standard Deviation)
#'
#' Calculates and provides solutions for the standard error of the mean (aka
#' standard deviation of the sampling distribution of the mean) using the sample
#' standard deviation (\eqn{s_{M} = s / \sqrt{n}}).
#'
#' @param SD Numeric scalar. Sample standard deviation.
#' @param n Numeric scalar. Sample size.
#' @param ... Additional arguments to override default behaviors (see
#'   [handcalcs_defaults()]
#'
#' @return `solve_s_M()` returns a list with the provided values (`SD`, `n`),
#'   the interim calculations (`sqrt_n`), the final value (`s_M`), the solution
#'   string (`solution`), and the bare formula (`formula`) in LaTeX format.
#'   `s_M_formula()` returns just the bare formula in LaTeX format as a
#'   character string.

#' @export
#'
#' @examples
#' solve_s_M(SD = 5, n = 25)
#' solve_s_M(SD = 5, n = 50)
#' solve_s_M(SD = 5, n = 100)
#'
#' # If you just want the bare formula as a string, use a formula function():
#' s_M_formula()
#'
solve_s_M<- function(SD,
										 n,
										 ...) {

	# Check argument validity:
	stopifnot(is.numeric(SD), is.numeric(n))
	stopifnot(length(SD) == 1, length(n) == 1)
	stopifnot(SD > 0, n > 0)

	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts<- get_handcalcs_opts(...)

	# Use the appropriate equals sign for an aligned environment
	equals <- ifelse(opts$use_aligned, "&=", "=")

	# Round initial values
	SD <- rnd(SD, opts$round_interim)

	# Calculate s_M:
	sqrt_n <- rnd(sqrt(n), opts$round_interim)
	s_M <- rnd((SD/sqrt_n), opts$round_final)

	# Get base formula without LaTeX math/aligned blocks
	s_M.f <- s_M_formula(use_aligned = opts$use_aligned,
											 add_math = FALSE,
											 add_aligned = FALSE)

	# Create the solution string, with rounded values (minimally displayed)
	solution <- glue_solution(
		s_M.f,
		"<<equals>> \\frac{<<SD>>}{\\sqrt{<<n>>}} = \\frac{<<SD>>}{<<sqrt_n>>} = \\mathbf{<<s_M>>}",
		sqrt_n = fmt(sqrt_n, get_digits(sqrt_n, opts$round_interim)),
		# Round based on the final calculated value unless round_to is set to
		# 'sigfigs', in which case just present the final rounded value as is.
		s_M = ifelse(opts$round_to == 'sigfigs',
								 s_M,
								 fmt(s_M, get_digits(s_M, opts$round_final))))

	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	# Return list containing both values and solution string
	list(SD = SD,
			 s = SD,
			 n = n,
			 sqrt_n = sqrt_n,
			 s_M = s_M,
			 solution = solution,
			 formula = s_M.f)
}

#' @rdname solve_s_M
#'
#' @export
#'
s_M_formula <- function(...) {

	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts<- get_handcalcs_opts(...)

	# Use the appropriate equals sign for an aligned environment
	equals <- ifelse(opts$use_aligned, "&=", "=")

	# Create the formula string:
	solution<- lglue("s_{M} <<equals>> \\frac{s}{\\sqrt{n}}")

	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	return(solution)
}


#'One-sample *t*-test
#'
#'Calculates and provides solutions for the one-sample *t*-test (i.e., `t_obt`),
#'comparing an obtained sample mean against the population mean (\eqn{\mu}) when
#'the population standard deviation (\eqn{\sigma}) must be estimated using the
#'sample standard deviation (\eqn{s}, aka `SD`).
#'
#' Note that `t_obt` values are rounded to the value of `round_t` instead of the
#' value of `round_final` (see [handcalcs_defaults()]).
#'
#'@param M Numeric scalar. Sample mean.
#'@param mu Numeric scalar. Population mean.
#'@param SD Numeric scalar. Sample standard deviation. Required if s_M is not
#'  provided.
#'@param n Numeric scalar. Sample size. Required if s_M is not provided.
#'@param s_M Numeric scalar. Standard error of the mean. If not provided, will
#'  be calculated from `SD` and `n` (using [solve_s_M()]) and included in the
#'  solution string.
#'@param ... Additional arguments to override default behaviors (see
#'  [handcalcs_defaults()]
#'
#'@return `solve_t_one_sample()` returns a list with the provided values (`M`,
#'  `mu`, `SD`, `n`), the interim calculations (`df`, `s_M`, `M_diff`), the
#'  final value (`t_obt`), the solution string (`solution`), and the bare
#'  formula (`formula`) in LaTeX format. `t_one_sample_formula()` returns just
#'  the bare formula in LaTeX format as a character string.
#'@export
#'
#' @examples
#' solve_t_one_sample(M = 6, mu = 5, SD = 2, n = 100)
#'
#' # If you just want the formula:
#' t_one_sample_formula()
#'
solve_t_one_sample <- function(M,
															 mu,
															 SD,
															 n,
															 s_M,
															 ...) {

	stopifnot(is.numeric(M), is.numeric(mu))
	stopifnot(length(M) == 1, length(mu) == 1)

	if(missing(s_M) & (missing(SD) | missing(n))) {
		stop('Must supply either s_M or SD and n')
	}

	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts<- get_handcalcs_opts(...)

	# Use the appropriate equals sign for an aligned environment
	equals <- ifelse(opts$use_aligned, "&=", "=")

	# Calculate s_M from SD and n
	if(missing(s_M)) {
		stopifnot(is.numeric(SD), is.numeric(n))
		stopifnot(length(SD) == 1, length(n) == 1)
		stopifnot(SD > 0, n > 0)
		stopifnot(round(n) == n)

		s_M.lst <- solve_s_M(SD = SD,
												 n = n,
												 round_interim = opts$round_interim,
												 round_final = opts$round_interim,
												 add_math = FALSE,
												 add_aligned = FALSE,
												 use_aligned = opts$use_aligned)

		s_M <- s_M.lst$s_M
		s_M.solution <- s_M.lst$solution
	} else {
		s_M.solution <- NULL
	}

	stopifnot(is.numeric(s_M), length(s_M) == 1, s_M > 0)

	# Calculate z_obt and p:
	M_diff <- rnd(M - mu, opts$round_interim)
	t_obt <- rnd(M_diff/s_M, opts$round_t)

	# Get base formula without LaTeX math/aligned blocks
	t_obt.f <- t_one_sample_formula(use_aligned = opts$use_aligned,
																	add_math = FALSE,
																	add_aligned = FALSE)

	# Prepend the solution for s_M if calculating from components
	t_obt.solution <- ifelse(!is.null(s_M.solution),
													 paste(s_M.solution, ' \\\\ ', t_obt.f),
													 t_obt.f)

	solution <- glue_solution(
		t_obt.solution,
		"<<equals>> \\frac{<<M>> - <<mu>>}{<<s_M>>} = \\frac{<<M_diff>>}{<<s_M>>} = \\mathbf{<<t_obt>>}",
		M_diff = fmt(M_diff, get_digits(M_diff, opts$round_interim)),
		s_M = fmt(s_M, get_digits(s_M, opts$round_interim)),
		# Print values of 't_obt' to the precision of opts$round_final unless round_to is
		# set to 'sigfigs', in which case just present the final rounded value as is.
		t_obt = ifelse(opts$round_to == 'sigfigs', t_obt, fmt(t_obt, opts$round_t)))

	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	# Return list containing both values and solution string
	list(M = M,
			 mu = mu,
			 SD = if(missing(SD)) NULL else SD,
			 s = if(missing(SD)) NULL else SD,
			 n = if(missing(n)) NULL else n,
			 s_M = s_M,
			 M_diff = M_diff,
			 t_obt = t_obt,
			 solution = solution,
			 formula = t_obt.f)
}

#' @rdname solve_t_one_sample
#'
#' @export
#'
t_one_sample_formula <- function(...) {

	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts<- get_handcalcs_opts(...)

	# Use the appropriate equals sign for an aligned environment
	equals <- ifelse(opts$use_aligned, "&=", "=")

	# Create the formula string:
	solution<- lglue("t_{\\text{obt}} <<equals>> \\frac{M - \\mu}{s_{M}}")

	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	return(solution)
}
