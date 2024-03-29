#' Standard Error of the Mean (Known Population Standard Deviation)
#'
#' Calculates and provides solutions for the standard error of the mean (aka
#' standard deviation of the sampling distribution of the mean) when the
#' population standard deviation (\eqn{\sigma}) is known (\eqn{\sigma_{M} = \sigma
#' / \sqrt (n)}).
#'
#' @param sigma Numeric scalar. Population standard deviation.
#' @param n Numeric scalar. Sample size.
#' @param ... Additional arguments to override default behaviors (see
#'   [handcalcs_defaults()]
#'
#' @return `solve_sigma_M()` returns a list with the provided values (`sigma`,
#'   `n`), the interim calculations (`sqrt_n`), the final value (`sigma_M`), the
#'   solution string (`solution`), and the bare formula (`formula`) in LaTeX
#'   format. `sigma_M_formula()` returns just the bare formula in LaTeX format
#'   as a character string.

#' @export
#'
#' @examples
#' solve_sigma_M(sigma = 5, n = 25)
#' solve_sigma_M(sigma = 5, n = 50)
#' solve_sigma_M(sigma = 5, n = 100)
#'
#' # If you just want the bare formula as a string, use a formula function():
#' sigma_M_formula()
#'
solve_sigma_M<- function(sigma,
												 n,
												 ...) {

	# Check argument validity:
	stopifnot(is.numeric(sigma), is.numeric(n))
	stopifnot(length(sigma) == 1, length(n) == 1)
	stopifnot(sigma > 0, n > 0)

	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts<- get_handcalcs_opts(...)

	# Use the appropriate equals sign for an aligned environment
	equals <- ifelse(opts$use_aligned, "&=", "=")

	# Round initial values
	sigma <- rnd(sigma, opts$round_interim)

	# Calculate sigma_M:
	sqrt_n <- rnd(sqrt(n), opts$round_interim)
	sigma_M <- rnd((sigma/sqrt_n), opts$round_final)

	# Get base formula without LaTeX math/aligned blocks
	sigma_M.f <- sigma_M_formula(use_aligned = opts$use_aligned,
															 add_math = FALSE,
															 add_aligned = FALSE)

	# Create the solution string, with rounded values (minimally displayed)
	solution <- glue_solution(
		sigma_M.f,
		"<<equals>> \\frac{<<sigma>>}{\\sqrt{<<n>>}} = \\frac{<<sigma>>}{<<sqrt_n>>} = \\mathbf{<<sigma_M>>}",
		sqrt_n = fmt(sqrt_n, get_digits(sqrt_n, opts$round_interim)),
		# Round based on the final calculated value unless round_to is set to
		# 'sigfigs', in which case just present the final rounded value as is.
		sigma_M = ifelse(opts$round_to == 'sigfigs',
										 sigma_M,
										 fmt(sigma_M, get_digits(sigma_M, opts$round_final))))

	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	# Return list containing both values and solution string
	list(sigma = sigma,
			 n = n,
			 sqrt_n = sqrt_n,
			 sigma_M = sigma_M,
			 solution = solution,
			 formula = sigma_M.f)
}

#' @rdname solve_sigma_M
#'
#' @export
#'
sigma_M_formula <- function(...) {

	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts<- get_handcalcs_opts(...)

	# Use the appropriate equals sign for an aligned environment
	equals <- ifelse(opts$use_aligned, "&=", "=")

	# Create the formula string:
	solution<- lglue("\\sigma_{M} <<equals>> \\frac{\\sigma}{\\sqrt{n}}")

	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	return(solution)
}


#' One-sample *z*-test
#'
#' Calculates and provides solutions for the one-sample *z*-test (i.e.,
#' `z_obt`), comparing an obtained sample mean against a population with known
#' parameters (\eqn{\mu}, \eqn{\sigma}).
#'
#' Note that `z_obt` values are rounded to the value of `round_z` instead of the
#' value of `round_final` (see [handcalcs_defaults()]).
#'
#' @param M Numeric scalar. Sample mean.
#' @param mu Numeric scalar. Population mean.
#' @param sigma Numeric scalar. Population standard deviation. Required if
#'   sigma_M is not provided.
#' @param n Numeric scalar. Sample size. Required if sigma_M is not provided.
#' @param sigma_M Numeric scalar. Standard error of the mean. If not provided,
#'   will be calculated from `sigma` and `n` (using [solve_sigma_M()]) and
#'   included in the solution string.
#' @param ... Additional arguments to override default behaviors (see
#'   [handcalcs_defaults()]
#'
#' @return `solve_z_one_sample()` returns a list with the provided values (`M`, `mu`,
#'   `sigma`, `n`), the interim calculations (`sigma_M`, `M_diff`), the final
#'   value (`z_obt`), the solution string (`solution`), and the bare formula
#'   (`formula`) in LaTeX format. `z_one_sample_formula()` returns just the bare
#'   formula in LaTeX format as a character string.
#' @export
#'
#' @examples
#' solve_z_one_sample(M = 6, mu = 5, sigma = 2, n = 100)
#'
#' # If you just want the formula:
#' z_one_sample_formula()
#'
solve_z_one_sample <- function(M,
															 mu,
															 sigma,
															 n,
															 sigma_M,
															 ...) {

	stopifnot(is.numeric(M), is.numeric(mu))
	stopifnot(length(M) == 1, length(mu) == 1)

	if(missing(sigma_M) & (missing(sigma) | missing(n))) {
		stop('Must supply either sigma_M or sigma and n')
	}

	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts<- get_handcalcs_opts(...)

	# Use the appropriate equals sign for an aligned environment
	equals <- ifelse(opts$use_aligned, "&=", "=")

	# Calculate sigma_M from sigma and n
	if(missing(sigma_M)) {
		stopifnot(is.numeric(sigma), is.numeric(n))
		stopifnot(length(sigma) == 1, length(n) == 1)
		stopifnot(sigma > 0, n > 0)
		stopifnot(round(n) == n)

		sigma_M.lst <- solve_sigma_M(sigma = sigma,
																 n = n,
																 round_interim = opts$round_interim,
																 round_final = opts$round_interim,
																 add_math = FALSE,
																 add_aligned = FALSE,
																 use_aligned = opts$use_aligned)

		sigma_M <- sigma_M.lst$sigma_M
		sigma_M.solution <- sigma_M.lst$solution
	} else {
		sigma_M.solution <- NULL
	}

	stopifnot(is.numeric(sigma_M), length(sigma_M) == 1, sigma_M > 0)

	# Calculate z_obt and p:
	M_diff <- rnd(M - mu, opts$round_interim)
	z_obt <- rnd(M_diff/sigma_M, opts$round_z)

	# Get base formula without LaTeX math/aligned blocks
	z_obt.f <- z_one_sample_formula(use_aligned = opts$use_aligned,
																	add_math = FALSE,
																	add_aligned = FALSE)

	# Prepend the solution for sigma_M if calculating from components
	z_obt.solution <- ifelse(!is.null(sigma_M.solution),
													 paste(sigma_M.solution, ' \\\\ ', z_obt.f),
													 z_obt.f)

	solution <- glue_solution(
		z_obt.solution,
		# Put negative values of mu in brackets
		"<<equals>> \\frac{<<M>> - <<(mu)>>}{<<sigma_M>>} = \\frac{<<M_diff>>}{<<sigma_M>>} = \\mathbf{<<z_obt>>}",
		M_diff = fmt(M_diff, get_digits(M_diff, opts$round_interim)),
		sigma_M = fmt(sigma_M, get_digits(sigma_M, opts$round_interim)),
		# Print values of 'z_obt' to the precision of opts$round_z unless round_to is
		# set to 'sigfigs', in which case just present the final rounded value as is.
		z_obt = ifelse(opts$round_to == 'sigfigs', z_obt, fmt(z_obt, opts$round_z)))

	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	# Return list containing both values and solution string
	list(M = M,
			 mu = mu,
			 sigma = if(missing(sigma)) NULL else sigma,
			 n = if(missing(n)) NULL else n,
			 sigma_M = sigma_M,
			 M_diff = M_diff,
			 z_obt = z_obt,
			 solution = solution,
			 formula = z_obt.f)
}

#' @rdname solve_z_one_sample
#'
#' @export
#'
z_one_sample_formula <- function(...) {

	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts<- get_handcalcs_opts(...)

	# Use the appropriate equals sign for an aligned environment
	equals <- ifelse(opts$use_aligned, "&=", "=")

	# Create the formula string:
	solution<- lglue("z_{\\text{obt}} <<equals>> \\frac{M - \\mu}{\\sigma_{M}}")

	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	return(solution)
}
