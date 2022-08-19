#' Standard Error of the Mean Difference Score
#'
#' Calculates and provides solutions for the standard error of the mean
#' difference score using the standard deviation of the difference scores
#' (\eqn{s_{M_{D}} = s_{D} / \sqrt{n}}).
#'
#' @param s_D,SD_D Numeric scalar. Standard deviation of difference scores. May
#'   be named either `s_D` or `SD_D`.
#' @param n Numeric scalar. Sample size.
#' @param ... Additional arguments to override default behaviors (see
#'   [handcalcs_defaults()]
#'
#' @return `solve_s_M_D()` returns a list with the provided values (`s_D`,
#'   `SD_D`, `n`), the interim calculations (`sqrt_n`), the final value
#'   (`s_M_D`), the solution string (`solution`), and the bare formula
#'   (`formula`) in LaTeX format. `s_M_D_formula()` returns just the bare
#'   formula in LaTeX format as a character string.

#' @export
#'
#' @examples
#' # Starting with raw scores, calculate the difference scores first, then the SD:
#' x_1 <- rnorm(10)
#' x_2 <- rnorm(10)
#' x_D <- solve_diff_score(x_1 = x_1, x_2 = x_2)$x_D
#' s_D <- solve_sd_diff(x_D = x_D)$s_D
#' solve_s_M_D(s_D = s_D, n = 10)
#'
#' solve_s_M_D(s_D = 5, n = 25)
#' solve_s_M_D(s_D = 5, n = 50)
#' solve_s_M_D(s_D = 5, n = 100)
#'
#' # If you just want the bare formula as a string, use a formula function():
#' s_M_D_formula()
#'
solve_s_M_D<- function(s_D = NULL,
											 SD_D = NULL,
											 n,
											 ...) {
	# If SD_D is supplied instead of s_D, set s accordingly
	if(missing(s_D) & !missing(SD_D)) s_D <- SD_D

	# Check argument validity:
	stopifnot(is.numeric(s_D), is.numeric(n))
	stopifnot(length(s_D) == 1, length(n) == 1)
	stopifnot(s_D > 0, n > 0)

	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts<- get_handcalcs_opts(...)

	# Use the appropriate equals sign for an aligned environment
	equals <- ifelse(opts$use_aligned, "&=", "=")

	# Round initial values
	s_D <- rnd(s_D, opts$round_interim)

	# Calculate s_M:
	sqrt_n <- rnd(sqrt(n), opts$round_interim)
	s_M_D <- rnd((s_D/sqrt_n), opts$round_final)

	# Get base formula without LaTeX math/aligned blocks
	s_M_D.f <- s_M_D_formula(use_aligned = opts$use_aligned,
													 add_math = FALSE,
													 add_aligned = FALSE)

	# Create the solution string, with rounded values (minimally displayed)
	solution <- glue_solution(
		s_M_D.f,
		"<<equals>> \\frac{<<s_D>>}{\\sqrt{<<n>>}} = \\frac{<<s_D>>}{<<sqrt_n>>} = \\mathbf{<<s_M_D>>}",
		sqrt_n = fmt(sqrt_n, get_digits(sqrt_n, opts$round_interim)),
		# Round based on the final calculated value unless round_to is set to
		# 'sigfigs', in which case just present the final rounded value as is.
		s_M_D = ifelse(opts$round_to == 'sigfigs',
									 s_M_D,
									 fmt(s_M_D, get_digits(s_M_D, opts$round_final))))

	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	# Return list containing both values and solution string
	list(s_D = SD_D,
			 SD_D = SD_D,
			 n = n,
			 sqrt_n = sqrt_n,
			 s_M_D = s_M_D,
			 solution = solution,
			 formula = s_M_D.f)
}

#' @rdname solve_s_M_D
#'
#' @export
#'
s_M_D_formula <- function(...) {

	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts<- get_handcalcs_opts(...)

	# Use the appropriate equals sign for an aligned environment
	equals <- ifelse(opts$use_aligned, "&=", "=")

	# Create the formula string:
	solution<- lglue("s_{M_{D}} <<equals>> \\frac{s_{D}}{\\sqrt{n}}")

	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	return(solution)
}


#' Paired-samples *t*-test
#'
#' Calculates and provides solutions for the paired-samples *t*-test (i.e.,
#' `t_obt`), comparing the calculated mean difference score (\eqn{M_D}) from two
#' related samples against the population mean difference score under the null
#' hypothesis (\eqn{\mu_{D}}, which is virtually always zero).
#'
#' The standard error of the mean difference (\eqn{s_{M_{D}}}) can either be
#' provided directly (as `s_M_D`), or calculated using the standard deviation of
#' the difference scores (`SD_D`) and sample size (`n`) (\eqn{s_{M_{D}} =
#' \frac{SD_{D}}{\sqrt{n}}}).
#'
#' Note that `t_obt` values are rounded to the value of `round_t` instead of the
#' value of `round_final` (see [handcalcs_defaults()]).
#'
#' Also note that, because this value is virtually always zero (the default), this
#' function does not follow the same argument order as some other functions to
#' avoid issues with unnamed arguments, though it is generally recommended to
#' always name the arguments.
#'
#' @param M_D Numeric scalar. Mean difference of the samples.
#' @param s_M_D Numeric scalar. Standard error of the mean difference. If not
#'   provided, will be calculated from `s_D` and `n` (using [solve_s_M_D()])
#'   and included in the solution string.
#' @param s_D,SD_D Numeric scalar. Standard deviation of the difference scores.
#'   Required if s_M_D is not provided. May be named either `s_D` or `SD_D`.
#' @param n Numeric scalar. Sample size. Required if s_M_D is not provided.
#' @param mu_D Numeric scalar. Population mean difference score under the null
#'   hypothesis (default = 0).
#' @param ... Additional arguments to override default behaviors (see
#'   [handcalcs_defaults()]
#'
#' @return `solve_t_paired_samples()` returns a list with the provided values
#'   (`M_D`, `mu_D`, `s_D`, `SD_D`, `n`), the interim calculations (`df`, `s_M_D`,
#'   `M_diff`), the final value (`t_obt`), the solution string (`solution`), and
#'   the bare formula (`formula`) in LaTeX format. `t_paired_samples_formula()`
#'   returns just the bare formula in LaTeX format as a character string.
#' @export
#'
#' @examples
#' solve_t_paired_samples(M_D = 2, s_M_D = 10)
#' solve_t_paired_samples(M_D = 2, SD_D = 10, n = 100)
#'
#' # If you just want the formula:
#' t_paired_samples_formula()
#'
solve_t_paired_samples <- function(M_D,
																	 s_M_D,
																	 s_D,
																	 SD_D,
																	 n,
																	 mu_D = 0,
																	 ...) {

	# If SD_D is supplied instead of s_D, set s accordingly
	if(missing(s_D) & !missing(SD_D)) s_D <- SD_D

	# Check argument validity:
	stopifnot(is.numeric(M_D), is.numeric(mu_D))
	stopifnot(length(M_D) == 1, length(mu_D) == 1)

	if(missing(s_M_D) & (missing(s_D) | missing(n))) {
		stop('Must supply either s_M_D or s_D (or SD_D) and n')
	}

	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts<- get_handcalcs_opts(...)

	# Use the appropriate equals sign for an aligned environment
	equals <- ifelse(opts$use_aligned, "&=", "=")

	# Calculate s_M from SD and n
	if(missing(s_M_D)) {
		stopifnot(is.numeric(s_D), is.numeric(n))
		stopifnot(length(s_D) == 1, length(n) == 1)
		stopifnot(s_D > 0, n > 0)
		stopifnot(round(n) == n)

		s_M_D.lst <- solve_s_M_D(s_D = s_D,
														 n = n,
														 round_interim = opts$round_interim,
														 round_final = opts$round_interim,
														 add_math = FALSE,
														 add_aligned = FALSE,
														 use_aligned = opts$use_aligned)

		s_M_D <- s_M_D.lst$s_M_D
		s_M_D_solution <- s_M_D.lst$solution
	} else {
		s_M_D_solution <- NULL
	}

	stopifnot(is.numeric(s_M_D), length(s_M_D) == 1, s_M_D > 0)

	# Calculate M_diff and t_obt:
	M_diff <- rnd(M_D - mu_D, opts$round_interim)
	t_obt <- rnd(M_diff/s_M_D, opts$round_t)

	# Get base formula without LaTeX math/aligned blocks
	t_obt.f <- t_paired_samples_formula(use_aligned = opts$use_aligned,
																	add_math = FALSE,
																	add_aligned = FALSE)

	# Prepend the solution for s_M if calculating from components
	t_obt.solution <- ifelse(!is.null(s_M_D_solution),
													 paste(s_M_D_solution, ' \\\\ ', t_obt.f),
													 t_obt.f)

	solution <- glue_solution(
		t_obt.solution,
		# Put negative values of mu_D in brackets
		"<<equals>> \\frac{<<M_D>> - <<(mu_D)>>}{<<s_M_D>>} = \\frac{<<M_diff>>}{<<s_M_D>>} = \\mathbf{<<t_obt>>}",
		M_diff = fmt(M_diff, get_digits(M_diff, opts$round_interim)),
		s_M_D = fmt(s_M_D, get_digits(s_M_D, opts$round_interim)),
		# Print values of 't_obt' to the precision of opts$round_final unless round_to is
		# set to 'sigfigs', in which case just present the final rounded value as is.
		t_obt = ifelse(opts$round_to == 'sigfigs', t_obt, fmt(t_obt, opts$round_t)))

	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	# Return list containing both values and solution string
	list(M_D = M_D,
			 mu_D = mu_D,
			 s_D = if(missing(s_D)) NULL else s_D,
			 SD_D = if(missing(s_D)) NULL else s_D,
			 n = if(missing(n)) NULL else n,
			 s_M_D = s_M_D,
			 M_diff = M_diff,
			 t_obt = t_obt,
			 solution = solution,
			 formula = t_obt.f) %>%
		purrr::compact()
}

#' @rdname solve_t_paired_samples
#'
#' @export
#'
t_paired_samples_formula <- function(...) {

	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts<- get_handcalcs_opts(...)

	# Use the appropriate equals sign for an aligned environment
	equals <- ifelse(opts$use_aligned, "&=", "=")

	# Create the formula string:
	solution<- lglue("t_{\\text{obt}} <<equals>> \\frac{M_{D} - \\mu_{D}}{s_{M_{D}}}")

	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	return(solution)
}
