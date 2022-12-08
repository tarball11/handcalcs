#' Tukey's HSD Post Hoc Test (Studentized Range Statistic)
#'
#' Calculates and provides solutions for Tukey's HSD using the Studentized Range
#' Statistic (`Q_obt`). This is primarily useful in calculating post hoc tests
#' making all possible comparisons among `k` groups after a significant ANOVA,
#' while maintaining the overall Type I error rate (`alpha`). Requires both
#' group means (`M_1`, `M_2`) and sample sizes (`n_1`, `n_2`), as well as the
#' error term from the ANOVA (`MS_error`).
#'
#' The result (`Q_obt`) is calculated as
#' \eqn{\frac{|M_1-M_2|}{\sqrt{(MS_{error}/n_1 + MS_{error}/n_2)(1/2)}}}. To
#' determine significance, either use the built-in R function [ptukey()] or
#' compare to the critical value using the [solve_Q_crit()] function (which
#' itself makes use of the built-in [qtukey()] function).
#'
#'
#' @param M_1 Numeric scalar. Mean of sample 1.
#' @param M_2 Numeric scalar. Mean of sample 2.
#' @param n_1 Numeric scalar. Sample size of sample 1.
#' @param n_2 Numeric scalar. Sample size of sample 2.
#' @param MS_error Numeric scalar. Mean Squared Error term from the ANOVA.
#' @param ... Additional arguments to override default behaviors (see
#'   [handcalcs_defaults()]
#'
#' @return `solve_q_obt()` returns a list with the provided values (`M_1`,
#'   `M_2`, `n_1`, `n_2`, `MS_error`), the interim calculations (`M_diff`,
#'   `MS_error_n1`, `MS_error_n2`, `Sum_MS_error_n`, `s_M_diff2`, `s_M_diff`),
#'   the final value (`Q_obt`), the solution string (`solution`), and the bare
#'   formula (`formula`) in LaTeX format (note: empty values are removed from
#'   the list). `Q_obt_formula()` returns just the bare formula in LaTeX format
#'   as a character string.
#' @export
#'
#' @examples
#'
#' solve_Q_obt(M_1 = 5, M_2 = 4, n_1 = 52, n_2 = 50, MS_error = 2.5)
#'
solve_Q_obt <- function(M_1,
												M_2,
												n_1,
												n_2,
												MS_error,
												...) {

	# Check argument validity:
	stopifnot(is.numeric(M_1), length(M_1) == 1)
	stopifnot(is.numeric(M_2), length(M_2) == 1)
	stopifnot(is.numeric(n_1), length(n_1) == 1, n_1 > 0)
	stopifnot(is.numeric(n_2), length(n_2) == 1, n_2 > 0)
	stopifnot(is.numeric(MS_error), MS_error >= 0, length(MS_error) == 1)

	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts<- get_handcalcs_opts(...)

	# Use the appropriate equals sign for an aligned environment
	equals <- ifelse(opts$use_aligned, "&=", "=")

	# Calculate Q_obt:
	M_diff <- rnd(abs(M_1 - M_2), opts$round_interim)

	MS_error_n1 <- rnd(MS_error/n_1, opts$round_interim)
	MS_error_n2 <- rnd(MS_error/n_2, opts$round_interim)
	Sum_MS_error_n <- rnd(MS_error_n1 + MS_error_n2, opts$round_interim)
	s_M_diff2 <- rnd(Sum_MS_error_n*0.5, opts$round_interim)
	s_M_diff <- rnd(sqrt(s_M_diff2), opts$round_interim)
	Q_obt <- rnd(M_diff/s_M_diff, opts$round_final)

	# Get base formula without LaTeX math/aligned blocks
	Q_obt.f <- Q_obt_formula(use_aligned = opts$use_aligned,
													 add_math = FALSE,
													 add_aligned = FALSE)

	solution <- glue_solution(
		Q_obt.f,
		# Put negative values of M_2 in brackets
		"<<equals>> \\frac{|<<(M_1)>> - <<(M_2)>>|}{\\sqrt{(\\frac{<<MS_error>>}{<<n_1>>} + \\frac{<<MS_error>>}{<<n_2>>})(\\frac{1}{2})}}",
		"<<equals>> \\frac{|<<M_diff>>|}{\\sqrt{(<<MS_error_n1>> + <<MS_error_n2>>)(\\frac{1}{2})}} = \\frac{|<<M_diff>>|}{\\sqrt{(<<Sum_MS_error_n>>)(\\frac{1}{2})}} = \\frac{|<<M_diff>>|}{\\sqrt{<<s_M_diff2>>}} = \\frac{|<<M_diff>>|}{<<s_M_diff>>} = \\mathbf{<<Q_obt>>}",
		M_diff = fmt(M_diff, get_digits(M_diff, opts$round_interim)),
		s_M_diff = fmt(s_M_diff, get_digits(s_M_diff, opts$round_interim)),
		# Print values of 't_obt' to the precision of opts$round_final unless round_to is
		# set to 'sigfigs', in which case just present the final rounded value as is.
		Q_obt = ifelse(opts$round_to == 'sigfigs', Q_obt, fmt(Q_obt, opts$round_final)))

	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	# Return list containing both values and solution string
	list(M_1 = M_1,
			 M_2 = M_2,
			 n_1 = n_1,
			 n_2 = n_2,
			 MS_error = MS_error,
			 M_diff = M_diff,
			 MS_error_n1 = MS_error_n1,
			 MS_error_n2 = MS_error_n2,
			 Sum_MS_error_n = Sum_MS_error_n,
			 s_M_diff2 = s_M_diff2,
			 s_M_diff = s_M_diff,
			 Q_obt = Q_obt,
			 solution = solution,
			 formula = Q_obt.f)
}


#' @rdname solve_Q_obt
#'
#' @export
#'
Q_obt_formula <- function(...) {
	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts<- get_handcalcs_opts(...)

	# Use the appropriate equals sign for an aligned environment
	equals <- ifelse(opts$use_aligned, "&=", "=")

	# Create the formula string:
	solution<- lglue("Q_{\\text{obt}} <<equals>> \\frac{|M_{1} - M_{2}|}{\\sqrt{(\\frac{\\mathit{MS}_{\\text{error}}}{n_{1}} + \\frac{\\mathit{MS}_{\\text{error}}}{n_{2}})(\\frac{1}{2})}}")

	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	return(solution)

}



#' Fisher's LSD (Least Significant Difference) Test
#'
#' Calculates and provides solutions for Fisher's LSD, intended as a means to
#' conduct planned comparisons after a significant ANOVA. It is essentially the
#' same as an independent-samples t-test, except that it uses \eqn{MS_{error}}
#' in place of the pooled variance \eqn{s_{p}^2}.
#'
#' Requires both group means (`M_1`, `M_2`) and sample sizes (`n_1`, `n_2`), as
#' well as the error term from the ANOVA (`MS_error`).
#'
#' The result (`t_obt`) is calculated as
#' \eqn{\frac{|M_1-M_2|}{\sqrt{(MS_{error}/n_1 + MS_{error}/n_2)}}}. To
#' determine significance, either use the built-in R function [pt()] or compare
#' to the critical value using the [solve_t_crit()] function.
#'
#'
#' @param M_1 Numeric scalar. Mean of sample 1.
#' @param M_2 Numeric scalar. Mean of sample 2.
#' @param n_1 Numeric scalar. Sample size of sample 1.
#' @param n_2 Numeric scalar. Sample size of sample 2.
#' @param MS_error Numeric scalar. Mean Squared Error term from the ANOVA.
#' @param ... Additional arguments to override default behaviors (see
#'   [handcalcs_defaults()]
#'
#' @return `solve_t_obt()` returns a list with the provided values (`M_1`,
#'   `M_2`, `n_1`, `n_2`, `MS_error`), the interim calculations (`M_diff`,
#'   `MS_error_n1`, `MS_error_n2`, `Sum_MS_error_n`, `s_M_diff2`, `s_M_diff`),
#'   the final value (`t_obt`), the solution string (`solution`), and the bare
#'   formula (`formula`) in LaTeX format (note: empty values are removed from
#'   the list). `Q_obt_formula()` returns just the bare formula in LaTeX format
#'   as a character string.
#' @export
#'
#' @examples
#'
#' solve_LSD(M_1 = 5, M_2 = 4, n_1 = 52, n_2 = 50, MS_error = 2.5)
#'
solve_LSD <- function(M_1,
											M_2,
											n_1,
											n_2,
											MS_error,
											...) {

	# Check argument validity:
	stopifnot(is.numeric(M_1), length(M_1) == 1)
	stopifnot(is.numeric(M_2), length(M_2) == 1)
	stopifnot(is.numeric(n_1), length(n_1) == 1, n_1 > 0)
	stopifnot(is.numeric(n_2), length(n_2) == 1, n_2 > 0)
	stopifnot(is.numeric(MS_error), MS_error >= 0, length(MS_error) == 1)

	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts<- get_handcalcs_opts(...)

	# Use the appropriate equals sign for an aligned environment
	equals <- ifelse(opts$use_aligned, "&=", "=")

	# Calculate Q_obt:
	M_diff <- rnd(abs(M_1 - M_2), opts$round_interim)

	MS_error_n1 <- rnd(MS_error/n_1, opts$round_interim)
	MS_error_n2 <- rnd(MS_error/n_2, opts$round_interim)
	Sum_MS_error_n <- rnd(MS_error_n1 + MS_error_n2, opts$round_interim)
	s_M_diff2 <- rnd(Sum_MS_error_n, opts$round_interim)
	s_M_diff <- rnd(sqrt(s_M_diff2), opts$round_interim)
	t_obt <- rnd(M_diff/s_M_diff, opts$round_final)

	# Get base formula without LaTeX math/aligned blocks
	t_obt.f <- LSD_formula(use_aligned = opts$use_aligned,
												 add_math = FALSE,
												 add_aligned = FALSE)

	solution <- glue_solution(
		t_obt.f,
		# Put negative values of M_2 in brackets
		"<<equals>> \\frac{|<<(M_1)>> - <<(M_2)>>|}{\\sqrt{\\frac{<<MS_error>>}{<<n_1>>} + \\frac{<<MS_error>>}{<<n_2>>}}}",
		"<<equals>> \\frac{|<<M_diff>>|}{\\sqrt{<<MS_error_n1>> + <<MS_error_n2>>}} = \\frac{|<<M_diff>>|}{\\sqrt{<<Sum_MS_error_n>>}} = \\frac{|<<M_diff>>|}{\\sqrt{<<s_M_diff2>>}} = \\frac{|<<M_diff>>|}{<<s_M_diff>>} = \\mathbf{<<t_obt>>}",
		M_diff = fmt(M_diff, get_digits(M_diff, opts$round_interim)),
		s_M_diff = fmt(s_M_diff, get_digits(s_M_diff, opts$round_interim)),
		# Print values of 't_obt' to the precision of opts$round_final unless round_to is
		# set to 'sigfigs', in which case just present the final rounded value as is.
		t_obt = ifelse(opts$round_to == 'sigfigs', t_obt, fmt(t_obt, opts$round_final)))

	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	# Return list containing both values and solution string
	list(M_1 = M_1,
			 M_2 = M_2,
			 n_1 = n_1,
			 n_2 = n_2,
			 MS_error = MS_error,
			 M_diff = M_diff,
			 MS_error_n1 = MS_error_n1,
			 MS_error_n2 = MS_error_n2,
			 Sum_MS_error_n = Sum_MS_error_n,
			 s_M_diff2 = s_M_diff2,
			 s_M_diff = s_M_diff,
			 t_obt = t_obt,
			 solution = solution,
			 formula = t_obt.f)
}


#' @rdname solve_LSD
#'
#' @export
#'
LSD_formula <- function(...) {
	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts<- get_handcalcs_opts(...)

	# Use the appropriate equals sign for an aligned environment
	equals <- ifelse(opts$use_aligned, "&=", "=")

	# Create the formula string:
	solution<- lglue("t_{\\text{obt}} <<equals>> \\frac{|M_{1} - M_{2}|}{\\sqrt{\\frac{\\mathit{MS}_{\\text{error}}}{n_{1}} + \\frac{\\mathit{MS}_{\\text{error}}}{n_{2}}}}")

	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	return(solution)

}
