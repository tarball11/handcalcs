#' Pooled Variance
#'
#' Calculates the pooled variance (`s_p2`) for two independent samples from
#' sample sizes (`n_1` and `n_2`) and either sums of squares for each sample
#' (`SS_1` and `SS_2`), or both samples' standard deviations (accepting either
#' `s_1` and `s_2` or `SD_1` and `SD_2`).
#'
#' If providing sample sizes and sums of squares, pooled variance will be
#' calculated as \eqn{s_p^2=\frac{SS_1+SS_2}{n_1+n_2-2}}. If providing sample
#' sizes and standard deviations, pooled variance will be calculated as
#' \eqn{s_p^2=\frac{(n_{1}-1)(s_{1}^{2})+(n_{2}-1)(s_{2}^{2})}{n_1+n_2-2}}.
#'
#'
#' @param SS_1 Numeric scalar. Sum of squares of sample 1.
#' @param SS_2 Numeric scalar. Sum of squares of sample 2.
#' @param s_1,SD_1 Numeric scalar. Standard deviation of sample 1. (May be
#'   provided either as s_1 or SD_1.)
#' @param s_2,SD_2 Numeric scalar. Standard deviation of sample 2. (May be
#'   provided either as s_2 or SD_2.)
#' @param n_1 Numeric scalar. Sample size of sample 1.
#' @param n_2 Numeric scalar. Sample size of sample 2.
#' @param ... Additional arguments to override default behaviors (see
#'   [handcalcs_defaults()]
#'
#' @return `solve_s_p2()` returns a list with the provided values (`SS_1`,
#'   `SS_2`, `s_1`, `s_2`, `SD_1`, `SD_2` `n_1`, `n_2`), the interim
#'   calculations (`df_1`, `df_2`, `df` `s_12`, `s_22`, `df_s_12`, `df_s_22`,
#'   `SS`), the final value (`s_p2`), the solution string (`solution`), and the
#'   bare formula (`formula`) in LaTeX format (note: empty values are removed
#'   from the list). `s_p2_formula()` returns just the bare formula in LaTeX
#'   format as a character string.
#' @seealso [solve_s_M_diff()], [solve_t_indep_samples()]
#' @export
#'
#' @examples
#' solve_s_p2(SS_1 = 15, SS_2 = 20, n_1 = 40, n_2 = 50)
#' solve_s_p2(SD_1 = 5, SD_2 = 6, n_1 = 40, n_2 = 50)
#'
solve_s_p2 <- function(SS_1,
											 SS_2,
											 s_1,
											 s_2,
											 SD_1,
											 SD_2,
											 n_1,
											 n_2,
											 ...) {

	# Check argument validity:
	# Must supply sample sizes (n_1 and n_2)
	if(missing(n_1) | missing(n_2)) stop("Must supply sample sizes (n_1 and n_2).")
	stopifnot(is.numeric(n_1), is.numeric(n_2))
	stopifnot(length(n_1) == 1, length(n_2) == 1)
	stopifnot(n_1 > 0, n_2 > 0)

	# If SD is supplied instead of s, set s accordingly
	if(missing(s_1) & !missing(SD_1)) s_1 <- SD_1
	if(missing(s_2) & !missing(SD_2)) s_2 <- SD_2

	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts<- get_handcalcs_opts(...)

	# Use the appropriate equals sign for an aligned environment
	equals <- ifelse(opts$use_aligned, "&=", "=")

	# Some values are not returned below; set to NULL
	s_12 <- s_22 <- df_s_12 <- df_s_22 <- NULL

	if(!missing(SS_1) & !missing(SS_2)) {
		# Check argument validity:
		stopifnot(is.numeric(SS_1), is.numeric(SS_2))
		stopifnot(length(SS_1) == 1, length(SS_2) == 1)
		stopifnot(SS_1 > 0, SS_2 > 0)

		# Round initial values
		SS_1 <- rnd(SS_1, opts$round_interim)
		SS_2 <- rnd(SS_2, opts$round_interim)
		df_1 <- n_1 - 1
		df_2 <- n_2 - 1

		SS <- rnd(SS_1 + SS_2, opts$round_interim)
		df <- df_1 + df_2
		s_p2 <- rnd(SS/df, opts$round_final)

		# Get base formula without LaTeX math/aligned blocks
		s_p2.f <- s_p2_formula(type = 'SS',
													use_aligned = opts$use_aligned,
													add_math = FALSE,
													add_aligned = FALSE)
		# Create the solution string, with rounded values (minimally displayed)
		solution <- glue_solution(
			s_p2.f,
			"<<equals>> \\frac{<<SS_1>> + <<SS_2>>}{<<df_1>> + <<df_2>>} = \\frac{<<SS>>}{<<df>>} = \\mathbf{<<s_p2>>}",
			SS = fmt(SS, get_digits(SS, opts$round_interim)),
			# Round based on the final calculated value unless round_to is set to
			# 'sigfigs', in which case just present the final rounded value as is.
			s_p2 = ifelse(opts$round_to == 'sigfigs', s_p2, fmt(s_p2, get_digits(s_p2, opts$round_final))))

	} else if(!missing(s_1) & !missing(s_2)) {
		# Check argument validity:
		stopifnot(is.numeric(s_1), is.numeric(s_2))
		stopifnot(length(s_1) == 1, length(s_2) == 1)
		stopifnot(s_1 > 0, s_2 > 0)

		# Round initial values
		s_1 <- rnd(s_1, opts$round_interim)
		s_2 <- rnd(s_2, opts$round_interim)

		# Calculate s_p2
		s_12 <- rnd(s_1^2, opts$round_interim)
		s_22 <- rnd(s_2^2, opts$round_interim)
		df_1 <- n_1 - 1
		df_2 <- n_2 - 1
		df <- df_1 + df_2

		df_s_12 <- rnd(df_1*s_12, opts$round_interim)
		df_s_22 <- rnd(df_2*s_22, opts$round_interim)

		SS <- rnd(df_s_12 + df_s_22, opts$round_interim)
		s_p2 <- rnd(SS/df, opts$round_final)

		# Get base formula without LaTeX math/aligned blocks
		s_p2.f <- s_p2_formula(type = 's',
													use_aligned = opts$use_aligned,
													add_math = FALSE,
													add_aligned = FALSE)
		# Create the solution string, with rounded values (minimally displayed)
		solution <- glue_solution(
			s_p2.f,
			"<<equals>> \\frac{(<<n_1>> - 1)(<<s_1>>^{2}) + (<<n_2>> - 1)(<<s_2>>^{2})}{<<n_1>> + <<n_2>> - 2}",
			"<<equals>> \\frac{(<<df_1>>)(<<s_12>>) + (<<df_2>>)(<<s_22>>)}{<<df>>}",
			"<<equals>> \\frac{<<df_s_12>> + <<df_s_22>>}{<<df>>}",
			"<<equals>> \\frac{<<SS>>}{<<df>>}",
			"<<equals>> \\mathbf{<<s_p2>>}",
			SS = fmt(SS, get_digits(SS, opts$round_interim)),
			s_12 = fmt(s_12, get_digits(s_12, opts$round_interim)),
			s_22 = fmt(s_22, get_digits(s_22, opts$round_interim)),
			df_s_12 = fmt(df_s_12, get_digits(df_s_12, opts$round_interim)),
			df_s_22 = fmt(df_s_22, get_digits(df_s_22, opts$round_interim)),
			SS = fmt(SS, get_digits(SS, opts$round_interim)),
			# Round based on the final calculated value unless round_to is set to
			# 'sigfigs', in which case just present the final rounded value as is.
			s_p2 = ifelse(opts$round_to == 'sigfigs', s_p2, fmt(s_p2, get_digits(s_p2, opts$round_final))))
	} else {
		stop("Must supply either sample standard deviations (s_1 and s_2 or SD_1 and SD_2) or sums of squares (SS_1 and SS_2).")
	}

	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	# Return list containing both values and solution string
	list(SS_1 = if(missing(SS_1)) NULL else SS_1,
			 SS_2 = if(missing(SS_2)) NULL else SS_2,
			 s_1 = if(missing(s_1)) NULL else s_1,
			 s_2 = if(missing(s_2)) NULL else s_2,
			 SD_1 = if(missing(SD_1)) NULL else SD_1,
			 SD_2 = if(missing(SD_2)) NULL else SD_2,
			 n_1 = n_1,
			 n_2 = n_2,
			 df_1 = df_1,
			 df_2 = df_2,
			 s_12 = s_12,
			 s_22 = s_22,
			 df_s_12 = df_s_12,
			 df_s_22 = df_s_22,
			 SS = SS,
			 df = df,
			 s_p2 = s_p2,
			 solution = solution,
			 formula = s_p2.f) %>%
		purrr::compact()
}

#' @rdname solve_s_p2
#'
#' @export
#'
s_p2_formula <- function(type = 'SS',
												...) {
	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts<- get_handcalcs_opts(...)

	# Use the appropriate equals sign for an aligned environment
	equals <- ifelse(opts$use_aligned, "&=", "=")

	# Create the formula string:
	stem <- "s_{p}^{2} <<equals>> "
	SS.f <- " \\frac{\\mathit{SS}_{1} + \\mathit{SS}_{2}}{\\mathit{df}_{1} + \\mathit{df}_{2}} "
	s.f1 <- " = \\frac{(\\mathit{df}_{1})(s_{1}^{2}) + (\\mathit{df}_{2})(s_{2}^{2})}{\\mathit{df}_{1} + \\mathit{df}_{2}}"
	s.f2 <- " = \\frac{(n_{1} - 1)(s_{1}^{2}) + (n_{2} - 1)(s_{2}^{2})}{n_{1} + n_{2} - 2}"
	solution<- if(type == 'SS') lglue(stem, SS.f) else lglue(stem, SS.f, s.f1, s.f2)

	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	return(solution)
}


#' Standard Error of Difference Between Two Independent Sample Means
#'
#' Calculates the standard error of the difference between two independent
#' sample means (`s_M_diff`), used as the error term in the independent-samples
#' *t*-test. Requires sample sizes (`n_1` and `n_2`) and either the pooled
#' variance (`s_p2`), or both samples' standard deviations (accepting either
#' `s_1` and `s_2` or `SD_1` and `SD_2`).
#'
#' The standard error of the mean difference will be calculated using the pooled
#' variance as
#' \eqn{s_{M_{1}-M_{2}}=\sqrt{\frac{s_{p}^{2}}{n_{1}}+\frac{s_{p}^{2}}{n_{2}}}}.
#' If providing sample sizes and standard deviations instead, pooled variance
#' will be calculated from that information using the `[solve_s_p2()]` function.
#'
#' @param s_p2 Numeric scalar. Pooled variance.
#' @param s_1,SD_1 Numeric scalar. Standard deviation of sample 1.
#' @param s_2,SD_2 Numeric scalar. Standard deviation of sample 2.
#' @param n_1 Numeric scalar. Sample size of sample 1.
#' @param n_2 Numeric scalar. Sample size of sample 2.
#' @param ... Additional arguments to override default behaviors (see
#'   [handcalcs_defaults()]
#'
#' @return `solve_s_M_diff()` returns a list with the provided values (`s_p2`,
#'   `s_1`, `s_2`, `SD_1`, `SD_2` `n_1`, `n_2`), the interim calculations (`s_p2_n1`,
#'   `s_p2_n2`, `s_M_diff2`), the final value (`s_M_diff`), the solution string
#'   (`solution`), and the bare formula (`formula`) in LaTeX format (note: empty
#'   values are removed from the list). `s_M_diff_formula()` returns just the
#'   bare formula in LaTeX format as a character string.
#'
#' @seealso [solve_s_p2()], [solve_t_indep_samples()]
#'
#' @export
#'
#' @examples
#' solve_s_M_diff(s_p2 = 10, n_1 = 50, n_2 = 48)
#' solve_s_M_diff(s_1 = 4.5, s_2 = 5, n_1 = 50, n_2 = 48)
#'
#' # If providing standard deviation, can use either s_1 and s_2, or SD_1 and SD_2
#' solve_s_M_diff(SD_1 = 4.5, SD_2 = 5, n_1 = 50, n_2 = 48)
#'
solve_s_M_diff <- function(s_p2,
													 s_1,
													 s_2,
													 SD_1,
													 SD_2,
													 n_1,
													 n_2,
													 ...) {

	# If SD is supplied instead of s, set s accordingly
	if(missing(s_1) & !missing(SD_1)) s_1 <- SD_1
	if(missing(s_2) & !missing(SD_2)) s_2 <- SD_2

	# Must supply sample sizes (n_1 and n_2)
	if(missing(n_1) | missing(n_2)) stop("Must supply sample sizes (n_1 and n_2).")

	if(missing(s_p2) & (missing(s_1) | missing(s_2))) {
		stop("Must supply pooled variance (s_p2) or standard deviation (as s_1 and s_2 or SD_1 and SD_2).")
	}

	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts<- get_handcalcs_opts(...)

	# Use the appropriate equals sign for an aligned environment
	equals <- ifelse(opts$use_aligned, "&=", "=")

	if(missing(s_p2)) {
		s_p2.lst <- solve_s_p2(s_1 = s_1,
													 s_2 = s_2,
													 n_1 = n_1,
													 n_2 = n_2,
													 round_interim = opts$round_interim,
													 round_final = opts$round_interim,
													 add_math = FALSE,
													 add_aligned = FALSE,
													 use_aligned = opts$use_aligned)
	s_p2 <- s_p2.lst$s_p2
	s_p2_solution <- s_p2.lst$solution
	} else {
		s_p2_solution <- NULL
	}

	# Check argument validity:
	stopifnot(is.numeric(s_p2), is.numeric(n_1), is.numeric(n_2))
	stopifnot(length(s_p2) == 1, length(n_1) == 1, length(n_2) == 1)
	stopifnot(s_p2 > 0, n_1 > 0, n_2 > 0)

	# Round initial values
	s_p2 <- rnd(s_p2, opts$round_interim)

	# Calculate s_M_diff
	s_p2_n1 <- rnd(s_p2 / n_1, opts$round_interim)
	s_p2_n2 <- rnd(s_p2 / n_2, opts$round_interim)
	s_M_diff2 <- rnd(s_p2_n1 + s_p2_n2, opts$round_interim)
	s_M_diff <- rnd(sqrt(s_M_diff2), opts$round_final)

	# Get base formula without LaTeX math/aligned blocks
	s_M_diff.f <- s_M_diff_formula(use_aligned = opts$use_aligned,
																 add_math = FALSE,
																 add_aligned = FALSE)

	# Prepend the solution for s_p2 if calculating from components
	s_M_diff.solution <- ifelse(!is.null(s_p2_solution),
															paste(s_p2_solution, ' \\\\ ', s_M_diff.f),
															s_M_diff.f)


	# Create the solution string, with rounded values (minimally displayed)
	solution <- glue_solution(
		s_M_diff.solution,
		"<<equals>> \\sqrt{\\frac{<<s_p2>>}{<<n_1>>} + \\frac{<<s_p2>>}{<<n_2>>}} = \\sqrt{<<s_p2_n1>> + <<s_p2_n2>>} = \\sqrt{<<s_M_diff2>>} = \\mathbf{<<s_M_diff>>}",
		s_p2 = fmt(s_p2, get_digits(s_p2, opts$round_interim)),
		s_p2_n1 = fmt(s_p2_n1, get_digits(s_p2_n1, opts$round_interim)),
		s_p2_n2 = fmt(s_p2_n2, get_digits(s_p2_n2, opts$round_interim)),
		s_M_diff2 = fmt(s_M_diff2, get_digits(s_M_diff2, opts$round_interim)),
		# Round based on the final calculated value unless round_to is set to
		# 'sigfigs', in which case just present the final rounded value as is.
		s_M_diff = ifelse(opts$round_to == 'sigfigs', s_M_diff, fmt(s_M_diff, get_digits(s_M_diff, opts$round_final))))


	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	# Return list containing both values and solution string
	list(s_1 = if(missing(s_1)) NULL else s_1,
			 s_2 = if(missing(s_2)) NULL else s_2,
			 SD_1 = if(missing(SD_1)) NULL else SD_1,
			 SD_2 = if(missing(SD_2)) NULL else SD_2,
			 s_p2 = s_p2,
			 n_1 = n_1,
			 n_2 = n_2,
			 s_p2_n1 = s_p2_n1,
			 s_p2_n2 = s_p2_n2,
			 s_M_diff2 = s_M_diff2,
			 s_M_diff = s_M_diff,
			 solution = solution,
			 formula = s_M_diff.f) %>%
		purrr::compact()
}

#' @rdname solve_s_M_diff
#'
#' @export
#'
s_M_diff_formula <- function(...) {
	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts<- get_handcalcs_opts(...)

	# Use the appropriate equals sign for an aligned environment
	equals <- ifelse(opts$use_aligned, "&=", "=")

	# Create the formula string:
	solution<- lglue("s_{M_{1} - M_{2}} <<equals>> \\sqrt{\\frac{s_{p}^{2}}{n_{1}} + \\frac{s_{p}^{2}}{n_{2}}}")

	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	return(solution)

}

#' Independent-samples *t*-test
#'
#' Calculates and provides solutions for the independent-samples *t*-test (i.e.,
#' `t_obt`), comparing the calculated difference between two independent sample
#' means (\eqn{M_{1} - M_{2}}) against the predicted difference under the null
#' hypothesis (\eqn{\mu_{1} - \mu_{2}}, which is always zero, and thus not an
#' argument to this function).
#'
#' The error term (\eqn{s_{M_{1} - M_{2}}}) can be provided directly (as
#' `s_M_diff`) along with the two sample means (`M_1` and `M_2`) to calculate
#' `t_obt` as (\eqn{t_{obt}=\frac{M_{1}-M_{2}}{s_{M_{1}-M_{2}}}}). If the
#' standard error is not provided, it can be calculated using either the pooled
#' variance (`s_p2`) and the sample sizes (`n_1` and `n_2`), or both samples'
#' standard deviations (`s_1` and `s_2` or `SD_1` and `SD_2`) and sample sizes
#' (`n_1` and `n_2`) using the `[solve_s_M_diff()]` function. In that case,
#' those calculations will be included in the solution string.
#'
#' Note that `t_obt` values are rounded to the value of `round_t` instead of the
#' value of `round_final` (see [handcalcs_defaults()]).
#'
#' @param M_1 Numeric scalar. Mean of sample 1.
#' @param M_2 Numeric scalar. Mean of sample 2.
#' @param s_M_diff Numeric scalar. Standard error of the difference between two
#'   independent sample means. If not provided, can be calculated using the
#'   other arguments using the `[solve_s_M_diff()]` function.
#' @param s_p2 Numeric scalar. Pooled variance.
#' @param s_1,SD_1 Numeric scalar. Standard deviation of sample 1.
#' @param s_2,SD_2 Numeric scalar. Standard deviation of sample 2.
#' @param n_1 Numeric scalar. Sample size of sample 1.
#' @param n_2 Numeric scalar. Sample size of sample 2.
#' @param ... Additional arguments to override default behaviors (see
#'   [handcalcs_defaults()]
#'
#' @return `solve_t_indep_samples()` returns a list with the provided values
#'   (`M_1`, `M_2`, `s_M_diff`, `s_p2`, `s_1`, `s_2`, `SD_1`, `SD_2` `n_1`,
#'   `n_2`), the interim calculations (`M_diff`), the final value (`t_obt`), the
#'   solution string (`solution`), and the bare formula (`formula`) in LaTeX
#'   format (note: empty values are removed from the list).
#'   `t_indep_samples_formula()` returns just the bare formula in LaTeX format
#'   as a character string.
#'
#' @export
#'
#' @examples
#'
#' solve_t_indep_samples(M_1 = 5, M_2 = 4, s_M_diff = 0.5)
#'
#' # If not providing the error term, it can be calculated either from the
#' # sample sizes and either the pooled variance or sample standard deviations
#' solve_t_indep_samples(M_1 = 5, M_2 = 4, s_p2 = 5, n_1 = 40, n_2 = 45)
#' solve_t_indep_samples(M_1 = 5, M_2 = 4, s_1 = 2, s_2 = 3, n_1 = 40, n_2 = 45)
#'
#' # Standard deviations can be provided either as s_1 and s_2 or SD_1 and SD_2
#' solve_t_indep_samples(M_1 = 5, M_2 = 4, s_1 = 2, s_2 = 3, n_1 = 40, n_2 = 45)
#'
solve_t_indep_samples <- function(M_1,
																	M_2,
																	s_M_diff,
																	s_p2,
																	s_1,
																	s_2,
																	SD_1,
																	SD_2,
																	n_1,
																	n_2,
																	...) {

	# Check argument validity:
	stopifnot(is.numeric(M_1), is.numeric(M_2))
	stopifnot(length(M_1) == 1, length(M_2) == 1)

	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts<- get_handcalcs_opts(...)

	# Use the appropriate equals sign for an aligned environment
	equals <- ifelse(opts$use_aligned, "&=", "=")

	# Calculate s_M_diff if not provided
	if(missing(s_M_diff)) {
		# If SD is supplied instead of s, set s accordingly
		if(missing(s_1) & !missing(SD_1)) s_1 <- SD_1
		if(missing(s_2) & !missing(SD_2)) s_2 <- SD_2

		if(missing(n_1) | missing(n_2)) {
			stop("If not supplying s_M_diff, must supply sample sizes (n_1 and n_2).")
		}
		if(missing(s_p2) & (missing(s_1) | missing(s_2))) {
			stop("If not supplying s_M_diff, must supply either pooled variance (s_p2) or standard deviations (s_1 and s_2 or SD_1 and SD_2).")
		}

		s_M_diff.lst <- solve_s_M_diff(s_p2 = s_p2,
																	 s_1 = s_1,
																	 s_2 = s_2,
																	 n_1 = n_1,
																	 n_2 = n_2,
																	 round_interim = opts$round_interim,
																	 round_final = opts$round_interim,
																	 add_math = FALSE,
																	 add_aligned = FALSE,
																	 use_aligned = opts$use_aligned)

		# If the pooled variance was calculated as part of the s_M_diff,
		# make sure the value gets set so it is included in the return list
		if(missing(s_p2)) s_p2 <- s_M_diff.lst$s_p2

		s_M_diff <- s_M_diff.lst$s_M_diff
		s_M_diff_solution <- s_M_diff.lst$solution
	} else {
		s_M_diff_solution <- NULL
	}

	stopifnot(is.numeric(s_M_diff), length(s_M_diff) == 1, s_M_diff > 0)

	# Calculate t_obt:
	M_diff <- rnd(M_1 - M_2, opts$round_interim)
	t_obt <- rnd(M_diff/s_M_diff, opts$round_t)

	# Get base formula without LaTeX math/aligned blocks
	t_obt.f <- t_indep_samples_formula(use_aligned = opts$use_aligned,
																			add_math = FALSE,
																			add_aligned = FALSE)

	# Prepend the solution for s_M if calculating from components
	t_obt.solution <- ifelse(!is.null(s_M_diff_solution),
													 paste(s_M_diff_solution, ' \\\\ ', t_obt.f),
													 t_obt.f)

	solution <- glue_solution(
		t_obt.solution,
		# Put negative values of M_2 in brackets
		"<<equals>> \\frac{<<M_1>> - <<(M_2)>>}{<<s_M_diff>>} = \\frac{<<M_diff>>}{<<s_M_diff>>} = \\mathbf{<<t_obt>>}",
		M_diff = fmt(M_diff, get_digits(M_diff, opts$round_interim)),
		s_M_diff = fmt(s_M_diff, get_digits(s_M_diff, opts$round_interim)),
		# Print values of 't_obt' to the precision of opts$round_final unless round_to is
		# set to 'sigfigs', in which case just present the final rounded value as is.
		t_obt = ifelse(opts$round_to == 'sigfigs', t_obt, fmt(t_obt, opts$round_t)))

	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	# Return list containing both values and solution string
	list(M_1 = M_1,
			 M_2 = M_2,
			 s_1 = if(missing(s_1)) NULL else s_1,
			 s_2 = if(missing(s_2)) NULL else s_2,
			 SD_1 = if(missing(SD_1)) NULL else SD_1,
			 SD_2 = if(missing(SD_2)) NULL else SD_2,
			 s_p2 = if(missing(s_p2)) NULL else s_p2,
			 n_1 = if(missing(n_1)) NULL else n_1,
			 n_2 = if(missing(n_2)) NULL else n_2,
			 s_M_diff = s_M_diff,
			 M_diff = M_diff,
			 t_obt = t_obt,
			 solution = solution,
			 formula = t_obt.f) %>%
		purrr::compact()
}

#' @rdname solve_t_indep_samples
#'
#' @export
#'
t_indep_samples_formula <- function(...) {
	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts<- get_handcalcs_opts(...)

	# Use the appropriate equals sign for an aligned environment
	equals <- ifelse(opts$use_aligned, "&=", "=")

	# Create the formula string:
	solution<- lglue("t_{\\text{obt}} <<equals>> \\frac{M_{1} - M_{2}}{s_{M_{1} - M_{2}}}")

	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	return(solution)

}

