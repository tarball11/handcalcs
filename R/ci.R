#' Confidence Intervals Around Sample Mean (using *z* distribution)
#'
#' Calculates confidence intervals around a sample mean when the population
#' standard deviation (\eqn{\sigma}) is known: \eqn{M \pm (z_{1 -
#' \alpha/2})(\sigma_{M})}. May either provide `sigma_M`, or that value can be
#' calculated from `sigma` and `n`.
#'
#' Note that the critical value of *z* (`z_crit`) is rounded to the value of
#' `round_z` instead of the value of `round_interim` or `round_final` (see
#' [handcalcs_defaults()]).
#'
#' @param M Numeric scalar. Sample mean.
#' @param sigma Numeric scalar. Population standard deviation. Required if
#'   sigma_M is not provided.
#' @param n Numeric scalar. Sample size. Required if sigma_M is not provided.
#' @param sigma_M Numeric scalar. Standard error of the mean. If not provided,
#'   will be calculated from `sigma` and `n` (using [solve_sigma_M()]) and
#'   included in the solution string.
#' @param level Numeric scalar. Confidence level; defaults to 0.95 (95%
#'   confidence intervals).
#' @param ... Additional arguments to override default behaviors (see
#'   [handcalcs_defaults()]
#'
#' @return `solve_ci_z()` returns a list with the provided values (`M`, `sigma`,
#'   `n`, `sigma_M`), the interim calculations (`sigma_M`, `z_crit`,
#'   `marg_err`), the final value (`CI_lower`, `CI_upper`, as well as a named
#'   vector containing both values `CI`), the solution string (`solution`), and
#'   the bare formula (`formula`) in LaTeX format. `ci_z_formula()` returns just
#'   the bare formula in LaTeX format as a character string.

#' @export
#'
#' @examples
#'
#' solve_ci_z(M = 5, sigma_M = 0.2)
#'
#' # Defaults to 95% CIs, but other levels can be specified:
#' solve_ci_z(M = 5, sigma_M = 0.2, level = 0.99)
#'
#' # Will calculate sigma_M if sigma and n are provided, and include the
#' # calculation in the solution string
#' solve_ci_z(M = 5, sigma = 2, n = 100)
#'
#' # Note: if sigma_M is provided, will ignore sigma and n values
#' solve_ci_z(M = 5, sigma = 2, n = 100, sigma_M = 4)
#'
#' # If you just want the formula:
#' ci_z_formula()
#'
solve_ci_z <- function(M,
											 sigma,
											 n,
											 sigma_M,
											 level = 0.95,
											 ...) {
	# Check argument validity
	stopifnot(is.numeric(M), length(M) == 1)
	stopifnot(is.numeric(level), length(level) == 1, level > 0, level < 1)

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

	# Round initial values
	M <- rnd(M, opts$round_interim)
	sigma_M <- rnd(sigma_M, opts$round_interim)

	# Calculate CI
	z_crit <- rnd(qnorm((1-level)/2, lower.tail = FALSE), opts$round_z)


	marg_err <- rnd(z_crit*sigma_M, opts$round_interim)
	CI_lower <- rnd(M - marg_err, opts$round_final)
	CI_upper <- rnd(M + marg_err, opts$round_final)


	# Get base formula without LaTeX math/aligned blocks
	CI.formula <- ci_z_formula(level = level,
														 use_aligned = opts$use_aligned,
														 add_math = FALSE,
														 add_aligned = FALSE)

	# Prepend the solution for sigma_M if calculating from components
	CI.solution <- ifelse(!is.null(sigma_M.solution),
												paste(sigma_M.solution, ' \\\\ ', CI.formula),
												CI.formula)


	# Create the solution string, with rounded values (minimally displayed)
	solution <- glue_solution(
		CI.solution,
		"<<equals>> <<M>> \\pm (<<z_crit>>)(<<sigma_M>>)",# = <<M>> \\pm <<marg_err>>",
		# "<<equals>> <<M>> \\pm <<marg_err>>",
		# "<<equals>> \\mathbf{(<<CI_lower>>, <<CI_upper>>)}",
		"\\text{Lower bound} <<equals>> <<M>> - <<marg_err>> = \\mathbf{<<CI_lower>>}",
		"\\text{Upper bound} <<equals>> <<M>> + <<marg_err>> = \\mathbf{<<CI_upper>>}",
		M = ifelse(opts$round_to == 'sigfigs', M, fmt(M, opts$round_interim)),
		z_crit = ifelse(opts$round_to == 'sigfigs', z_crit, fmt(z_crit, opts$round_z)),
		sigma_M = ifelse(opts$round_to == 'sigfigs', sigma_M, fmt(sigma_M, opts$round_interim)),
		marg_err = ifelse(opts$round_to == 'sigfigs', marg_err, fmt(marg_err, opts$round_interim)),
		CI_lower = ifelse(opts$round_to == 'sigfigs', CI_lower, fmt(CI_lower, opts$round_final)),
		CI_upper = ifelse(opts$round_to == 'sigfigs', CI_upper, fmt(CI_upper, opts$round_final)))

	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	# Return list containing both values and solution string
	# Interpretation is included if requested
	list(M = M,
			 sigma = if(missing(sigma)) NULL else sigma,
			 n = if(missing(n)) NULL else n,
			 sigma_M = sigma_M,
			 level = level,
			 z_crit = z_crit,
			 marg_err = marg_err,
			 CI = c(CI_lower = CI_lower, CI_upper = CI_upper),
			 CI_lower = CI_lower,
			 CI_upper = CI_upper,
			 solution = solution,
			 formula = CI.formula) %>%
		purrr::compact()
}

#' @rdname solve_ci_z
#'
#' @export
#'
ci_z_formula <- function(level = 0.95,
												 ...) {

	# Check argument validity
	stopifnot(is.numeric(level), level > 0, level < 1)

	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts<- get_handcalcs_opts(...)

	# Use the appropriate equals sign for an aligned environment
	equals <- ifelse(opts$use_aligned, "&=", "=")

	level <- rnd(level, opts$round_interim)

	solution <- lglue("<<level*100>>\\% \\ \\text{CI} <<equals>> M \\pm (z_{1 - \\alpha/2})(\\sigma_{M})")

	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	return(solution)
}



#' Confidence Intervals Around Sample Mean (using *t* distribution)
#'
#' Calculates confidence intervals around a sample mean when the population
#' standard deviation (\eqn{\sigma}) must be estimated using the sample standard
#' deviation (\eqn{s}, aka `SD`) \eqn{M \pm (t_{1 - \alpha/2})(s_{M})}. May
#' either provide `s_M`, or that value can be calculated from `SD` and `n`.
#'
#' Optionally, provide the value of mu (\eqn{mu}, the population standard
#' deviation under the null hypothesis), which will return the confidence
#' interval around the mean difference: \eqn{(M - \mu) \pm (t_{1 -
#' \alpha/2})(s_{M})}.
#'
#' Note that the critical value of *t* (`t_crit`) is rounded to the value of
#' `round_t` instead of the value of `round_interim` or `round_final` (see
#' [handcalcs_defaults()]).
#'
#' @param M Numeric scalar. Sample mean.
#' @param s,SD Numeric scalar. Sample standard deviation. May be named either
#'   `s` or `SD`. Required if s_M is not provided.
#' @param n,df Numeric scalar. Must supply either sample size (n) or degrees of
#'   freedom (df, \eqn{df = n - 1}). Whichever argument is not provided will be
#'   calculated.
#' @param s_M Numeric scalar. Standard error of the mean. If not provided, will
#'   be calculated from `SD` and `n` (using [solve_s_M()]) and included in the
#'   solution string.
#' @param mu Numeric scalar. Population mean under the null hypothesis. If
#'   provided, will return the confidence interval around the mean difference
#'   (\eqn{M - \mu}).
#' @param level Numeric scalar. Confidence level; defaults to 0.95 (95%
#'   confidence intervals).
#' @param ... Additional arguments to override default behaviors (see
#'   [handcalcs_defaults()]
#'
#' @return `solve_ci_t()` returns a list with the provided values (`M`, `SD`,
#'   `n`, `s_M`), the interim calculations (`s_M`, `t_crit`, `marg_err`), the
#'   final value (`CI_lower`, `CI_upper`, as well as a named vector containing
#'   both values `CI`), the solution string (`solution`), and the bare formula
#'   (`formula`) in LaTeX format. `ci_t_formula()` returns just the bare formula
#'   in LaTeX format as a character string.
#' @export
#'
#' @examples
#'
#' solve_ci_t(M = 55, s_M = 1, df = 99)
#'
#' # Defaults to 95% CIs, but other levels can be specified:
#' solve_ci_t(M = 55, s_M = 1, df = 99, level = 0.99)
#'
#' # Will calculate s_M if s and n (or df) are provided, and include the
#' # calculation in the solution string
#' solve_ci_t(M = 55, s = 10, n = 100)
#'
#' # Note: if s_M is provided, it takes precedence; s will be recalculated
#' solve_ci_t(M = 55, s = 10, n = 100, s_M = 4)
#'
#' # If mu is provided, will return the CI of the mean difference (M - mu).
#' solve_ci_t(M = 55, s_M = 1, n = 100, mu = 50)
#'
#' # If you just want the formula:
#' ci_t_formula()
#'
#' # To get the formula for the mean difference, supply a non-null value for mu:
#' ci_t_formula(mu = TRUE)
#'
solve_ci_t <- function(M,
											 s,
											 SD,
											 n,
											 df,
											 s_M,
											 mu = NULL,
											 level = 0.95,
											 ...) {
	# Check argument validity
	stopifnot(is.numeric(M), length(M) == 1)

	# If SD is supplied instead of s, pass that along as variance.
	if(missing(s) & !missing(SD)) s <- SD

	# Must supply either n or df; missing argument will be calculated.
	if(missing(n) & missing(df)) stop('Must supply either n or df.')
	if(missing(n)) n <- df + 1
	if(missing(df)) df <- n - 1

	if(missing(s_M) & missing(s)) stop('Must supply either s_M or s (SD).')

	if(!is.null(mu) & !is.numeric(mu)) stop('mu must be a numeric argument.')
	stopifnot(is.numeric(level), length(level) == 1, level > 0, level < 1)


	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts<- get_handcalcs_opts(...)

	# Use the appropriate equals sign for an aligned environment
	equals <- ifelse(opts$use_aligned, "&=", "=")

	# Calculate s_M from s and n
	if(missing(s_M)) {
		stopifnot(is.numeric(s), is.numeric(n))
		stopifnot(length(s) == 1, length(n) == 1)
		stopifnot(s > 0, n > 0)
		stopifnot(round(n) == n)

		s_M.lst <- solve_s_M(s = s,
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
		sqrt_n <- rnd(sqrt(n), opts$round_interim)
		s <- rnd(s_M*sqrt_n, opts$round_interim)
	}

	stopifnot(is.numeric(s_M), length(s_M) == 1, s_M > 0)

	if(missing(df)) df <- n - 1
	stopifnot(is.numeric(df), length(df) == 1, df > 0)

	# Round initial values
	M <- rnd(M, opts$round_interim)
	s_M <- rnd(s_M, opts$round_interim)

	# Estimate the Mean Diff if mu is provided.
	if(!is.null(mu)) {
		mu <- rnd(mu, opts$round_interim)
		M_diff <- M - mu
		estimate <- M_diff
	} else {
		M_diff <- NULL
		estimate <- M
	}

	# Calculate CI
	t_crit <- rnd(qt(p = (1-level)/2, df = df, lower.tail = FALSE), digits = opts$round_t)

	marg_err <- rnd(t_crit*s_M, opts$round_interim)
	CI_lower <- rnd(estimate - marg_err, opts$round_final)
	CI_upper <- rnd(estimate + marg_err, opts$round_final)


	# Get base formula without LaTeX math/aligned blocks
	CI.formula <- ci_t_formula(mu = mu,
														 level = level,
														 use_aligned = opts$use_aligned,
														 add_math = FALSE,
														 add_aligned = FALSE)

	# Prepend the solution for s_M if calculating from components
	CI.solution <- ifelse(!is.null(s_M.solution),
												paste(s_M.solution, ' \\\\ ', CI.formula),
												CI.formula)


	# Create the solution string, with rounded values (minimally displayed)
	solution <- glue_solution(
		CI.solution,
		{ if(!is.null(mu)) "<<equals>> (<<M>> - <<mu>>) \\pm (<<t_crit>>)(<<s_M>>)" },
		"<<equals>> <<estimate>> \\pm (<<t_crit>>)(<<s_M>>)",# = <<M>> \\pm <<marg_err>>",
		# "<<equals>> <<M>> \\pm <<marg_err>>",
		# "<<equals>> \\mathbf{(<<CI_lower>>, <<CI_upper>>)}",
		"\\text{Lower bound} <<equals>> <<M>> - <<marg_err>> = \\mathbf{<<CI_lower>>}",
		"\\text{Upper bound} <<equals>> <<M>> + <<marg_err>> = \\mathbf{<<CI_upper>>}",
		M = ifelse(opts$round_to == 'sigfigs', M, fmt(M, opts$round_interim)),
		t_crit = ifelse(opts$round_to == 'sigfigs', t_crit, fmt(t_crit, opts$round_t)),
		s_M = ifelse(opts$round_to == 'sigfigs', s_M, fmt(s_M, opts$round_interim)),
		marg_err = ifelse(opts$round_to == 'sigfigs', marg_err, fmt(marg_err, opts$round_interim)),
		CI_lower = ifelse(opts$round_to == 'sigfigs', CI_lower, fmt(CI_lower, opts$round_final)),
		CI_upper = ifelse(opts$round_to == 'sigfigs', CI_upper, fmt(CI_upper, opts$round_final)))

	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	# Return list containing both values and solution string
	# Interpretation is included if requested
	list(M = M,
			 s = if(missing(s)) NULL else s,
			 SD = if(missing(s)) NULL else s,
			 n = if(missing(n)) NULL else n,
			 df = df,
			 s_M = s_M,
			 mu = mu,
			 level = level,
			 t_crit = t_crit,
			 marg_err = marg_err,
			 CI = c(CI_lower = CI_lower, CI_upper = CI_upper),
			 CI_lower = CI_lower,
			 CI_upper = CI_upper,
			 solution = solution,
			 formula = CI.formula) %>%
		purrr::compact()
}

#' @rdname solve_ci_t
#'
#' @export
#'
ci_t_formula <- function(mu = NULL,
												 level = 0.95,
												 ...) {

	# Check argument validity
	stopifnot(is.numeric(level), level > 0, level < 1)

	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts<- get_handcalcs_opts(...)

	# Use the appropriate equals sign for an aligned environment
	equals <- ifelse(opts$use_aligned, "&=", "=")

	# Round CI level
	level <- rnd(level, opts$round_interim)

	# Estimate Mean or Mean Difference:
	estimate <- ifelse(!is.null(mu), "(M - \\mu)", "M")

	solution <- lglue("<<level*100>>\\% \\ \\text{CI} <<equals>> <<estimate>> \\pm (t_{1 - \\alpha/2})(s_{M})")

	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	return(solution)
}




# t (Mean Difference for paired samples):
# "95% CI = M_D \\pm (t_{\\alpha/2})(s_{M_{D}})"

# t (Mean Difference for independent samples):
# "95% CI = M_1 - M_2 \\pm (t_{\\alpha/2})(s_{(M_{1} - M_{2})})"
