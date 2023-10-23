#' Confidence Intervals Around Sample Mean (using *z* distribution)
#'
#' Calculates confidence intervals around a sample mean when the population
#' standard deviation (\eqn{\sigma}) is known: \eqn{M \pm (z^{*})(\sigma_{M})},
#' where \eqn{z^{*}} is the two-tailed critical value of *z* for 1 - the
#' confidence level (e.g., for 95% confidence intervals, the two-tailed value of
#' *z* for \eqn{\alpha = 1 - 0.95 = 0.05}). May either provide `sigma_M`, or
#' that value can be calculated from `sigma` and `n`.
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

	# Round CI level
	level <- rnd(level, opts$round_interim)
	alpha <- 1 - level

	# Solution string:
	solution <- lglue("<<level*100>>\\% \\ \\text{CI} <<equals>> M \\pm (z^{*})(\\sigma_{M})")

	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	return(solution)
}



#' Confidence Intervals Around Sample Mean (using *t* distribution)
#'
#' Calculates confidence intervals around a sample mean when the population
#' standard deviation (\eqn{\sigma}) must be estimated using the sample standard
#' deviation (\eqn{s}, aka `SD`) \eqn{M \pm (t^{*})(s_{M})}, where \eqn{t^{*}}
#' is the two-tailed critical value of *t* (with \eqn{df = n - 1}) for 1 - the
#' confidence level (e.g., for 95% confidence intervals, the two-tailed value of
#' *t* for \eqn{\alpha = 1 - 0.95 = 0.05}). May either provide `s_M`, or that
#' value can be calculated from `SD` and `n`.
#'
#' Optionally, provide the value of `mu` (\eqn{mu}), the predicted value of the
#' mean under the null hypothesis, which will return the confidence interval
#' around the mean difference: \eqn{(M - \mu) \pm (t_{1 - \alpha/2})(s_{M})}.
#'
#' You must provide either `df` or `n`. If one is provided, the other value will
#' be calculated (\eqn{df = n - 1}). If both are provided, this allows for the
#' possibility of errors that produce incorrect results. However, it also allows
#' for the possibility of using a different value of `df` than `n - 1`, for
#' instance of requiring use of a statistical table that does not have that
#' exact value of `df` listed.
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
#' @return `solve_ci_t()` returns a list with the provided values (`M`, `s`,
#'   `SD`, `n`, `s_M`), the interim calculations (`s_M`, `t_crit`, `marg_err`),
#'   the final value (`CI_lower`, `CI_upper`, as well as a named vector
#'   containing both values `CI`), the solution string (`solution`), and the
#'   bare formula (`formula`) in LaTeX format. `ci_t_formula()` returns just the
#'   bare formula in LaTeX format as a character string.
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

	# If SD is supplied instead of s, pass that along as standard deviation
	if(missing(s) & !missing(SD)) s <- SD

	# Must supply either n or df; missing argument will be calculated.
	if(missing(n) & missing(df)) stop('Must supply either n or df.')
	if(missing(n)) n <- df + 1
	if(missing(df)) df <- n - 1
	stopifnot(is.numeric(df), length(df) == 1, df > 0)
	stopifnot(is.numeric(n), length(n) == 1, df > 0)

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
		{ if(is.null(mu)) "<<equals>> <<M>> \\pm (<<t_crit>>)(<<s_M>>)" },
		"<<equals>> <<estimate>> \\pm <<marg_err>>",
		# "<<equals>> \\mathbf{(<<CI_lower>>, <<CI_upper>>)}",
		"\\text{Lower bound} <<equals>> <<estimate>> - <<marg_err>> = \\mathbf{<<CI_lower>>}",
		"\\text{Upper bound} <<equals>> <<estimate>> + <<marg_err>> = \\mathbf{<<CI_upper>>}",
		M = ifelse(opts$round_to == 'sigfigs', M, fmt(M, opts$round_interim)),
		estimate = ifelse(opts$round_to == 'sigfigs', estimate, fmt(estimate, opts$round_interim)),
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
	alpha <- 1 - level

	# Estimate Mean or Mean Difference:
	estimate <- ifelse(!is.null(mu), "(M - \\mu)", "M")

	# Solution string:
	solution <- lglue("<<level*100>>\\% \\ \\text{CI} <<equals>> <<estimate>> \\pm (t^{*})(s_{M})")

	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	return(solution)
}





#' Confidence Intervals Around Mean Difference Score (using *t* distribution)
#'
#' Calculates confidence intervals around the mean difference score \eqn{M_{D}
#' \pm (t^{*})(s_{M_{D}})}, where \eqn{t^{*}} is the two-tailed critical value
#' of *t* (with \eqn{df = n - 1}) for 1 - the confidence level (e.g., for 95%
#' confidence intervals, the two-tailed value of *t* for \eqn{\alpha = 1 - 0.95
#' = 0.05}). May either provide `s_M_D`, or that value can be calculated from
#' `SD_D` and `n`.
#'
#' Optionally, provide the value of `mu_D` (\eqn{mu_{D}}), the value of the mean
#' difference predicted by the null hypothesis), which will return the
#' confidence interval around the mean difference: \eqn{(M_{D} - \mu_{D}) \pm
#' (t_{1 - \alpha/2})(s_{M_{D}})}.
#'
#' You must provide either `df` or `n`. If one is provided, the other value will
#' be calculated (\eqn{df = n - 1}). If both are provided, this allows for the
#' possibility of errors that produce incorrect results. However, it also allows
#' for the possibility of using a different value of `df` than `n - 1`, for
#' instance of requiring use of a statistical table that does not have that
#' exact value of `df` listed.
#'
#' Note that the critical value of *t* (`t_crit`) is rounded to the value of
#' `round_t` instead of the value of `round_interim` or `round_final` (see
#' [handcalcs_defaults()]).
#'
#' @param M_D Numeric scalar. Mean of the difference scores.
#' @param s_D,SD_D Numeric scalar. Standard deviation of the difference scores.
#'   May be named either `s_D` or `SD_D`. Required if s_M_D is not provided.
#' @param n,df Numeric scalar. Must supply either sample size (n) or degrees of
#'   freedom (df, \eqn{df = n - 1}). Whichever argument is not provided will be
#'   calculated.
#' @param s_M_D Numeric scalar. Standard error of the mean of the difference
#'   scores. If not provided, will be calculated from `s_D` (or `SD_D`) and `n`
#'   (using [solve_s_M_D()]) and included in the solution string.
#' @param mu_D Numeric scalar. Predicted mean difference score under the null
#'   hypothesis. If provided, will return the confidence interval around the
#'   mean difference (\eqn{M_{D} - \mu_{D}}).
#' @param level Numeric scalar. Confidence level; defaults to 0.95 (95%
#'   confidence intervals).
#' @param ... Additional arguments to override default behaviors (see
#'   [handcalcs_defaults()]
#'
#' @return `solve_ci_t_paired()` returns a list with the provided values
#'   (`M_D`,`s_D`, `SD_D`, `n`, `s_M_D`), the interim calculations (`s_M_D`,
#'   `t_crit`, `marg_err`), the final value (`CI_lower`, `CI_upper`, as well as
#'   a named vector containing both values `CI`), the solution string
#'   (`solution`), and the bare formula (`formula`) in LaTeX format.
#'   `ci_t_paired_formula()` returns just the bare formula in LaTeX format as a
#'   character string.
#' @export
#'
#' @examples
#'
#' solve_ci_t_paired(M_D = 5, s_M_D = 1, df = 99)
#'
#' # Defaults to 95% CIs, but other levels can be specified:
#' solve_ci_t_paired(M_D = 5, s_M_D = 1, df = 99, level = 0.99)
#'
#' # Will calculate s_M_D if s_D and n (or df) are provided, and include the
#' # calculation in the solution string
#' solve_ci_t_paired(M_D = 5, s_D = 10, n = 100)
#'
#' # Note: if s_M_D is provided, it takes precedence; s_D will be recalculated
#' solve_ci_t_paired(M_D = 5, s_D = 10, n = 100, s_M_D = 4)
#'
#' # If mu is provided, will return the CI of the mean difference (M_D - mu_D).
#' solve_ci_t_paired(M_D = 5, s_M_D = 1, n = 100, mu_D = 0)
#'
#' # If you just want the formula:
#' ci_t_paired_formula()
#'
#' # To get the formula for the mean difference, supply a non-null value for mu:
#' ci_t_paired_formula(mu_D = TRUE)
#'
solve_ci_t_paired <- function(M_D,
															s_D,
															SD_D,
															n,
															df,
															s_M_D,
															mu_D = NULL,
															paired = FALSE,
															level = 0.95,
															...) {
	# Check argument validity
	stopifnot(is.numeric(M_D), length(M_D) == 1)

	# If SD is supplied instead of s, pass that along as variance.
	if(missing(s_D) & !missing(SD_D)) s_D <- SD_D

	# Must supply either n or df; missing argument will be calculated.
	if(missing(n) & missing(df)) stop('Must supply either n or df.')
	if(missing(n)) n <- df + 1
	if(missing(df)) df <- n - 1

	if(missing(s_M_D) & missing(s_D)) stop('Must supply either s_M_D or s_D (SD_D).')

	if(!is.null(mu_D) & !is.numeric(mu_D)) stop('mu_D must be a numeric argument.')
	stopifnot(is.numeric(level), length(level) == 1, level > 0, level < 1)


	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts<- get_handcalcs_opts(...)

	# Use the appropriate equals sign for an aligned environment
	equals <- ifelse(opts$use_aligned, "&=", "=")

	# Calculate s_M_D from s_D and n
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
		s_M_D.solution <- s_M_D.lst$solution
	} else {
		s_M_D.solution <- NULL
		sqrt_n <- rnd(sqrt(n), opts$round_interim)
		s_D <- rnd(s_M_D*sqrt_n, opts$round_interim)
	}

	stopifnot(is.numeric(s_M_D), length(s_M_D) == 1, s_M_D > 0)

	if(missing(df)) df <- n - 1
	stopifnot(is.numeric(df), length(df) == 1, df > 0)

	# Round initial values
	M_D <- rnd(M_D, opts$round_interim)
	s_M_D <- rnd(s_M_D, opts$round_interim)

	# Estimate the Mean Diff if mu is provided.
	if(!is.null(mu_D)) {
		mu_D <- rnd(mu_D, opts$round_interim)
		M_diff <- M_D - mu_D
		estimate <- M_diff
	} else {
		M_diff <- NULL
		estimate <- M_D
	}

	# Calculate CI
	t_crit <- rnd(qt(p = (1-level)/2, df = df, lower.tail = FALSE), digits = opts$round_t)

	marg_err <- rnd(t_crit*s_M_D, opts$round_interim)
	CI_lower <- rnd(estimate - marg_err, opts$round_final)
	CI_upper <- rnd(estimate + marg_err, opts$round_final)


	# Get base formula without LaTeX math/aligned blocks
	CI.formula <- ci_t_paired_formula(mu_D = mu_D,
																		level = level,
																		use_aligned = opts$use_aligned,
																		add_math = FALSE,
																		add_aligned = FALSE)

	# Prepend the solution for s_M if calculating from components
	CI.solution <- ifelse(!is.null(s_M_D.solution),
												paste(s_M_D.solution, ' \\\\ ', CI.formula),
												CI.formula)


	# Create the solution string, with rounded values (minimally displayed)
	solution <- glue_solution(
		CI.solution,
		{ if(!is.null(mu_D)) "<<equals>> (<<M_D>> - <<mu_D>>) \\pm (<<t_crit>>)(<<s_M_D>>)" },
		"<<equals>> <<estimate>> \\pm (<<t_crit>>)(<<s_M_D>>)",# = <<M>> \\pm <<marg_err>>",
		# "<<equals>> <<M>> \\pm <<marg_err>>",
		# "<<equals>> \\mathbf{(<<CI_lower>>, <<CI_upper>>)}",
		"\\text{Lower bound} <<equals>> <<M_D>> - <<marg_err>> = \\mathbf{<<CI_lower>>}",
		"\\text{Upper bound} <<equals>> <<M_D>> + <<marg_err>> = \\mathbf{<<CI_upper>>}",
		M_D = ifelse(opts$round_to == 'sigfigs', M_D, fmt(M_D, opts$round_interim)),
		t_crit = ifelse(opts$round_to == 'sigfigs', t_crit, fmt(t_crit, opts$round_t)),
		s_M_D = ifelse(opts$round_to == 'sigfigs', s_M_D, fmt(s_M_D, opts$round_interim)),
		marg_err = ifelse(opts$round_to == 'sigfigs', marg_err, fmt(marg_err, opts$round_interim)),
		CI_lower = ifelse(opts$round_to == 'sigfigs', CI_lower, fmt(CI_lower, opts$round_final)),
		CI_upper = ifelse(opts$round_to == 'sigfigs', CI_upper, fmt(CI_upper, opts$round_final)))

	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	# Return list containing both values and solution string
	# Interpretation is included if requested
	list(M_D = M_D,
			 s_D = if(missing(s_D)) NULL else s_D,
			 SD_D = if(missing(s_D)) NULL else s_D,
			 n = if(missing(n)) NULL else n,
			 df = df,
			 s_M_D = s_M_D,
			 mu_D = mu_D,
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

#' @rdname solve_ci_t_paired
#'
#' @export
#'
ci_t_paired_formula <- function(mu_D = NULL,
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
	alpha <- 1 - level

	# Estimate Mean or Mean Difference:
	estimate <- ifelse(!is.null(mu_D), "(M_{D} - \\mu_{D})", "M_{D}")

	# Solution string:
	solution <- lglue("<<level*100>>\\% \\ \\text{CI} <<equals>> <<estimate>> \\pm (t^{*})(s_{M_{D}})")

	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	return(solution)
}


#' Confidence Intervals of Difference Between Independent Means (using *t*
#' distribution)
#'
#' Calculates confidence intervals for the difference between two independent
#' sample means: \eqn{(M_{1} - M_{2}) \pm (t^{*})(s_{M_{1} - M_{2}})}, where
#' \eqn{t^{*}} is the two-tailed critical value of *t* (with \eqn{df = n_1 + n_2
#' - 2}) for 1 - the confidence level (e.g., for 95% confidence intervals, the
#' two-tailed value of *t* for \eqn{\alpha = 1 - 0.95 = 0.05}). May either
#' provide `s_M_diff`, or that value can be calculated by providing the sample
#' sizes (`n_1` and `n_2`) and either the pooled variance (`s_p2`) or the two
#' sample standard deviations (`s_1` and `s_2`).
#'
#' You must provide either `df` or the two sample sizes (`n_1` and `n_2`). If
#' sample sizes are provided, `df` will be calculated (\eqn{df = n_1 + n_2 -
#' 2}). If both are provided, `df` will be used, which allows for the
#' possibility of conflicting values that produce undesired results. However, it
#' also allows for the possibility of using a different value of `df` than
#' \eqn{n_1 + n_2 - 2}, such as when using a statistical table that does not
#' have that exact value of `df` listed.
#'
#' Note that the critical value of *t* (`t_crit`) is rounded to the value of
#' `round_t` instead of the value of `round_interim` or `round_final` (see
#' [handcalcs_defaults()]).
#'
#' @param M_1,M_2 Numeric scalars. Means of samples 1 and 2.
#' @param s_1,s_2,SD_1,SD_2 Numeric scalars. Standard deviations of samples 1
#'   and 2. May be named either `s_1` and `s_2` or `SD_1` and `SD_2`. Required
#'   if not providing pooled variance (`s_p2`) or the standard error of the
#'   differences (`s_M_diff`).
#' @param s_p2 Numeric scalar. Pooled variance. Will be calculated from sample
#'   standard deviations (`s_1`, `s_2`) and sample sizes (`n_1` and `n_2`) if
#'   those values are provided.
#' @param n_1,n_2 Numeric scalars. Sample sizes of samples 1 and 2. Required if
#'   s_M_diff is not provided. Also required if `df` is not provided.
#' @param df  Numeric scalar. Degrees of freedom (df, \eqn{df = n_1 + n_2 - 1}).
#'   If not provided, will be calculated from `n_1` and `n_2`. calculated.
#' @param s_M_diff Numeric scalar. Standard error of the difference of the
#'   means. If not provided, will be calculated (using [solve_s_M_diff()]) from
#'   sample sizes (`n_1` and `n_2`) and standard deviations (`s_1`, `s_2`) and
#'   included in the solution string.
#' @param level Numeric scalar. Confidence level; defaults to 0.95 (95%
#'   confidence intervals).
#' @param ... Additional arguments to override default behaviors (see
#'   [handcalcs_defaults()]
#'
#' @return `solve_ci_t_indep()` returns a list with all provided values (`M_1`,
#'   `M_2`, `s_1`, `s_2`, `SD_1`, `SD_2`, `n_1`, `n_2`, `df`, `s_M_diff`,
#'   `level`), the interim calculations (`s_M_diff`, `t_crit`, `marg_err`), the
#'   final value (`CI_lower`, `CI_upper`, as well as a named vector containing
#'   both values `CI`), the solution string (`solution`), and the bare formula
#'   (`formula`) in LaTeX format. `ci_t_indep_formula()` returns just the bare
#'   formula in LaTeX format as a character string.
#' @export
#'
#' @examples
#'
#' solve_ci_t_indep(M_1 = 55, M_2 = 50, s_M_diff = 1, df = 98)
#'
#' # Defaults to 95% CIs, but other levels can be specified:
#' solve_ci_t_indep(M_1 = 55, M_2 = 50, s_M_diff = 1, df = 98, level = 0.99)
#'
#' # If s_M_diff is not provided, can be calculated from sample sizes and either
#' # pooled variance (s_p2):
#' solve_ci_t_indep(M_1 = 50, M_2 = 45, n_1 = 50, n_2 = 50, s_p2 = 0.5)
#'
#' # or standard deviations (s_1 and s_2)
#' solve_ci_t_indep(M_1 = 50, M_2 = 45, n_1 = 50, n_2 = 50, s_1 = 1.5, s_2 = 2.0)
#'
#' # If you just want the formula:
#' ci_t_indep_formula()
#'
#'
solve_ci_t_indep <- function(M_1, M_2,
														 s_1, s_2,
														 SD_1, SD_2,
														 s_p2,
														 n_1, n_2,
														 df,
														 s_M_diff,
														 level = 0.95,
														 ...) {
	# Check argument validity
	stopifnot(is.numeric(M_1), length(M_1) == 1)
	stopifnot(is.numeric(M_2), length(M_2) == 1)

	# If SD is supplied instead of s, pass that along as standard deviation.
	if(missing(s_1) & !missing(SD_1)) s_1 <- SD_1
	if(missing(s_2) & !missing(SD_2)) s_2 <- SD_2

	# Must supply either n_1 and n_2 or df.
	if((missing(n_1) | missing(n_2)) & missing(df)) stop('Must supply either sample sizes (n_1 and n_2) or df.')
	if(missing(df)) df <- (n_1 - 1) + (n_2 - 1)
	stopifnot(is.numeric(df), length(df) == 1, df > 0)

	# If standard error of the differences is not provided, must supply sample
	# sizes and either the pooled variance or sample standard deviations
	if(missing(s_M_diff)) {
		if(missing(n_1) | missing(n_2)) stop('If not supplying standard error of the differences (s_M_diff), must include sample sizes (n_1 and n_2)')
		if(missing(s_p2) & (missing(s_1) | missing(s_2))) stop('If not supplying standard error of the differences (s_M_diff), must include either pooled variance (s_p2) or sample standard deviations (s_1 and s_2)')
	}

	# Make sure level argument is valid
	stopifnot(is.numeric(level), length(level) == 1, level > 0, level < 1)

	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts <- get_handcalcs_opts(...)

	# Use the appropriate equals sign for an aligned environment
	equals <- ifelse(opts$use_aligned, "&=", "=")

	# Calculate s_M_diff sample sizes and pooled variance or standard deviations
	if(missing(s_M_diff)) {
		# Verify sample size arguments:
		stopifnot(is.numeric(n_1), is.numeric(n_2))
		stopifnot(length(n_1) == 1, length(n_2) == 1)
		stopifnot(n_1 > 0, n_2 > 0)
		stopifnot(round(n_1) == n_1, round(n_2) == n_2)

		# Use either pooled variance or sample standard deviations:
		if(!missing(s_p2)) {
			stopifnot(is.numeric(s_p2))
			stopifnot(length(s_p2) == 1)
			stopifnot(s_p2 > 0)

			s_M_diff.lst <- solve_s_M_diff(s_p2 = s_p2,
																		 n_1 = n_1,
																		 n_2 = n_2,
																		 round_interim = opts$round_interim,
																		 round_final = opts$round_interim,
																		 add_math = FALSE,
																		 add_aligned = FALSE,
																		 use_aligned = opts$use_aligned)
		} else {
			stopifnot(is.numeric(s_1), is.numeric(s_2))
			stopifnot(length(s_1) == 1, length(s_2) == 1)
			stopifnot(s_1 > 0, s_2 > 0)

			s_M_diff.lst <- solve_s_M_diff(s_1 = s_1,
																		 s_2 = s_2,
																		 n_1 = n_1,
																		 n_2 = n_2,
																		 round_interim = opts$round_interim,
																		 round_final = opts$round_interim,
																		 add_math = FALSE,
																		 add_aligned = FALSE,
																		 use_aligned = opts$use_aligned)
		}

		s_M_diff <- s_M_diff.lst$s_M_diff
		s_p2 <- s_M_diff.lst$s_p2
		s_M_diff.solution <- s_M_diff.lst$solution
	} else {
		s_M_diff.solution <- NULL
	}

	stopifnot(is.numeric(s_M_diff), length(s_M_diff) == 1, s_M_diff > 0)

	# Round initial values
	M_1 <- rnd(M_1, opts$round_interim)
	M_2 <- rnd(M_2, opts$round_interim)
	s_M_diff <- rnd(s_M_diff, opts$round_interim)

	# Calculate CI
	M_diff <- rnd(M_1 - M_2, opts$round_interim)

	t_crit <- rnd(qt(p = (1-level)/2, df = df, lower.tail = FALSE), digits = opts$round_t)

	marg_err <- rnd(t_crit*s_M_diff, opts$round_interim)
	CI_lower <- rnd(M_diff - marg_err, opts$round_final)
	CI_upper <- rnd(M_diff + marg_err, opts$round_final)


	# Get base formula without LaTeX math/aligned blocks
	CI.formula <- ci_t_indep_formula(level = level,
																	 use_aligned = opts$use_aligned,
																	 add_math = FALSE,
																	 add_aligned = FALSE)

	# Prepend the solution for s_M_diff if calculating from components
	CI.solution <- ifelse(!is.null(s_M_diff.solution),
												paste(s_M_diff.solution, ' \\\\ ', CI.formula),
												CI.formula)


	# Create the solution string, with rounded values (minimally displayed)
	solution <- glue_solution(
		CI.solution,
		"<<equals>> (<<M_1>> - <<M_2>>) \\pm (<<t_crit>>)(<<s_M_diff>>)",
		"<<equals>> <<M_diff>> \\pm <<marg_err>>",
		"\\text{Lower bound} <<equals>> <<M_diff>> - <<marg_err>> = \\mathbf{<<CI_lower>>}",
		"\\text{Upper bound} <<equals>> <<M_diff>> + <<marg_err>> = \\mathbf{<<CI_upper>>}",
		M_1 = ifelse(opts$round_to == 'sigfigs', M_1, fmt(M_1, opts$round_interim)),
		M_2 = ifelse(opts$round_to == 'sigfigs', M_2, fmt(M_2, opts$round_interim)),
		M_diff = ifelse(opts$round_to == 'sigfigs', M_diff, fmt(M_diff, opts$round_interim)),
		t_crit = ifelse(opts$round_to == 'sigfigs', t_crit, fmt(t_crit, opts$round_t)),
		s_M_diff = ifelse(opts$round_to == 'sigfigs', s_M_diff, fmt(s_M_diff, opts$round_interim)),
		marg_err = ifelse(opts$round_to == 'sigfigs', marg_err, fmt(marg_err, opts$round_interim)),
		CI_lower = ifelse(opts$round_to == 'sigfigs', CI_lower, fmt(CI_lower, opts$round_final)),
		CI_upper = ifelse(opts$round_to == 'sigfigs', CI_upper, fmt(CI_upper, opts$round_final)))

	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	# Return list containing both values and solution string
	# Interpretation is included if requested
	list(M_1 = M_1,
			 M_2 = M_2,
			 s_1 = if(missing(s_1)) NULL else s_1,
			 s_2 = if(missing(s_2)) NULL else s_2,
			 SD_1 = if(missing(s_1)) NULL else s_1,
			 SD_2 = if(missing(s_2)) NULL else s_2,
			 s_p2 = if(missing(s_p2)) NULL else s_p2,
			 n_1 = if(missing(n_1)) NULL else n_1,
			 n_2 = if(missing(n_2)) NULL else n_2,
			 df = df,
			 s_M_diff = s_M_diff,
			 level = level,
			 M_diff,
			 t_crit = t_crit,
			 marg_err = marg_err,
			 CI = c(CI_lower = CI_lower, CI_upper = CI_upper),
			 CI_lower = CI_lower,
			 CI_upper = CI_upper,
			 solution = solution,
			 formula = CI.formula) %>%
		purrr::compact()
}

#' @rdname solve_ci_t_indep
#'
#' @export
#'
ci_t_indep_formula <- function(level = 0.95,
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
	alpha <- 1 - level

	# Solution string:
	solution <- lglue("<<level*100>>\\% \\ \\text{CI} <<equals>> (M_{1} - M_{2}) \\pm (t^{*})(s_{M_{1} - M_{2}})")

	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	return(solution)
}

# t (Mean Difference for independent samples):
# "95% CI = M_1 - M_2 \\pm (t_{\\alpha/2})(s_{(M_{1} - M_{2})})"


