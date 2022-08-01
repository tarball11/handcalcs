#' Confidence Intervals Around Sample Mean (using *z* distribution)
#'
#' Calculates confidence intervals around a sample mean when the population
#' standard deviation (\eqn{\sigma}) is known: \eqn{M \pm (z_{1 -
#' \alpha/2})(\sigma_{M})}. May either provide `sigma_M`, or that value can be
#' calculated from `sigma` and `N`.
#'
#' @param M Numeric scalar. Sample mean.
#' @param sigma Numeric scalar. Population standard deviation. Required if
#'   sigma_M is not provided.
#' @param N Numeric scalar. Sample size. Required if sigma_M is not provided.
#' @param sigma_M Numeric scalar. Standard error of the mean. If not provided,
#'   will be calculated from `sigma` and `N` (using [solve_sigma_M()]) and
#'   included in the solution string.
#' @param level Numeric scalar. Confidence level; defaults to 0.95 (95%
#'   confidence intervals).
#' @param ... Additional arguments to override default behaviors (see
#'   [handcalcs_defaults()]
#'
#' @return `solve_ci_z()` returns a list with the provided values (`M`, `sigma`,
#'   `N`, `sigma_M`), the interim calculations (`sigma_M`, `z_crit`,
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
#' # Will calculate sigma_M if sigma and N are provided, and include the
#' # calculation in the solution string
#' solve_ci_z(M = 5, sigma = 2, N = 100)
#'
#' # Note: if sigma_M is provided, will ignore sigma and N values
#' solve_ci_z(M = 5, sigma = 2, N = 100, sigma_M = 4)
#'
#' # If you just want the formula:
#' ci_z_formula()
#'
solve_ci_z <- function(M,
											 sigma,
											 N,
											 sigma_M,
											 level = 0.95,
											 ...) {
	# Check argument validity
	stopifnot(is.numeric(M), length(M) == 1)
	stopifnot(is.numeric(level), length(level) == 1, level > 0, level < 1)

	if(missing(sigma_M) & (missing(sigma) | missing(N))) {
		stop('Must supply either sigma_M or sigma and N')
	}

	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts<- get_handcalcs_opts(...)

	# Use the appropriate equals sign for an aligned environment
	equals <- ifelse(opts$use_aligned, "&=", "=")

	# Calculate sigma_M from sigma and N
	if(missing(sigma_M)) {
		stopifnot(is.numeric(sigma), is.numeric(N))
		stopifnot(length(sigma) == 1, length(N) == 1)
		stopifnot(sigma > 0, N > 0)
		stopifnot(round(N) == N)

		sigma_M.lst <- solve_sigma_M(sigma = sigma,
																 N = N,
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
			 N = if(missing(N)) NULL else N,
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



# t (One-sample Mean):
# "95% CI = M \\pm (t_{\\alpha/2})(s_{M})"

# t (Mean Difference for paired samples):
# "95% CI = M_D \\pm (t_{\\alpha/2})(s_{M_{D}})"

# t (Mean Difference for independent samples):
# "95% CI = M_1 - M_2 \\pm (t_{\\alpha/2})(s_{(M_{1} - M_{2})})"
