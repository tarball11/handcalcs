#' Interpret Cohen's d
#'
#' @param d Numeric. Value of Cohen's d (effect size in standard deviation
#'   units).
#' @param ... Additional arguments to override default behaviors (see
#'   [handcalcs_defaults()]
#'
#' @return Interpretation as a character string: small (d <= 0.2), medium (0.2 >
#'   d <= 0.8), or large (d > 0.8)
#' @export
#'
#' @examples
#' interpret_d(seq(0, 1, by = 0.1))
interpret_d<- function(d,
											 ...) {
	stopifnot(is.numeric(d))

	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts<- get_handcalcs_opts(...)

	d <- rnd(abs(d), opts$round_final)

	dplyr::case_when(d < 0.1 ~ 'trivial',
									 d <= 0.25 ~ 'small',
									 d < 0.4 ~ 'small-medium',
									 d <= 0.6 ~ 'medium',
									 d < 0.75 ~ 'medium-large',
									 d >= 0.75 ~ 'large')
}


#' Effect size (Cohen's d) for one-sample *z*-test
#'
#' Calculates and presents solution string for effect size (Cohen's d) comparing
#' a single sample to a population mean when the population standard deviation
#' is known: difference between observed sample mean (`M`) and population mean
#' (`mu`) in units of the population standard deviation (`sigma`).
#'
#' @param M Numeric scalar. Sample mean.
#' @param mu Numeric scalar. Population mean.
#' @param sigma Numeric scalar. Population standard deviation.
#' @param interpret Logical scalar. Should the interpretation (as produced by
#'   [interpret_d()] be included in the solution string?)
#' @param ... Additional arguments to override default behaviors (see
#'   [handcalcs_defaults()]
#'
#' @return `solve_cohens_d_one_z()` returns a list with the provided values
#'   (`M`, `mu`, `sigma`), the interim calculations (`M_diff`), the final
#'   value (`d`), the solution string (`solution`), and the bare formula
#'   (`formula`) in LaTeX format. `cohens_d_one_z_formula()` returns just the
#'   bare formula in LaTeX format as a character string.
#' @export
#'
#' @examples
#'
#' solve_cohens_d_one_z(M = 6, mu = 5, sigma = 2)
#'
#' # Include the interpretation of the effect size:
#' solve_cohens_d_one_z(M = 6, mu = 5, sigma = 2, interpret = TRUE)
#'
#' # If you just want the formula:
#' cohens_d_one_z_formula()
solve_cohens_d_one_z <- function(M,
																 mu,
																 sigma,
																 interpret = FALSE,
																 ...) {
	# Check argument validity
	stopifnot(is.numeric(M), length(M) == 1)
	stopifnot(is.numeric(mu), length(mu) == 1)
	stopifnot(is.numeric(sigma), length(sigma) == 1, sigma > 0)

	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts<- get_handcalcs_opts(...)

	# Use the appropriate equals sign for an aligned environment
	equals <- ifelse(opts$use_aligned, "&=", "=")

	# Round initial values
	M <- rnd(M, opts$round_interim)
	mu <- rnd(mu, opts$round_interim)
	sigma <- rnd(sigma, opts$round_interim)

	# Calculate d
	M_diff <- rnd(M - mu, opts$round_interim)
	d <- rnd(M_diff / sigma, opts$round_final)

	# Get base formula without LaTeX math/aligned blocks
	d.formula <- cohens_d_one_z_formula(use_aligned = opts$use_aligned,
																			add_math = FALSE,
																			add_aligned = FALSE)

	# Get the interpretation (if requested)
	d.interp <- ifelse(interpret, paste0("(", interpret_d(d), ")"), '')

	# Create the solution string, with rounded values (minimally displayed)
	solution <- glue_solution(
		d.formula,
		"<<equals>> \\frac{<<M>> - <<mu>>}{<<sigma>>} = \\frac{<<M_diff>>}{<<sigma>>} = \\mathbf{<<d>>} <<d.interp>>",
		# Round based on the final calculated value unless round_to is set to
		# 'sigfigs', in which case just present the final rounded value as is.
		d = ifelse(opts$round_to == 'sigfigs', d, fmt(d, get_digits(d, opts$round_final))))

	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	# Return list containing both values and solution string
	# Interpretation is included if requested
	list(M = M,
			 mu = mu,
			 sigma = sigma,
			 M_diff = M_diff,
			 d = d,
			 d.interp = if(!interpret) NULL else interpret_d(d),
			 solution = solution,
			 formula = d.formula) %>%
		purrr::compact()
}

#' @rdname solve_cohens_d_one_z
#'
#' @export
#'
cohens_d_one_z_formula <- function(...) {
	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts<- get_handcalcs_opts(...)

	# Use the appropriate equals sign for an aligned environment
	equals <- ifelse(opts$use_aligned, "&=", "=")

	solution <- lglue("d <<equals>> \\frac{M - \\mu}{\\sigma}")

	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	return(solution)
}

#' Effect site (Cohen's d) for one-sample *t*-test
#'
#' Calculates and presents solution string for effect site (Cohen's d) comparing
#' a single sample to a population mean when the population standard deviation
#' is *not* known (and must be estimated): difference between observed sample
#' mean (`M`) and population mean (`mu`) in units of the sample standard
#' deviation (`SD`).
#'
#' @param M Numeric scalar. Sample mean.
#' @param mu Numeric scalar. Population mean.
#' @param SD Numeric scalar. Sample standard deviation.
#' @param interpret Logical scalar. Should the interpretation (as produced by
#'   [interpret_d()] be included in the solution string?)
#' @param ... Additional arguments to override default behaviors (see
#'   [handcalcs_defaults()]
#'
#' @return `solve_cohens_d_one_t()` returns a list with the provided values
#'   (`M`, `mu`, `SD`), the interim calculations (`M_diff`), the final value
#'   (`d`), the solution string (`solution`), and the bare formula (`formula`)
#'   in LaTeX format. `cohens_d_one_t_formula()` returns just the bare formula
#'   in LaTeX format as a character string.
#' @export
#'
#' @examples
#'
#' solve_cohens_d_one_t(M = 6, mu = 5, SD = 2)
#'
#' # Include the interpretation of the effect size:
#' solve_cohens_d_one_t(M = 6, mu = 5, SD = 2, interpret = TRUE)
#'
#' # If you just want the formula:
#' cohens_d_one_t_formula()
solve_cohens_d_one_t <- function(M,
																 mu,
																 SD,
																 interpret = FALSE,
																 ...) {
	# Check argument validity
	stopifnot(is.numeric(M), length(M) == 1)
	stopifnot(is.numeric(mu), length(mu) == 1)
	stopifnot(is.numeric(SD), length(SD) == 1, SD > 0)

	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts<- get_handcalcs_opts(...)

	# Use the appropriate equals sign for an aligned environment
	equals <- ifelse(opts$use_aligned, "&=", "=")

	# Round initial values
	M <- rnd(M, opts$round_interim)
	mu <- rnd(mu, opts$round_interim)
	SD <- rnd(SD, opts$round_interim)

	# Calculate d
	M_diff <- rnd(M - mu, opts$round_interim)
	d <- rnd(M_diff / SD, opts$round_final)

	# Get base formula without LaTeX math/aligned blocks
	d.formula <- cohens_d_one_t_formula(use_aligned = opts$use_aligned,
																			add_math = FALSE,
																			add_aligned = FALSE)

	# Get the interpretation (if requested)
	d.interp <- ifelse(interpret, paste0("(", interpret_d(d), ")"), '')

	# Create the solution string, with rounded values (minimally displayed)
	solution <- glue_solution(
		d.formula,
		"<<equals>> \\frac{<<M>> - <<mu>>}{<<SD>>} = \\frac{<<M_diff>>}{<<SD>>} = \\mathbf{<<d>>} <<d.interp>>",
		# Round based on the final calculated value unless round_to is set to
		# 'sigfigs', in which case just present the final rounded value as is.
		d = ifelse(opts$round_to == 'sigfigs', d, fmt(d, get_digits(d, opts$round_final))))

	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	# Return list containing both values and solution string
	# Interpretation is included if requested
	list(M = M,
			 mu = mu,
			 SD = SD,
			 M_diff = M_diff,
			 d = d,
			 d.interp = if(!interpret) NULL else interpret_d(d),
			 solution = solution,
			 formula = d.formula) %>%
		purrr::compact()
}

#' @rdname solve_cohens_d_one_t
#'
#' @export
#'
cohens_d_one_t_formula <- function(...) {
	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts<- get_handcalcs_opts(...)

	# Use the appropriate equals sign for an aligned environment
	equals <- ifelse(opts$use_aligned, "&=", "=")

	solution <- lglue("d <<equals>> \\frac{M - \\mu}{s}")

	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	return(solution)
}

#' Effect site (Cohen's d) for paired-samples *t*-test
#'
#' Calculates and presents solution string for effect site (Cohen's d) comparing
#' sample means from related (paired) samples: the mean of the difference scores
#' (`M_D`) compared to the expected difference (`mu_D`) in units of the standard
#' deviation of the difference scores (`SD_D`).
#'
#' @param M_D Numeric scalar. Mean of the difference scores between samples.
#' @param mu_D Numeric scalar. Population mean (default: 0).
#' @param SD_D Numeric scalar. Standard deviation of the difference scores.
#' @param interpret Logical scalar. Should the interpretation (as produced by
#'   [interpret_d()] be included in the solution string?)
#' @param ... Additional arguments to override default behaviors (see
#'   [handcalcs_defaults()]
#'
#' @return `solve_cohens_d_paired_t()` returns a list with the provided values
#'   (`M_D`, `mu_D`, `SD_D`), the interim calculations (`M_diff`), the final
#'   value (`d`), the solution string (`solution`), and the bare formula
#'   (`formula`) in LaTeX format. `cohens_d_paired_t_formula()` returns just the
#'   bare formula in LaTeX format as a character string.
#' @export
#'
#' @examples
#'
#' solve_cohens_d_paired_t(M_D = 1, SD_D = 2)
#'
#' # Include the interpretation of the effect size:
#' solve_cohens_d_paired_t(M_D = 1, SD_D = 2, interpret = TRUE)
#'
#' # Although mu is virtually always zero, you can set it to be something else:
#' solve_cohens_d_paired_t(M_D = 1, mu_D = 0.5, SD_D = 2)
#'
#' # If you just want the formula:
#' cohens_d_paired_t_formula()
#'
solve_cohens_d_paired_t <- function(M_D,
																		mu_D = 0,
																		SD_D,
																		interpret = FALSE,
																		...) {
	# Check argument validity
	stopifnot(is.numeric(M_D), length(M_D) == 1)
	stopifnot(is.numeric(mu_D), length(mu_D) == 1)
	stopifnot(is.numeric(SD_D), length(SD_D) == 1, SD_D > 0)

	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts<- get_handcalcs_opts(...)

	# Use the appropriate equals sign for an aligned environment
	equals <- ifelse(opts$use_aligned, "&=", "=")

	# Round initial values
	M_D <- rnd(M_D, opts$round_interim)
	mu_D <- rnd(mu_D, opts$round_interim)
	SD_D <- rnd(SD_D, opts$round_interim)

	# Calculate d
	M_diff <- rnd(M_D - mu_D, opts$round_interim)
	d <- rnd(M_diff / SD_D, opts$round_final)

	# Get base formula without LaTeX math/aligned blocks
	d.formula <- cohens_d_paired_t_formula(use_aligned = opts$use_aligned,
																				 add_math = FALSE,
																				 add_aligned = FALSE)

	# Get the interpretation (if requested)
	d.interp <- ifelse(interpret, paste0("(", interpret_d(d), ")"), '')

	# Create the solution string, with rounded values (minimally displayed)
	solution <- glue_solution(
		d.formula,
		"<<equals>> \\frac{<<M_D>> - <<mu_D>>}{<<SD_D>>} = \\frac{<<M_diff>>}{<<SD_D>>} = \\mathbf{<<d>>} <<d.interp>>",
		# Round based on the final calculated value unless round_to is set to
		# 'sigfigs', in which case just present the final rounded value as is.
		d = ifelse(opts$round_to == 'sigfigs', d, fmt(d, get_digits(d, opts$round_final))))

	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	# Return list containing both values and solution string
	# Interpretation is included if requested
	list(M_D = M_D,
			 mu_D = mu_D,
			 SD_D = SD_D,
			 M_diff = M_diff,
			 d = d,
			 d.interp = if(!interpret) NULL else interpret_d(d),
			 solution = solution,
			 formula = d.formula) %>%
		purrr::compact()
}

#' @rdname solve_cohens_d_paired_t
#'
#' @export
#'
cohens_d_paired_t_formula <- function(...) {
	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts<- get_handcalcs_opts(...)

	# Use the appropriate equals sign for an aligned environment
	equals <- ifelse(opts$use_aligned, "&=", "=")

	solution <- lglue("d <<equals>> \\frac{M_{D} - \\mu_{D}}{s_{D}}")

	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	return(solution)
}

#' Effect site (Cohen's d) for independent-samples *t*-test
#'
#' Calculates and presents solution string for effect site (Cohen's d) comparing
#' sample means from independent samples: the difference between sample means
#' (`M_1` and `M_2`) in units of the pooled standard deviation (`SD_p`), which
#' can also be given as the pooled variance (`SD2_p`). One of those two
#' arguments must be provided, but not both.
#'
#' @param M_1 Numeric scalar. Mean of sample 1.
#' @param M_2 Numeric scalar. Mean of sample 2.
#' @param SD2_p Numeric scalar. Pooled variance. Required if SD_p is not
#'   provided.
#' @param SD_p Numeric scalar. Pooled standard deviation. Required if SD2_p is
#'   not provided.
#' @param interpret Logical scalar. Should the interpretation (as produced by
#'   [interpret_d()] be included in the solution string?)
#' @param ... Additional arguments to override default behaviors (see
#'   [handcalcs_defaults()]
#'
#' @return `solve_cohens_d_indep_t()` returns a list with the provided values
#'   (`M_1`, `M_2`, `SD2_p`, `SD_p`), the interim calculations (`M_diff`), the
#'   final value (`d`), the solution string (`solution`), and the bare formula
#'   (`formula`) in LaTeX format. `cohens_d_indep_t_formula()` returns just the
#'   bare formula in LaTeX format as a character string.
#' @export
#'
#' @examples
#'
#' # Must provide either the pooled SD or the pooled variance:
#' solve_cohens_d_indep_t(M_1 = 6, M_2 = 5, SD_p = 2)
#' solve_cohens_d_indep_t(M_1 = 6, M_2 = 5, SD2_p = 4)
#'
#' # Include the interpretation of the effect size:
#' solve_cohens_d_indep_t(M_1 = 6, M_2 = 5, SD_p = 2, interpret = TRUE)
#'
#' # You must provide either the pooled variance (SD2_p) or the pooled
#' # standard deviation (SD_p), but not both.
#' \dontrun{solve_cohens_d_indep_t(M_1 = 6, M_5 = 2)}
#' \dontrun{solve_cohens_d_indep_t(M_1 = 6, M_5 = 2, SD2_p = 4, SD_p = 2)}
#'
#' # If you just want the formula:
#' cohens_d_indep_t_formula()
#'
solve_cohens_d_indep_t <- function(M_1,
																	 M_2,
																	 SD2_p,
																	 SD_p,
																	 interpret = FALSE,
																	 ...) {
	# Check argument validity
	stopifnot(is.numeric(M_1), length(M_1) == 1)
	stopifnot(is.numeric(M_2), length(M_2) == 1)

	# Must provide either the pooled variance (SD2_p) or the pooled standard
	# deviation (SD_p), but not both. Also make sure both pooled SD and variance
	# are accounted for:
	if(!xor(missing(SD2_p), missing(SD_p))) {
		stop('Must supply either the pooled variance (SD2_p) or pooled standard deviation (SD_p), but not both.')
	} else if(!missing(SD2_p)) {
		stopifnot(is.numeric(SD2_p), length(SD2_p) == 1, SD2_p > 0)
		SD_p <- sqrt(SD2_p)
	} else if(!missing(SD_p)) {
		stopifnot(is.numeric(SD_p), length(SD_p) == 1, SD_p > 0)
		SD2_p <- SD_p^2
	}

	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts<- get_handcalcs_opts(...)

	# Use the appropriate equals sign for an aligned environment
	equals <- ifelse(opts$use_aligned, "&=", "=")

	# Round initial values
	M_1 <- rnd(M_1, opts$round_interim)
	M_2 <- rnd(M_2, opts$round_interim)
	SD2_p <- rnd(SD2_p, opts$round_interim)
	SD_p <- rnd(SD_p, opts$round_interim)

	# Calculate d
	M_diff <- rnd(M_1 - M_2, opts$round_interim)
	d <- rnd(M_diff / SD_p, opts$round_final)

	# Get base formula without LaTeX math/aligned blocks
	d.formula <- cohens_d_indep_t_formula(use_aligned = opts$use_aligned,
																				add_math = FALSE,
																				add_aligned = FALSE)

	# Get the interpretation (if requested)
	d.interp <- ifelse(interpret, paste0("(", interpret_d(d), ")"), '')

	# Create the solution string, with rounded values (minimally displayed)
	solution <- glue_solution(
		d.formula,
		"<<equals>> \\frac{<<M_1>> - <<M_2>>}{\\sqrt{<<SD2_p>>}} = \\frac{<<M_diff>>}{<<SD_p>>} = \\mathbf{<<d>>} <<d.interp>>",
		# Round based on the final calculated value unless round_to is set to
		# 'sigfigs', in which case just present the final rounded value as is.
		d = ifelse(opts$round_to == 'sigfigs', d, fmt(d, get_digits(d, opts$round_final))))

	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	# Return list containing both values and solution string
	# Interpretation is included if requested
	list(M_1 = M_1,
			 M_2 = M_2,
			 SD2_p = SD2_p,
			 SD_p = SD_p,
			 M_diff = M_diff,
			 d = d,
			 d.interp = if(!interpret) NULL else interpret_d(d),
			 solution = solution,
			 formula = d.formula) %>%
		purrr::compact()
}

#' @rdname solve_cohens_d_indep_t
#'
#' @export
#'
cohens_d_indep_t_formula <- function(...) {
	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts<- get_handcalcs_opts(...)

	# Use the appropriate equals sign for an aligned environment
	equals <- ifelse(opts$use_aligned, "&=", "=")

	solution <- lglue("d <<equals>> \\frac{M_{1} - M_{2}}{\\sqrt{s^{2}_{p}}} = \\frac{M_{1} - M_{2}}{s_{p}}")

	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	return(solution)
}
