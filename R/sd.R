#' Population Standard Deviation
#'
#' Calculates population standard deviation (\eqn{\sigma = \sqrt(SS / n)})
#' either from a set of raw values (`x`) OR from the sum of squares (`SS`) and
#' sample size (`n`), OR from the population variance (`sigma2`). If providing
#' raw data, it will include the calculation of SS in the solution string. If
#' providing `SS` and `n`, it will include the calculation of the population
#' variance in the solution string. Otherwise, if only `sigma2` is provided,
#' will simply show the final calculation (\eqn{\sqrt(\sigma^2)}).
#'
#' @param x Numeric vector of raw data values.
#' @param SS Numeric scalar: sum of squares.
#' @param n Numeric scalar: sample size.
#' @param sigma2 Numeric scalar: population variance (\eqn{\sigma^2}).
#' @param sub_val Character scalar. Subscript for the value to be calculated in
#'   the formula, in this case the population standard deviation (e.g.,
#'   \eqn{\sigma_{x}}). Leave empty to report no subscript.
#' @param sym_x Character scalar. Symbol to represent x in formula when
#'   calculating from raw data (default: "X").
#' @param sub_x Character scalar. Subscript for x in the formula when
#'   calculating from raw data (e.g., \eqn{X_{D}})
#' @param SS.f Formula to use for sum of squares calculation (either
#'   solve_sum_squares or solve_sum_squares2). Only used when calculating SS
#'   from raw data (`x`).
#' @param ... Additional arguments to override default behaviors (see
#'   \code{\link{handcalcs_defaults}}).
#'
#' @return \code{solve_sigma2()} returns a list with the interim values and
#'   calculations (`x`, `SS`, `n`, `sigma2`), the final value (`sigma`), the
#'   solution string (`solution`), and the bare formula (`formula`) in LaTeX
#'   format. `sigma_formula` returns just the bare formula in LaTeX format as a
#'   character string.
#'
#' @export
#'
#' @examples
#' # Simple usage: generate raw data, produce results
#' (x <- sample(x = 1:10, size = 20, replace = TRUE))
#' solve_sigma(x = x, sub = 'X')
#'
#' # Alternatively, provide SS and n
#' solve_sigma(SS = 100, n = 10)
#'
#' # Or, simply provide sigma2
#' solve_sigma(sigma2 = 10)
#'
#' # Must provide enough values for calculation:
#' \dontrun{
#' solve_sigma(SS = 100)
#' }
#'
#' # If you just want the bare formula as a string, use a formula function():
#' sigma_formula()
#'
#' # Can set parameters to change symbols used:
#' sigma_formula(sub_val = "Y")
solve_sigma <- function(x,
												SS,
												n,
												sigma2,
												sub_val = "",
												sym_x = "X",
												sub_x = "",
												SS.f = solve_sum_squares,
												...) {

	solve_std_dev(mode = 'population',
								x = x,
								SS = SS,
								n = n,
								variance = sigma2,
								sub_val = sub_val,
								sym_x = sym_x,
								sub_x = sub_x,
								SS.f = SS.f,
								...)
}




#' @rdname solve_sigma
#'
#' @export
#'
sigma_formula <- function(sub_val = "",
													...) {

	std_dev_formula(mode = 'population',
									sub_val = sub_val,
									...)
}

#' Sample Standard Deviation
#'
#' Calculates sample standard deviation (\eqn{s = \sqrt[SS / (n - 1)]}) either
#' from a set of raw values (`x`) OR from the sum of squares (`SS`) and sample
#' size (`n`), OR from the sample variance (`s2`). If providing raw data, it
#' will include the calculation of SS in the solution string. If providing `SS`
#' and `n`, it will include the calculation of the sample variance in the
#' solution string. Otherwise, if only `s2` is provided, will simply show the
#' final calculation (\eqn{\sqrt(s^2)}).
#'
#' @param x Numeric vector of raw data values.
#' @param SS Numeric scalar: sum of squares.
#' @param n Numeric scalar: sample size.
#' @param s2,SD2  Numeric scalar: sample variance (\eqn{s^2}). May be named
#'   either `s2` or `SD2`.
#' @param sub_val Character scalar. Subscript for the value to be calculated in
#'   the formula, in this case the mean (e.g., \eqn{s_{x}}). Leave empty to
#'   report no subscript.
#' @param sym_x Character scalar. Symbol to represent x in formula when
#'   calculating from raw data (default: "X").
#' @param sub_x Character scalar. Subscript for x in the formula when
#'   calculating from raw data (e.g., \eqn{X_{D}})
#' @param SS.f Formula to use for sum of squares calculation (either
#'   solve_sum_squares or solve_sum_squares2). Only used when calculating SS
#'   from raw data (`x`).
#' @param ... Additional arguments to override default behaviors (see
#'   \code{\link{handcalcs_defaults}}).
#'
#' @return \code{solve_sd()} returns a list with the interim values and
#'   calculations (`x`, `SS`, `n`, `s2`), the final value (both as `s` and as
#'   `SD`), the solution string (`solution`), and the bare formula (`formula`)
#'   in LaTeX format. `sigma2_formula` returns just the bare formula in LaTeX
#'   format as a character string.
#'
#' @export
#'
#' @examples
#' # Simple usage: generate raw data, produce results
#' (x <- sample(x = 1:10, size = 20, replace = TRUE))
#' solve_s(x = x, sub_val = 'X')
#'
#' # Alternatively, provide SS and n
#' solve_s(SS = 100, n = 10)
#'
#' # Or, simply provide s2
#' solve_s(s2 = 10)
#'
#' # Must provide enough values for calculation:
#' \dontrun{
#' solve_s(SS = 100)
#' }
#'
#' # If you just want the bare formula as a string, use a formula function():
#' s_formula()
#'
#' # Can set parameters to change symbols used:
#' s_formula(sub_val = "Y")
solve_sd <- function(x,
										 SS,
										 n,
										 s2,
										 SD2,
										 sub_val = "",
										 sym_x = "X",
										 sub_x = "",
										 SS.f = solve_sum_squares,
										 ...) {

	# If SD2 is supplied instead of s2, pass that along as variance.
	if(missing(s2) & !missing(SD2)) s2 <- SD2

	solve_std_dev(mode = 'sample',
								x = x,
								SS = SS,
								n = n,
								variance = s2,
								sub_val = sub_val,
								sym_x = sym_x,
								sub_x = sub_x,
								SS.f = SS.f,
								...)
}


#' @rdname solve_sd
#'
#' @export
#'
sd_formula <- function(sub_val = "",
											 ...) {

	std_dev_formula(mode = 'sample',
									sub_val = sub_val,
									...)
}


# Function doing the hard work of solving for population/sample standard deviation:
solve_std_dev <- function(mode,
													x,
													SS,
													n,
													variance,
													sub_val = "",
													sym_x = "X",
													sub_x = "",
													SS.f = solve_sum_squares,
													...) {

	# Must know proper mode for calculation
	stopifnot(mode %in% c('population', 'sample'))

	if(missing(x) & (missing(SS) | missing(n)) & missing(variance)) {
		stop('Must supply x, OR SS and n, OR variance.')
	}
	stopifnot(is.character(sub_val), length(sub_val) == 1)
	stopifnot(is.character(sym_x), nchar(sym_x) == 1)

	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts <- get_handcalcs_opts(...)

	# Use the appropriate equals sign for an aligned environment
	equals <- ifelse(opts$use_aligned, "&=", "=")

	# Only display initial formula/calculations if x (or SS and n) are supplied
	std_dev.solution <- NULL

	# Calculate variance from raw data or SS and n.
	if(!missing(x) | (!missing(SS) & !missing(n))) {
		var.lst <- solve_variance(mode = mode,
															x = x,
															SS = SS,
															n = n,
															sub_val = sub_val,
															sym_x = sym_x,
															sub_x = sub_x,
															show_summation = opts$show_summation,
															abbrev_sum = opts$abbrev_sum,
															round_interim = opts$round_interim,
															round_final = opts$round_interim,
															add_math = FALSE,
															add_aligned = FALSE,
															use_aligned = opts$use_aligned)

		# Get values from list (some may be NULL):
		x <- var.lst$x
		n <- var.lst$n
		SS <- var.lst$SS
		variance <- ifelse(mode == 'population', var.lst$sigma2, var.lst$s2)
		var.solution <- var.lst$solution
	} else {
		# Set missing return values to NULL
		x <- n <- SS <- NULL

		stopifnot(is.numeric(variance))
		var.lst <- list(variance = variance)
		var.solution = NULL
	}

	# Calculate standard deviation:
	std_dev <- round(sqrt(variance), opts$round_final)

	# Get base formula without LaTeX math/aligned blocks
	std_dev.f <- std_dev_formula(mode = mode,
															 sub_val = sub_val,
															 use_aligned = opts$use_aligned,
															 add_math = FALSE,
															 add_aligned = FALSE)

	# Prepend the solution for sum of squares if calculating from raw data
	std_dev.solution <- ifelse(!is.null(var.solution),
														 paste(var.solution, ' \\\\ ', std_dev.f),
														 std_dev.f)



	# Create the solution string, with rounded values (minimally displayed)
	solution <- glue_solution(
		std_dev.solution,
		"<<equals>> \\sqrt{<<variance>>}",
		"<<equals>> \\mathbf{<<std_dev.fmt>>}",
		# Round based on the precision of the final calculated value unless round_to is
		# set to 'sigfigs', in which case just present the final rounded value as is.
		std_dev.fmt = ifelse(opts$round_to == 'sigfigs',
												 std_dev,
												 fmt(std_dev, get_digits(std_dev, opts$round_final)))
	)

	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	# Return list containing both values and solution string
	# Names are specific to the mode; NULL values are removed
	list(x = x,
			 SS = SS,
			 n = n,
			 sigma2 = if(mode == 'population') variance else NULL,
			 sigma = if(mode == 'population') std_dev else NULL,
			 s2 = if(mode == 'population') NULL else variance,
			 SD2 = if(mode == 'population') NULL else variance,
			 s = if(mode == 'population') NULL else std_dev,
			 SD = if(mode == 'population') NULL else std_dev,
			 solution = solution,
			 formula = std_dev.f) %>%
		purrr::compact()
}

# Function doing the hard work of producing the base formula for the
# population/sample standard deviation:
std_dev_formula <- function(mode,
														sub_val = "",
														...) {

	# Must know proper mode for calculation
	stopifnot(mode %in% c('population', 'sample'))

	# Check argument validity
	stopifnot(is.character(as.character(sub_val)), length(sub_val) == 1)

	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts<- get_handcalcs_opts(...)

	# Use the appropriate equals sign for an aligned environment
	equals <- ifelse(opts$use_aligned, "&=", "=")

	# Create the solution string, with rounded values (minimally displayed)
	if(mode == "population") {
		solution<- lglue("\\sigma_{<<sub_val>>} <<equals>> \\sqrt{\\sigma^2}")
	} else {
		solution<- lglue("s_{<<sub_val>>} <<equals>> \\sqrt{s^2}")
	}

	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	return(solution)
}
