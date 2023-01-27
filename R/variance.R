#' Population Variance
#'
#' Calculates population variance (\eqn{\sigma^2 = SS / n}) either from a set of
#' raw values (`x`) OR from the sum of squares (`SS`) and sample size (`n`). If
#' providing raw data, it will include the calculation of SS in the solution
#' string.
#'
#' @param x Numeric vector of raw data values.
#' @param SS Numeric scalar: sum of squares.
#' @param n Numeric scalar: sample size.
#' @param sub_val Character scalar. Subscript for the value to be calculated in
#'   the formula, in this case the population variance (e.g.,
#'   \eqn{\sigma^2_{x}}). Leave empty to report no subscript.
#' @param sym_x Character scalar. Symbol to represent x in formula when
#'   calculating from raw data (default: "X").
#' @param sub_x Character scalar. Subscript for x in the formula when
#'   calculating from raw data (e.g., \eqn{X_{D}})
#' @param SS.f Formula to use for sum of squares calculation (either
#'   solve_sum_squares or solve_sum_squares2). Only used when calculating SS
#'   from raw data (`x`).
#' @param ... Additional arguments to override default behaviors (see
#'   [handcalcs_defaults()]
#'
#' @return \code{solve_sigma2()} returns a list with the interim values and
#'   calculations (`x`, `SS`, `n`), the final value (`sigma2`), the solution
#'   string (`solution`), and the bare formula (`formula`) in LaTeX format.
#'   `sigma2_formula` returns just the bare formula in LaTeX format as a
#'   character string.
#'
#' @export
#'
#' @examples
#' # Simple usage: generate raw data, produce results
#' (x <- sample(x = 1:10, size = 20, replace = TRUE))
#' solve_sigma2(x = x, sub_val = 'X')
#'
#' # Alternatively, provide SS and n
#' solve_sigma2(SS = 100, n = 10)
#'
#' # Must provide enough values for calculation:
#' \dontrun{
#' solve_sigma2(SS = 100)
#' }
#'
#' # If you just want the bare formula as a string, use a formula function():
#' sigma2_formula()
#'
#' # Can set parameters to change symbols used:
#' sigma2_formula(sub_val = "Y")
#'
solve_sigma2 <- function(x,
												 SS,
												 n,
												 sub_val = "",
												 sym_x = "X",
												 sub_x = "",
												 SS.f = solve_sum_squares,
												 ...) {

	solve_variance(mode = 'population',
								 x = x,
								 SS = SS,
								 n = n,
								 sub_val = sub_val,
								 sym_x = sym_x,
								 sub_x = sub_x,
								 SS.f = SS.f,
								 ...)
}

#' @rdname solve_sigma2
#'
#' @export
#'
sigma2_formula <- function(sub_val = "",
													 ...) {

	variance_formula(mode = 'population',
									 sub_val = sub_val,
									 ...)
}


#' Sample Variance
#'
#' Calculates sample variance (\eqn{s^2}) either from a set of raw values (`x`)
#' OR from the sum of squares (`SS`) and sample size (`n`). If providing raw
#' data, it will include the calculation of SS in the solution string.
#'
#' @param x Numeric vector of raw data values.
#' @param SS Numeric scalar: sum of squares.
#' @param n Numeric scalar: sample size.
#' @param sub_val Character scalar. Subscript for the value to be calculated in
#'   the formula, in this case the sample variance (e.g., \eqn{s^2_{x}}). Leave
#'   empty to report no subscript.
#' @param sym_x Character scalar. Symbol to represent x in formula when
#'   calculating from raw data (default: "X").
#' @param sub_x Character scalar. Subscript for x in the formula when
#'   calculating from raw data (e.g., \eqn{X_{D}})
#' @param SS.f Function to use for sum of squares calculation (either
#'   solve_sum_squares or solve_sum_squares2). Only used when calculating SS
#'   from raw data (`x`).
#' @param ... Additional arguments to override default behaviors (see
#'   \code{\link{handcalcs_defaults}}).
#'
#' @return \code{solve_s2()} returns a list with the interim values and
#'   calculations (`x`, `SS`, `n`), the final value (both as `s2` and as `SD2`),
#'   the solution string (`solution`), and the bare formula (`formula`) in LaTeX
#'   format. `s2_formula` returns just the bare formula in LaTeX format as a
#'   character string.
#'
#' @export
#'
#' @examples
#' # Simple usage: generate raw data, produce results
#' (x <- sample(x = 1:10, size = 20, replace = TRUE))
#' solve_s2(x = x, sub_val = 'X')
#'
#' # Alternatively, provide SS and n
#' solve_s2(SS = 100, n = 10)
#'
#' # Must provide enough values for calculation:
#' \dontrun{
#' solve_s2(SS = 100)
#' }
#'
#' # If you just want the bare formula as a string, use a formula function():
#' s2_formula()
#'
#' # Can set parameters to change symbols used:
#' s2_formula(sub_val = "Y")
#'
solve_s2 <- function(x,
										 SS,
										 n,
										 sub_val = "",
										 sym_x = "X",
										 sub_x = "",
										 SS.f = solve_sum_squares,
										 ...) {
	solve_variance(mode = 'sample',
								 x = x,
								 SS = SS,
								 n = n,
								 sub_val = sub_val,
								 sym_x = sym_x,
								 sub_x = sub_x,
								 SS.f = SS.f,
								 ...)
}

#' @rdname solve_s2
#'
#' @export
#'
s2_formula <- function(sub_val = "",
											 ...) {
	variance_formula(mode = 'sample',
									 sub_val = sub_val,
									 ...)
}


# Function doing the hard work of solving for population/sample variance:
solve_variance <- function(mode,
													 x,
													 SS,
													 n,
													 sub_val = "",
													 sym_x = "X",
													 sub_x = "",
													 SS.f = solve_sum_squares,
													 ...) {
	# Must know proper mode for calculation
	stopifnot(mode %in% c('population', 'sample'))

	if(missing(x) & (missing(SS) | missing(n))) {
		stop('Must supply either x or SS and n')
	}
	stopifnot(is.character(sub_val), length(sub_val) == 1)
	stopifnot(is.character(sym_x), nchar(sym_x) == 1)

	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts <- get_handcalcs_opts(...)

	# Use the appropriate equals sign for an aligned environment
	equals <- ifelse(opts$use_aligned, "&=", "=")

	# Only display the formula/calculation of SS if raw data are supplied
	SS.solution <- NULL

	# Calculate SS from raw data
	if(!missing(x)) {
		stopifnot(is.numeric(x), any(!is.na(x)))

		SS.lst <- SS.f(x = x,
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

		SS <- SS.lst$SS
		n <- SS.lst$n
		SS.solution <- SS.lst$solution
	}

	stopifnot(is.numeric(SS))
	stopifnot(is.numeric(n), n > 0)

	# Calculate variance:
	variance <- if(mode == 'sample') (SS / (n - 1)) else (SS / n)
	variance <- rnd(variance, opts$round_final)

	# Get base formula without LaTeX math/aligned blocks
	var.f <- variance_formula(mode = mode,
														sub_val = sub_val,
														use_aligned = opts$use_aligned,
														add_math = FALSE,
														add_aligned = FALSE)

	# Prepend the solution for sum of squares if calculating from raw data
	var.solution <- ifelse(!is.null(SS.solution),
												 paste(SS.solution, ' \\\\ ', var.f),
												 var.f)

	# Round based on the precision of the final calculated value unless round_to is
	# set to 'sigfigs', in which case just present the final rounded value as is.
	variance.fmt <- ifelse(opts$round_to == 'sigfigs',
												 variance,
												 fmt(variance, get_digits(variance, opts$round_final)))


	# Create the solution string, with rounded values (minimally displayed)
	if(mode == "population") {
		solution <- glue_solution(
			var.solution,
			"<<equals>> \\frac{<<SS>>}{<<n>>}",
			"<<equals>> \\mathbf{<<variance.fmt>>}")
	} else {
		solution <- glue_solution(
			var.solution,
			"<<equals>> \\frac{<<SS>>}{<<n>> - 1}",
			"<<equals>> \\frac{<<SS>>}{<<n - 1>>}",
			"<<equals>> \\mathbf{<<variance.fmt>>}")
	}

	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	# Return list containing both values and solution string
	# Names are specific to the mode; NULL values are removed
	list(x = if(missing(x)) NULL else x,
			 SS = SS,
			 n = n,
			 sigma2 = if(mode == 'population') variance else NULL,
			 s2 = if(mode == 'population') NULL else variance,
			 SD2 = if(mode == 'population') NULL else variance,
			 solution = solution,
			 formula = var.f) %>%
		purrr::compact()
}

# Function doing the hard work of producing the base formula for the
# population/sample variance:
variance_formula <- function(mode,
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
		solution<- lglue("\\sigma^2_{<<sub_val>>} <<equals>> \\frac{\\mathit{SS}_{<<sub_val>>}}{N}")
	} else {
		solution<- lglue("s^2_{<<sub_val>>} <<equals>> \\frac{\\mathit{SS}_{<<sub_val>>}}{\\mathit{df}}")
	}

	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	return(solution)
}
