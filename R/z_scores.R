#' Converting Raw Scores to Z-scores
#'
#' Functions to convert raw scores to z-scores and vice versa.
#' `solve_x_to_z()` takes a raw score (`x`), along with the distribution's
#' mean (`M`) and standard deviation (`SD`), and calculates the standard score
#' (`z`). `solve_z_to_x()` takes a z-score (`z`), along with the
#' distribution's mean (`M`) and standard deviation (`SD`), and calculates the
#' raw score (`x`). If you just want the bare formula for either step, use
#' `x_to_z_formula()` or `z_to_x_formula()`.
#'
#' Note that *z*-scores are rounded to the value of `round_z` instead of the
#' value of `round_interim` or `round_final` (see [handcalcs_defaults()]).
#'
#' @param x Numeric scalar. Raw score to convert to a z-score.
#' @param z Numeric scalar. Z-score to convert to a raw score (x).
#' @param M Numeric scalar. Mean of distribution.
#' @param SD Numeric scalar. Standard Deviation of distribution.
#' @param sub_val Character scalar. Subscript for the value to be calculated in the
#'   formula, in this case the *z*-score (e.g., \eqn{z_{x}}). Leave empty to report
#'   no subscript.
#' @param sym_x Character scalar. Symbol to represent x in formula (default:
#'   "X").
#' @param population Logical. Is this a population, rather than a sample? This
#'   is only relevant for displaying the formula in the solutions, which will
#'   use Greek letters to denote the population mean (\eqn{\mu}) and standard
#'   deviation (\eqn{\sigma}).
#' @param ... Additional arguments to override default behaviors (see
#'   [handcalcs_defaults()]).
#'
#' @return `solve_x_to_z()` returns a list with the given values, the
#'   interim calculations (`M_diff`), the final value (`z`), the
#'   solution string (`solution`), and the bare formula (`formula`) in
#'   LaTeX format.
#'
#'   `solve_z_to_x()` returns a list with the given values, the interim
#'   calculations (`zSD`), the final value (`x`), the solution string
#'   (`solution`), and the bare formula (`formula`) in LaTeX format.
#'
#'   The two formula functions (`x_to_z_formula()`, `z_to_x_formula()`)
#'   return just the bare formula in LaTeX format as a character string.
#'
#' @export
#'
#' @examples
#' # Convert a raw score to a z-score:
#' M <- 10
#' SD <- 2
#' (z.lst <- solve_x_to_z(x = 5, M = M, SD = SD))
#'
#' # And convert it back to a x
#' solve_z_to_x(z = z.lst$z, M = M, SD = SD)
#'
#' # Or calculate a novel value:
#' solve_z_to_x(z = 1.1, M = M, SD = SD)
#'
#' # Can change the symbols used:
#' # If you just want the bare formula as a string, use the formula functions:
#' x_to_z_formula()
#' z_to_x_formula()
#'
#' # Set population to TRUE to show Greek symbols:
#' x_to_z_formula(population = TRUE)
#' z_to_x_formula(population = TRUE)
#'
solve_x_to_z <- function(x,
												 M,
												 SD,
												 sub_val = "",
												 sym_x = "x",
												 population = FALSE,
												 ...) {

	# Check argument validity
	stopifnot(is.numeric(x), length(x) == 1)
	stopifnot(is.numeric(M), length(M) == 1)
	stopifnot(is.numeric(SD), length(SD) == 1, SD > 0)
	stopifnot(is.character(as.character(sub_val)), length(sub_val) == 1)
	stopifnot(is.character(sym_x), nchar(sym_x) == 1)

	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts<- get_handcalcs_opts(...)

	# Use the appropriate equals sign for an aligned environment
	equals <- ifelse(opts$use_aligned, "&=", "=")

	# Calculate z-score
	x <- rnd(x, opts$round_interim)
	M <- rnd(M, opts$round_interim)
	SD <- rnd(SD, opts$round_interim)

	M_diff <- rnd(x - M, opts$round_interim)
	z <- rnd(M_diff / SD, opts$round_z)

	# Get base formula without LaTeX math/aligned blocks
	Z.formula <- x_to_z_formula(sub_val = sub_val,
															sym_x = sym_x,
															population = population,
															use_aligned = opts$use_aligned,
															add_math = FALSE,
															add_aligned = FALSE)

	# Create the solution string, with rounded values (minimally displayed)
	solution <- glue_solution(
		Z.formula,
		# Put negative values of M in brackets
		"<<equals>> \\frac{<<x>> - <<(M)>>}{<<SD>>}",
		"<<equals>> \\frac{<<M_diff>>}{<<SD>>}",
		"<<equals>> \\mathbf{<<z>>}",
		# Round based on the precision of x and the final calculated value unless
		# round_to is set to 'sigfigs', in which case just present the final rounded
		# value as is.
		z = ifelse(opts$round_to == 'sigfigs', z, fmt(z, opts$round_z)))

	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	# Return list containing both values and solution string
	list(x = x,
			 M = M,
			 mu = M,
			 SD = SD,
			 sigma = SD,
			 M_diff = M_diff,
			 z = z,
			 solution = solution,
			 formula = Z.formula)
}


#' @rdname solve_x_to_z
#'
#' @export
#'
x_to_z_formula <- function(sub_val = "",
													 sym_x = "x",
													 population = FALSE,
													 ...) {
	# Check argument validity
	stopifnot(is.character(as.character(sub_val)), length(sub_val) == 1)
	stopifnot(is.character(sym_x), nchar(sym_x) == 1)

	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts<- get_handcalcs_opts(...)

	# Use the appropriate equals sign for an aligned environment
	equals <- ifelse(opts$use_aligned, "&=", "=")

	M.sym <- ifelse(population, '\\mu', 'M')
	SD.sym <- ifelse(population, '\\sigma', 's')

	solution <- lglue("z_{<<sub_val>>} <<equals>> \\frac{<<sym_x>> - <<M.sym>>}{<<SD.sym>>}")

	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	return(solution)
}



#' @rdname solve_x_to_z
#'
#' @export
#'
solve_z_to_x <- function(z,
												 M,
												 SD,
												 sub_val = "",
												 sym_x = "x",
												 population = FALSE,
												 ...) {

	# Check argument validity
	stopifnot(is.numeric(z), length(z) == 1)
	stopifnot(is.numeric(M), length(M) == 1)
	stopifnot(is.numeric(SD), length(SD) == 1, SD > 0)
	stopifnot(is.character(as.character(sub_val)), length(sub_val) == 1)
	stopifnot(is.character(sym_x), nchar(sym_x) == 1)

	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts<- get_handcalcs_opts(...)

	# Use the appropriate equals sign for an aligned environment
	equals <- ifelse(opts$use_aligned, "&=", "=")

	# Calculate z-score
	z <- rnd(z, opts$round_z)
	M <- rnd(M, opts$round_interim)
	SD <- rnd(SD, opts$round_interim)

	zSD <- rnd(z*SD, opts$round_interim)
	x <- rnd(zSD + M, opts$round_interim)

	# Get base formula without LaTeX math/aligned blocks
	X.formula <- z_to_x_formula(sub_val = sub_val,
															sym_x = sym_x,
															population = population,
															use_aligned = opts$use_aligned,
															add_math = FALSE,
															add_aligned = FALSE)

	# Create the solution string, with rounded values (minimally displayed)
	solution <- glue_solution(
		X.formula,
		# Put negative values of M in brackets
		"<<equals>> (<<z>>)(<<SD>>) + <<(M)>>",
		"<<equals>> <<zSD>> + <<M>>",
		"<<equals>> \\mathbf{<<x>>}",
		# Round based on the precision of x and the final calculated value unless
		# round_to is set to 'sigfigs', in which case just present the final rounded
		# value as is.
		x = ifelse(opts$round_to == 'sigfigs',
							 x,
							 fmt(x, get_digits(x, opts$round_final))))

	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	# Return list containing both values and solution string
	list(z = z,
			 M = M,
			 mu = M,
			 SD = SD,
			 sigma = SD,
			 zSD = zSD,
			 x = x,
			 solution = solution,
			 formula = X.formula)
}


#' @rdname solve_x_to_z
#'
#' @export
#'
z_to_x_formula <- function(sub_val = "",
													 sym_x = "x",
													 population = FALSE,
													 ...) {
	# Check argument validity
	stopifnot(is.character(as.character(sub_val)), length(sub_val) == 1)
	stopifnot(is.character(sym_x), nchar(sym_x) == 1)

	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts<- get_handcalcs_opts(...)

	# Use the appropriate equals sign for an aligned environment
	equals <- ifelse(opts$use_aligned, "&=", "=")

	M.sym <- ifelse(population, '\\mu', 'M')
	SD.sym <- ifelse(population, '\\sigma', 's')

	solution <- lglue("<<sym_x>>_{<<sub_val>>} <<equals>> z<<SD.sym>> + <<M.sym>>")

	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	return(solution)
}
