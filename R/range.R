#' Range: calculate value (with rounding) and present solutions
#'
#' @param x Numeric vector.
#' @param sub Character scalar. Name of numeric vector (reported as subscript in
#'   solutions). Leave empty to report no subscript.
#' @param sym Character scalar. Symbol to represent x in formula (default: "X").
#'   Only one character allowed.
#' @param ... Additional arguments to override default behaviors (see
#'   \code{\link{handcalcs_defaults}}).
#'
#' @return \code{solve_range()} returns a list with the interim calculations
#'   (\code{x_max}, \code{x_min}), the final value (\code{Range}), the solution
#'   string (\code{solution}), and the bare formula (\code{formula}) in LaTeX
#'   format. \code{range_formula} returns just the bare formula in LaTeX format
#'   as a character string.
#'
#' @export
#'
#' @examples
#' #' x <- sample(x = 1:10, size = 20, replace = TRUE)
#' solve_range(x, sub = "Y")
#'
#' # Note that the raw data values get rounded by solve_range(x), so the exact
#' # value of the range will differ from the value calculated by range(x)
#' # when there are a lot of digits past zero.
#' (x <- rnorm(20))
#' Range <- solve_range(x, round_interim = 2)$Range
#' Range == diff(range(x))
#'
#' # If you just want the bare formula as a string, use range_formula():
#' range_formula()
#'
solve_range <- function(x,
												sub = "",
												sym = "X",
												...) {
	# Check argument validity; Disallow missing values
	stopifnot(is.numeric(x), any(!is.na(x)))
	stopifnot(is.character(sub), length(sub) == 1)
	stopifnot(is.character(sym), nchar(sym) == 1)

	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts<- get_handcalcs_opts(...)

	# Use the appropriate equals sign for an aligned environment
	equals <- ifelse(opts$use_aligned, "&=", "=")

	# Calculate median
	x <- rnd(x, opts$round_interim)
	x_max <- max(x)
	x_min <- min(x)
	Range <- rnd(x_max - x_min, opts$round_final)

	# Get base formula without LaTeX math/aligned blocks
	Range.formula <- range_formula(sub = sub,
																 sym = sym,
																 use_aligned = opts$use_aligned,
																 add_math = FALSE,
																 add_aligned = FALSE)

	# Create the solution string, with rounded values (minimally displayed)
	solution <- glue_solution(
		Range.formula,
		"<<equals>> <<x_max>> - <<x_min>>",
		"<<equals>> \\mathbf{<<Range>>}",
		Range = fmt(Range, get_digits(Range, opts$round_final))
	)

	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	# Return list containing both values and solution string
	list(x = x, x_max = x_max, x_min = x_min, Range = Range,
			 solution = solution, formula = Range.formula)
}

#' @rdname solve_range
#'
#' @export
#'
range_formula <- function(sub = "",
													sym = "X",
													...) {
	# Check argument validity
	stopifnot(is.character(as.character(sub)), length(sub) == 1)
	stopifnot(is.character(sym), nchar(sym) == 1)

	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts<- get_handcalcs_opts(...)

	# Use the appropriate equals sign for an aligned environment
	equals <- ifelse(opts$use_aligned, "&=", "=")

	solution<- lglue("\\text{Range}_{<<sub>>} <<equals>> <<sym>>_{max} - <<sym>>_{min}")

	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	return(solution)
}

