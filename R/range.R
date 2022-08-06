#' Range: calculate value (with rounding) and present solutions
#'
#' @param x Numeric vector.
#' @param sub_val Character scalar. Subscript for the value to be calculated the
#'   formula, in this case the range (e.g., \eqn{Range_{x}}). Leave empty to report
#'   no subscript.
#' @param sym_x Character scalar. Symbol to represent x in formula (default:
#'   "X").
#' @param ... Additional arguments to override default behaviors (see
#'   [handcalcs_defaults()]).
#'
#' @return `solve_range()` returns a list with the interim calculations
#'   (`x_max`, `x_min`), the final value (`Range`), the solution
#'   string (`solution`), and the bare formula (`formula`) in LaTeX
#'   format. `range_formula()` returns just the bare formula in LaTeX format
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
												sub_val = "",
												sym_x = "X",
												...) {
	# Check argument validity; Disallow missing values
	stopifnot(is.numeric(x), any(!is.na(x)))
	stopifnot(is.character(sub_val), length(sub_val) == 1)
	stopifnot(is.character(sym_x), nchar(sym_x) == 1)

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
	Range.formula <- range_formula(sub_val = sub_val,
																 sym_x = sym_x,
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
range_formula <- function(sub_val = "",
													sym_x = "X",
													...) {
	# Check argument validity
	stopifnot(is.character(as.character(sub_val)), length(sub_val) == 1)
	stopifnot(is.character(sym_x), nchar(sym_x) == 1)

	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts<- get_handcalcs_opts(...)

	# Use the appropriate equals sign for an aligned environment
	equals <- ifelse(opts$use_aligned, "&=", "=")

	solution<- lglue("\\text{Range}_{<<sub_val>>} <<equals>> <<sym_x>>_{max} - <<sym_x>>_{min}")

	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	return(solution)
}

