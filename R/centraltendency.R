#' Arithmetic Mean
#'
#' Calculate value of arithmetic mean (with rounding) and present solutions.
#' \code{solve_mean()} produces the interim calculations and a full solution
#' string, starting with the bare formula. If you just want the bare formula,
#' use \code{mean_formula()}.
#'
#' @param x Numeric vector of values for which to calculate the mean.
#' @param sub_val Character scalar. Subscript for the value to be calculated in
#'   the formula, in this case the mean (e.g., \eqn{M_{x}}). Leave empty to
#'   report no subscript.
#' @param sym_x Character scalar. Symbol to represent x in formula (default:
#'   "X").
#' @param sub_x Character scalar. Subscript for x in the formula (e.g.,
#'   \eqn{X_{D}})
#' @param ... Additional arguments to override default behaviors (see
#'   \code{\link{handcalcs_defaults}}).
#'
#' @return \code{solve_mean()} returns a list with the interim calculations
#'   (`SumX`, `n`), the final value (`M`), the solution string (`solution`), and
#'   the bare formula (`formula`) in LaTeX format. `mean_formula` returns just
#'   the bare formula in LaTeX format as a character string.
#'
#' @export
#'
#' @examples
#' x <- sample(x = 1:10, size = 20, replace = TRUE)
#' solve_mean(x)
#'
#' # Note that the value calculated by solve_mean(x) will be different than the
#' # value calculated by mean(x) when rounding occurs.
#' (x <- rnorm(20))
#' solve_mean(x, sub = "Y", sym = "Y")
#'
#' # Can override the default values for rounding interim and final values:
#' (x <- rnorm(20))
#' solve_mean(x, round_interim = 2, round_final = 2)
#'
#' # The value of sub can be long, but not sym.
#' \dontrun{
#' solve_mean(x, sub = "Donuts", sym = "Donuts")
#' }
#'
#' # If you just want the bare formula as a string, use mean_formula():
#' mean_formula()
#'
#' # Can set parameters to change symbols used:
#' mean_formula(sub = "Y", sym = "Y")
#'
solve_mean <- function(x,
											 sub_val = "",
											 sym_x = "X",
											 sub_x = "",
											 ...) {

	# Check argument validity
	stopifnot(is.numeric(x), any(!is.na(x)))
	stopifnot(is.character(as.character(sub_val)), length(sub_val) == 1)
	stopifnot(is.character(sym_x))
	stopifnot(is.character(sub_x))

	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts<- get_handcalcs_opts(...)

	# Use the appropriate equals sign for an aligned environment
	equals <- ifelse(opts$use_aligned, "&=", "=")

	# Calculate mean
	x <- rnd(x, opts$round_interim)
	n <- length(x)
	SumX <- rnd(sum(x), opts$round_interim)
	M <- rnd(SumX / n, opts$round_final)

	# Get base formula without LaTeX math/aligned blocks
	M.formula <- mean_formula(sub_val = sub_val,
														sym_x = sym_x,
														sub_x = sub_x,
														show_summation = opts$show_summation,
														use_aligned = opts$use_aligned,
														add_math = FALSE,
														add_aligned = FALSE)

	# Create the solution string, with rounded values (minimally displayed)
	solution <- glue_solution(
		M.formula,
		# Show the summation step, if set
		if(opts$show_summation) {"<<equals>> \\frac{<<Num>>}{<<n>>}"},
		"<<equals>> \\frac{<<SumX>>}{<<n>>}",
		"<<equals>> \\mathbf{<<M>>}",
		# Put negative values of x in brackets
		Num = summation(lglue("<<(x)>>"), abbrev_sum = opts$abbrev_sum),
		SumX = fmt(SumX, get_digits(SumX, opts$round_interim)),
		# Round based on the precision of x and the final calculated value unless
		# round_to is set to 'sigfigs', in which case just present the final rounded
		# value as is.
		M = ifelse(opts$round_to == 'sigfigs',
							 M,
							 fmt(M, get_digits(c(x, M), opts$round_final))))

	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	# Return list containing both values and solution string
	list(x = x,
			 n = n,
			 SumX = SumX,
			 M = M,
			 solution = solution,
			 formula = M.formula)
}


#' @rdname solve_mean
#'
#' @export
#'
mean_formula <- function(sub_val = "",
												 sym_x = "X",
												 sub_x = "",
												 ...) {
	# Check argument validity
	stopifnot(is.character(as.character(sub_val)), length(sub_val) == 1)
	stopifnot(is.character(sym_x))
	stopifnot(is.character(sub_x))

	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts <- get_handcalcs_opts(...)

	# Use the appropriate equals sign for an aligned environment
	equals <- ifelse(opts$use_aligned, "&=", "=")

	# Create the subscripts for the summation sequence:
	sum_seq = c('1', '2', 'n')
	sub_x_seq <- if(nchar(sub_x) == 0) sum_seq else lglue("<<sub_x>>_{<<sum_seq>>}")

	solution <- lglue(
		"M_{<<sub_val>>} <<equals>> \\frac{\\sum{<<sym_x>>_{<<sub_x>>}}}{n}",
		if(opts$show_summation) {
			"\\\\ <<equals>> \\frac{<<sym_x>>_{<<sub_x_seq[1]>>} + <<sym_x>>_{<<sub_x_seq[2]>>} + \\cdots + <<sym_x>>_{<<sub_x_seq[3]>>}}{n}"
		})

	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	return(solution)
}


#' Median: calculate value (with rounding) and present solutions
#'
#' @param x Numeric vector.
#' @param sub_val Character scalar. Subscript for the value to be calculated in
#'   the formula, in this case the median (e.g., \eqn{Median_{x}}). Leave empty
#'   to report no subscript.
#' @param ... Additional arguments to override default behaviors (see
#'   \code{\link{handcalcs_defaults}}).
#'
#' @return A list with the median value (`Med`) and the solution (`solution`) in
#'   LaTeX format.
#' @export
#'
#' @examples
#' #' x <- sample(x = 1:10, size = 20, replace = TRUE)
#' solve_median(x, sub = "Y")
#'
#' # Note that the raw data values get rounded by solve_median(x), so the exact
#' # value of the median will differ from the value calculated by median(x)
#' # when there are a lot of digits past zero.
#' (x <- rnorm(20))
#' Med <- solve_median(x, round_interim = 2)$Med
#' Med == median(x)
#'
solve_median <- function(x,
												 sub_val = "",
												 ...) {
	# Check argument validity; Disallow missing values
	stopifnot(is.numeric(x), any(!is.na(x)))
	stopifnot(is.character(sub_val), length(sub_val) == 1)

	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts<- get_handcalcs_opts(...)

	# Use the appropriate equals sign for an aligned environment
	equals <- ifelse(opts$use_aligned, "&=", "=")

	# Calculate median
	x <- rnd(x, opts$round_interim)
	Med <- rnd(median(x), opts$round_final)

	# Create the solution string, with rounded values (minimally displayed)
	solution <- glue_solution(
		"\\text{Median}_{<<sub_val>>} <<equals>> \\mathbf{<<Med>>}",
		Med = fmt(Med, get_digits(Med, opts$round_final))
	)

	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	# Return list containing both values and solution string
	list(x = x, Med = Med, solution = solution)
}

#' Mode: calculate value (with rounding) and present solutions
#'
#' @param x Numeric vector.
#' @param sub_val Character scalar. Subscript for the value to be calculated in
#'   the formula, in this case the mode (e.g., \eqn{Mode_{x}}). Leave empty to
#'   report no subscript.
#' @param ... Additional arguments to override default behaviors (see
#'   \code{\link{handcalcs_defaults}}).
#'
#' @return A list with the mode value(s) (`Mode`) and the solution (`solution`)
#'   in LaTeX format.
#' @export
#'
#' @examples
#' #' x <- sample(x = 1:10, size = 20, replace = TRUE)
#' solve_mode(x)
#'
#' # Note that the raw data values get rounded by solve_mode(x), so the exact
#' # value(s) provided may differ from the values in the (unrounded) raw data.
#' (x <- rnorm(20))
#' solve_mode(x, round_interim = 2)$Mode
#'
solve_mode <- function(x,
											 sub_val = "",
											 ...) {
	# Check argument validity; Disallow missing values
	stopifnot(is.numeric(x), any(!is.na(x)))
	stopifnot(is.character(sub_val), length(sub_val) == 1)


	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts<- get_handcalcs_opts(...)

	# Use the appropriate equals sign for an aligned environment
	equals <- ifelse(opts$use_aligned, "&=", "=")

	# Calculate mode(s)
	x <- rnd(x, opts$round_interim)
	x <- sort(x)
	x.max <- which(table(x) == max(table(x)))
	Mode <- as.numeric(names(x.max))

	# Create the solution string, with rounded values (minimally displayed)
	solution <- glue_solution(
		"\\text{Mode}_{<<sub_val>>} <<equals>> \\mathbf{<<Mode>>}",
		Mode = paste(Mode, collapse = ", ")
	)

	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	# Return list containing both values and solution string
	list(x = x, Mode = Mode, solution = solution)
}

#' Weighted (aka overall) Mean
#'
#' Calculate value of weighted (aka overall) mean (with rounding) and present
#' solutions. \code{solve_weighted_mean()} produces the interim calculations and
#' a full solution string, starting with the bare formula. If you just want the
#' bare formula, use \code{weighted_mean_formula()}.
#'
#' @param Samples List of means and sample sizes. Must be a list of vectors with
#'   names M and n, one vector per subgroup (see examples).
#' @param ... Additional arguments to override default behaviors (see
#'   \code{\link{handcalcs_defaults}}).
#'
#' @return \code{solve_weighted_mean()} returns a list with vectors for the
#'   means (`M`) and sample sizes (`n`), the interim calculations (`Sum_M_x_n`,
#'   `Sum_n`), the final value (`M_w`), the solution string (`solution`), and
#'   the bare formula (`formula`) in LaTeX format. `weighted_mean_formula`
#'   returns just the bare formula in LaTeX format as a character string.
#'
#' @export
#'
#' @examples
#' l <- list(c(M = 10, n = 20), c(M = 15, n = 25), c(M = 20, n = 30), c(M = 25, n = 35))
#' solve_weighted_mean(l)
#'
#' # For a large number of subgroups, you may wish to truncate the solution:
#' solve_weighted_mean(l, abbrev_sum = 3)
#'
#' # If you just want the bare formula as a string, use weighted_mean_formula():
#' weighted_mean_formula()

solve_weighted_mean <- function(Samples,
																...) {
	# Samples must be a list with at least two subgroups represented
	stopifnot(is.list(Samples), length(Samples) > 1)
	# Disallow missing values.
	stopifnot(all(!is.na(purrr::flatten_dbl(Samples))))
	# Samples must be list of vectors of length 2, with values named M and n
	stopifnot(all(purrr::map_lgl(Samples, ~ length(.x) == 2)))
	stopifnot(all(purrr::flatten_lgl(purrr::map(Samples, ~ names(.x) == c("M", "n")))))

	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts<- get_handcalcs_opts(...)

	# Use the appropriate equals sign for an aligned environment
	equals <- ifelse(opts$use_aligned, "&=", "=")

	# Number of subgroups
	k <- length(Samples)

	# Get vectors of means, sample sizes, and subscripts (1:k)
	M <- purrr::map_dbl(Samples, "M") %>% rnd(opts$round_interim)
	n <- purrr::map_dbl(Samples, "n") %>% round(0)
	sub <- 1:k

	# Calculate weighted mean (M_w)
	M_x_n <- rnd(M * n, opts$round_interim)
	Sum_M_x_n <- rnd(sum(M_x_n), opts$round_interim)
	Sum_n <- sum(n)
	M_w <- rnd(Sum_M_x_n / Sum_n, opts$round_final)

	# Get base formula without LaTeX math/aligned blocks
	M_w.formula <- weighted_mean_formula(use_aligned = opts$use_aligned,
																			 show_summation = opts$show_summation,
																			 add_math = FALSE,
																			 add_aligned = FALSE)

	# Create the solution string, with rounded values (minimally displayed)
	solution <- glue_solution(
		M_w.formula,
		# Show the summation step, if set
		if(opts$show_summation) {"<<equals>> \\frac{<<Num>>}{<<Denom>>}"},
		"<<equals>> \\frac{<<Sum_M_x_n>>}{<<Sum_n>>}",
		"<<equals>> \\mathbf{<<M_w>>}",
		Num = summation(lglue("(<<M>>)(<<n>>)"), abbrev_sum = opts$abbrev_sum),
		Denom = summation(lglue("<<n>>"), abbrev_sum = opts$abbrev_sum),
		Sum_M_x_n = fmt(Sum_M_x_n, get_digits(Sum_M_x_n, opts$round_interim)),
		# Round based on the precision of x and the final calculated value unless
		# round_to is set to 'sigfigs', in which case just present the final rounded
		# value as is.
		M_w = ifelse(opts$round_to == 'sigfigs',
								 M_w,
								 fmt(M_w, get_digits(c(M, M_w), opts$round_final)))
	)

	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	# Return list containing both values and solution string
	list(M = M,
			 n = n,
			 Sum_M_x_n = Sum_M_x_n,
			 Sum_n = Sum_n,
			 M_w = M_w,
			 solution = solution,
			 formula = M_w.formula)
}

#' @rdname solve_weighted_mean
#'
#' @export
#'
weighted_mean_formula <- function(...) {
	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts<- get_handcalcs_opts(...)

	# Use the appropriate equals sign for an aligned environment
	equals <- ifelse(opts$use_aligned, "&=", "=")

	solution<- lglue("M_{\\text{weighted}} <<equals>> \\frac{\\sum{M_{i}n_{i}}}{\\sum{n_{i}}}",
									 if(opts$show_summation) {"\\\\ <<equals>> \\frac{<<Num>>}{<<Denom>>}"},
									 Num = "(M_{1})(n_{1}) + (M_{2})(n_{2}) + \\cdots + (M_{k})(n_{k})",
									 Denom = "n_{1} + n_{2} + \\cdots + n_{k}")

	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	return(solution)
}
