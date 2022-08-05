#' Difference Scores
#'
#' Calculates and provides solutions for calculating difference scores
#' (\eqn{X_{D} = X_{2} - X_{1}}).
#'
#' @param x_1 Numeric vector of values.
#' @param x_2 Numeric vector of values.
#' @param sym_x Character scalar. Symbol to represent x in formula (default:
#'   "X").
#' @param abbrev_at Numeric scalar. At what length of `x_1` should it abbreviate
#'   the list of values within the solution using an ellipsis? If the length of
#'   `x_1` is greater than or equal to `abbrev_at`, it will abbreviate the
#'   sequence (e.g., "1, 2, ..., 6"). Otherwise, will show the entire sequence
#'   (e.g., "1, 2, 3, 4, 5, 6).
#' @param ... Additional arguments to override default behaviors (see
#'   [handcalcs_defaults()]).
#'
#' @return `solve_diff_score()` returns a list with the provided values (`x_1`,
#'   `x_2`), the final value (the vector of difference scores, `x_D`), the
#'   solution string (`solution`), and the bare formula (`formula`) in LaTeX
#'   format. `diff_score_formula()` returns just the bare formula in LaTeX
#'   format as a character string.
#' @export
#'
#' @examples
#'
#' x_1 <- rnorm(10, mean = 0)
#' x_2 <- rnorm(10, mean = 1)
#' solve_diff_score(x_1 = x_1, x_2 = x_2)
#'
#' # Can use a different symbol for X:
#' solve_diff_score(x_1 = x_1, x_2 = x_2, sym_x = "Y")
#'
#' # If you just want the bare formula as a string, use the formula function():
#' diff_score_formula()
#'
solve_diff_score <- function(x_1,
														 x_2,
														 sym_x = "X",
														 abbrev_at = 5,
														 ...) {
	# Check argument validity:
	stopifnot(is.numeric(x_1), any(!is.na(x_1)), length(x_1) > 0)
	stopifnot(is.numeric(x_2), any(!is.na(x_2)), length(x_1) > 0)
	stopifnot(length(x_1) == length(x_2))
	stopifnot(is.character(sym_x), nchar(sym_x) == 1)

	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts <- get_handcalcs_opts(...)

	# Use the appropriate equals sign for an aligned environment
	equals <- ifelse(opts$use_aligned, "&=", "=")

	# Round initial values
	x_1 <- rnd(x_1, opts$round_interim)
	x_2 <- rnd(x_2, opts$round_interim)

	# Calculate diff scores
	x_D <- rnd(x_2 - x_1, opts$round_final)
	n <- length(x_D)

	# Get base formula without LaTeX math/aligned blocks
	x_D.formula <- diff_score_formula(sym_x = sym_x,
																		use_aligned = opts$use_aligned,
																		add_math = FALSE,
																		add_aligned = FALSE)
	# Format values for solution string:
	x_1.fmt <- fmt(x_1, get_digits(c(x_1, x_2)))
	x_2.fmt <- fmt(x_2, get_digits(c(x_1, x_2)))
	x_D.fmt <- fmt(x_D, get_digits(x_D))

	# Create the solution string, with rounded values (minimally displayed)
	solution <- glue_solution(
		x_D.formula,
		"<<equals>> <<D.1>>",
		"<<equals>> <<D.2>>",
		D.1 = comma_list(lglue("(<<x_2.fmt>> - <<x_1.fmt>>)"), abbrev_at = abbrev_at),
		D.2 = comma_list(lglue("<<x_D.fmt>>"), abbrev_at = abbrev_at))

	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	# Return list containing both values and solution string
	list(x_1 = x_1,
			 x_2 = x_2,
			 x_D = x_D,
			 solution = solution,
			 formula = x_D.formula)
}

#' @rdname solve_diff_score
#'
#' @export
#'
diff_score_formula <- function(sym_x = "X",
															 ...) {

	# Check argument validity
	stopifnot(is.character(sym_x), nchar(sym_x) == 1)

	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts<- get_handcalcs_opts(...)

	# Use the appropriate equals sign for an aligned environment
	equals <- ifelse(opts$use_aligned, "&=", "=")

	# Create the formula string:
	solution<- lglue("<<sym_x>>_{D} <<equals>> <<sym_x>>_{2} - <<sym_x>>{1}")

	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	return(solution)
}

#' Mean Difference Score
#'
#' Calculate and present solutions for the mean of a set of difference scores.
#' This can either be provided as a raw set of scores provided as `x_D`,
#' (\eqn{M_{D} = \frac{\sum{X_{D}}}{N}}), or as the means of two different sets
#' of values provided as `M_1` and `M_2`, (\eqn{M_{D} = M_{2} - M_{1}}).
#'
#' @param x_D Numeric vector of difference scores for which to calculate the
#'   mean.
#' @param M_1 Numeric scalar. Mean of first set of values.
#' @param M_2 Numeric scalar. Mean of second set of values.
#' @param sym_x Character scalar. Symbol to represent x in formula (default:
#'   "X").
#' @param type Character scalar. What kind of formula to print? Can either be
#'   "x_D" for showing the formula calculated from raw differences cores (the
#'   default), or "M_diff" for showing the difference between group means. (When
#'   the `mean_diff_formula()` is called by `solve_mean_diff()`, chooses based
#'   on the type of data provided.)
#' @param ... Additional arguments to override default behaviors (see
#'   `[handcalcs_defaults()]`).
#'
#' @return `solve_mean_diff()` returns a list with the provided values (`x_D` or
#'   `M_1` and `M_2`), the interim calculations (`SumX_D` and `n`, if
#'   calculating from `x_D`), the final value (`M_D`), the solution string
#'   (`solution`), and the bare formula (`formula`) in LaTeX format.
#'   `mean_diff_formula()` returns just the bare formula in LaTeX format as a
#'   character string.
#'
#'
#' @export
#'
#' @examples
#' x_1 <- rnorm(10, mean = 0)
#' x_2 <- rnorm(10, mean = 1)
#' x_D <- solve_diff_score(x_1 = x_1, x_2 = x_2)$x_D
#' solve_mean_diff(x_D)
#'
#' # If you just want the bare formula as a string, use the formula function:
#' mean_diff_formula()
#' mean_diff_formula(type = "M_diff")
solve_mean_diff <- function(x_D,
														M_1,
														M_2,
														sym_x = "X",
														...) {

	# Check argument validity:
	stopifnot(is.character(sym_x), nchar(sym_x) == 1)

	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts <- get_handcalcs_opts(...)

	# Use the appropriate equals sign for an aligned environment
	equals <- ifelse(opts$use_aligned, "&=", "=")

	# Set unused calculated values to NULL (only values that get set will be returned)
	n <- SumX_D <- NULL

	if(!missing(x_D)) {
		stopifnot(is.numeric(x_D), any(!is.na(x_D)), length(x_D) > 0)

		# Round initial values
		x_D <- rnd(x_D, opts$round_interim)

		x_D.lst <- solve_mean(x_D,
													sub_val = "D",
													sym_x = sym_x,
													sub_x = "D",
													...)

		# Fix the names in the returned list:
		x_D.lst[c('x', 'n', 'SumX', 'M', 'solution', 'formula')] %>%
			purrr::set_names(c('x_D', 'n', 'SumX_D', 'M_D', 'solution', 'formula'))

	} else {
		stopifnot(!missing(M_1), !missing(M_2))
		stopifnot(is.numeric(M_1), any(!is.na(M_1)), length(M_1) == 1)
		stopifnot(is.numeric(M_2), any(!is.na(M_2)), length(M_2) == 1)

		# Round initial values
		M_1 <- rnd(M_1, opts$round_interim)
		M_2 <- rnd(M_2, opts$round_interim)

		# Calculate mean
		M_D <- rnd(M_2 - M_1, opts$round_final)

		# Get base formula without LaTeX math/aligned blocks
		M_D.formula <- mean_diff_formula(type = "M_diff",
																		 use_aligned = opts$use_aligned,
																		 add_math = FALSE,
																		 add_aligned = FALSE)

		# Create the solution string, with rounded values (minimally displayed)
		solution <- glue_solution(
			M_D.formula,
			"<<equals>> <<M_2>> - <<M_1>>",
			"<<equals>> \\mathbf{<<M_D>>}",
			# Print the final calculated value minimally rounded unless round_to is
			# set to 'sigfigs', in which case just present the final rounded value as is.
			M_D = ifelse(opts$round_to == 'sigfigs', M_D, fmt(M_D, get_digits(M_D, opts$round_final))))

		# Add LaTeX math code, if desired.
		if (opts$add_aligned) solution <- add_aligned(solution)
		if (opts$add_math) solution <- add_math(solution)

		# Return list containing both values and solution string
		list(M_1 = M_1,
				 M_2 = M_2,
				 M_D = M_D,
				 solution = solution,
				 formula = M_D.formula)
	}
}

#' @rdname solve_mean_diff
#'
#' @export
#'
mean_diff_formula <- function(type = "x_D",
															sym_x = "X",
															...) {
	# Check argument validity
	type <- match.arg(type, choices = c("x_D", "M_diff"))
	stopifnot(!is.character(sym_x))

	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts<- get_handcalcs_opts(...)

	# Use the appropriate equals sign for an aligned environment
	equals <- ifelse(opts$use_aligned, "&=", "=")

	if(type == 'x_D') {
		solution <- mean_formula(sub_val = "D", sym_x = sym_x, sub_x = "D",
														 show_summation = opts$show_summation)

	} else if(type == 'M_diff') {
		solution<- lglue("M_{D} <<equals>> M_{2} - M_{1}")
	}

	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	return(solution)
}



#' Difference Scores: Sum of Squares, Variance, Standard Deviation
#'
#' Convenience wrappers for calculating sum of squares ([solve_sum_squares()],
#' [solve_sum_squares2()]), variance ([solve_s2()]), and standard deviation
#' ([solve_sd()]) when dealing with difference scores. Essentially just
#' ensures that they use the "D" subscript appropriately.
#'
#' @param x_D Numeric vector of difference scores.
#' @param sym_x Character scalar. Symbol to represent x in formula (default:
#'   "X").
#' @param SS_D Numeric scalar: sum of squares of difference scores.
#' @param n Numeric scalar: sample size.
#' @param s2_D Numeric scalar: sample variance of difference scores (\eqn{s_{D}^2}).
#' @param SS.f Formula to use for sum of squares calculation (either
#'   'solve_sum_squares' or 'solve_sum_squares2'). Only used when calculating SS
#'   from raw difference scores (`x_D`).
#' @param ... Additional arguments to override default behaviors (see
#'   `[handcalcs_defaults()]`).
#'
#' @return List of values. See the original functions for more information:
#'   [solve_sum_squares()], [solve_sum_squares2()], [solve_s2()],
#'   [solve_sd()].
#' @export
#'
#' @examples
#' solve_sum_squares_diff(x_D = 1:10)
#'
#' solve_sum_squares2_diff(x_D = 1:10)
#'
#' solve_s2_diff(x_D = 1:10)
#'
#' solve_sd_diff(x_D = 1:10)
solve_sum_squares_diff <- function(x_D,
																	 sym_x = "X",
																	 ...) {

	solve_sum_squares(x = x_D,
										sub_val = "D",
										sym_x = sym_x,
										sub_x = "D",
										...)
}

#' @rdname solve_sum_squares_diff
#'
#' @export
#'
solve_sum_squares2_diff <- function(x_D,
																	 sym_x = "X",
																	 ...) {
	solve_sum_squares2(x = x_D,
										 sub_val = "D",
										 sym_x = sym_x,
										 sub_x = "D",
										 ...)
}

#' @rdname solve_sum_squares_diff
#'
#' @export
#'
solve_s2_diff <- function(x_D,
													SS_D,
													n,
													sym_x = "X",
													SS.f = solve_sum_squares,
													...) {
	solve_s2(x = x_D,
					 SS = SS_D,
					 n = n,
					 sub_val = "D",
					 sym_x = sym_x,
					 sub_x = "D",
					 SS.f = SS.f,
					 ...)

}

#' @rdname solve_sum_squares_diff
#'
#' @export
#'
solve_sd_diff <- function(x_D,
													SS_D,
													n,
													s2_D,
													sym_x = "X",
													SS.f = solve_sum_squares,
													...) {

	solve_sd(x = x_D,
					 SS = SS_D,
					 n = n,
					 s2 = s2_D,
					 sub_val = "D",
					 sym_x = sym_x,
					 sub_x = "D",
					 SS.f = SS.f,
					 ...)
}
