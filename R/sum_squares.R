#' Sum of Squares
#'
#' Calculate value of sum of squares (with rounding) and present solutions.
#' `solve_sum_squares()` produces the interim calculations and a full solution
#' string, starting with the bare formula. If you just want the bare formula,
#' use `sum_squares_formula()`. To generate the table demonstrating the
#' calculations, use `sum_squares_table`.
#'
#' The standard functions use the conceptual formula: \deqn{SS = \sum(X - M)^2}.
#'
#' The '2' functions (`solve_sum_squares2`, `sum_squares_formula2`) use the
#' alternative computational formula: \deqn{SS = \sum(X^2) - [(\sum X)^2] / n}
#'
#'
#' @param x Numeric vector.
#' @param sub_val Character scalar. Subscript for the value to be calculated in
#'   the formula, in this case the sum of squares (e.g., \eqn{SS_{x}}). Leave
#'   empty to report no subscript.
#' @param sym_x Character scalar. Symbol to represent x in formula (default:
#'   "X").
#' @param sub_x Character scalar. Subscript for x in the formula (e.g.,
#'   \eqn{X_{D}})
#' @param SS.lst List of values produced by `solve_sum_squares` or
#'   `solve_sum_squares2`. Be sure to use the table function that corresponds to
#'   the solution function.
#' @param ... Additional arguments to override default behaviors (see
#'   [handcalcs_defaults()]).
#'
#' @return The two solutions functions (`solve_sum_squares`,
#'   `solve_sum_squares2`) return a named list with the interim
#'   calculations, the final value (`SS`), the solution string
#'   (`solution`), and the bare formula (`formula`) in LaTeX format.
#'   (Note that the two functions will return different values for the interim
#'   calculations based on using different formulas.) The two formula functions
#'   (`sum_squares_formula`, `sum_squares_formula2`) return the bare
#'   formula in LaTeX format as a character string. The two table functions
#'   (`sum_squares_table`, `sum_squares_table2`) return a gt object
#'   which can be rendered into HTML or PDF.
#'
#' @export
#'
#' @examples
#' # The formulas will return similar results when rounding is minimal
#' (x <- sample(x = 1:10, size = 20, replace = TRUE))
#' solve_sum_squares(x)$SS
#' solve_sum_squares2(x)$SS
#'
#' # They are more likely to differ when there is a lot of rounding:
#' (x <- rnorm(20))
#' solve_sum_squares(x)
#' solve_sum_squares2(x)
#'
#' # Can override the default values for rounding interim and final values:
#' solve_sum_squares(x, round_interim = 2, round_final = 2)
#'
#' # The value of sub_val can be long, but not sym_x.
#' \dontrun{
#' solve_sum_squares(x, sub_val = "Donuts", sym_x = "Donuts")
#' }
#'
#' # If you just want the bare formula as a string, use a formula function():
#' sum_squares_formula()
#' sum_squares_formula2()
#'
#' # Can set parameters to change symbols used:
#' sum_squares_formula(sub_val = "Y", sym_x = "Y")
#'
solve_sum_squares <- function(x,
															sub_val = "",
															sym_x = "X",
															sub_x = "",
															...) {

	# Check argument validity
	stopifnot(is.numeric(x), any(!is.na(x)))
	stopifnot(is.character(sub_val), length(sub_val) == 1)
	stopifnot(is.character(sym_x), nchar(sym_x) == 1)
	stopifnot(is.character(sub_x))

	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts <- get_handcalcs_opts(...)

	# Use the appropriate equals sign for an aligned environment
	equals <- ifelse(opts$use_aligned, "&=", "=")

	# Calculate sum of squares
	x <- rnd(x, opts$round_interim)
	n <- length(x)

	# First, calculate the mean (note: consider the mean an interim calculation)
	M <- solve_mean(x,
									round_interim = opts$round_interim,
									round_final = opts$round_interim)$M

	# Deviation scores
	Dev <- rnd(x - M, opts$round_interim)
	# Squared deviation scores
	DevSq <- rnd(Dev^2, opts$round_interim)
	# Sum of the squared deviations
	SS <- rnd(sum(DevSq), opts$round_final)

	# Get base formula without LaTeX math/aligned blocks
	SS.formula <- sum_squares_formula(sub_val = sub_val,
																		sym_x = sym_x,
																		sub_x = sub_x,
																		show_summation = opts$show_summation,
																		use_aligned = opts$use_aligned,
																		add_math = FALSE,
																		add_aligned = FALSE)

	# Create the solution string, with rounded values (minimally displayed)
	solution <- glue_solution(
		SS.formula,
		if(opts$show_summation) {"<<equals>> <<Sum.1>>"},
		if(opts$show_summation) {"<<equals>> <<Sum.2>>"},
		if(opts$show_summation) {"<<equals>> <<Sum.3>>"},
		"<<equals>> \\mathbf{<<SS>>}",
		# Put x & M in brackets if negative (avoid confusion in deviation scores)
		Sum.1 = summation(lglue("(<<[x]>> - <<[M]>>)^2"), abbrev_sum = opts$abbrev_sum),
		Sum.2 = summation(lglue("(<<Dev>>)^2"), abbrev_sum = opts$abbrev_sum),
		Sum.3 = summation(lglue("<<DevSq>>"), abbrev_sum = opts$abbrev_sum),
		# Round based on the precision of x and the final calculated value unless
		# round_to is set to 'sigfigs', in which case just present the final rounded
		# value as is.
		SS = ifelse(opts$round_to == "sigfigs",
								SS,
								fmt(SS, get_digits(c(x, SS), opts$round_final))
		)
	)

	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	# Return list containing both values and solution string
	list(x = x,
			 M = M,
			 n = n,
			 Dev = Dev,
			 DevSq = DevSq,
			 SS = SS,
			 solution = solution,
			 formula = SS.formula)
}

#' @rdname solve_sum_squares
#'
#' @export
#'
sum_squares_formula <- function(sub_val = "",
																sym_x = "X",
																sub_x = "",
																...) {
	# Check argument validity
	stopifnot(is.character(sub_val), length(sub_val) == 1)
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
		"SS_{<<sub_val>>} <<equals>> \\sum(<<sym_x>>_{<<sub_x>>} - M_{<<sub_val>>})^{2}",
		if(opts$show_summation) {"\\\\ <<equals>> (<<sym_x>>_{<<sub_x_seq[1]>>} - M_{<<sub_val>>})^2 + (<<sym_x>>_{<<sub_x_seq[2]>>} - M_{<<sub_val>>})^2 + \\cdots + (<<sym_x>>_{<<sub_x_seq[3]>>} - M_{<<sub_val>>})^2"})

	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	return(solution)
}


#' @rdname solve_sum_squares
#'
#' @export
#'
sum_squares_table <- function(SS.lst,
															sub_val = "",
															sym_x = "X",
															sub_x = "",
															...) {

	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts <- get_handcalcs_opts(...)

	tibble::tibble(x = SS.lst$x,
								 Dev = SS.lst$Dev,
								 DevSq = SS.lst$DevSq) %>%
		gt::gt() %>%
		gt::cols_label(
			x = lglue('$$<<sym_x>>_{<<sub_x>>}$$'),
			Dev = lglue('$$(<<sym_x>>_{<<sub_x>>} - M_{<<sub_val>>})$$'),
			DevSq = lglue('$$(<<sym_x>>_{<<sub_x>>} - M_{<<sub_val>>})^2$$')) %>%
		gt::grand_summary_rows(
			columns = c(x, Dev, DevSq),
			fns = list("$$\\sum$$" = ~fmt(sum(.),
																		get_digits(., opts$round_interim))),
			formatter = gt::fmt_passthrough) %>%
		table_fmt()
}



#' @rdname solve_sum_squares
#'
#' @export
solve_sum_squares2 <- function(x,
															 sub_val = "",
															 sym_x = "X",
															 sub_x = "",
															 ...) {

	# Check argument validity
	stopifnot(is.numeric(x), any(!is.na(x)))
	stopifnot(is.character(sub_val), length(sub_val) == 1)
	stopifnot(is.character(sym_x), nchar(sym_x) == 1)
	stopifnot(is.character(sub_x))

	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts <- get_handcalcs_opts(...)

	# Use the appropriate equals sign for an aligned environment
	equals <- ifelse(opts$use_aligned, "&=", "=")

	# Calculate sum of squares
	x <- rnd(x, opts$round_interim)

	# Square of x values
	XSq <- rnd(x^2, opts$round_interim)
	# Sum of squared x values
	Sum_XSq <- rnd(sum(XSq), opts$round_interim)

	# Sum of X
	SumX <- rnd(sum(x), opts$round_interim)
	# Square of sum of X
	Sq_SumX <- rnd(SumX^2, opts$round_interim)
	n <- length(x)
	Sq_SumX_n <- rnd(Sq_SumX / n, opts$round_interim)

	# Sum of squares
	SS <- rnd(Sum_XSq - Sq_SumX_n, opts$round_final)

	# Get base formula without LaTeX math/aligned blocks
	SS.formula <- sum_squares_formula2(sub_val = sub_val,
																		 sym_x = sym_x,
																		 show_summation = opts$show_summation,
																		 use_aligned = opts$use_aligned,
																		 add_math = FALSE,
																		 add_aligned = FALSE)

	# Create the solution string, with rounded values (minimally displayed)
	solution <- glue_solution(
		SS.formula,
		if(opts$show_summation) {"<<equals>> <<Sum_XSq.1>> - \\frac{(<<SumX.1>>)^2}{<<n>>}"},
		if(opts$show_summation) {"<<equals>> <<Sum_XSq.2>> - \\frac{(<<SumX>>)^2}{<<n>>}"},
		"<<equals>> <<Sum_XSq>> - \\frac{(<<SumX>>)^2}{<<n>>}",
		"<<equals>> <<Sum_XSq>> - \\frac{<<Sq_SumX>>}{<<n>>}",
		"<<equals>> <<Sum_XSq>> - <<Sq_SumX_n>>",
		"<<equals>> \\mathbf{<<SS>>}",
		Sum_XSq.1 = summation(lglue("(<<x>>)^2"), abbrev_sum = opts$abbrev_sum),
		Sum_XSq.2 = summation(lglue("(<<XSq>>)"), abbrev_sum = opts$abbrev_sum),
		SumX.1 = summation(lglue("<<[x]>>"), abbrev_sum = opts$abbrev_sum),
		# Round based on the precision of x and the final calculated value unless
		# round_to is set to 'sigfigs', in which case just present the final rounded
		# value as is.
		SS = ifelse(opts$round_to == "sigfigs",
								SS,
								fmt(SS, get_digits(c(x, SS), opts$round_final)))
	)

	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	# Return list containing both values and solution string
	list(x = x,
			 n = n,
			 XSq = XSq,
			 Sum_XSq = Sum_XSq,
			 SumX = SumX,
			 Sq_SumX = Sq_SumX,
			 Sq_SumX_n = Sq_SumX_n,
			 SS = SS,
			 solution = solution,
			 formula = SS.formula)
}


#' @rdname solve_sum_squares
#'
#' @export
#'
sum_squares_formula2 <- function(sub_val = "",
																 sym_x = "X",
																 sub_x = "",
																 ...) {
	# Check argument validity
	stopifnot(is.character(sub_val), length(sub_val) == 1)
	stopifnot(is.character(sym_x), nchar(sym_x) == 1)
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
		"SS_{<<sub_val>>} <<equals>> \\sum{(<<sym_x>>_{<<sub_x>>}^2)} - \\frac{(\\sum{<<sym_x>>_{<<sub_x>>}})^2}{N}",
		if(opts$show_summation) {"\\\\ <<equals>> (<<sym_x>>_{<<sub_x_seq[1]>>}^2 + <<sym_x>>_{<<sub_x_seq[2]>>}^2 + \\cdots + <<sym_x>>_{<<sub_x_seq[3]>>}^2) - \\frac{(<<sym_x>>_{<<sub_x_seq[1]>>} + <<sym_x>>_{<<sub_x_seq[2]>>} + \\cdots + <<sym_x>>_{<<sub_x_seq[3]>>})^2}{N}"})

	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	return(solution)
}

#' @rdname solve_sum_squares
#'
#' @export
#'
sum_squares_table2 <- function(SS.lst,
															 sym_x = "X",
															 sub_x = "",
															 ...) {

	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts <- get_handcalcs_opts(...)

	tibble::tibble(x = SS.lst$x,
								 XSq = SS.lst$XSq) %>%
		gt::gt() %>%
		gt::cols_label(
			x = lglue('$$<<sym_x>>_{<<sub_x>>}$$'),
			XSq = lglue('$$<<sym_x>>_{<<sub_x>>}^2$$')) %>%
		gt::grand_summary_rows(
			columns = c(x, XSq),
			fns = list("$$\\sum$$" = ~fmt(sum(.),
																		get_digits(., opts$round_interim))),
			formatter = gt::fmt_passthrough) %>%
		table_fmt()
}

# Adds additional formatting to sum of squares tables.
table_fmt <- function(gt_obj) {
	# General left-right border style
	lr_bdr <- gt::cell_borders(sides = c('left', 'right'),
														 weight = gt::px(2),
														 color = '#CCCCCC',
														 style = 'solid')
	# General
	tb_bdr <- gt::cell_borders(sides = c('top', 'bottom'),
														 weight = 0)

	gt_obj %>%
		gt::tab_options(table.font.size = 12,
										column_labels.vlines.width = 1,
										column_labels.vlines.color = '#000000') %>%
		gt::opt_vertical_padding(scale = 0.20) %>%
		gt::opt_horizontal_padding(scale = 1.5) %>%
		gt::opt_table_font(font = list('Serif',
																	 gt::google_font(name = 'Inconsolata'))) %>%
		gt::tab_style(style = lr_bdr,
									locations = gt::cells_body()) %>%
		gt::tab_style(style = lr_bdr,
									locations = gt::cells_column_labels()) %>%
		gt::tab_style(style = lr_bdr,
									locations = gt::cells_grand_summary()) %>%
		gt::tab_style(style = tb_bdr,
									locations = gt::cells_stub()) %>%
		gt::tab_style(style = tb_bdr,
									locations = gt::cells_stubhead())


}
