#' Arithmetic Mean
#'
#' Calculate value of arithmetic mean (with rounding) and present solutions.
#' \code{solve_mean()} produces the interim calculations and a full solution
#' string, starting with the bare formula. If you just want the bare formula,
#' use \code{mean_formula()}.
#'
#' @param x Numeric vector.
#' @param sub Character scalar. Name of numeric vector (reported as subscript in
#'   solutions). Leave empty to report no subscript.
#' @param sym Character scalar. Symbol to represent x in formula (default: "X").
#'   Only one character allowed.
#' @param abbrev_sum Numeric scalar. Maximum length of x before it abbreviates
#'   explicit summation within the solution using an ellipsis? (See
#'   \code{\link{summation}}.)
#' @param ... Additional arguments to override default behaviors (see
#'   \code{\link{handcalcs_defaults}}).
#'
#' @return \code{solve_mean()} returns a list with the interim calculations
#'   (\code{SumX}, \code{n}), the final value (\code{M}), the solution string
#'   (\code{solution}), and the bare formula (\code{formula}) in LaTeX format.
#'   \code{mean_formula} returns just the bare formula in LaTeX format as a
#'   character string.
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
                       sub = "",
                       sym = "X",
											 abbrev_sum = 5,
                       ...) {

	# Check argument validity
  stopifnot(is.numeric(x), any(!is.na(x)))
  stopifnot(is.character(as.character(sub)), length(sub) == 1)
  stopifnot(is.character(sym), nchar(sym) == 1)
  stopifnot(is.numeric(abbrev_sum), abbrev_sum >= 3)

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
  M.formula <- mean_formula(sub = sub,
  													sym = sym,
  													use_aligned = opts$use_aligned,
  													add_math = FALSE,
  													add_aligned = FALSE)

  # Create the solution string, with rounded values (minimally displayed)
  solution <- glue_solution(
    M.formula,
    "<<equals>> \\frac{<<Num>>}{<<n>>}",
    "<<equals>> \\frac{<<SumX>>}{<<n>>}",
    "<<equals>> \\mathbf{<<M>>}",
    Num = summation(lglue("<<x>>"), abbrev_sum = abbrev_sum),
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
mean_formula <- function(sub = "",
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

  solution<- lglue(
  	"M_{<<sub>>} <<equals>> \\frac{\\sum{<<sym>>}}{n} \\\\",
  	"<<equals>> \\frac{<<sym>>_{1} + <<sym>>_{2} + \\cdots + <<sym>>_{n}}{n}")

  # Add LaTeX math code, if desired.
  if (opts$add_aligned) solution <- add_aligned(solution)
  if (opts$add_math) solution <- add_math(solution)

  return(solution)
}


#' Median: calculate value (with rounding) and present solutions
#'
#' @param x Numeric vector.
#' @param sub Character scalar. Name of numeric vector (reported as subscript in
#'   solutions). Leave empty to report no subscript.
#' @param ... Additional arguments to override default behaviors (see
#'   \code{\link{handcalcs_defaults}}).
#'
#' @return A list with the median value (\code{Med}) and the solution
#'   (\code{solution}) in LaTeX format.
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
                         sub = "",
                         ...) {
  # Check argument validity; Disallow missing values
  stopifnot(is.numeric(x), any(!is.na(x)))
  stopifnot(is.character(sub), length(sub) == 1)

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
    "\\text{Median}_{<<sub>>} <<equals>> \\mathbf{<<Med>>}",
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
#' @param sub Character scalar. Name of numeric vector (reported as subscript in
#'   solutions). Leave empty to report no subscript.
#' @param ... Additional arguments to override default behaviors (see
#'   \code{\link{handcalcs_defaults}}).
#'
#' @return A list with the mode value(s) (\code{Mode}) and the solution
#'   (\code{solution}) in LaTeX format.
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
                       sub = "",
                       ...) {
  # Check argument validity; Disallow missing values
  stopifnot(is.numeric(x), any(!is.na(x)))
  stopifnot(is.character(sub), length(sub) == 1)


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
    "\\text{Mode}_{<<sub>>} <<equals>> \\mathbf{<<Mode>>}",
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
#' @param abbrev_sum Numeric scalar. Maximum length of x before it abbreviates
#'   explicit summation within the solution using an ellipsis? (See
#'   \code{\link{summation}}.)
#' @param ... Additional arguments to override default behaviors (see
#'   \code{\link{handcalcs_defaults}}).
#'
#' @return \code{solve_weighted_mean()} returns a list with vectors for the
#'   means (\code{M}) and sample sizes (\code{n}), the interim calculations
#'   (\code{Sum_M_x_n}, \code{Sum_n}), the final value (\code{M_w}), the
#'   solution string (\code{solution}), and the bare formula (\code{formula}) in
#'   LaTeX format. \code{weighted_mean_formula} returns just the bare formula in
#'   LaTeX format as a character string.
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
																abbrev_sum = 5,
																...) {
	# Samples must be a list with at least two subgroups represented
  stopifnot(is.list(Samples), length(Samples) > 1)
  # Disallow missing values.
  stopifnot(all(!is.na(purrr::flatten_dbl(Samples))))
  # Samples must be list of vectors of length 2, with values named M and n
  stopifnot(all(purrr::map_lgl(Samples, ~ length(.x) == 2)))
  stopifnot(all(purrr::flatten_lgl(purrr::map(Samples, ~ names(.x) == c("M", "n")))))
  # Confirm abbrev_sum is a valid value
  stopifnot(is.numeric(abbrev_sum), abbrev_sum >= 3)

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
  																		 add_math = FALSE,
  																		 add_aligned = FALSE)

  # Create the solution string, with rounded values (minimally displayed)
  solution <- glue_solution(
    M_w.formula,
    "<<equals>> \\frac{<<Num>>}{<<Denom>>}",
    "<<equals>> \\frac{<<Sum_M_x_n>>}{<<Sum_n>>}",
    "<<equals>> \\mathbf{<<M_w>>}",
    Num = summation(lglue("(<<M>>)(<<n>>)"), abbrev_sum = abbrev_sum),
    Denom = summation(lglue("<<n>>"), abbrev_sum = abbrev_sum),
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

	solution<- lglue("M_{weighted} <<equals>> \\frac{<<Num>>}{<<Denom>>}",
									 Num = "(M_{1})(n_{1}) + (M_{2})(n_{2}) + \\cdots + (M_{k})(n_{k})",
									 Denom = "n_{1} + n_{2} + \\cdots + n_{k}")

  # Add LaTeX math code, if desired.
  if (opts$add_aligned) solution <- add_aligned(solution)
  if (opts$add_math) solution <- add_math(solution)

  return(solution)
}
