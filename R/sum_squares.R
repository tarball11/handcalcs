#' Sum of Squares
#'
#' Calculate value of sum of squares (with rounding) and present solutions.
#' \code{solve_sum_squares()} produces the interim calculations and a full
#' solution string, starting with the bare formula. If you just want the bare
#' formula, use \code{sum_squares_formula()}.
#'
#' The standard functions use the conceptual formula:
#' \deqn{SS = \sum(X - M)^2}.
#'
#' The '2' functions (\code{solve_sum_squares2},
#' \code{sum_squares_formula2}) use the alternative computational formula:
#' \deqn{SS = \sum(X^2) - [(\sum X)^2] / n}
#'
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
#' @return The two solutions functions (\code{solve_sum_squares},
#'   \code{solve_sum_squares2}) return a named list with the interim
#'   calculations, the final value (\code{SS}), the solution string
#'   (\code{solution}), and the bare formula (\code{formula}) in LaTeX format.
#'   (Note that the two functions will return different values for the interim
#'   calculations based on using different formulas.) The two formula functions
#'   (\code{sum_squares_formula}, \code{sum_squares_formula2}) return the bare
#'   formula in LaTeX format as a character string.
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
#' # The value of sub can be long, but not sym.
#' \dontrun{
#' solve_sum_squares(x, sub = "Donuts", sym = "Donuts")
#' }
#'
#' # If you just want the bare formula as a string, use a formula function():
#' sum_squares_formula()
#' sum_squares_formula2()
#'
#' # Can set parameters to change symbols used:
#' sum_squares_formula(sub = "Y", sym = "Y")
#'
solve_sum_squares <- function(x,
                              sub = "",
                              sym = "X",
															abbrev_sum = 4,
															...) {

  # Check argument validity
  stopifnot(is.numeric(x), any(!is.na(x)))
  stopifnot(is.character(sub), length(sub) == 1)
  stopifnot(is.character(sym), nchar(sym) == 1)
  stopifnot(is.numeric(abbrev_sum), abbrev_sum >= 3)

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
  SS.formula <- sum_squares_formula(sub = sub,
  																	sym = sym,
  																	use_aligned = opts$use_aligned,
  																	add_math = FALSE,
  																	add_aligned = FALSE)

  # Create the solution string, with rounded values (minimally displayed)
  solution <- glue_solution(
    SS.formula,
    "<<equals>> <<Sum.1>>",
    "<<equals>> <<Sum.2>>",
    "<<equals>> <<Sum.3>>",
    "<<equals>> \\textbf{<<SS>>}",
    # Put M in brackets if it's negative (avoid confusion in deviation scores)
    Sum.1 = summation(lglue("(<<x>> - <<M>>)^2",
    									M = ifelse(M < 0, paste0('[', M, ']'), M)),
    									abbrev_sum = abbrev_sum),
    Sum.2 = summation(lglue("(<<Dev>>)^2"), abbrev_sum = abbrev_sum),
    Sum.3 = summation(lglue("<<DevSq>>"), abbrev_sum = abbrev_sum),
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
sum_squares_formula <- function(sub = "",
                                sym = "X",
                                ...) {
  # Check argument validity
  stopifnot(is.character(sub), length(sub) == 1)
  stopifnot(is.character(sym), nchar(sym) == 1)

  # Get list of options (allowing user to override defaults) for rounding
  # behavior and for presenting solutions in LaTeX environment.
  opts <- get_handcalcs_opts(...)

  # Use the appropriate equals sign for an aligned environment
  equals <- ifelse(opts$use_aligned, "&=", "=")

  solution <- lglue(
    "SS_{<<sub>>} <<equals>> \\sum(<<sym>> - M_{<<sub>>})^{2} \\\\",
    "<<equals>> (<<sym>>_{1} - M_{<<sub>>})^2 + (<<sym>>_{2} - M_{<<sub>>})^2 + \\cdots + (<<sym>>_{n} - M_{<<sub>>})^2")

  # Add LaTeX math code, if desired.
  if (opts$add_aligned) solution <- add_aligned(solution)
  if (opts$add_math) solution <- add_math(solution)

  return(solution)
}


#' @rdname solve_sum_squares
#'
#' @export
solve_sum_squares2 <- function(x,
                               sub = "",
                               sym = "X",
															 abbrev_sum = 4,
															 ...) {

  # Check argument validity
  stopifnot(is.numeric(x), any(!is.na(x)))
  stopifnot(is.character(sub), length(sub) == 1)
  stopifnot(is.character(sym), nchar(sym) == 1)
  stopifnot(is.numeric(abbrev_sum), abbrev_sum >= 3)

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
  SS.formula <- sum_squares_formula2(sub = sub,
  																	 sym = sym,
  																	 use_aligned = opts$use_aligned,
  																	 add_math = FALSE,
  																	 add_aligned = FALSE)

  # Create the solution string, with rounded values (minimally displayed)
  solution <- glue_solution(
    SS.formula,
    "<<equals>> <<Sum_XSq.1>> - \\frac{(<<SumX.1>>)^2}{<<n>>}",
    "<<equals>> <<Sum_XSq.2>> - \\frac{(<<SumX>>)^2}{<<n>>}",
    "<<equals>> <<Sum_XSq>> - \\frac{<<Sq_SumX>>}{<<n>>}",
    "<<equals>> <<Sum_XSq>> - <<Sq_SumX_n>>",
    "<<equals>> \\textbf{<<SS>>}",
    Sum_XSq.1 = summation(lglue("(<<x>>)^2"),
    											abbrev_sum = abbrev_sum),
    Sum_XSq.2 = summation(lglue("(<<XSq>>)"),
    											abbrev_sum = abbrev_sum),
    SumX.1 = summation(lglue("[<<x>>]"), abbrev_sum = abbrev_sum),
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
sum_squares_formula2 <- function(sub = "",
                                 sym = "X",
                                 ...) {
  # Check argument validity
  stopifnot(is.character(sub), length(sub) == 1)
  stopifnot(is.character(sym), nchar(sym) == 1)

  # Get list of options (allowing user to override defaults) for rounding
  # behavior and for presenting solutions in LaTeX environment.
  opts <- get_handcalcs_opts(...)

  # Use the appropriate equals sign for an aligned environment
  equals <- ifelse(opts$use_aligned, "&=", "=")

  solution <- lglue(
    "SS_{<<sub>>} <<equals>> \\sum{(<<sym>>^2)} - \\frac{(\\sum{<<sym>>})^2}{N} \\\\",
    "<<equals>> (<<sym>>_{1}^2 + <<sym>>_{2}^2 + \\cdots + <<sym>>_{n}^2) - \\frac{(<<sym>>_{1} + <<sym>>_{2} + \\cdots + <<sym>>_{n})^2}{N}")

  # Add LaTeX math code, if desired.
  if (opts$add_aligned) solution <- add_aligned(solution)
  if (opts$add_math) solution <- add_math(solution)

  return(solution)
}
