#' Arithmetic mean: calculate value (with rounding) and present solutions
#'
#' @param x Numeric vector.
#' @param sub Character scalar. Name of numeric vector (reported as subscript in
#'   solutions). Leave empty to report no subscript.
#' @param sym Character scalar. Symbol to represent x in formula (default: "X").
#'   Only one character allowed.
#' @param ... Additional arguments to override default behaviors (see
#'   \code{\link{handcalcs_defaults}}).
#'
#' @return A list with the interim calculations (\code{SumX}, \code{n}), the
#'   final value (\code{M}), and the solution string (\code{solution}) in LaTeX format.
#' @export
#'
#' @examples
#' x <- sample(x = 1:10, size = 20, replace = TRUE)
#' solve_mean(x)
#'
#' # Note that the value calculated by solve_mean(x) will be different than the
#' # value calculated by mean(x) when rounding occurs.
#' (x <- rnorm(20))
#' solve_mean(x, sub = "Y")
#'
#' # The value of sub can be long, but not sym.
#' solve_mean(x, sub = "Donuts")
#'
solve_mean <- function(x,
                       sub = "",
                       sym = ifelse(nchar(sub) == 1, sub, "X"),
											 ...) {
  # Check argument validity; Disallow missing values
  stopifnot(is.numeric(x), any(!is.na(x)))
  stopifnot(is.character(sub), length(sub) == 1)
  stopifnot(is.character(sym), nchar(sym) == 1)
	opts<- list(...)

  # Round values to round_interim at each interim step
  round_interim <- check_defaults('round_interim', opts)
  # Round values to round_final at final step
  round_final <- check_defaults('round_final', opts)

  # Calculate mean
  x <- round(x, round_interim)
  n <- length(x)
  SumX <- round(sum(x), round_interim)
  M <- round(SumX / n, round_final)

  # Use the appropriate equals sign for an aligned environment
  use_aligned <- check_defaults('use_aligned', opts)
  equals <- ifelse(use_aligned, "&=", "=")

  # Create the solution string, with rounded values (minimally displayed)
  solution <- glue_solution(
  	"M_{[sub]} [equals] \\frac{\\sum{[sym]}}{N}",
  	"[equals] \\frac{[SumX]}{[n]}",
  	"[equals] \\textbf{[M]}",
  	SumX = fmt(SumX, get_digits(SumX, round_interim)),
  	M = fmt(M, get_digits(M, round_final))
  	)

  # Add LaTeX math code, if desired.
  add_math <- check_defaults('add_math', opts)
  add_aligned <- check_defaults('add_aligned', opts)
  if (add_aligned) solution <- add_aligned(solution)
  if (add_math) solution <- add_math(solution)

  # Return list containing both values and solution string
  l <- list(x = x, n = n, SumX = SumX, M = M, solution = solution)

  return(l)
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
  opts<- list(...)

  # Round values to round_interim at each interim step
  round_interim <- check_defaults('round_interim', opts)
  # Round values to round_final at final step
  round_final <- check_defaults('round_final', opts)

  # Calculate median
  x <- round(x, round_interim)
  x <- sort(x)
  Med <- round(median(x), round_final)

  # Use the appropriate equals sign for an aligned environment
  use_aligned <- check_defaults('use_aligned', opts)
  equals <- ifelse(use_aligned, "&=", "=")

  # Create the solution string, with rounded values (minimally displayed)
  solution <- glue_solution(
  	"\\text{Median}_{[sub]} [equals] \\textbf{[Med]}",
  	Med = fmt(Med, get_digits(Med, round_final))
  )

  # Add LaTeX math code, if desired.
  add_math <- check_defaults('add_math', opts)
  add_aligned <- check_defaults('add_aligned', opts)
  if (add_aligned) solution <- add_aligned(solution)
  if (add_math) solution <- add_math(solution)

  # Return list containing both values and solution string
  l <- list(x = x, Med = Med, solution = solution)

  return(l)
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
#' # value(s) provided may differ from the values in the raw data.
#' (x <- rnorm(20))
#' solve_mode(x, round_interim = 2)$Mode
#'
solve_mode <- function(x,
                       sub = "",
											 ...) {
  # Check argument validity; Disallow missing values
  stopifnot(is.numeric(x), any(!is.na(x)))
  stopifnot(is.character(sub), length(sub) == 1)
  opts<- list(...)

  # Round values to round_interim at each interim step
  round_interim <- check_defaults('round_interim', opts)
  # Round values to round_final at final step
  round_final <- check_defaults('round_final', opts)

  # Calculate mode(s)
  x <- round(x, round_interim)
  x <- sort(x)
  x.max <- which(table(x) == max(table(x)))
  Mode <- as.numeric(names(x.max))

  # Use the appropriate equals sign for an aligned environment
  use_aligned <- check_defaults('use_aligned', opts)
  equals <- ifelse(use_aligned, "&=", "=")

  # Create the solution string, with rounded values (minimally displayed)
  solution <- glue_solution(
  	"\\text{Mode}_{[sub]} [equals] \\textbf{[Mode]}",
  	Mode = paste(Mode, collapse = ", ")
  )

  # Add LaTeX math code, if desired.
  add_math <- check_defaults('add_math', opts)
  add_aligned <- check_defaults('add_aligned', opts)
  if (add_aligned) solution <- add_aligned(solution)
  if (add_math) solution <- add_math(solution)

  # Return list containing both values and solution string
  l <- list(x = x, Mode = Mode, solution = solution)

  return(l)
}
