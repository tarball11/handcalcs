
# Encloses the solution in a LaTeX aligned block
add_aligned<- function(solution) {
	paste0("\\begin{aligned} ", solution, "\\end{aligned}")
}

# Encloses the solution in a LaTeX display math block
add_math<- function(solution) {
	paste0("$$ ", solution, "$$")
}

# LaTeX-friendly glue
lglue <- function(..., .envir = parent.frame()) {
	glue::glue(..., .envir = .envir, .open = "<<", .close = ">>")
}

# Wrapper for glue::glue() to generate solution strings, using defaults useful
# for LaTeX strings
glue_solution <- function(..., .envir = parent.frame()) {
	glue::glue(...,
						 .envir = .envir,
						 .sep = " \\\\ ",
						 .open = "<<",
						 .close = ">>",
	)
}


#' Explicit Summation
#'
#' Takes a character vector of values that would be part of a summation sequence
#' (e.g., produced by \code{glue::glue()}) and returns a string enumerating the
#' process. For instance, if you wished to sum the integers 1 to 3, this would
#' be spelled out as "1 + 2 + 3". By default, will abbreviate the summation for
#' vectors longer than `\code{abbrev_sum}` by using an ellipsis (LaTeX: `\cdots`).
#'
#' @param sum_values Character vector (or vector of values that can be coerced
#'   to character) of values to be summed.
#' @param abbrev_sum Numeric scalar indicating when to abbreviate the summation
#'   (with an ellipsis). If the length of sum_values exceeds the value of
#'   abbrev_sum, will return abbreviated string. (Note: minimum value is 3.)
#'
#' @return Explicit summation string (scalar). For instance,
#'   \code{summation(1:4)} would return "1 + 2 + 3 + 4".
#' @export
#'
#' @examples
#' # Simplest use will take any string (or values that can be coerced):
#' summation(1:4)
#'
#' # More complex uses are possible (e.g., sum of squared deviations)
#' x <- sample(1:20, size = 10)
#' M <- round(mean(x), 2)
#' sum_values <- glue::glue("(<<x>> - <<M>>)^2", .open = "<<", .close = ">>")
#' summation(sum_values)
#'
#' # If you wish to avoid abbreviation, set the value higher:
#' summation(sum_values, abbrev_sum = length(sum_values))
#'
summation<- function(sum_values, abbrev_sum = 5) {
	stopifnot(is.character(as.character(sum_values)))
	stopifnot(is.numeric(abbrev_sum), abbrev_sum >= 3)

	n <- length(sum_values)

	if(n <= 2 | n <= abbrev_sum) {
		sum_values %>%
			glue::glue_collapse(sep = " + ")
	} else {
		lglue("<<sum_values[1]>> + <<sum_values[2]>> + \\cdots + <<sum_values[n]>>")
	}
}
