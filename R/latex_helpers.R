
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


#' Abbreviate Math Sequences
#'
#' Three different functions designed to take a character vector of values that
#' would be part of a sequence that would be abbreviated with an ellipsis
#' (LaTeX: `\cdots`), if the sequence is sufficiently long. If the length of the
#' sequence is greater than or equal to `abbrev_at`, it will be abbreviated. If
#' not, the full sequence will be shown.
#'
#' `summation()` takes a sequence of values to be summed and returns a string
#' enumerating the process. For instance, if you wished to sum the integers 1 to
#' 100, this would be spelled out as "1 + 2 + ... + 100".
#'
#' `comma_list()` takes a set of values to be listed in a comma separated list.
#' For instance, if you wished to list the integers 1 to 100, this would be
#' spelled out as "1, 2, ..., 100".
#'
#' `math_list()` is a more general form, allowing you to specify the string
#' separating the values. (`summation()` and `comma_list()` are just wrappers
#' around `math_list()`.)
#'
#' @param values Character vector (or vector of values that can be coerced to
#'   character) of values to be summed.
#' @param abbrev_at,abbrev_sum Numeric scalar indicating when to abbreviate the
#'   summation with an ellipsis (default = 6). If the length of `values` exceeds
#'   the value of abbrev_at, will return abbreviated string. (Note: minimum
#'   value is 3.) Note: `abbrev_sum` is an alias for `abbrev_at` for
#'   `summation()`, which defaults to the value of `abbrev_sum` set globally.
#'
#' @return String containing the (possibly abbreviated) sequence. For instance,
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
#' values <- glue::glue("(<<x>> - <<M>>)^2", .open = "<<", .close = ">>")
#' summation(values)
#'
#' # If you wish to avoid abbreviation, set the value higher:
#' summation(values, abbrev_at = length(values))
#'
#' # Comma-separated lists are also simple:
#' comma_list(1:15)
#'
#' # Other types of sequences are possible with math_list()
#' math_list(1:10, sep = "*")
#'
math_list <- function(values, sep, abbrev_at = 6) {
	stopifnot(is.character(as.character(values)))
	stopifnot(is.character(as.character(sep)))
	stopifnot(is.numeric(abbrev_at), abbrev_at >= 3)

	n <- length(values)

	if(n <= 2 | n < abbrev_at) {
		values %>%
			glue::glue_collapse(sep = sep)
	} else {
		lglue("<<values[1]>><<sep>><<values[2]>><<sep>>\\cdots<<sep>><<values[n]>>")
	}
}

#' @rdname math_list
#'
#' @export
#'
summation <- function(values,
											abbrev_sum = get_handcalcs_opts()$abbrev_sum,
											abbrev_at = get_handcalcs_opts()$abbrev_sum) {
	if(missing(abbrev_sum) & missing(abbrev_at)) stop("Must supply an argument for abbrev_sum or abbrev_at.")
	if(missing(abbrev_at)) abbrev_at = abbrev_sum
	if(missing(abbrev_sum)) abbrev_sum = abbrev_at

	math_list(values, sep = " + ", abbrev_at = abbrev_at)
}
#' @rdname math_list
#'
#' @export
#'
comma_list <- function(values, abbrev_at = 6) math_list(values, sep = ", ", abbrev_at = abbrev_at)


