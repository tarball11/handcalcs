#' Solve arithmetic mean
#'
#' @param x A numeric vector
#' @param nm Name of numeric vector (reported as subscript in solutions)
#' @param precision Number of digits to round for interim calculation steps
#' @param digits Number of digits to report for final result
#'
#' @return A list with the interim calculations (SumX, N), the final value (M), and the solution (slv)
#' @export
#'
#' @examples
#' x <- 1:10
#' solve_mean(x)
#'
#' (x<- rnorm(20))
#' solve_mean(x, nm='Y')
#'
#'
solve_mean<- function(x, nm = 'X', precision = 4, digits = 4) {
	# Check argument type validity
	stopifnot(is.numeric(x),
						is.numeric(precision),
						is.numeric(digits),
						length(precision) == 1,
						length(digits) == 1)
	# Disallow missing values
	stopifnot(any(!is.na(x)))

	# x = rnorm(10)

	# Tibble containing calculations
	tbl = tibble::tibble(x = x) %>%
		dplyr::mutate(x = round(x, precision)) %>%
		dplyr::summarize(SumX = sum(x) %>% round(precision),
										 N = dplyr::n(),
										 M = (SumX/N) %>% round(digits))

	# Solution string
	slv = tbl %>%
		glue::glue_data("M_{[nm]} &= \\frac{\\Sigma [nm]}{N}
										= \\frac{[SumX]}{[N]}
										= \\textbf{[sprintf('%1.4f', M)]}",
										.open='[', .close = ']')

	# Return list containing both values and solution string
	l = as.list(tbl) %>% append(list(slv = slv))

	return(l)
}
