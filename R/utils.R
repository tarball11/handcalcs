rnd <- function(x, digits, round_to = get_handcalcs_opts()$round_to) {
	stopifnot(is.numeric(x))
	stopifnot(is.numeric(digits), length(digits) == 1)
	stopifnot(round_to %in% c('decimals', 'sigfigs'))

	if(round_to == 'sigfigs') {
		signif(x, digits)
	} else {
		round(x, digits)
	}
}

#' Figures out the minumum number of digits (up to four) to report after the decimal place
#'
#' @param x Numeric vector.
#' @param max_digits Numeric scalar: maximum number of digits.
#'
#' @return
#' @export
#'
#' @examples
#'
#' get_digits(rnorm(20))
#'
#'
get_digits<- function(x, max_digits = 4) {
	# Don't let the number of digits get out of control.
	stopifnot(is.numeric(max_digits), max_digits <= 20)

	for(i in 0:max_digits) if(all(x == round(x, i), na.rm=TRUE)) break
	return(i)
}

# Formats a number as a string rounded to the appropriate number of digits
fmt<- function(x, digits = 4) {
	sprintf(paste0('%1.', digits, 'f'), x)
}

# Wrapper for glue::glue() to generate solution strings, using defaults useful
# for LaTeX strings
glue_solution <- function(..., .envir = parent.frame()) {
	glue::glue(...,
						 .envir = .envir,
						 .sep = " \\\\ ",
						 .open = "[",
						 .close = "]",
	)
}
