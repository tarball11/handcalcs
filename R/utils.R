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

#' Get Minimum Digits
#'
#' Figures out the minimum number of digits (up to four) to report after the
#' decimal place.
#'
#' @param x Numeric vector.
#' @param max_digits Numeric scalar: maximum number of digits.
#'
#' @return Numeric scalar: smallest number of digits to which the number(s) can
#'   be rounded (up to max_digits) without losing precision.
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

# Returns the rows in the tbl that have values of x closest to the given vals
get_closest_x <- function(tbl, val) {
	l <- c()
	for(i in 1:length(val)) {
		l <- append(l, which.min(abs(tbl$x - val[i])))
	}

	dplyr::slice(tbl, l)
}
