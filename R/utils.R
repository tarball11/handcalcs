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


fmt<- function(x, digits) {
	sprintf(paste0('%1.', digits, 'f'), x)
}
