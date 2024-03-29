#' Show Raw Data
#'
#' Produces a simple table (`[gt::gt()]` object) of raw data by putting the data
#' into rows (using specified `width`). Note: raw data will be rounded to the
#' value of round_interim (either supplied as an option, or consulting the
#' global default value).
#'
#' @param x Numeric vector.
#' @param width Numeric scalar. Width of table to be produced.
#' @param sort_x Character. Should data be sorted before being displayed? Can
#'   specify ascending or descending sort.
#' @param return_type Determines what type of object to return: either "gt" (for
#'   a `[(gt::gt()]` object), or "kable" (for a `[(knitr::kable()]` object with
#'   additional formatting from `[kableExtra::kableExtra()]`).
#' @param font_size Numeric scalar. Font size (in pixels).
#' @param ... Additional arguments to override default behaviors (see
#'   [handcalcs_defaults()]).
#'
#' @return Depends on `return_type`: Either a `[gt::gt()]` object
#'   (`return_type="gt"`), or a `[knitr::kable()]` object
#'   (`return_type="kable"`).
#' @export
#'
#' @examples
#' # Generate sample data:
#' x = sample(1:20, 20, replace = TRUE)
#' show_raw_data(x, sort = 'asc')
#'
#' # Note rounding behavior:
#' x = rnorm(20)
#' show_raw_data(x, round_interim = 2)
#'
#' # If width is not a multiple of the length of x, will pad with empty cells:
#' show_raw_data(x, width = 7)
#'
show_raw_data<- function(x,
												 width = 10,
												 sort_x = c('none', 'asc', 'desc'),
												 return_type = c('gt', 'kable'),
												 font_size = 12,
												 ...) {

	# Check argument validity
	stopifnot(is.numeric(x), any(!is.na(x)))
	stopifnot(is.numeric(width), width >= 1)
	stopifnot(is.numeric(font_size), font_size >= 2)

	sort_x <- match.arg(sort_x)
	return_type <- match.arg(return_type)
	if(missing(return_type)) return_type == 'gt'

	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts <- get_handcalcs_opts(...)

	# Round x values
	x <- round(x, opts$round_interim)

	# Sort the data, if requested
	if(sort_x == 'asc') {
		x <- sort(x, decreasing = FALSE)
	} else if (sort_x == 'desc') {
		x <- sort(x, decreasing = FALSE)
	}

	# Dimensions
	l <- length(x)
	n_row = ceiling(l / width)

	# Make sure the width is not set wider than length of x
	if(width > l) width <- l

	# Add padding if necessary
	padding = (n_row * width) - l
	if(padding > 0) x = c(x, rep(NA, padding))

	# Split into rows, then convert into appropriate object.
	df <- split(x, 1:width) %>%
		tibble::as_tibble()

	if(return_type == 'gt') {
		gt::gt(df) %>%
			gt::tab_options(table.font.size = font_size,
											column_labels.hidden = TRUE) %>%
			gt::cols_align(align = 'right') %>%
			gt::opt_table_lines('all') %>%
			gt::sub_missing(missing_text = '')
	} else {
		knitr::kable(df, format='latex', row.names=FALSE, col.names = NULL, booktabs=TRUE) %>%
			kableExtra::kable_styling(position = "center", latex_options = 'HOLD_position')
	}
}
