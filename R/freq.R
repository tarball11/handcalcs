#' Frequency Tables
#'
#' Produces both ungrouped and grouped frequency tables (as a `[gt::gt()]`
#' object) from a set of raw data.
#'
#' @param x Numeric vector.
#' @param grouped Logical. Should this be a grouped frequency table?
#' @param start Numeric scalar. For ungrouped frequency tables, lowest value
#'   represented in the table (default = `min(x)`). For grouped frequency
#'   tables, the lower bound of the lowest interval.
#' @param width Numeric scalar. Width of interval for grouped frequency tables.
#'   (Ignored if grouped is FALSE.)
#' @param rf Logical. Should a column for relative frequency be included
#'   (default = TRUE)? Note: values will be rounded to the value of
#'   round_interim.
#' @param cf Logical. Should a column for cumulative frequency be included
#'   (default = FALSE)?
#' @param cp Logical. Should a column for cumulative percentage be included
#'   (default = FALSE)? Note: values will be rounded to the value of
#'   round_final.
#' @param pr Logical. Should a column for percentile rank be included (default =
#'   FALSE)? Note: values will be rounded to the value of round_final.
#' @param font_size Numeric scalar. Font size of table in points (as rendered by
#'   `[gt::gt()]`).
#' @param ... Additional arguments to override default behaviors (see
#'   [handcalcs_defaults()]).
#'
#' @return A gt object.
#' @export
#'
#' @examples
#'
#' x <- sample(x = 1:20, size = 20, replace = TRUE)
#'
#' # Ungrouped:
#' get_freq_tbl(x, grouped = FALSE)
#' get_freq_tbl(x, grouped = FALSE, cf = TRUE, cp = TRUE, pr = TRUE, round_final = 2)
#'
#' # Grouped:
#' get_freq_tbl(x, grouped = TRUE)
#' get_freq_tbl(x, grouped = TRUE, width = 2, cf = TRUE, cp = TRUE, pr = TRUE, round_final = 2)
#'
#' # Also works on non-numeric data:
#' x <- sample(x = LETTERS, size = 20, replace = TRUE)
#' get_freq_tbl(x, grouped = FALSE)
#'
#' # When x is a factor, will include zeros for missing levels
#' x <- factor(x, levels = LETTERS)
#' get_freq_tbl(x, grouped = FALSE)
#'
get_freq_tbl<- function(x,
												grouped = TRUE,
												start = 0,
												width = 5,
												rf = TRUE,
												cf = FALSE,
												cp = FALSE,
												pr = FALSE,
												font_size = 12,
												...) {

	# Check argument validity
	stopifnot(length(x) > 0)
	stopifnot(is.logical(grouped))

	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts<- get_handcalcs_opts(...)


	if(grouped) {
		if(!is.numeric(x)) stop("Cannot have a grouped frequency table with non-numeric data.")
		stopifnot(is.numeric(width), length(width) == 1)
		stopifnot(is.numeric(start), length(start) == 1)

		lower <- seq(start, max(x)+1, width)

		freq.tbl <- tibble::tibble(x = x,
															 lb = lower[findInterval(x,
															 												lower-0.5,
															 												left.open = TRUE,
															 												rightmost.closed = TRUE)]) %>%
			dplyr::count(lb) %>%
			dplyr::full_join(tibble::tibble(lb = lower), by='lb') %>%
			dplyr::mutate(ub = lb + width - 1,
										n = ifelse(is.na(n), 0, n),
										intervals = paste(lb, ub, sep='-'),
										center = (ub + lb)/2) %>%
			dplyr::arrange(lb) %>%
			dplyr::select(x = intervals, f = n) %>%
			dplyr::mutate(x = forcats::fct_inorder(x))

	} else {
		# Generate frequencies:
		freq.tbl <- tibble::tibble(x) %>%
			dplyr::count(x, name = 'f')

		# Add missing values to sequence
		if(is.factor(x)) {
			# For factors, add missing levels
			freq.tbl <- freq.tbl %>%
				tidyr::complete(x = levels(x), fill = list(f=0))
		} else if(is.numeric(x) & all(x == floor(x))) {
			# For numbers that are effectively integers, fill in the missing values
			freq.tbl <- freq.tbl %>%
				tidyr::complete(x = seq(min(x), max(x)), fill = list(f=0))
		}
	}

	# Add additional columns
	if(rf) freq.tbl <- freq.tbl %>% dplyr::mutate(rf = rnd(f/sum(f), opts$round_interim))
	if(cf) freq.tbl <- freq.tbl %>% dplyr::mutate(cf = cumsum(f))
	if(cp) freq.tbl <- freq.tbl %>% dplyr::mutate(`c%` = rnd(cf/sum(f)*100, opts$round_interim))
	if(pr) freq.tbl <- freq.tbl %>% dplyr::mutate(pr = dplyr::lag(`c%`, default=0))

	# Format the columns with decimals
	if(rf) freq.tbl <- freq.tbl %>% dplyr::mutate(rf = fmt(rf, opts$round_interim))
	if(cp) freq.tbl <- freq.tbl %>% dplyr::mutate(`c%` = fmt(`c%`, opts$round_final))
	if(pr) freq.tbl <- freq.tbl %>% dplyr::mutate(pr = fmt(pr, opts$round_final))

	# Sort ascending for factors, descending for numeric
	if(is.factor(x)) freq.tbl <- dplyr::arrange(freq.tbl, x)
	if(is.numeric(x)) freq.tbl <- dplyr::arrange(freq.tbl, dplyr::desc(x))

	# Convert to gt, add formatting
	freq.tbl %>%
		gt::gt() %>%
		gt::tab_options(table.font.size = font_size) %>%
		gt::tab_style(style = gt::cell_text(style = "italic"),
									locations = gt::cells_column_labels()) %>%
		gt::cols_align(align = 'right')

}

