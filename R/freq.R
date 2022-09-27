#' Frequency Tables
#'
#' Produces both ungrouped and grouped frequency tables (as a `[gt::gt()]` table
#' object) from a set of raw data.
#'
#' By default, just calculates frequencies. Optionally, can include additional
#' columns for relative frequency (`rf`), cumulative frequency (`cf`),
#' cumulative percentage (`cp`), and percentile rank (`pr`). Additional
#' customization options for the `[gt::gt()]` table object are also available.
#'
#' Note that only numeric vectors can be used with the grouped option. Also note
#' that numeric vectors are sorted descending, whereas character vectors and
#' factors are sorted ascending.
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
#' @param x_name Character scalar. Name of x column in stub head (default =
#'   "x").
#' @param font_size Numeric scalar. Font size of table in points (as rendered by
#'   `[gt::gt()]`).
#' @param solutions If `TRUE` (the default), renders the table as expected. If
#'   `FALSE`, will show all of the requested columns, but will not display any
#'   of the calculated values.
#' @param return_type Determines what type of object to return: either "tbl"
#'   (for a `[tibble::tibble()]` object), "gt" (for a `[(gt::gt()]` object, the default), or
#'   "kable" (for a `[(knitr::kable()]` object with additional formatting from
#'   `[kableExtra::kableExtra()]`).
#' @param ... Additional arguments to override default behaviors (see
#'   [handcalcs_defaults()]).
#'
#' @return Depends on `return_type`: Either a `[tibble::tibble()]` object
#'   (`return_type="tbl"`), `[gt::gt()]` object (`return_type="gt"`), or a
#'   `[knitr::kable()]` object (`return_type="kable"`).
#' @export
#'
#' @examples
#'
#' x <- sample(x = 1:20, size = 20, replace = TRUE)
#'
#' # Ungrouped:
#' get_freq_tbl(x, grouped = FALSE, round_interim = 3)
#' get_freq_tbl(x, grouped = FALSE, cf = TRUE, cp = TRUE, pr = TRUE, round_final = 2)
#'
#' # Grouped:
#' get_freq_tbl(x, grouped = TRUE)
#' get_freq_tbl(x, grouped = TRUE, width = 3, start = 0, cf = TRUE, cp = TRUE, pr = TRUE, round_final = 2)
#'
#' # Also works on non-numeric data:
#' x <- sample(x = LETTERS, size = 20, replace = TRUE)
#' get_freq_tbl(x, grouped = FALSE)
#' get_freq_tbl(x, grouped = FALSE, cf = TRUE, cp = TRUE, pr = TRUE)
#'
#' # When x is a factor, will include zeros for missing levels
#' x <- factor(sample(x = LETTERS, size = 20, replace = TRUE), levels = LETTERS)
#' get_freq_tbl(x, grouped = FALSE)
#' get_freq_tbl(x, grouped = FALSE, cf = TRUE, cp = TRUE, pr = TRUE)
#'
get_freq_tbl<- function(x,
												grouped = TRUE,
												start,
												width = 5,
												rf = TRUE,
												cf = FALSE,
												cp = FALSE,
												pr = FALSE,
												x_name = "x",
												solutions = TRUE,
												return_type = c('gt', 'kable', 'tbl'),
												font_size = 12,
												...) {

	# Check argument validity
	stopifnot(length(x) > 0)
	stopifnot(is.logical(grouped))
	stopifnot(is.logical(solutions))

	return_type <- match.arg(return_type)
	if(missing(return_type)) return_type == 'gt'

	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts <- get_handcalcs_opts(...)


	if(grouped) {
		if(!is.numeric(x)) stop("Cannot have a grouped frequency table with non-numeric data.")
		stopifnot(is.numeric(width), length(width) == 1)

		# Set the default lower bound of the lowest interval if not provided
		if(missing(start)) start <- min(x)

		# Sequence of lower bounds
		lower <- seq(start, max(x)+1, width)

		# Figure out which interval each x value would fall into
		freq.tbl <- tibble::tibble(x = x,
															 lb = lower[findInterval(x,
															 												lower-0.5,
															 												left.open = TRUE,
															 												rightmost.closed = TRUE)]) %>%
			# Generate frequencies
			dplyr::count(lb, name = 'f') %>%
			dplyr::full_join(tibble::tibble(lb = lower), by='lb') %>%
			# Identify upper and lower bounds, then combine into intervals col
			dplyr::mutate(ub = lb + width - 1,
										f = ifelse(is.na(f), 0, f),
										intervals = paste(lb, ub, sep='-')) %>%
			dplyr::arrange(lb) %>%
			# Just need the x (intervals) and f columns
			dplyr::select(x = intervals, f) %>%
			dplyr::mutate(x = forcats::fct_inorder(x))

	} else {
		# Generate frequencies:
		freq.tbl <- tibble::tibble(x) %>%
			dplyr::count(x, name = 'f')

		# Add missing values to sequence
		if(is.factor(x)) {
			# For factors, add missing levels; be sure to retain factor level order
			x_lvls <- levels(x)
			freq.tbl <- freq.tbl %>%
				tidyr::complete(x = levels(x), fill = list(f=0)) %>%
				dplyr::mutate(x = factor(x, levels = x_lvls)) %>%
				dplyr::arrange(x)

		} else if(is.numeric(x) && all(x == floor(x))) {
			# For numbers that are effectively integers, fill in the missing values
			freq.tbl <- freq.tbl %>%
				tidyr::complete(x = seq(min(x), max(x)), fill = list(f=0))
		}
	}

	# Blank out frequencies if hiding solutions:
	if(!solutions) freq.tbl <- freq.tbl %>% dplyr::mutate(f = NA)

	# Add additional columns (will be calculated as NA if solutions = TRUE)
	if(rf) freq.tbl <- freq.tbl %>% dplyr::mutate(rf = rnd(f/sum(f), opts$round_interim))
	if(cf) freq.tbl <- freq.tbl %>% dplyr::mutate(cf = cumsum(f))
	if(cp) freq.tbl <- freq.tbl %>% dplyr::mutate(`c%` = rnd(cf/sum(f)*100, opts$round_interim))
	if(pr) freq.tbl <- freq.tbl %>% dplyr::mutate(pr = dplyr::lag(`c%`, default = if(solutions) 0 else NA))

	# Sort numeric vectors descending
	if(is.numeric(x)) freq.tbl <- dplyr::arrange(freq.tbl, dplyr::desc(x))


	if(return_type == 'tbl') {
		freq.tbl
	} else if(return_type == 'gt') {
		# Convert to gt, add formatting
		freq.gt <- freq.tbl %>%
			# Set the x column as the label
			gt::gt(rowname_col = "x") %>%
			# Change the name of the x column, if necessary
			gt::tab_stubhead(label = x_name) %>%
			# Overall table style/size
			gt::tab_options(table.font.size = font_size) %>%
			gt::cols_width(tidyselect::any_of(c('f', 'cf')) ~ gt::px(40),
										 tidyselect::any_of(c('rf', 'c%', 'pr')) ~ gt::px(60)) %>%
			# Style the column labels
			gt::tab_style(style = gt::cell_text(style = "italic",
																					weight = "bold",
																					align = "right"),
										locations = list(gt::cells_stubhead(), gt::cells_column_labels())) %>%
			gt::cols_align(align = 'right') %>%
			gt::grand_summary_rows(
				columns = c(f),
				fns = list(Total = ~sum(.)),
				missing_text = "",
				formatter = gt::fmt_number,
				decimals = 0)

		# Format the columns with decimals (if presenting solutions)
		if(rf & solutions) freq.gt <- freq.gt %>% gt::fmt_number(rf, decimals = opts$round_interim)
		if(cp & solutions) freq.gt <- freq.gt %>% gt::fmt_number(`c%`, decimals = opts$round_final)
		if(pr & solutions) freq.gt <- freq.gt %>% gt::fmt_number(pr, decimals = opts$round_final)

		# Include rf in summary row if appropriate
		if(rf) {
			freq.gt <- freq.gt %>%
				gt::grand_summary_rows(
					columns = c(rf),
					fns = list(Total = ~sum(.)),
					missing_text = "",
					formatter = gt::fmt_number,
					decimals = opts$round_interim)
		}

		# Blank everything out if hiding solutions by turning NAs into empty text
		freq.gt %>%
			gt::sub_missing(missing_text = "")

	} else if(return_type == 'kable') {
		# Calculate summary row:
		summary.tbl <- dplyr::summarize(freq.tbl, x = 'Total', f = sum(f))

		if(rf) summary.tbl$rf <- freq.tbl %>% dplyr::summarize(rf = sum(rf)) %>% dplyr::pull()
		if(cf) summary.tbl$cf <- ""
		if(cp) summary.tbl$cp <- ""
		if(pr) summary.tbl$pr <- ""

		# To simplify joining with summary table, convert all columns to character
		# (Note: use fmt() for other columns to ensure correct number of decimals)
		freq.tbl <- dplyr::mutate(freq.tbl, x = as.character(x), f = as.character(f))

		# Format the columns with decimals (if presenting solutions)
		if(rf) freq.tbl <- dplyr::mutate(freq.tbl, rf = fmt(rf, digits = opts$round_interim))
		if(cp) freq.tbl <- dplyr::mutate(freq.tbl, `c%` = fmt(`c%`, digits = opts$round_final))
		if(pr) freq.tbl <- dplyr::mutate(freq.tbl, pr = fmt(pr, digits = opts$round_final))

		summary.tbl <- dplyr::mutate(summary.tbl, f = as.character(f))
		if(rf) summary.tbl <- dplyr::mutate(summary.tbl, rf = fmt(rf, digits = opts$round_interim))

		# Combine frequency table with summary row
		full.tbl <- dplyr::bind_rows(freq.tbl, summary.tbl)

		# Replace NA values with empty strings for each column in data
		freq.kbl <- tidyr::replace_na(full.tbl, list(f = ""))

		if(rf) freq.kbl <- tidyr::replace_na(freq.kbl, list(rf = ""))
		if(cf) freq.kbl <- tidyr::replace_na(freq.kbl, list(cf = ""))
		if(cp) freq.kbl <- tidyr::replace_na(freq.kbl, list(cp = ""))
		if(pr) freq.kbl <- tidyr::replace_na(freq.kbl, list(pr = ""))

		# Add additional formating using kableExtra package:
		freq.kbl %>%
			knitr::kable(format='latex',
									 row.names=FALSE,
									 booktabs=TRUE,
									 linesep = "",
									 align = c('r', 'c', 'c', 'c', 'c', 'c')) %>%
			kableExtra::kable_styling(position = "center",
																latex_options = 'HOLD_position') %>%
			kableExtra::row_spec(0, italic = TRUE) %>%
			kableExtra::row_spec(1:nrow(freq.kbl), hline_after = TRUE) %>%
			kableExtra::column_spec(2:(ncol(freq.kbl)-1),
															width = '2cm',
															border_left = TRUE) %>%
			kableExtra::column_spec(ncol(freq.kbl),
															width = '2cm',
															border_left = TRUE,
															border_right = TRUE)
	}
}

