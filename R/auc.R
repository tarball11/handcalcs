#' Calculating Area Under the Normal Curve
#'
#' Takes a given value of `z` (e.g., a *z*-score) and calculates the specified
#' area under the curve (AUC). The type argument determines what area is being
#' calculated and how the value of `z` is interpreted:
#'
#' \describe{
#'
#' \item{\code{above}}{Calculates the area above the given value of `z`.}
#'
#' \item{\code{below}}{Calculates the area above the given value of `z`.}
#'
#' \item{\code{between}}{Calculates the area under the curve between two values
#' of `z`. When a single value of `z` is provided, calculates the area under the
#' curve between the positive and negative equivalent values of `z`.}
#'
#' \item{\code{tails}}{Calculates the area under the curve more extreme (i.e.,
#' in the tails) of the two values of `z`. Note that when two values are
#' provided, one must be positive and one negative. When a single value of `z`
#' is provided, calculates the area under the curve more extreme than the
#' positive and negative equivalent values of `z` (in the tails).}
#'
#' }
#'
#' Note that *z*-scores are rounded to the value of `round_z` instead of the
#' value of `round_interim` or `round_final` (see [handcalcs_defaults()]).
#'
#' @param z Numeric. For type values of `above` and `below`, must be a single
#'   value. For type values of `between` and `tails`, can be of length 1 or 2.
#' @param type Character. Determines what type of area is to be calculated:
#'   `above`, `below`, `between`, or `tails`.
#' @param ... Additional arguments to override default behaviors (see
#'   [handcalcs_defaults()]).
#'
#' @return Returns a list with the given value (`z`), the interim
#'   calculations (`a1` and `a2` for `type = between` and `type = tails`), the
#'   final value (`a`), and the solution string (`solution`).
#' @export
#'
#' @examples
#'
#' # Calculates the area above and below a single z value.
#' solve_z_to_auc(1, 'above')
#' solve_z_to_auc(1, 'below')
#'
#' # When two values are provided (for between and tails), can calculate area between:
#' solve_z_to_auc(z = c(-2, 1), 'between')
#' solve_z_to_auc(z = c(-2, 1), 'tails')
#'
#' # When only one value of z is provided for between and tails, presumes symmetry:
#' solve_z_to_auc(2, 'between')
#' solve_z_to_auc(2, 'tails')
#'
solve_z_to_auc <- function(z,
													 type = c('above', 'below', 'between', 'tails'),
													 ...) {

	# Check argument validity
	stopifnot(!missing(type))
	type <- match.arg(type)

	stopifnot(is.numeric(z))
	if(type %in% c('above', 'below')) stopifnot(length(z) == 1)
	if(type %in% c('between', 'tails')) stopifnot(length(z) == 1 | length(z) == 2)

	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts<- get_handcalcs_opts(...)

	# Use the appropriate equals sign for an aligned environment
	equals <- ifelse(opts$use_aligned, "&=", "=")

	# Round the initial value
	z <- rnd(z, opts$round_z)

	# a1 and a2 are not always returned. Null values are dropped from the return list.
	a1 <- a2 <- NULL

	if(type == 'above') {
		# Calculate area
		a <- rnd(pnorm(q = z, lower.tail=FALSE), opts$round_interim)

		# Create the solution string, with rounded values (minimally displayed)
		solution <- glue_solution(
			"A_{\\text{above}} <<equals>> p(z > <<z>>) = \\mathbf{<<a>>}",
			# Print value of 'z' to the precision of opts$round_z unless round_to is set
			# to 'sigfigs', in which case just present the final rounded value as is.
			z = ifelse(opts$round_to == 'sigfigs', z, fmt(z, opts$round_z)),
			# Print value of 'a' rounded to the precision of the final calculated value
			# unless round_to is set to 'sigfigs', in which case just present the final
			# rounded value as is.
			a = ifelse(opts$round_to == 'sigfigs',
								 a,
								 fmt(a, get_digits(a, opts$round_final))))

	} else if(type == 'below') {
		# Calculate area
		a <- rnd(pnorm(q=z, lower.tail=TRUE), opts$round_final)

		# Create the solution string, with rounded values (minimally displayed)
		solution <- glue_solution(
			"A_{\\text{below}} <<equals>> p(z < <<z>>) = \\mathbf{<<a>>}",
			# Print value of 'z' to the precision of opts$round_z unless round_to is set
			# to 'sigfigs', in which case just present the final rounded value as is.
			z = ifelse(opts$round_to == 'sigfigs', z, fmt(z, opts$round_z)),
			# Print value of 'a' rounded to the precision of the final calculated value
			# unless round_to is set to 'sigfigs', in which case just present the final
			# rounded value as is.
			a = ifelse(opts$round_to == 'sigfigs',
								 a,
								 fmt(a, get_digits(a, opts$round_final))))


	} else if(type == 'between') {
		if(length(z) == 1) z <- sort(c(-z, z))
		z1 <- min(z)
		z2 <- max(z)

		# Calculate area
		a1 <- rnd(pnorm(q=z1, lower.tail=TRUE), opts$round_interim)
		a2 <- rnd(pnorm(q=z2, lower.tail=TRUE), opts$round_interim)

		a <- rnd(a2 - a1, opts$round_final)

		# Create the solution string, with rounded values (minimally displayed)
		solution <- glue_solution(
			"A_{1} <<equals>> p(z < <<z1>>) = <<a1>>",
			"A_{2} <<equals>> p(z < <<z2>>) = <<a2>>",
			"A_{\\text{between}} <<equals>> A_{2} - A_{1} = <<a2>> - <<a1>> = \\mathbf{<<a>>}",
			# Print values of 'z' to the precision of opts$round_z unless round_to is set
			# to 'sigfigs', in which case just present the final rounded value as is.
			z1 = ifelse(opts$round_to == 'sigfigs', z1, fmt(z1, opts$round_z)),
			z2 = ifelse(opts$round_to == 'sigfigs', z2, fmt(z2, opts$round_z)),
			# Print value of 'a' rounded to the precision of the final calculated value
			# unless round_to is set to 'sigfigs', in which case just present the final
			# rounded value as is.
			a1 = fmt(a1, get_digits(a1, opts$round_interim)),
			a2 = fmt(a2, get_digits(a2, opts$round_interim)),
			a = ifelse(opts$round_to == 'sigfigs',
								 a,
								 fmt(a, get_digits(a, opts$round_final))))


	} else if(type == 'tails') {
		if(length(z) == 1) z <- sort(c(-z, z))
		z1 <- min(z)
		z2 <- max(z)

		# For tails, must have one positive and one negative z-value.
		stopifnot(z1 < 0, z2 > 0)

		# Calculate area
		a1 <- rnd(pnorm(q=z1, lower.tail=TRUE), opts$round_interim)
		a2 <- rnd(pnorm(q=z2, lower.tail=FALSE), opts$round_interim)

		a <- rnd(a1 + a2, opts$round_final)

		# Create the solution string, with rounded values (minimally displayed)
		solution <- glue_solution(
			"A_{1} <<equals>> p(z < <<z1>>) = <<a1>>",
			"A_{2} <<equals>> p(z > <<z2>>) = <<a2>>",
			"A_{\\text{tails}} <<equals>> A_{1} + A_{2} = <<a1>> + <<a2>> = \\mathbf{<<a>>}",
			# Print values of 'z' to the precision of opts$round_z unless round_to is set
			# to 'sigfigs', in which case just present the final rounded value as is.
			z1 = ifelse(opts$round_to == 'sigfigs', z1, fmt(z1, opts$round_z)),
			z2 = ifelse(opts$round_to == 'sigfigs', z2, fmt(z2, opts$round_z)),
			# Print value of 'a' rounded to the precision of the final calculated value
			# unless round_to is set to 'sigfigs', in which case just present the final
			# rounded value as is.
			a1 = fmt(a1, get_digits(a1, opts$round_interim)),
			a2 = fmt(a2, get_digits(a2, opts$round_interim)),
			a = ifelse(opts$round_to == 'sigfigs',
								 a,
								 fmt(a, get_digits(a, opts$round_final))))
	}


	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	# Return list containing both values and solution string
	list(z = z,
			 a1 = a1,
			 a2 = a2,
			 a = a,
			 solution = solution) %>%
		purrr::compact()

}


#' Calculating Values of Z from Area Under the Normal Curve
#'
#' Takes a given area under the curve (AUC) and calculates the corresponding
#' value of `z` (e.g., a *z*-score). The type argument determines what area is
#' being provided:
#'
#' \describe{
#'
#' \item{\code{above}}{Value of `a` is the area above the value of `z` to be
#' calculated.}
#'
#' \item{\code{above}}{Value of `a` is the area below the value of `z` to be
#' calculated.}
#'
#' \item{\code{between}}{Value of `a` is the area between two values of `z`.
#' Always returns a positive and negative value of `z`.}
#'
#' \item{\code{tails}}{Value of `a` is the area more extreme than two values of
#' `z` (i.e., in the tails). (Note: this is the total area in the tails, not
#' just in one tail.) Always returns a positive and negative value of `z`.}
#'
#' }
#'
#' Note that *z*-scores are rounded to the value of `round_z` instead of the
#' value of `round_interim` or `round_final` (see [handcalcs_defaults()]).
#'
#' @param a Numeric scalar. Value of area specified by `type`. Must be between 0
#'   and 1.
#' @param type Character. Determines what type of area is being provided:
#'   `above`, `below`, `between`, or `tails`.
#' @param ... Additional arguments to override default behaviors (see
#'   [handcalcs_defaults()]).
#'
#' @return Returns a list with the provided value (`a`), any interim areas
#'   calculated (`a_above`, `a_below`, `a_between`, `a_1tail`, `a_2tails`,
#'   depending on the type of area provided), the final value (`z` for `type =
#'   above` and `type = below`; `z1` and `z2` for `type = between` and `type =
#'   tails`), and the solution string (`solution`).
#' @export
#'
#' @examples
#'
#' # Calculates the area above and below a single z value.
#' solve_auc_to_z(a = 0.25, 'above')
#' solve_auc_to_z(a = 0.25, 'below')
#'
#' solve_auc_to_z(a = 0.68, 'between')
#' solve_auc_to_z(a = 0.32, 'tails')
#'
#'
solve_auc_to_z <- function(a,
													 type = c('above', 'below', 'between', 'tails'),
													 ...) {

		# Check argument validity
		stopifnot(!missing(type))
		type <- match.arg(type)

		stopifnot(is.numeric(a), length(a) == 1, a > 0, a < 1)

		# Get list of options (allowing user to override defaults) for rounding
		# behavior and for presenting solutions in LaTeX environment.
		opts<- get_handcalcs_opts(...)

		# Use the appropriate equals sign for an aligned environment
		equals <- ifelse(opts$use_aligned, "&=", "=")

		# Round the initial value
		a <- rnd(a, opts$round_interim)

		# Some values are not always returned. Null values are dropped from the return list.
		z <- z1 <- z2 <- NULL
		a_above <- a_below <- a_between <- a_2tails <- a_1tail <- NULL

		if(type == 'above') {
			# Calculate z
			a_above <- a
			z <- rnd(qnorm(p=a, lower.tail=FALSE), opts$round_z)

			# Create the solution string, with rounded values (minimally displayed)
			solution <- glue_solution(
				"A_{\\text{above}} <<equals>> <<a_above>> \\rightarrow z = \\mathbf{<<z>>}",
				a_above = ifelse(opts$round_to == 'sigfigs', a_above, fmt(a_above, opts$round_interim)),
				# Print value of 'z' to the precision of opts$round_z unless round_to is
				# set to 'sigfigs', in which case just present the final rounded value as is.
				z = ifelse(opts$round_to == 'sigfigs', z, fmt(z, opts$round_z)))

		} else if(type == 'below') {
			# Calculate z
			a_below <- a
			z <- rnd(qnorm(p=a, lower.tail=TRUE), opts$round_z)

			# Create the solution string, with rounded values (minimally displayed)
			solution <- glue_solution(
				"A_{\\text{below}} <<equals>> <<a_below>> \\rightarrow z = \\mathbf{<<z>>}",
				a_below = ifelse(opts$round_to == 'sigfigs', a_below, fmt(a_below, opts$round_interim)),
				# Print value of 'z' to the precision of opts$round_z unless round_to is
				# set to 'sigfigs', in which case just present the final rounded value as is.
				z = ifelse(opts$round_to == 'sigfigs', z, fmt(z, opts$round_z)))


		} else if(type == 'between') {
			# Calculate z
			a_between <- a
			a_2tails <- rnd(1 - a_between, opts$round_interim)
			a_1tail <- rnd(a_2tails/2, opts$round_interim)
			z1 <- rnd(qnorm(p = a_1tail, lower.tail = TRUE), opts$round_z)
			z2 <- rnd(qnorm(p = a_1tail, lower.tail = FALSE), opts$round_z)

			# Create the solution string, with rounded values (minimally displayed)
			solution <- glue_solution(
				"A_{\\text{between}} <<equals>> <<a>>",
				"A_{\\text{2-tails}} <<equals>> 1 - A_{\\text{between}} = 1 - <<a_between>> = <<a_2tails>>",
				"A_{\\text{1-tail}} <<equals>> \\frac{A_{\\text{2-tails}}}{2} = \\frac{<<a_2tails>>}{2} = <<a_1tail>>",
				"A_{\\text{below}} <<equals>> <<a_1tail>> \\rightarrow z_{\\text{lower}} = \\mathbf{<<z1>>}",
				"A_{\\text{above}} <<equals>> <<a_1tail>> \\rightarrow z_{\\text{upper}} = \\mathbf{<<z2>>}",
				a_between = ifelse(opts$round_to == 'sigfigs', a_between, fmt(a_between, opts$round_interim)),
				a_2tails = ifelse(opts$round_to == 'sigfigs', a_2tails, fmt(a_2tails, opts$round_interim)),
				a_1tail = ifelse(opts$round_to == 'sigfigs', a_1tail, fmt(a_1tail, opts$round_interim)),
				# Print values of 'z' to the precision of opts$round_z unless round_to is
				# set to 'sigfigs', in which case just present the final rounded value as is.
				z1 = ifelse(opts$round_to == 'sigfigs', z1, fmt(z1, opts$round_z)),
				z2 = ifelse(opts$round_to == 'sigfigs', z2, fmt(z2, opts$round_z)))

		} else if(type == 'tails') {
			# Calculate z
			a_2tails <- a
			a_1tail <- rnd(a_2tails/2, opts$round_interim)

			z1 <- rnd(qnorm(p = a_1tail, lower.tail = TRUE), opts$round_z)
			z2 <- rnd(qnorm(p = a_1tail, lower.tail = FALSE), opts$round_z)

			# Create the solution string, with rounded values (minimally displayed)
			solution <- glue_solution(
				"A_{\\text{1-tail}} <<equals>> \\frac{A_{\\text{2-tails}}}{2} = \\frac{<<a_2tails>>}{2} = <<a_1tail>>",
				"A_{\\text{below}} <<equals>> <<a_1tail>> \\rightarrow z_{\\text{lower}} = \\mathbf{<<z1>>}",
				"A_{\\text{above}} <<equals>> <<a_1tail>> \\rightarrow z_{\\text{upper}} = \\mathbf{<<z2>>}",
				a_2tails = ifelse(opts$round_to == 'sigfigs', a_2tails, fmt(a_2tails, opts$round_interim)),
				a_1tail = ifelse(opts$round_to == 'sigfigs', a_1tail, fmt(a_1tail, opts$round_interim)),
				# Print values of 'z' to the precision of opts$round_z unless round_to is
				# set to 'sigfigs', in which case just present the final rounded value as is.
				z1 = ifelse(opts$round_to == 'sigfigs', z1, fmt(z1, opts$round_z)),
				z2 = ifelse(opts$round_to == 'sigfigs', z2, fmt(z2, opts$round_z)))
		}

		# Add LaTeX math code, if desired.
		if (opts$add_aligned) solution <- add_aligned(solution)
		if (opts$add_math) solution <- add_math(solution)

		# Return list containing both values and solution string
		list(a = a,
				 a_above = a_above,
				 a_below = a_below,
				 a_between = a_between,
				 a_2tails = a_2tails,
				 a_1tail = a_1tail,
				 z1 = z1,
				 z2 = z2,
				 z = z,
				 solution = solution) %>%
			purrr::compact()
}

