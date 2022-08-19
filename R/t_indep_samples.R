solve_s_p2 <- function(SS_1,
											 SS_2,
											 s_1,
											 s_2,
											 SD_1,
											 SD_2,
											 n_1,
											 n_2,
											 ...) {

	# Check argument validity:
	# Must supply sample sizes (n_1 and n_2)
	if(missing(n_1) | missing(n_2)) stop("Must supply sample sizes (n_1 and n_2).")
	stopifnot(is.numeric(n_1), is.numeric(n_2))
	stopifnot(length(n_1) == 1, length(n_2) == 1)
	stopifnot(n_1 > 0, n_2 > 0)

	# If SD is supplied instead of s, set s accordingly
	if(missing(s_1) & !missing(SD_1)) s_1 <- SD_1
	if(missing(s_2) & !missing(SD_2)) s_2 <- SD_2

	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts<- get_handcalcs_opts(...)

	# Use the appropriate equals sign for an aligned environment
	equals <- ifelse(opts$use_aligned, "&=", "=")

	# Some values are not returned below; set to NULL
	s_12 <- s_22 <- df_s_12 <- df_s_22 <- NULL

	if(!missing(SS_1) & !missing(SS_2)) {
		# Check argument validity:
		stopifnot(is.numeric(SS_1), is.numeric(SS_2))
		stopifnot(length(SS_1) == 1, length(SS_2) == 1)
		stopifnot(SS_1 > 0, SS_2 > 0)

		# Round initial values
		SS_1 <- rnd(SS_1, opts$round_interim)
		SS_2 <- rnd(SS_2, opts$round_interim)
		df_1 <- n_1 - 1
		df_2 <- n_2 - 1

		SS <- rnd(SS_1 + SS_2, opts$round_interim)
		df <- df_1 + df_2
		s_p2 <- rnd(SS/df, opts$round_final)

		# Get base formula without LaTeX math/aligned blocks
		s_p2.f <- s_p2_formula(type = 'SS',
													use_aligned = opts$use_aligned,
													add_math = FALSE,
													add_aligned = FALSE)
		# Create the solution string, with rounded values (minimally displayed)
		solution <- glue_solution(
			s_p2.f,
			"<<equals>> \\frac{<<SS_1>> + <<SS_2>>}{<<df_1>> + <<df_2>>} = \\frac{<<SS>>}{<<df>>} = \\mathbf{<<s_p2>>}",
			SS = fmt(SS, get_digits(SS, opts$round_interim)),
			# Round based on the final calculated value unless round_to is set to
			# 'sigfigs', in which case just present the final rounded value as is.
			s_p2 = ifelse(opts$round_to == 'sigfigs', s_p2, fmt(s_p2, get_digits(s_p2, opts$round_final))))

	} else if(!missing(s_1) & !missing(s_2)) {
		# Check argument validity:
		stopifnot(is.numeric(s_1), is.numeric(s_2))
		stopifnot(length(s_1) == 1, length(s_2) == 1)
		stopifnot(s_1 > 0, s_2 > 0)

		# Round initial values
		s_1 <- rnd(s_1, opts$round_interim)
		s_2 <- rnd(s_2, opts$round_interim)

		# Calculate s_p2
		s_12 <- rnd(s_1^2, opts$round_interim)
		s_22 <- rnd(s_2^2, opts$round_interim)
		df_1 <- n_1 - 1
		df_2 <- n_2 - 1
		df <- df_1 + df_2

		df_s_12 <- rnd(df_1*s_12, opts$round_interim)
		df_s_22 <- rnd(df_2*s_22, opts$round_interim)

		SS <- rnd(df_s_12 + df_s_22, opts$round_interim)
		s_p2 <- rnd(SS/df, opts$round_final)

		# Get base formula without LaTeX math/aligned blocks
		s_p2.f <- s_p2_formula(type = 's',
													use_aligned = opts$use_aligned,
													add_math = FALSE,
													add_aligned = FALSE)
		# Create the solution string, with rounded values (minimally displayed)
		solution <- glue_solution(
			s_p2.f,
			"<<equals>> \\frac{(<<n_1>> - 1)(<<s_1>>^{2}) + (<<n_2>> - 1)(<<s_2>>^{2})}{<<n_1>> + <<n_2>> - 2}",
			"<<equals>> \\frac{(<<df_1>>)(<<s_12>>) + (<<df_2>>)(<<s_22>>)}{<<df>>}",
			"<<equals>> \\frac{<<df_s_12>> + <<df_s_22>>}{<<df>>}",
			"<<equals>> \\frac{<<SS>>}{<<df>>}",
			"<<equals>> \\mathbf{<<s_p2>>}",
			SS = fmt(SS, get_digits(SS, opts$round_interim)),
			s_12 = fmt(s_12, get_digits(s_12, opts$round_interim)),
			s_22 = fmt(s_22, get_digits(s_22, opts$round_interim)),
			df_s_12 = fmt(df_s_12, get_digits(df_s_12, opts$round_interim)),
			df_s_22 = fmt(df_s_22, get_digits(df_s_22, opts$round_interim)),
			SS = fmt(SS, get_digits(SS, opts$round_interim)),
			# Round based on the final calculated value unless round_to is set to
			# 'sigfigs', in which case just present the final rounded value as is.
			s_p2 = ifelse(opts$round_to == 'sigfigs', s_p2, fmt(s_p2, get_digits(s_p2, opts$round_final))))
	} else {
		stop("Must supply either sample standard deviations (s_1 and s_2 or SD_1 and SD_2) or sums of squares (SS_1 and SS_2).")
	}

	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	# Return list containing both values and solution string
	list(SS_1 = if(missing(SS_1)) NULL else SS_1,
			 SS_2 = if(missing(SS_2)) NULL else SS_2,
			 s_1 = if(missing(s_1)) NULL else s_1,
			 s_2 = if(missing(s_2)) NULL else s_2,
			 SD_1 = if(missing(SD_1)) NULL else SD_1,
			 SD_2 = if(missing(SD_2)) NULL else SD_2,
			 n_1 = n_1,
			 n_2 = n_2,
			 df_1 = df_1,
			 df_2 = df_2,
			 s_12 = s_12,
			 s_22 = s_22,
			 df_s_12 = df_s_12,
			 df_s_22 = df_s_22,
			 SS = SS,
			 df = df,
			 s_p2 = s_p2,
			 solution = solution,
			 formula = s_p2.f) %>%
		purrr::compact()
}

#' @rdname solve_s_p2
#'
#' @export
#'
s_p2_formula <- function(type = 'SS',
												...) {
	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts<- get_handcalcs_opts(...)

	# Use the appropriate equals sign for an aligned environment
	equals <- ifelse(opts$use_aligned, "&=", "=")

	# Create the formula string:
	stem <- "s_{p}^{2} <<equals>> "
	SS.f <- " \\frac{\\mathit{SS}_{1} + \\mathit{SS}_{2}}{\\mathit{df}_{1} + \\mathit{df}_{2}} "
	s.f <- " = \\frac{(n_{1} - 1)(s_{1}^{2}) + (n_{2} - 1)(s_{2}^{2})}{n_{1} + n_{2} - 2}"
	solution<- if(type == 'SS') lglue(stem, SS.f) else lglue(stem, SS.f, s.f)

	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	return(solution)
}


solve_s_M_diff <- function(s_p2,
													 s_1,
													 s_2,
													 SD_1,
													 SD_2,
													 n_1,
													 n_2,
													 ...) {

	# If SD is supplied instead of s, set s accordingly
	if(missing(s_1) & !missing(SD_1)) s_1 <- SD_1
	if(missing(s_2) & !missing(SD_2)) s_2 <- SD_2

	# Must supply sample sizes (n_1 and n_2)
	if(missing(n_1) | missing(n_2)) stop("Must supply sample sizes (n_1 and n_2).")

	if(missing(s_p2) & (missing(s_1) | missing(s_2))) {
		stop("Must supply pooled variance (s_p2) or standard deviation (as s_1 and s_2 or SD_1 and SD_2).")
	}

	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts<- get_handcalcs_opts(...)

	# Use the appropriate equals sign for an aligned environment
	equals <- ifelse(opts$use_aligned, "&=", "=")

	if(missing(s_p2)) {
		s_p2.lst <- solve_s_p2(s_1 = s_1,
													 s_2 = s_2,
													 n_1 = n_1,
													 n_2 = n_2,
													 round_interim = opts$round_interim,
													 round_final = opts$round_interim,
													 add_math = FALSE,
													 add_aligned = FALSE,
													 use_aligned = opts$use_aligned)
	s_p2 <- s_p2.lst$s_p2
	s_p2_solution <- s_p2.lst$solution
	} else {
		s_p2_solution <- NULL
	}

	# Check argument validity:
	stopifnot(is.numeric(s_p2), is.numeric(n_1), is.numeric(n_2))
	stopifnot(length(s_p2) == 1, length(n_1) == 1, length(n_2) == 1)
	stopifnot(s_p2 > 0, n_1 > 0, n_2 > 0)

	# Round initial values
	s_p2 <- rnd(s_p2, opts$round_interim)

	# Calculate s_M_diff
	s_p2_n1 <- rnd(s_p2 / n_1, opts$round_interim)
	s_p2_n2 <- rnd(s_p2 / n_2, opts$round_interim)
	s_M_diff2 <- rnd(s_p2_n1 + s_p2_n2, opts$round_interim)
	s_M_diff <- rnd(sqrt(s_M_diff2), opts$round_final)

	# Get base formula without LaTeX math/aligned blocks
	s_M_diff.f <- s_M_diff_formula(use_aligned = opts$use_aligned,
																 add_math = FALSE,
																 add_aligned = FALSE)

	# Prepend the solution for s_p2 if calculating from components
	s_M_diff.solution <- ifelse(!is.null(s_p2_solution),
															paste(s_p2_solution, ' \\\\ ', s_M_diff.f),
															s_M_diff.f)


	# Create the solution string, with rounded values (minimally displayed)
	solution <- glue_solution(
		s_M_diff.solution,
		"<<equals>> \\sqrt{\\frac{<<s_p2>>}{<<n_1>>} + \\frac{<<s_p2>>}{<<n_2>>}} = \\sqrt{<<s_p2_n1>> + <<s_p2_n2>>} = \\sqrt{<<s_M_diff2>>} = \\mathbf{<<s_M_diff>>}",
		s_p2 = fmt(s_p2, get_digits(s_p2, opts$round_interim)),
		s_p2_n1 = fmt(s_p2_n1, get_digits(s_p2_n1, opts$round_interim)),
		s_p2_n2 = fmt(s_p2_n2, get_digits(s_p2_n2, opts$round_interim)),
		s_M_diff2 = fmt(s_M_diff2, get_digits(s_M_diff2, opts$round_interim)),
		# Round based on the final calculated value unless round_to is set to
		# 'sigfigs', in which case just present the final rounded value as is.
		s_M_diff = ifelse(opts$round_to == 'sigfigs', s_M_diff, fmt(s_M_diff, get_digits(s_M_diff, opts$round_final))))


	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	# Return list containing both values and solution string
	list(s_1 = if(missing(s_1)) NULL else s_1,
			 s_2 = if(missing(s_2)) NULL else s_2,
			 SD_1 = if(missing(SD_1)) NULL else SD_1,
			 SD_2 = if(missing(SD_2)) NULL else SD_2,
			 s_p2 = s_p2,
			 n_1 = n_1,
			 n_2 = n_2,
			 s_p2_n1 = s_p2_n1,
			 s_p2_n2 = s_p2_n2,
			 s_M_diff2 = s_M_diff2,
			 s_M_diff = s_M_diff,
			 solution = solution,
			 formula = s_M_diff.f) %>%
		purrr::compact()
}

s_M_diff_formula <- function(...) {
	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts<- get_handcalcs_opts(...)

	# Use the appropriate equals sign for an aligned environment
	equals <- ifelse(opts$use_aligned, "&=", "=")

	# Create the formula string:
	solution<- lglue("s_{M_{1} - M_{2}} <<equals>> \\sqrt{\\frac{s_{p}^{2}}{n_{1}} + \\frac{s_{p}^{2}}{n_{2}}}")

	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	return(solution)

}

solve_t_indep_samples <- function(M_1,
																	M_2,
																	s_M_diff,
																	s_p2,
																	s_1,
																	s_2,
																	SD_1,
																	SD_2,
																	n_1,
																	n_2,
																	...) {

	# Check argument validity:
	stopifnot(is.numeric(M_1), is.numeric(M_2))
	stopifnot(length(M_1) == 1, length(M_2) == 1)

	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts<- get_handcalcs_opts(...)

	# Use the appropriate equals sign for an aligned environment
	equals <- ifelse(opts$use_aligned, "&=", "=")

	# Calculate s_M_diff if not provided
	if(missing(s_M_diff)) {
		# If SD is supplied instead of s, set s accordingly
		if(missing(s_1) & !missing(SD_1)) s_1 <- SD_1
		if(missing(s_2) & !missing(SD_2)) s_2 <- SD_2

		if(missing(n_1) | missing(n_2)) {
			stop("If not supplying s_M_diff, must supply sample sizes (n_1 and n_2).")
		}
		if(missing(s_p2) & (missing(s_1) | missing(s_2))) {
			stop("If not supplying s_M_diff, must supply either pooled variance (s_p2) or standard deviations (s_1 and s_2 or SD_1 and SD_2).")
		}

		s_M_diff.lst <- solve_s_M_diff(s_p2 = s_p2,
																	 s_1 = s_1,
																	 s_2 = s_2,
																	 n_1 = n_1,
																	 n_2 = n_2,
																	 round_interim = opts$round_interim,
																	 round_final = opts$round_interim,
																	 add_math = FALSE,
																	 add_aligned = FALSE,
																	 use_aligned = opts$use_aligned)

		s_M_diff <- s_M_diff.lst$s_M_diff
		s_M_diff_solution <- s_M_diff.lst$solution
	} else {
		s_M_diff_solution <- NULL
	}

	stopifnot(is.numeric(s_M_diff), length(s_M_diff) == 1, s_M_diff > 0)

	# Calculate t_obt:
	M_diff <- rnd(M_1 - M_2, opts$round_interim)
	t_obt <- rnd(M_diff/s_M_diff, opts$round_t)

	# Get base formula without LaTeX math/aligned blocks
	t_obt.f <- t_indep_samples_formula(use_aligned = opts$use_aligned,
																			add_math = FALSE,
																			add_aligned = FALSE)

	# Prepend the solution for s_M if calculating from components
	t_obt.solution <- ifelse(!is.null(s_M_diff_solution),
													 paste(s_M_diff_solution, ' \\\\ ', t_obt.f),
													 t_obt.f)

	solution <- glue_solution(
		t_obt.solution,
		# Put negative values of M_2 in brackets
		"<<equals>> \\frac{<<M_1>> - <<(M_2)>>}{<<s_M_diff>>} = \\frac{<<M_diff>>}{<<s_M_diff>>} = \\mathbf{<<t_obt>>}",
		M_diff = fmt(M_diff, get_digits(M_diff, opts$round_interim)),
		s_M_diff = fmt(s_M_diff, get_digits(s_M_diff, opts$round_interim)),
		# Print values of 't_obt' to the precision of opts$round_final unless round_to is
		# set to 'sigfigs', in which case just present the final rounded value as is.
		t_obt = ifelse(opts$round_to == 'sigfigs', t_obt, fmt(t_obt, opts$round_t)))

	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	# Return list containing both values and solution string
	list(M_1 = M_1,
			 M_2 = M_2,
			 s_1 = if(missing(s_1)) NULL else s_1,
			 s_2 = if(missing(s_2)) NULL else s_2,
			 SD_1 = if(missing(SD_1)) NULL else SD_1,
			 SD_2 = if(missing(SD_2)) NULL else SD_2,
			 s_p2 = if(missing(s_p2)) NULL else s_p2,
			 n_1 = if(missing(n_1)) NULL else n_1,
			 n_2 = if(missing(n_2)) NULL else n_2,
			 s_M_diff = s_M_diff,
			 M_diff = M_diff,
			 t_obt = t_obt,
			 solution = solution,
			 formula = t_obt.f) %>%
		purrr::compact()
}

t_indep_samples_formula <- function(...) {
	# Get list of options (allowing user to override defaults) for rounding
	# behavior and for presenting solutions in LaTeX environment.
	opts<- get_handcalcs_opts(...)

	# Use the appropriate equals sign for an aligned environment
	equals <- ifelse(opts$use_aligned, "&=", "=")

	# Create the formula string:
	solution<- lglue("t_{\\text{obt}} <<equals>> \\frac{M_{1} - M_{2}}{s_{M_{1} - M_{2}}}")

	# Add LaTeX math code, if desired.
	if (opts$add_aligned) solution <- add_aligned(solution)
	if (opts$add_math) solution <- add_math(solution)

	return(solution)

}

