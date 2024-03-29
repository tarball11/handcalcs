% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/es.R
\name{solve_cohens_d_paired_t}
\alias{solve_cohens_d_paired_t}
\alias{cohens_d_paired_t_formula}
\title{Effect site (Cohen's d) for paired-samples \emph{t}-test}
\usage{
solve_cohens_d_paired_t(M_D, mu_D = 0, SD_D, interpret = FALSE, ...)

cohens_d_paired_t_formula(...)
}
\arguments{
\item{M_D}{Numeric scalar. Mean of the difference scores between samples.}

\item{mu_D}{Numeric scalar. Population mean (default: 0).}

\item{SD_D}{Numeric scalar. Standard deviation of the difference scores.}

\item{interpret}{Logical scalar. Should the interpretation (as produced by
\code{\link[=interpret_d]{interpret_d()}} be included in the solution string?)}

\item{...}{Additional arguments to override default behaviors (see
\code{\link[=handcalcs_defaults]{handcalcs_defaults()}}}
}
\value{
\code{solve_cohens_d_paired_t()} returns a list with the provided values
(\code{M_D}, \code{mu_D}, \code{SD_D}), the interim calculations (\code{M_diff}), the final
value (\code{d}), the solution string (\code{solution}), and the bare formula
(\code{formula}) in LaTeX format. \code{cohens_d_paired_t_formula()} returns just the
bare formula in LaTeX format as a character string.
}
\description{
Calculates and presents solution string for effect site (Cohen's d) comparing
sample means from related (paired) samples: the mean of the difference scores
(\code{M_D}) compared to the expected difference (\code{mu_D}) in units of the standard
deviation of the difference scores (\code{SD_D}).
}
\examples{

solve_cohens_d_paired_t(M_D = 1, SD_D = 2)

# Include the interpretation of the effect size:
solve_cohens_d_paired_t(M_D = 1, SD_D = 2, interpret = TRUE)

# Although mu is virtually always zero, you can set it to be something else:
solve_cohens_d_paired_t(M_D = 1, mu_D = 0.5, SD_D = 2)

# If you just want the formula:
cohens_d_paired_t_formula()

}
