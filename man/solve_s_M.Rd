% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/t_one_sample.R
\name{solve_s_M}
\alias{solve_s_M}
\alias{s_M_formula}
\title{Standard Error of the Mean (Unknown Population Standard Deviation)}
\usage{
solve_s_M(s, SD, n, ...)

s_M_formula(...)
}
\arguments{
\item{s, SD}{Numeric scalar. Sample standard deviation. May be named either
\code{s} or \code{SD}.}

\item{n}{Numeric scalar. Sample size.}

\item{...}{Additional arguments to override default behaviors (see
\code{\link[=handcalcs_defaults]{handcalcs_defaults()}}}
}
\value{
\code{solve_s_M()} returns a list with the provided values (\code{s}, \code{SD}, \code{n}),
the interim calculations (\code{sqrt_n}), the final value (\code{s_M}), the solution
string (\code{solution}), and the bare formula (\code{formula}) in LaTeX format.
\code{s_M_formula()} returns just the bare formula in LaTeX format as a
character string.
}
\description{
Calculates and provides solutions for the standard error of the mean (aka
standard deviation of the sampling distribution of the mean) using the sample
standard deviation (\eqn{s_{M} = s / \sqrt{n}}).
}
\examples{
solve_s_M(s = 5, n = 25)
solve_s_M(s = 5, n = 50)
solve_s_M(s = 5, n = 100)

# If you just want the bare formula as a string, use a formula function():
s_M_formula()

}
