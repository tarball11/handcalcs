% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/diff_scores.R
\name{solve_mean_diff}
\alias{solve_mean_diff}
\alias{mean_diff_formula}
\title{Mean Difference Score}
\usage{
solve_mean_diff(x_D, M_1, M_2, sym_x = "X", ...)

mean_diff_formula(type = "x_D", sym_x = "X", ...)
}
\arguments{
\item{x_D}{Numeric vector of difference scores for which to calculate the
mean.}

\item{M_1}{Numeric scalar. Mean of first set of values.}

\item{M_2}{Numeric scalar. Mean of second set of values.}

\item{sym_x}{Character scalar. Symbol to represent x in formula (default:
"X").}

\item{...}{Additional arguments to override default behaviors (see
\verb{[handcalcs_defaults()]}).}

\item{type}{Character scalar. What kind of formula to print? Can either be
"x_D" for showing the formula calculated from raw differences cores (the
default), or "M_diff" for showing the difference between group means. (When
the \code{mean_diff_formula()} is called by \code{solve_mean_diff()}, chooses based
on the type of data provided.)}
}
\value{
\code{solve_mean_diff()} returns a list with the provided values (\code{x_D} or
\code{M_1} and \code{M_2}), the interim calculations (\code{SumX_D} and \code{n}, if
calculating from \code{x_D}), the final value (\code{M_D}), the solution string
(\code{solution}), and the bare formula (\code{formula}) in LaTeX format.
\code{mean_diff_formula()} returns just the bare formula in LaTeX format as a
character string.
}
\description{
Calculate and present solutions for the mean of a set of difference scores.
This can either be provided as a raw set of scores provided as \code{x_D},
(\eqn{M_{D} = \frac{\sum{X_{D}}}{N}}), or as the means of two different sets
of values provided as \code{M_1} and \code{M_2}, (\eqn{M_{D} = M_{2} - M_{1}}).
}
\examples{
x_1 <- rnorm(10, mean = 0)
x_2 <- rnorm(10, mean = 1)
x_D <- solve_diff_score(x_1 = x_1, x_2 = x_2)$x_D
solve_mean_diff(x_D)

# If you just want the bare formula as a string, use the formula function:
mean_diff_formula()
mean_diff_formula(type = "M_diff")
}
