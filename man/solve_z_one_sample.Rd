% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/z_one_sample.R
\name{solve_z_one_sample}
\alias{solve_z_one_sample}
\alias{z_one_sample_formula}
\title{One-sample \emph{z}-test}
\usage{
solve_z_one_sample(M, mu, sigma, n, sigma_M, ...)

z_one_sample_formula(...)
}
\arguments{
\item{M}{Numeric scalar. Sample mean.}

\item{mu}{Numeric scalar. Population mean.}

\item{sigma}{Numeric scalar. Population standard deviation. Required if
sigma_M is not provided.}

\item{n}{Numeric scalar. Sample size. Required if sigma_M is not provided.}

\item{sigma_M}{Numeric scalar. Standard error of the mean. If not provided,
will be calculated from \code{sigma} and \code{n} (using \code{\link[=solve_sigma_M]{solve_sigma_M()}}) and
included in the solution string.}

\item{...}{Additional arguments to override default behaviors (see
\code{\link[=handcalcs_defaults]{handcalcs_defaults()}}}
}
\value{
\code{solve_z_one_sample()} returns a list with the provided values (\code{M}, \code{mu},
\code{sigma}, \code{n}), the interim calculations (\code{sigma_M}, \code{M_diff}), the final
value (\code{z_obt}), the solution string (\code{solution}), and the bare formula
(\code{formula}) in LaTeX format. \code{z_one_sample_formula()} returns just the bare
formula in LaTeX format as a character string.
}
\description{
Calculates and provides solutions for the one-sample \emph{z}-test (i.e.,
\code{z_obt}), comparing an obtained sample mean against a population with known
parameters (\eqn{\mu}, \eqn{\sigma}).
}
\details{
Note that \code{z_obt} values are rounded to the value of \code{round_z} instead of the
value of \code{round_final} (see \code{\link[=handcalcs_defaults]{handcalcs_defaults()}}).
}
\examples{
solve_z_one_sample(M = 6, mu = 5, sigma = 2, n = 100)

# If you just want the formula:
z_one_sample_formula()

}
