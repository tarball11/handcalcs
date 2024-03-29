% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ci.R
\name{solve_ci_t}
\alias{solve_ci_t}
\alias{ci_t_formula}
\title{Confidence Intervals Around Sample Mean (using \emph{t} distribution)}
\usage{
solve_ci_t(M, s, SD, n, df, s_M, mu = NULL, level = 0.95, ...)

ci_t_formula(mu = NULL, level = 0.95, ...)
}
\arguments{
\item{M}{Numeric scalar. Sample mean.}

\item{s, SD}{Numeric scalar. Sample standard deviation. May be named either
\code{s} or \code{SD}. Required if s_M is not provided.}

\item{n, df}{Numeric scalar. Must supply either sample size (n) or degrees of
freedom (df, \eqn{df = n - 1}). Whichever argument is not provided will be
calculated.}

\item{s_M}{Numeric scalar. Standard error of the mean. If not provided, will
be calculated from \code{SD} and \code{n} (using \code{\link[=solve_s_M]{solve_s_M()}}) and included in the
solution string.}

\item{mu}{Numeric scalar. Population mean under the null hypothesis. If
provided, will return the confidence interval around the mean difference
(\eqn{M - \mu}).}

\item{level}{Numeric scalar. Confidence level; defaults to 0.95 (95\%
confidence intervals).}

\item{...}{Additional arguments to override default behaviors (see
\code{\link[=handcalcs_defaults]{handcalcs_defaults()}}}
}
\value{
\code{solve_ci_t()} returns a list with the provided values (\code{M}, \code{s},
\code{SD}, \code{n}, \code{s_M}), the interim calculations (\code{s_M}, \code{t_crit}, \code{marg_err}),
the final value (\code{CI_lower}, \code{CI_upper}, as well as a named vector
containing both values \code{CI}), the solution string (\code{solution}), and the
bare formula (\code{formula}) in LaTeX format. \code{ci_t_formula()} returns just the
bare formula in LaTeX format as a character string.
}
\description{
Calculates confidence intervals around a sample mean when the population
standard deviation (\eqn{\sigma}) must be estimated using the sample standard
deviation (\eqn{s}, aka \code{SD}) \eqn{M \pm (t^{*})(s_{M})}, where \eqn{t^{*}}
is the two-tailed critical value of \emph{t} (with \eqn{df = n - 1}) for 1 - the
confidence level (e.g., for 95\% confidence intervals, the two-tailed value of
\emph{t} for \eqn{\alpha = 1 - 0.95 = 0.05}). May either provide \code{s_M}, or that
value can be calculated from \code{SD} and \code{n}.
}
\details{
Optionally, provide the value of \code{mu} (\eqn{mu}), the predicted value of the
mean under the null hypothesis, which will return the confidence interval
around the mean difference: \eqn{(M - \mu) \pm (t_{1 - \alpha/2})(s_{M})}.

You must provide either \code{df} or \code{n}. If one is provided, the other value will
be calculated (\eqn{df = n - 1}). If both are provided, this allows for the
possibility of errors that produce incorrect results. However, it also allows
for the possibility of using a different value of \code{df} than \code{n - 1}, for
instance of requiring use of a statistical table that does not have that
exact value of \code{df} listed.

Note that the critical value of \emph{t} (\code{t_crit}) is rounded to the value of
\code{round_t} instead of the value of \code{round_interim} or \code{round_final} (see
\code{\link[=handcalcs_defaults]{handcalcs_defaults()}}).
}
\examples{

solve_ci_t(M = 55, s_M = 1, df = 99)

# Defaults to 95\% CIs, but other levels can be specified:
solve_ci_t(M = 55, s_M = 1, df = 99, level = 0.99)

# Will calculate s_M if s and n (or df) are provided, and include the
# calculation in the solution string
solve_ci_t(M = 55, s = 10, n = 100)

# Note: if s_M is provided, it takes precedence; s will be recalculated
solve_ci_t(M = 55, s = 10, n = 100, s_M = 4)

# If mu is provided, will return the CI of the mean difference (M - mu).
solve_ci_t(M = 55, s_M = 1, n = 100, mu = 50)

# If you just want the formula:
ci_t_formula()

# To get the formula for the mean difference, supply a non-null value for mu:
ci_t_formula(mu = TRUE)

}
