% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/latex_helpers.R
\name{math_list}
\alias{math_list}
\alias{summation}
\alias{comma_list}
\title{Abbreviate Math Sequences}
\usage{
math_list(values, sep, abbrev_at = 6)

summation(
  values,
  abbrev_sum = get_handcalcs_opts()$abbrev_sum,
  abbrev_at = get_handcalcs_opts()$abbrev_sum
)

comma_list(values, abbrev_at = 6)
}
\arguments{
\item{values}{Character vector (or vector of values that can be coerced to
character) of values to be summed.}

\item{abbrev_at, abbrev_sum}{Numeric scalar indicating when to abbreviate the
summation with an ellipsis (default = 6). If the length of \code{values} exceeds
the value of abbrev_at, will return abbreviated string. (Note: minimum
value is 3.) Note: \code{abbrev_sum} is an alias for \code{abbrev_at} for
\code{summation()}, which defaults to the value of \code{abbrev_sum} set globally.}
}
\value{
String containing the (possibly abbreviated) sequence. For instance,
\code{summation(1:4)} would return "1 + 2 + 3 + 4".
}
\description{
Three different functions designed to take a character vector of values that
would be part of a sequence that would be abbreviated with an ellipsis
(LaTeX: \verb{\\cdots}), if the sequence is sufficiently long. If the length of the
sequence is greater than or equal to \code{abbrev_at}, it will be abbreviated. If
not, the full sequence will be shown.
}
\details{
\code{summation()} takes a sequence of values to be summed and returns a string
enumerating the process. For instance, if you wished to sum the integers 1 to
100, this would be spelled out as "1 + 2 + ... + 100".

\code{comma_list()} takes a set of values to be listed in a comma separated list.
For instance, if you wished to list the integers 1 to 100, this would be
spelled out as "1, 2, ..., 100".

\code{math_list()} is a more general form, allowing you to specify the string
separating the values. (\code{summation()} and \code{comma_list()} are just wrappers
around \code{math_list()}.)
}
\examples{
# Simplest use will take any string (or values that can be coerced):
summation(1:4)

# More complex uses are possible (e.g., sum of squared deviations)
x <- sample(1:20, size = 10)
M <- round(mean(x), 2)
values <- glue::glue("(<<x>> - <<M>>)^2", .open = "<<", .close = ">>")
summation(values)

# If you wish to avoid abbreviation, set the value higher:
summation(values, abbrev_at = length(values))

# Comma-separated lists are also simple:
comma_list(1:15)

# Other types of sequences are possible with math_list()
math_list(1:10, sep = "*")

}
