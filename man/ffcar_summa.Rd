% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/R05-Reporting.R
\name{ffcar_summa}
\alias{ffcar_summa}
\title{Flexible Formatted Summary Statistics}
\usage{
ffcar_summa(
  vec_values,
  chr_syntax = "[[M]] ([[SD]])",
  lgc_remove_NA = TRUE,
  int_digits = 2,
  lgc_pad = FALSE,
  vec_categories = NULL,
  fun_custom = NULL,
  ...
)
}
\arguments{
\item{vec_values}{A vector of values.}

\item{chr_syntax}{A character string with identifiers in
the form \code{[[.]]} where \code{.} can be a variety
of letter sets for different summary statistics.
The function will substitute the appropriate
summary statistic computed over \code{Vec_values}
in place of the identifier.}

\item{lgc_remove_NA}{Logical; if \code{TRUE} removes
\code{NA} values before computing statistics.}

\item{int_digits}{An integer value - the number of
digits to round to.}

\item{lgc_pad}{Logical; if \code{TRUE} pads decimal
places with zeros if needed.}

\item{vec_categories}{A vector of values to
match \code{vec_values} for certain statistics
(e.g., percentages, counts).}

\item{fun_custom}{An optional user-defined function
that takes \code{vec_values} as a first argument
and returns a vector of values. The i-th
outputted value will then be substituted for the
corresponding identifier \code{[[i]]}.}

\item{...}{Additional arguments for the user-defined
function.}
}
\value{
A character string.
}
\description{
Function that creates nicely formatted character strings
with summary statistics based on user-supplied identifiers
via a simple, intuitive syntax.
}
\details{
This function provides some simple syntax to allow users
to write out a custom phrase for reporting summary statistics.
The function then searches the input for identifiers -
once found, the function the computes the appropriate
summary statistic and substitues the numeric result
in place of the given identifier.

For example, a user can provide the phrase:

\code{'Mean = [[M]]'},

and the function will then substitute the sample mean
for \code{vec_values} for the identifier \code{[[M]]}.

Pre-defined identifiers are:
\itemize{
\item \code{[[N]]} = Sample size;
\item \code{[[M]]} = Mean;
\item \code{[[SD]]} = Standard deviation;
\item \code{[[SE]]} = Standard error of the mean;
\item \code{[[Mn]]} = Minimum;
\item \code{[[Q1]]} = 1st quartile;
\item \code{[[Md]]} = Median;
\item \code{[[Q3]]} = 2nd quartile;
\item \code{[[Mx]]} = Maximum;
\item \code{[[IQR]]} = Inter-quartile range;
\item \code{[[C]]} = Counts/frequencies;
\item \code{[[P]]} = Percent;
\item \code{[[Pr]]} = Proportion.
}

Users can also pass in a custom function \code{f}
that takes \code{x} as a first argument and
returns a vector of values. Then element \code{i}
from the outputted vector is substituted for
the identifier \code{[[i]]}.
}
\examples{
# Example using 'iris' data set
data("iris")
# Continuous variable - sepal length
x <- iris$Sepal.Length

# Mean and standard deviation
ffcar_summa(x)
# Median and IQR
ffcar_summa(x, "[[Md]] ([[IQR]])")
# Pad to 2 decimal places
ffcar_summa(x, "[[Md]] ([[IQR]])", lgc_pad = TRUE)
# Mean (SD); N [min, 1st, 2nd, and 3rd quartile, max]
ffcar_summa(x, paste0(
    "[[N]]; [[M]] ([[SD]]); ",
    "[[[Mn]], [[Q1]], [[Md]], [[Q3]], [[Mx]]]"
  ), int_digits = 1)

# Custom measures via user-defined function
# Geometric mean
fun_geometric <- function(x) {
  exp( mean( log(x) ) )
}
ffcar_summa(x, "[[M]] vs. [[1]]",
  fun_custom = fun_geometric
)

# Example using 'mtcars' data set
# Categorical variable - # of forward gears
data("mtcars")
x <- mtcars$gear

# Percent and counts for 3 forward gears
ffcar_summa(x == 3, "[[P]]\% ([[C]] out of [[N]])")
# Percent and counts for 4 or 5 forward gears
ffcar_summa(x, "[[P]]\% ([[C]] out of [[N]])",
  vec_categories = c(4, 5)
)

}
