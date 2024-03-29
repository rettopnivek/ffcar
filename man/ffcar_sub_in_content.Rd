% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/R05-Reporting.R
\name{ffcar_sub_in_content}
\alias{ffcar_sub_in_content}
\title{Substitute Content for Placeholder Text}
\usage{
ffcar_sub_in_content(
  chr_string,
  chr_values = NULL,
  chr_brackets = c("[[", "]]")
)
}
\arguments{
\item{chr_string}{A character string with placeholder
text to replace.}

\item{chr_values}{A named character vector (or vector
that can be coerced to a character vector) with
the elements to substitute in based on similarly
labeled placeholder text.}

\item{chr_brackets}{A character vector with the left
and right bracketing symbols used to denote text to
replace.}
}
\value{
A character string where the placeholder text
has been replaced with the relevant content.
}
\description{
Function to substitute in content based on
placeholder text in a character string.
}
\examples{
string <-
  'Results were significant, [[GLb]] = [[VCE]]' \%paste\%
  ' (SE = [[VSE]]), p = [[VPV]].'
# Named vector with names matching placeholder
# text to replace
values <- c( VCE = '1.0', VSE = '0.2', VPV = '0.002' )
ffcar_sub_in_content( string, values )

}
