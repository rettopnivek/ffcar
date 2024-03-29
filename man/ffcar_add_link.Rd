% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/R05-Reporting.R
\name{ffcar_add_link}
\alias{ffcar_add_link}
\title{Create RMarkdown Code for a Link}
\usage{
ffcar_add_link(chr_content = "", chr_link = "")
}
\arguments{
\item{chr_content}{A character string, the
text content of the hyperlink.}

\item{chr_link}{The internal link to use.}
}
\value{
A character string.
}
\description{
Function that creates the code to add a line
with a link (e.g., a link to the table of
contents) to a RMarkdown file.
}
