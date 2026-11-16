# Create RMarkdown Code for R Code

Function that creates the code to add an R code segment to a RMarkdown
file.

## Usage

``` r
ffcar_add_code(chr_code, chr_tag = "", chr_options = ", echo=FALSE")
```

## Arguments

- chr_code:

  A character vector with the lines of R code to add.

- chr_tag:

  A character string, an optional code segment label.

- chr_options:

  A character string, optional R code segment options.

## Value

A character vector.
