# Return Statement Based on Logical Value

Function to return one of two possible statements based on whether a
logical value is `TRUE` or `FALSE`. Useful, for example, to
automatically generate statements regarding statistical significance.

## Usage

``` r
ffcar_conditional_statement(lgc_condition, chr_statement = NULL)
```

## Arguments

- lgc_condition:

  A logical value.

- chr_statement:

  A character vector with two strings, (1) the statement to return when
  `FALSE` and (2) the statement to return when `TRUE`. Defaults to a
  statement on statistical significance.

## Value

A character string.

## Examples

``` r
ffcar_conditional_statement( FALSE )
#> [1] "not statistically significant"
ffcar_conditional_statement( TRUE )
#> [1] "statistically significant"

# Custom statements
ffcar_conditional_statement(
  FALSE, c( 'non-significant', 'significant' )
)
#> [1] "non-significant"
ffcar_conditional_statement(
  TRUE, c( 'non-significant', 'significant' )
)
#> [1] "significant"
```
