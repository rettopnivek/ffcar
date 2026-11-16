# Reformat Columns in Data Frame

Function to reformat columns in a data frame of results (e.g., combining
different columns into nicely formatted character strings of results).

## Usage

``` r
ffcar_reformat_columns(
  dtf_results,
  vec_columns,
  chr_template = NULL,
  fun_transform = NULL,
  int_digits = NULL,
  lgc_pad = TRUE,
  chr_common = "estimate and standard error"
)
```

## Arguments

- dtf_results:

  A data frame with the values to format.

- vec_columns:

  A vector of column indices, either integers or character strings.

- chr_template:

  A character string giving the desired format of results. Specify
  columns using `'[[CX]]'` where X refers to the column number. See
  [ffcar_unicode_lookup](https://rettopnivek.github.io/ffcar/reference/ffcar_unicode_lookup.md)
  for more details.

- fun_transform:

  A function, an optional transformation to apply before formatting
  (e.g., applying the `exp` function to convert log-odds to odds
  ratios).

- int_digits:

  An integer vector giving the number of digits to round to for each
  extracted column.

- chr_common:

  A character string, allows quick specification of some common
  templates (uncertainty intervals, estimates and standard errors,
  frequencies and percentages, p-values).

## Value

A data frame.

## Examples

``` r
dtf_results <- data.frame(
  Estimate.raw = rnorm( 3 ),
  SE.raw = exp( rnorm( 3 ) )
)
dtf_results$P_value.raw <- pnorm(
  abs( dtf_results$Estimate )/dtf_results$SE.raw, lower.tail = FALSE
)*2
dtf_results$Test <- ffcar_reformat_columns(
  dtf_results, 1:3,
  '[[GLb]] = [[C1]] (SE = [[C2]]), p = [[C3]]',
  int_digits = c( 2, 2, 3 )
)
print( dtf_results )
#>   Estimate.raw    SE.raw P_value.raw                             Test
#> 1   -1.4000435 0.9944442   0.1591710 β = -1.40 (SE = 0.99), p = 0.159
#> 2    0.2553171 1.8618167   0.8909254  β = 0.26 (SE = 1.86), p = 0.891
#> 3   -2.4372636 3.1531804   0.4395496 β = -2.44 (SE = 3.15), p = 0.440
```
