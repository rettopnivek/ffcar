# Pull Values From Matrix

Function to extract values over a specified set of rows and columns from
a matrix. Useful, for example, for pulling results from the
`coefficients` matrix returned by
[`base::summary()`](https://rdrr.io/r/base/summary.html) when working
with `lm` or `glm` objects.

## Usage

``` r
ffcar_pull_from_matrix(mat_values, int_rows, int_columns, chr_labels = NULL)
```

## Arguments

- mat_values:

  A matrix.

- int_rows:

  An integer vector, the rows to subset.

- int_columns:

  An integer vector, the columns to subset.

- chr_labels:

  A character vector matching in length to `int_columns`, optional new
  labels for extracted columns.

## Value

The subset of rows from `mat_values`.

## Examples

``` r
# Example data set
data( "mtcars" )
# Multiple regression summary
mat_coefficients <-
  summary( lm( mpg ~ wt + cyl + hp, data = mtcars ) )$coefficients
# Pull estimates, standard errors, and p-values
ffcar_pull_from_matrix(
  mat_coefficients,
  int_rows = 2:4, int_columns = c(1:2, 4),
  chr_labels = c( 'Estimate', 'SE', 'P_value' )
)
#>       Estimate         SE      P_value
#> wt  -3.1669731 0.74057588 0.0001994765
#> cyl -0.9416168 0.55091638 0.0984800975
#> hp  -0.0180381 0.01187625 0.1400151550

# Applied over multiple analyses
lst_summaries <- list(
  wt = summary( lm( mpg ~ wt, data = mtcars ) )$coefficients,
  cyl = summary( lm( mpg ~ cyl, data = mtcars ) )$coefficients,
  hp = summary( lm( mpg ~ hp, data = mtcars ) )$coefficients
)

dtf_summary <- sapply(
  lst_summaries, ffcar_pull_from_matrix,
  int_rows = 2, int_columns = c(1:2, 4),
  chr_labels = c( 'Estimate', 'SE', 'P_value' )
) |> t()

```
