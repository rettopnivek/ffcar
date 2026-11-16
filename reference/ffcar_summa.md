# Flexible Formatted Summary Statistics

Function that creates nicely formatted character strings with summary
statistics based on user-supplied identifiers via a simple, intuitive
syntax.

## Usage

``` r
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
```

## Arguments

- vec_values:

  A vector of values.

- chr_syntax:

  A character string with identifiers in the form `[[.]]` where `.` can
  be a variety of letter sets for different summary statistics. The
  function will substitute the appropriate summary statistic computed
  over `Vec_values` in place of the identifier.

- lgc_remove_NA:

  Logical; if `TRUE` removes `NA` values before computing statistics.

- int_digits:

  An integer value - the number of digits to round to.

- lgc_pad:

  Logical; if `TRUE` pads decimal places with zeros if needed.

- vec_categories:

  A vector of values to match `vec_values` for certain statistics (e.g.,
  percentages, counts).

- fun_custom:

  An optional user-defined function that takes `vec_values` as a first
  argument and returns a vector of values. The i-th outputted value will
  then be substituted for the corresponding identifier `[[i]]`.

- ...:

  Additional arguments for the user-defined function.

## Value

A character string.

## Details

This function provides some simple syntax to allow users to write out a
custom phrase for reporting summary statistics. The function then
searches the input for identifiers - once found, the function the
computes the appropriate summary statistic and substitues the numeric
result in place of the given identifier.

For example, a user can provide the phrase:

`'Mean = [[M]]'`,

and the function will then substitute the sample mean for `vec_values`
for the identifier `[[M]]`.

Pre-defined identifiers are:

- `[[N]]` = Sample size;

- `[[M]]` = Mean;

- `[[SD]]` = Standard deviation;

- `[[SE]]` = Standard error of the mean;

- `[[Mn]]` = Minimum;

- `[[Q1]]` = 1st quartile;

- `[[Md]]` = Median;

- `[[Q3]]` = 2nd quartile;

- `[[Mx]]` = Maximum;

- `[[IQR]]` = Inter-quartile range;

- `[[C]]` = Counts/frequencies;

- `[[P]]` = Percent;

- `[[Pr]]` = Proportion.

Users can also pass in a custom function `f` that takes `x` as a first
argument and returns a vector of values. Then element `i` from the
outputted vector is substituted for the identifier `[[i]]`.

## Examples

``` r
# Example using 'iris' data set
data("iris")
# Continuous variable - sepal length
x <- iris$Sepal.Length

# Mean and standard deviation
ffcar_summa(x)
#> [1] "5.84 (0.83)"
# Median and IQR
ffcar_summa(x, "[[Md]] ([[IQR]])")
#> [1] "5.8 (1.3)"
# Pad to 2 decimal places
ffcar_summa(x, "[[Md]] ([[IQR]])", lgc_pad = TRUE)
#> [1] "5.80 (1.30)"
# Mean (SD); N [min, 1st, 2nd, and 3rd quartile, max]
ffcar_summa(x, paste0(
    "[[N]]; [[M]] ([[SD]]); ",
    "[[[Mn]], [[Q1]], [[Md]], [[Q3]], [[Mx]]]"
  ), int_digits = 1)
#> [1] "150; 5.8 (0.8); [4.3, 5.1, 5.8, 6.4, 7.9]"

# Custom measures via user-defined function
# Geometric mean
fun_geometric <- function(x) {
  exp( mean( log(x) ) )
}
ffcar_summa(x, "[[M]] vs. [[1]]",
  fun_custom = fun_geometric
)
#> [1] "5.84 vs. 5.79"

# Example using 'mtcars' data set
# Categorical variable - # of forward gears
data("mtcars")
x <- mtcars$gear

# Percent and counts for 3 forward gears
ffcar_summa(x == 3, "[[P]]% ([[C]] out of [[N]])")
#> [1] "46.88% (15 out of 32)"
# Percent and counts for 4 or 5 forward gears
ffcar_summa(x, "[[P]]% ([[C]] out of [[N]])",
  vec_categories = c(4, 5)
)
#> [1] "53.12% (17 out of 32)"
```
