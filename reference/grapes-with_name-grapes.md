# Operator to Index Vector by Names

Operator that returns the element(s) in a named vector whose names
partially match a specified character string.

## Usage

``` r
lhs %with_name% rhs
```

## Arguments

- lhs:

  A vector.

- rhs:

  A character string.

## Value

The element(s) in `lhs` whose names contain the `rhs` character string.

## Examples

``` r
x <- c( A1 = 1, A2 = 2, B1 = 3, B2 = 4 )
x %with_name% 'A'
#> Error in loadNamespace(x): there is no package called ‘checkmate’
x %with_name% 'B'
#> Error in loadNamespace(x): there is no package called ‘checkmate’
```
