# Operator to Find Element Containing String

Operator that returns the element in a character vector containing a
specified character string.

## Usage

``` r
lhs %containing% rhs
```

## Arguments

- lhs, rhs:

  Character vectors

## Value

A character vector, the element(s) in `lhs` containing the `rhs`
character string.

## Examples

``` r
s <- c( 'file_1', 'file_2', 'file_3' )
s %containing% '2'
#> Error in loadNamespace(x): there is no package called ‘checkmate’
# Returns empty set if no matches
s %containing% '4'
#> Error in loadNamespace(x): there is no package called ‘checkmate’
```
