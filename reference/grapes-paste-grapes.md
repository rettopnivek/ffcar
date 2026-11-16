# Operator to Combine Two Strings

The operator `%paste%` combines character strings.

## Usage

``` r
rhs %paste% lhs
```

## Arguments

- rhs, lhs:

  R objects that can be converted to character vectors.

## Value

A character vector.

## Details

The call `rhs %paste% lhs` is equivalent to `paste0(rhs, lhs)`.

## Examples

``` r
'Hello' %paste% ' ' %paste% 'world'
#> [1] "Hello world"
```
