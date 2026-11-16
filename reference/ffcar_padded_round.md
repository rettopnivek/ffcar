# Round a Value and Pad Number of Trailing Zeros

Function that rounds a value to a desired number of digits and

## Usage

``` r
ffcar_padded_round(num_value, int_digits)
```

## Value

A character string with the rounded values.

## Examples

``` r
ffcar_padded_round( c( 1, 1.1, 1.116, 1.1116 ), 2 )
#> [1] "1.00" "1.10" "1.12" "1.11"
```
