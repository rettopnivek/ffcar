# Unicode Lookup Table

Function that returns useful unicode with shorthand labels for easy
lookup. Note that for certain systems, the code points used here may not
render properly.

## Usage

``` r
ffcar_unicode_lookup()
```

## Value

A named character vector with unicode and associated shorthand labels.

## Examples

``` r
# Greek letters
ffcar_unicode_lookup() %with_name% 'GL'
#> Error in loadNamespace(x): there is no package called ‘checkmate’
# Superscripts
ffcar_unicode_lookup() %with_name% 'SP'
#> Error in loadNamespace(x): there is no package called ‘checkmate’
# Subscripts
ffcar_unicode_lookup() %with_name% 'SB'
#> Error in loadNamespace(x): there is no package called ‘checkmate’
# Math symbols
ffcar_unicode_lookup() %with_name% 'MS'
#> Error in loadNamespace(x): there is no package called ‘checkmate’
# Error bar symbols
ffcar_unicode_lookup() %with_name% 'EB'
#> Error in loadNamespace(x): there is no package called ‘checkmate’
```
