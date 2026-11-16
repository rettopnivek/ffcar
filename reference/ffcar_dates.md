# Functions to Extract Dates and Times as Strings

Functions that extract the current date and time (in UTC) and convert
them to formatted strings.

## Usage

``` r
ffcar_ymd_hm(chr_sep = "_", chr_split = "-")

ffcar_ymd(chr_sep = "_")

ffcar_mdy(chr_sep = "_")
```

## Arguments

- chr_sep:

  A character string, the separator between the year, month, day, hour,
  and minutes.

- chr_split:

  A character string, the separator between the date and time.

## Value

A character string.

## Examples

``` r
ffcar_ymd_hm()
#> Error in loadNamespace(x): there is no package called ‘checkmate’
ffcar_ymd()
#> Error in loadNamespace(x): there is no package called ‘checkmate’
ffcar_mdy()
#> Error in loadNamespace(x): there is no package called ‘checkmate’
```
