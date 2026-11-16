# Create RMarkdown Code for Headers

Function that creates a header at a specified level for a RMarkdown
file.

## Usage

``` r
ffcar_add_header(chr_header, int_level = 3)
```

## Arguments

- chr_header:

  A character string, the header content.

- int_level:

  An integer value between 1 and 5, the level for the header.

## Value

A character vector.

## Examples

``` r
ffcar_add_header( 'Example' )
#> [1] "### Example"
```
