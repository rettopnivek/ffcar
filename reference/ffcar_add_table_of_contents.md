# Create RMarkdown Code for a Table of Contents

Function that creates the code to add a table of contents linking to the
section headers to a RMarkdown file.

## Usage

``` r
ffcar_add_table_of_contents(chr_headers, chr_sections = "")
```

## Arguments

- chr_headers:

  A character string, the text content (i.e., the section header).

- chr_sections:

  A character string, the section number (e.g., `'11'`, `'22'`).

## Value

A character vector.
