# Create RMarkdown File for Report

Function to initialize a .Rmd file for a `ffcar`-compatible report.

## Usage

``` r
ffcar_create_report(
  chr_file_name,
  chr_date = NULL,
  chr_title = "Analysis Report",
  chr_author = "Kevin Potter"
)
```

## Arguments

- chr_file_name:

  A character string, the file name for the report.

- chr_date:

  A character vector, the date for the file name and the date for the
  report header, respectively.

- chr_title:

  A character string, the title of the report.

- chr_author:

  A character string, the author(s) of the report.

## Value

An .Rmd file.
