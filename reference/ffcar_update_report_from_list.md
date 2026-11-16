# Update Report From Content in List

Given a list of lists specifying inputs for the 'add' functions from
`Ffcar`, append new content to a `.Rmd` file.

## Usage

``` r
ffcar_update_report_from_list(lst_report, chr_rmd_file, int_max_TOC_level = 2)
```

## Arguments

- lst_report:

  A list of lists. Each sublist contains first, a character string
  specifying the 'add' function to call, either `'header'`,
  `'paragraph'`, `'code'`, `'figure'`, `'table'`, `'link'`, or
  `'table_of_contents'`. Second, the sublist contains a character string
  of the form `'X_Y'`, where `X` is a label denoting a section and `'Y'`
  is a number from 1 to 5 denoting the section level. Finally, the
  remainder of the list consists of the inputs to pass to the specified
  'add' function.

- chr_rmd_file:

  A character string, the name for the .Rmd file to update.

- int_max_TOC_level:

  A integer value, the maximum number of tabs to delineate subsections
  (value between 1 to 6).

## Value

Updated content for the file specified by `chr_rm_file`.
