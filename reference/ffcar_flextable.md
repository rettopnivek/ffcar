# Create APA-Style Flextable Object

Function that creates an APA-style table using
[`flextable::flextable()`](https://davidgohel.github.io/flextable/reference/flextable.html).

## Usage

``` r
ffcar_flextable(
  dtf_table,
  chr_header_row_1,
  chr_header_row_2 = NULL,
  chr_header_row_3 = NULL,
  lst_alignment = NULL,
  int_font_size = 11,
  chr_font_family = "Arial",
  lgc_color_rows = NULL,
  chr_row_color = NULL,
  lgc_merge_header_cells = TRUE
)
```

## Arguments

- dtf_table:

  A data frame with the table content.

- chr_header_row_1:

  A character vector matching in length to the number of columns of
  `dtf_table`, the headers for the columns.

- chr_header_row_2:

  A character vector matching in length to the number of columns of
  `dtf_table`, an optional second line for the column headers.

- chr_header_row_3:

  A character vector matching in length to the number of columns of
  `dtf_table`, an optional third line for the column headers.

- lst_alignment:

  A named list consisting of the elements `left`, `center`, and/or
  `right`, each consisting of the column indices to apply the
  corresponding alignment.

- int_font_size:

  An integer value, the font size for the table.

- chr_font_family:

  A character string, the font family.

- lgc_color_rows:

  A logical vector matching in length to the number of rows of
  `dtf_table`, specifying the rows to be colored.

- chr_row_color:

  A character string giving the optional background color for rows. If
  no input is provided for `lgc_color_rows`, results in every other row
  being colored.

- lgc_merge_header_cells:

  A logical value; if `TRUE` merges equivalent header cells into a
  single cell.

## Value

A `flextable` object.

## Examples

``` r
data(mtcars)
dtf_example <- mtcars[1:4, 1:3]
ft <- ffcar_flextable(
  dtf_example,
  c( 'Miles per gallon', 'Cylinders', 'Displacement' )
)
ft


.cl-d8bad469{}.cl-9f1c1ce9{font-family:'Arial';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-6477613b{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-a87b1e2d{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-b3ba1f30{width:1.355in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(0, 0, 0, 1.00);border-top: 1.5pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-e739c534{width:0.922in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(0, 0, 0, 1.00);border-top: 1.5pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-faa3c626{width:1.211in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(0, 0, 0, 1.00);border-top: 1.5pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-f409bd89{width:1.355in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-7d37e3dd{width:0.922in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-940b76a2{width:1.211in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-76ea59ae{width:1.355in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-d2e6a568{width:0.922in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-8b034fec{width:1.211in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}


Miles per gallon
```
