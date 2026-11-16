# Create RMarkdown Code for Tables

Function that creates the code to add a table (assumed to be a flextable
object) to a RMarkdown file.

## Usage

``` r
ffcar_add_table(chr_index, chr_title, chr_caption, chr_file_name = "")
```

## Arguments

- chr_index:

  A character string, assumed to be a pre-existing list containing the
  specified table (e.g., `lst_example$table`).

- chr_title:

  A character string, an optional title for the table.

- chr_caption:

  A character vector, an optional table caption.

- chr_file_name:

  An character string, an optional file name. If provided, a word
  document containing the table will be saved to the 'Downloads' folder.

## Value

A character vector.
