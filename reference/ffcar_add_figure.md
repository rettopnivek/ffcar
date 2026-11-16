# Create RMarkdown Code for Figures

Function that creates the code to add a figure (assumed to be a PNG
image saved to a local folder) to a RMarkdown file.

## Usage

``` r
ffcar_add_figure(chr_index, chr_title = "", chr_caption = "")
```

## Arguments

- chr_index:

  A character string, assumed to be an indexed character vector
  containing the relative path to the figure (e.g.,
  `chr_figure_paths[1]`).

- chr_title:

  A character string, an optional title for the figure.

- chr_caption:

  A character vector, an optional figure caption.

## Value

A character vector.
