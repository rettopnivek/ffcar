# Save PNG File

Function to save a PNG file using default options.

## Usage

``` r
ffcar_png(
  chr_file_name,
  num_width = 5,
  num_height = 5,
  chr_units = "in",
  int_res = 300
)
```

## Arguments

- chr_file_name:

  A character string, the path for file.

- num_width:

  A numeric value, the width of the image (by default assumed to be in
  inches).

- num_height:

  A numeric value, the height of the image (by default assumed to be in
  inches).

- chr_units:

  A character string, the units for the width and height (default is
  `'in' for inches`).

- int_res:

  An integer value, the resolution for the image in ppi (pixels per
  inch).

## Value

A figure saved to the specified path.

## Details

Most academic journals expect figures to have dimensions in either 5 or
7.25 inches and require high-resolution images (typically 300 ppi).
