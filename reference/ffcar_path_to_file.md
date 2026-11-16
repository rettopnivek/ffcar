# Extract Relative Path to File

Function to extract the relative path to a file using the output from
the
[ffcar_list_file_paths](https://rettopnivek.github.io/ffcar/reference/ffcar_list_file_paths.md)
function and a partial file name.

## Usage

``` r
ffcar_path_to_file(lst_files, chr_partial_name, chr_subfolder = "")
```

## Arguments

- lst_files:

  A list, output from the
  [ffcar_list_file_paths](https://rettopnivek.github.io/ffcar/reference/ffcar_list_file_paths.md)
  function.

- chr_partial_name:

  A character string, the file name (or part of it) to extract.

- chr_subfolder:

  A character string, the subfolder that contains the file, if relevant.

## Value

A character string.
