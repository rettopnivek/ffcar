# Functions for figures
# Written by Kevin Potter
# email: kevin.w.potter@gmail.com
# Please email me directly if you
# have any questions or comments
# Last updated 2023-03-03

# Table of contents
# 1) ffcar_png

#### 1) ffcar_png ####
#' Save PNG File
#'
#' Function to save a PNG file using default options.
#'
#' @param chr_file_name A character string, the path for file.
#' @param num_width A numeric value, the width of the image
#'   (by default assumed to be in inches).
#' @param num_height A numeric value, the height of the image
#'   (by default assumed to be in inches).
#' @param chr_units A character string, the units for the
#'   width and height (default is \code{'in' for inches}).
#' @param int_res An integer value, the resolution for the image
#'   in ppi (pixels per inch).
#'
#' @details
#' Most academic journals expect figures to have dimensions in
#' either 5 or 7.25 inches and require high-resolution images
#' (typically 300 ppi).
#'
#' @return A figure saved to the specified path.
#'
#' @export

ffcar_png <- function(
    chr_file_name,
    num_width = 5,
    num_height = 5,
    chr_units = 'in',
    int_res = 300 ) {

  png(
    chr_file_name,
    width = num_width, height = num_height,
    units = chr_units, res = int_res
  )

}

