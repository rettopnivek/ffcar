# Functions for working with files
# Written by Kevin Potter
# email: kevin.w.potter@gmail.com
# Please email me directly if you
# have any questions or comments
# Last updated 2023-03-03

# Table of contents
# 1) ffcar_create_report_folder
# 2) ffcar_list_file_paths
# 3) ffcar_path_to_file

#### 1) ffcar_create_report_folder ####
#' Create Folders and Subfolders for Report
#'
#' Function to create a folder and subfolders for
#' a standard \code{ffcar}-compatible report.
#'
#' @param chr_folder_name A character string, the
#'   folder name. For compatibility with \code{ffcar}
#'   functions, folder names must start with a letter
#'   and can only contain letters, numbers, underscores,
#'   or periods.
#'
#' @return Creates a folder with relevant subfolders.
#'
#' @export

ffcar_create_report_folder <- function(
    chr_folder_name ) {


  # Create specific folder for report
  dir.create( chr_folder_name )

  # Create sub-folders

  dir.create( paste0( chr_folder_name, "/Source" ) )
  dir.create( paste0( chr_folder_name, "/Previous_versions" ) )
  dir.create( paste0( chr_folder_name, "/Figures" ) )
  dir.create( paste0( chr_folder_name, "/Downloads" ) )

}

#### 2) ffcar_list_file_paths ####
#' Create List of Files and Folder Paths
#'
#' Function to create a list detailing all
#' files, folders, and files within each folder.
#' Assumes only one level of nested files:
#' Folder -> Files and subfolders -> Nested files.
#'
#' @param chr_folder A character string, the path to
#'   the folder with the files, subfolders, and nested
#'   files.
#'
#' @return A list.
#'
#' @export

ffcar_list_file_paths <- function(
    chr_folder ) {

  # Initialize output
  lst_file_paths <- list(
    files = "",
    folders = "",
    main = chr_folder
  )

  chr_all_elements <- dir( path = chr_folder )

  chr_files <- chr_all_elements %containing% "."

  chr_subfolders <- chr_all_elements[
    !( chr_all_elements %in% chr_files )
  ]

  # Number of sub-folders
  N_subfolders <- length( chr_subfolders )

  if ( length( chr_files ) > 0 ) {
    lst_file_paths[[1]] <- chr_files
  }

  if ( N_subfolders > 0 ) {

    lst_subfolders <- lapply(
      1:N_subfolders, function(l) c()
    )
    names( lst_subfolders ) <- chr_subfolders

    for ( sf in 1:N_subfolders ) {

      lst_subfolders[[sf]] <-
        dir( path = paste0( chr_folder, "/", chr_subfolders[sf] ) )

    }

    lst_file_paths$folders <- lst_subfolders

  }

  return( lst_file_paths )
}

#### 3) ffcar_path_to_file ####
#' Extract Relative Path to File
#'
#' Function to extract the relative
#' path to a file using the output
#' from the [ffcar::ffcar_list_file_paths]
#' function and a partial file name.
#'
#' @param lst_files A list, output from the
#'   [ffcar::ffcar_list_file_paths] function.
#' @param chr_partial_name A character string,
#'   the file name (or part of it) to extract.
#' @param chr_subfolder A character string,
#'   the subfolder that contains the file,
#'   if relevant.
#'
#' @return A character string.
#'
#' @export

ffcar_path_to_file <- function(
    lst_files,
    chr_partial_name,
    chr_subfolder = "" ) {

  # If no subfolder specified
  if ( chr_subfolder == "" ) {

    chr_out <-
      lst_files$files %containing% chr_partial_name

    # Close 'If no subfolder specified'
  } else {

    chr_out <-
      lst_files$folders[[ chr_subfolder]] %containing% chr_partial_name

    chr_out <- paste0(
      chr_subfolder, '/', chr_out
    )

    # Close else for 'If no subfolder specified'
  }

  chr_out <- paste0(
    lst_files$main, '/',
    chr_out
  )

  return( chr_out )
}

