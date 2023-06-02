# Utility functions
# Written by Kevin Potter
# email: kevin.w.potter@gmail.com
# Please email me directly if you
# have any questions or comments
# Last updated 2023-06-01

# Table of contents
# 1) Dates and times as strings
#   1.1) ffcar_ymd_hm
#   1.2) ffcar_ymd
#   1.3) ffcar_mdy
# 2) Additional utility functions
#   2.1) ffcar_pull_from_matrix
#   2.2) ffcar_transfer_results_to_table
# 3) Operators
#   3.1) %paste%
#   3.2) %containing%
#   3.3) `%with_name%`
#   3.4) `%pull_sublist%`

#### 1) Dates and times as strings ####
#' Functions to Extract Dates and Times as Strings
#'
#' Functions that extract the current date and time
#' (in UTC) and convert them to formatted strings.
#'
#' @param chr_sep A character string, the separator
#'   between the year, month, day, hour, and minutes.
#' @param chr_split A character string, the separator
#'   between the date and time.
#'
#' @return A character string.
#'
#' @name ffcar_dates
#'
#' @examples
#' ffcar_ymd_hm()
#' ffcar_ymd()
#' ffcar_mdy()
NULL

#### 1.1) ffcar_ymd_hm ####
#' @rdname ffcar_dates
#' @export

ffcar_ymd_hm <- function(
    chr_sep = '_',
    chr_split = '-' ) {

  # Check inputs
  checkmate::assert_string( chr_sep )
  checkmate::assert_string( chr_split )

  chr_out <- format(
    as.POSIXct( Sys.time(), tz = "UTC" ),
    paste0(
      "%Y", chr_sep, "%m", chr_sep, "%d",
      chr_split,
      "%H", chr_sep, "%M"
    )
  )

  return( chr_out )
}

#### 1.2) ffcar_ymd ####
#' @rdname ffcar_dates
#' @export

ffcar_ymd <- function(
    chr_sep = '_' ) {

  # Check inputs
  checkmate::assert_string( chr_sep )

  chr_out <- format(
    as.POSIXct( Sys.time(), tz = "UTC" ),
    paste0(
      "%Y", chr_sep, "%m", chr_sep, "%d"
    )
  )

  return( chr_out )
}

#### 1.3) ffcar_mdy ####
#' @rdname ffcar_dates
#' @export

ffcar_mdy <- function(
    chr_sep = '_' ) {

  # Check inputs
  checkmate::assert_string( chr_sep )

  chr_out <- format(
    as.POSIXct( Sys.time(), tz = "UTC" ),
    paste0(
      "%m", chr_sep, "%d", chr_sep, "%Y"
    )
  )

  return( chr_out )
}

#### 2) Additional utility functions ####

#### 2.1) ffcar_pull_from_matrix ####
#' Pull Values From Matrix
#'
#' Function to extract values over a specified set
#' of rows and columns from a matrix. Useful, for
#' example, for pulling results from the
#' \code{coefficients} matrix returned by
#' [base::summary()] when working with \code{lm}
#' or \code{glm} objects.
#'
#' @param mat_values A matrix.
#' @param int_rows An integer vector, the rows to
#'   subset.
#' @param int_columns An integer vector, the columns
#'   to subset.
#' @param chr_labels A character vector matching in
#'   length to \code{int_columns}, optional new
#'   labels for extracted columns.
#'
#' @return The subset of rows from \code{mat_values}.
#'
#' @examples
#' # Example data set
#' data( "mtcars" )
#' # Multiple regression summary
#' mat_coefficients <-
#'   summary( lm( mpg ~ wt + cyl + hp, data = mtcars ) )$coefficients
#' # Pull estimates, standard errors, and p-values
#' ffcar_pull_from_matrix(
#'   mat_coefficients,
#'   int_rows = 2:4, int_columns = c(1:2, 4),
#'   chr_labels = c( 'Estimate', 'SE', 'P_value' )
#' )
#'
#' # Applied over multiple analyses
#' lst_summaries <- list(
#'   wt = summary( lm( mpg ~ wt, data = mtcars ) )$coefficients,
#'   cyl = summary( lm( mpg ~ cyl, data = mtcars ) )$coefficients,
#'   hp = summary( lm( mpg ~ hp, data = mtcars ) )$coefficients
#' )
#'
#' dtf_summary <- sapply(
#'   lst_summaries, ffcar_pull_from_matrix,
#'   int_rows = 2, int_columns = c(1:2, 4),
#'   chr_labels = c( 'Estimate', 'SE', 'P_value' )
#' ) |> t()
#'
#'
#' @export

ffcar_pull_from_matrix <- function(
    mat_values,
    int_rows,
    int_columns,
    chr_labels = NULL ) {

  mat_out <- mat_values[ int_rows, int_columns ]
  if ( !is.null( chr_labels ) ) {

    if ( length(int_rows) > 1 ) {
      colnames( mat_out ) <- chr_labels
    } else {
      names( mat_out ) <- chr_labels
    }

  }

  return( mat_out )
}

#### 2.2) ffcar_transfer_results_to_table ####
#' Transfer Results From List to Table
#'
#' Given a list with multiple summaries of model estimates,
#' transfers specified rows and columns to a new data frame.
#' Useful, for example, if multiple summaries for
#' \code{lm} or \code{glm} objects exist and a user wants
#' the summaries in a single table.
#'
#' @param lst_results A list with matrices containing the
#'   result summaries.
#' @param dtf_table A data frame, the table to add the
#'   results to.
#' @param obj_rows Either (a) an integer vector specifying
#'   the rows to index from each matrix in \code{lst_results},
#'   or (b) if different rows should be extracted for each matrix,
#'   a list matching in length to \code{lst_results} with the
#'   specific rows to index per each matrix.
#' @param obj_columns Either (a) an integer vector specifying
#'   the columns to index from each matrix in \code{lst_results},
#'   or (b) if different columns should be extracted for each matrix,
#'   a list matching in length to \code{lst_results} with the
#'   specific columns to index per each matrix.
#' @param obj_labels Either (a) a character vector specifying
#'   the columns to update in \code{dtf_table}, or (b) if
#'   different columns in \code{dtf_table} should be updated
#'   for each matrix in \code{lst_results}, a list matching in
#'   length to \code{lst_results} with the specific columns to update.
#'
#' @return A data frame.
#'
#' @examples
#' # List of matrices with regression results
#' lst_results <- list(
#'   wt = summary( lm( mpg ~ wt, data = mtcars ) )$coefficients,
#'   cyl = summary( lm( mpg ~ cyl, data = mtcars ) )$coefficients,
#'   hp = summary( lm( mpg ~ hp, data = mtcars ) )$coefficients
#' )
#'
#' dtf_table <- data.frame(
#'   Estimate = NA,
#'   SE = NA,
#'   P_value = NA
#' )
#'
#' dtf_table <- ffcar_transfer_results_to_table(
#'   lst_results, dtf_table,
#'   obj_rows = 2, obj_columns = c(1:2, 4),
#'   obj_labels = c( 'Estimate', 'SE', 'P_value' )
#' )
#'
#' @export

ffcar_transfer_results_to_table <- function(
    lst_results,
    dtf_table,
    obj_rows,
    obj_columns,
    obj_labels ) {

  L <- length( lst_results )

  # If integer vector for rows
  if ( is.vector( obj_rows ) ) {

    lst_rows <- lapply( 1:L, function(i) obj_rows )

    # Close 'If integer vector for rows'
  } else {

    # Check input is actually a list
    checkmate::assert_list(
      obj_rows
    )

    lst_rows <- obj_rows

    # Close else for 'If integer vector for rows'
  }

  # If integer vector for columns
  if ( is.vector( obj_columns ) ) {

    lst_columns <- lapply( 1:L, function(i) obj_columns )

    # Close 'If integer vector for columns'
  } else {

    # Check input is actually a list
    checkmate::assert_list(
      obj_columns
    )

    lst_columns <- obj_columns

    # Close else for 'If integer vector for columns'
  }

  # If character vector for labels
  if ( is.vector( obj_labels ) ) {

    lst_labels <- lapply( 1:L, function(i) obj_labels )

    # Close 'If character vector for labels'
  } else {

    # Check input is actually a list
    checkmate::assert_list(
      obj_labels
    )

    lst_labels <- obj_labels

    # Close else for 'If character vector for labels'
  }

  # Create index for rows to update
  int_index <- unlist(
    lapply( 1:L, function(l) {
      rep( l, length( lst_rows[[l]] ) )
    } )
  )

  # Copy table
  dtf_updated_table <- dtf_table

  # Expand table to update
  if ( nrow( dtf_updated_table ) == 1 ) {

    dtf_updated_table <- dtf_updated_table[
      rep( 1, length(int_index) ),
    ]

    # Close 'Expand table to update'
  }

  # Loop over elements
  for ( l in 1:L ) {

    # Check that number of columns match labels
    checkmate::assert_true(
      length( lst_columns[[l]] ) == length( lst_labels[[l]] )
    )

    # Check that labels index table columns
    checkmate::assert_true(
      all( lst_labels[[l]] %in% colnames( dtf_updated_table ) )
    )

    mat_values <- ffcar_pull_from_matrix(
      lst_results[[l]],
      int_rows = lst_rows[[l]],
      int_columns = lst_columns[[l]],
      chr_labels = lst_labels[[l]]
    )

    dtf_updated_table[ int_index == l, lst_labels[[l]] ] <- mat_values

    # Close 'Loop over elements'
  }

  return( dtf_updated_table )
}

#### 3) Operators ####

#### 3.1) %paste% ####
#' Operator to Combine Two Strings
#'
#' The operator \code{%paste%} combines character strings.
#'
#' @param rhs,lhs R objects that can be converted to
#'   character vectors.
#'
#' @details
#' The call \code{rhs %paste% lhs} is equivalent to
#' \code{paste0(rhs, lhs)}.
#'
#' @return A character vector.
#'
#' @examples
#' 'Hello' %paste% ' ' %paste% 'world'
#'
#' @export

`%paste%` <- function(
    rhs,
    lhs ) {

  return( paste0( rhs, lhs ) )

}

#### 3.2) %containing% ####
#' Operator to Find Element Containing String
#'
#' Operator that returns the element in
#' a character vector containing
#' a specified character string.
#'
#' @param lhs,rhs Character vectors
#'
#' @return A character vector, the
#' element(s) in \code{lhs} containing
#' the \code{rhs} character string.
#'
#' @examples
#' s <- c( 'file_1', 'file_2', 'file_3' )
#' s %containing% '2'
#' # Returns empty set if no matches
#' s %containing% '4'
#'
#' @export

`%containing%` <- function(
    lhs,
    rhs ) {

  # Check inputs
  checkmate::assert_string( rhs )
  checkmate::assert_true( is.character( lhs ) )

  lgc_partial_matches <- grepl( rhs, lhs, fixed = TRUE )

  if ( any( lgc_partial_matches ) ) {
    return( lhs[ lgc_partial_matches ] )
  } else {
    return( c() )
  }

}

#### 3.3) `%with_name%` ####
#' Operator to Index Vector by Names
#'
#' Operator that returns the element(s) in
#' a named vector whose names partially match
#' a specified character string.
#'
#' @param lhs A vector.
#' @param rhs A character string.
#'
#' @return The element(s) in \code{lhs} whose
#' names contain the \code{rhs} character string.
#'
#' @examples
#' x <- c( A1 = 1, A2 = 2, B1 = 3, B2 = 4 )
#' x %with_name% 'A'
#' x %with_name% 'B'
#'
#' @export

`%with_name%` <- function(
    lhs,
    rhs ) {

  # Check inputs
  checkmate::assert_string( rhs )
  checkmate::assert_true( !is.null( names( lhs ) ) )

  chr_names <- names( lhs )

  lgc_partial_matches <- grepl( rhs, chr_names, fixed = TRUE )

  if ( any( lgc_partial_matches ) ) {
    return( lhs[ lgc_partial_matches ] )
  } else {
    return( c() )
  }

}

#### 3.4) `%pull_sublist%` ####
#' Operator to Index Sublists in a List
#'
#' Operator that returns the content of a
#' sublist at a specified level for a list of lists.
#'
#' @param lst_content A list of lists.
#' @param obj_levels Either a character vector with
#'   the names for the sublists or an integer vector
#'   with the indices.
#'
#' @return The element(s) in \code{lhs} whose
#' names contain the \code{rhs} character string.
#'
#' @examples
#' # List with 1 nested level
#' lst <- list(
#'   SL1 = list( A = 1, B = 2, C = 3 ),
#'   SL2 = list( A = 4, B = 5, C = 6 )
#' )
#' # Extract element 'A' from each sublist
#' lst %pull_sublist% 'A'
#' # Extract 1st element from each sublist
#' lst %pull_sublist% 1
#' # Extract element 'B' from each sublist
#' lst %pull_sublist% 'B'
#' # Extract 2nd element from each sublist
#' lst %pull_sublist% 2
#'
#' # List with two nested levels
#' lst <- list(
#'   SL1 = list(
#'     SL11 = list( A = 1, B = 2 ),
#'     SL12 = list( A = 3, B = 4 )
#'   ),
#'   SL2 = list(
#'     SL21 = list( A = 5, B = 6 ),
#'     SL22 = list( A = 7, B = 8 )
#'   )
#' )
#' # Extract 1st element from level 1 (SLX1)
#' lst %pull_sublist% 1
#' # Extract 1st element from level 2 (SLX2[[1]])
#' lst %pull_sublist% c( 2, 1 )
#'
#' @export

`%pull_sublist%` <- function(
    lst_lists,
    obj_levels ) {

  lst_out <- NULL

  SL <- length( obj_levels )
  L <- length( lst_lists )

  if ( SL == 1 ) {

    lst_out <- lapply(
      1:L, function(l) {
        lst_lists[[l]][[ obj_levels[1] ]]
      }
    )

  }

  if ( SL == 2 ) {

    lst_out <- lapply(
      1:L, function(l) {
        lst_lists[[l]][[ obj_levels[1] ]][[ obj_levels[2] ]]
      }
    )

  }

  if ( SL == 3 ) {

    lst_out <- lapply(
      1:L, function(l) {
        lst_lists[[l]][[
          chr_levels[1]
        ]][[ chr_levels[2] ]][[ obj_levels[3] ]]
      }
    )

  }

  if ( SL == 4 ) {

    lst_out <- lapply(
      1:L, function(l) {
        lst_lists[[l]][[
          chr_levels[1]
        ]][[ chr_levels[2] ]][[ obj_levels[3] ]][[ obj_levels[4] ]]
      }
    )

  }

  if ( is.null( lst_out ) ) {
    stop( "Check specification of 'chr_levels' argument" )
  }

  return( lst_out )
}

