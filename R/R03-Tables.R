# Functions for tables
# Written by Kevin Potter
# email: kevin.w.potter@gmail.com
# Please email me directly if you
# have any questions or comments
# Last updated 2023-06-02

# Table of contents
# 1) ffcar_flextable
#   1.1) Create table header
#   1.2) Create flextable object
#   1.3) Format table to be in APA-style
#   1.4) Additional table formatting
# 2) ffcar_reformat_columns
# 3) ffcar_design_table

#### 1) ffcar_flextable ####
#' Create APA-Style Flextable Object
#'
#' Function that creates an APA-style table
#' using [flextable::flextable()].
#'
#' @param dtf_table A data frame with the table content.
#' @param chr_header_row_1 A character vector matching in
#'   length to the number of columns of \code{dtf_table},
#'   the headers for the columns.
#' @param chr_header_row_2 A character vector matching in
#'   length to the number of columns of \code{dtf_table},
#'   an optional second line for the column headers.
#' @param chr_header_row_3 A character vector matching in
#'   length to the number of columns of \code{dtf_table},
#'   an optional third line for the column headers.
#' @param lst_alignment A named list consisting of the
#'   elements \code{left}, \code{center}, and/or \code{right},
#'   each consisting of the column indices to apply the
#'   corresponding alignment.
#' @param int_font_size An integer value, the font size for
#'   the table.
#' @param chr_font_family A character string, the font family.
#' @param lgc_color_rows A logical vector matching
#'   in length to the number of rows of \code{dtf_table},
#'   specifying the rows to be colored.
#' @param chr_row_color A character string giving the
#'   optional background color for rows. If no input is
#'   provided for \code{lgc_color_rows}, results in
#'   every other row being colored.
#' @param lgc_merge_header_cells A logical value; if
#'   \code{TRUE} merges equivalent header cells into a
#'   single cell.
#'
#' @return A \code{flextable} object.
#'
#' @examples
#' data(mtcars)
#' dtf_example <- mtcars[1:4, 1:3]
#' ft <- ffcar_flextable(
#'   dtf_example,
#'   c( 'Miles per gallon', 'Cylinders', 'Displacement' )
#' )
#' ft
#'
#' @export

ffcar_flextable <- function(
    dtf_table,
    chr_header_row_1,
    chr_header_row_2 = NULL,
    chr_header_row_3 = NULL,
    lst_alignment = NULL,
    int_font_size = 11,
    chr_font_family = 'Arial',
    lgc_color_rows = NULL,
    chr_row_color = NULL,
    lgc_merge_header_cells = TRUE ) {

  #### 1.1) Create table header ####

  # Initialize data frame with header information
  dtf_table_header = data.frame(
    col_keys = colnames( dtf_table ),
    colA = paste0( "C", 1:ncol( dtf_table ) ),
    stringsAsFactors = FALSE
  )

  # First row
  dtf_table_header$colA <- chr_header_row_1
  # Optional second row
  if ( !is.null( chr_header_row_2 ) ) {

    dtf_table_header$colB <- chr_header_row_2

    # Close 'Optional second row'
  }
  # Optional third row
  if ( !is.null( chr_header_row_3 ) ) {

    dtf_table_header$colC <- chr_header_row_3

    # Close 'Optional second row'
  }

  #### 1.2) Create flextable object ####

  dtf_input <- dtf_table

  # Convert columns to character strings
  for ( j in 1:ncol( dtf_input ) ) {
    dtf_input[[j]] = as.character( dtf_input[[j]] )
  }

  # Create flextable object
  ft = flextable::flextable(
    dtf_input,
    col_keys = dtf_table_header$col_keys
  )

  # Add header with detailed column labels
  ft = ft |>
    flextable::set_header_df(
      mapping = dtf_table_header,
      key = 'col_keys'
    )

  #### 1.3) Format table to be in APA-style ####

  # Remove default borders
  ft = ft |>
    flextable::border_remove()

  # Add borders for top and bottom typical
  # for APA-style tables
  ft = ft |>
    flextable::hline_top(
      border = officer::fp_border(
        width = 1.5,
        color = 'black'
      ), part = "header" ) |>
    flextable::hline_bottom(
      border = officer::fp_border(
        width = 1.5,
        color = 'black'
      ), part = "header" ) |>
    flextable::hline_bottom(
      border = officer::fp_border(
        width = 1.5,
        color = 'black'
      ), part = "body" )

  #### 1.4) Additional table formatting ####

  # Default alignment

  # Center-align cells
  ft = ft |>
    flextable::align(
      align = "center",
      part = "all"
    )
  # Left-align 1st column
  ft = ft |>
    flextable::align(
      j = 1,
      align = "left",
      part = "all"
    )

  # Custom alignment
  if ( !is.null( lst_alignment ) ) {

    if ( !is.null( lst_alignment$left ) ) {
      for ( j in lst_alignment$left ) {
        ft = ft |>
          flextable::align(
            j = j,
            align = "left",
            part = "all"
          )
      }
    }

    if ( !is.null( lst_alignment$right ) ) {
      for ( j in lst_alignment$right ) {
        ft = ft |>
          flextable::align(
            j = j,
            align = "right",
            part = "all"
          )
      }
    }

    if ( !is.null( lst_alignment$center ) ) {
      for ( j in lst_alignment$center ) {
        ft = ft |>
          flextable::align(
            j = j,
            align = "center",
            part = "all"
          )
      }
    }

  }

  if ( lgc_merge_header_cells ) {

    # Merge identical cells in header
    ft = ft |>
      flextable::merge_h( part = "header" )

  }

  # Adjust font size
  ft = ft |>
    flextable::fontsize( size = int_font_size, part = "all" )

  # Adjust font family
  ft = ft |>
    flextable::font( fontname = chr_font_family, part = "all" )

  # Resize cells for nicer formatting
  ft = flextable::autofit( ft )

  # If coloring for rows is given
  if ( !is.null( chr_row_color ) ) {

    # By default color alternating rows
    if ( is.null( lgc_color_rows ) ) {

      int_rows <- seq( 1, nrow( dtf_table ), 2 )

      # Close 'By default color alternating rows'
    } else {

      int_rows <- which( lgc_color_rows )

      # Close else for 'By default color alternating rows'
    }

    ft = ft |>
      bg( i = int_rows,
          bg = chr_row_color, part = 'body' )

    # Close 'If coloring for rows is given'
  }

  # Return flextable object
  return( ft )
}

#### 2) ffcar_reformat_columns ####
#' Reformat Columns in Data Frame
#'
#' Function to reformat columns in a data frame of results
#' (e.g., combining different columns into nicely formatted
#' character strings of results).
#'
#' @param dtf_results A data frame with the values to format.
#' @param vec_columns A vector of column indices, either
#'   integers or character strings.
#' @param chr_template A character string giving the desired
#'   format of results. Specify columns using \code{'[[CX]]'}
#'   where X refers to the column number. See
#'   [ffcar::ffcar_unicode_lookup] for more details.
#' @param fun_transform A function, an optional transformation
#'   to apply before formatting (e.g., applying the \code{exp}
#'   function to convert log-odds to odds ratios).
#' @param int_digits An integer vector giving the number of
#'   digits to round to for each extracted column.
#' @param chr_common A character string, allows quick specification
#'   of some common templates (uncertainty intervals,
#'   estimates and standard errors, frequencies and percentages,
#'   p-values).
#'
#' @return A data frame.
#'
#' @examples
#' dtf_results <- data.frame(
#'   Estimate.raw = rnorm( 3 ),
#'   SE.raw = exp( rnorm( 3 ) )
#' )
#' dtf_results$P_value.raw <- pnorm(
#'   abs( dtf_results$Estimate )/dtf_results$SE.raw, lower.tail = FALSE
#' )*2
#' dtf_results$Test <- ffcar_reformat_columns(
#'   dtf_results, 1:3,
#'   '[[GLb]] = [[C1]] (SE = [[C2]]), p = [[C3]]',
#'   int_digits = c( 2, 2, 3 )
#' )
#' print( dtf_results )
#'
#' @export

ffcar_reformat_columns <- function(
    dtf_results,
    vec_columns,
    chr_template = NULL,
    fun_transform = NULL,
    int_digits = NULL,
    lgc_pad = TRUE,
    chr_common = 'estimate and standard error' ) {

  N_columns <- length( vec_columns )

  lst_common_templates <- list(
    uncertainty_interval = c(
      'Uncertainty interval',
      'uncertainty interval',
      'UI', 'ui',
      'Confidence interval',
      'confidence interval',
      'Credible interval',
      'credible interval',
      'CI', 'ci',
      'uncertainty_interval'
    ),
    estimate_and_SE = c(
      'Estimate and standard error',
      'estimate and standard error',
      'Estimate and SE',
      'estimate and SE',
      'Est. and SE',
      'est. and SE',
      'Est and SE',
      'est and SE',
      'estimate_and_SE'
    ),
    frequency_and_percent = c(
      'Frequency and percent',
      'frequency and percent',
      'Freq. and percent',
      'freq. and percent',
      'Freq and percent',
      'freq and percent',
      'N (%)',
      'n (%)',
      'frequency_and_percent'
    ),
    p_value = c(
      'P-value',
      'p-value',
      'P value',
      'p value',
      'PV',
      'pv',
      'p_value'
    )
  )

  if ( is.null( fun_transform ) ) {
    # Identity function
    fun_transform <- function(x) return(x)
  }

  if ( is.null( chr_template ) ) {

    if ( chr_common %in% lst_common_templates$uncertainty_interval ) {
      chr_template <- '[[C1]] to [[C2]]'
    }

    if ( chr_common %in% lst_common_templates$estimate_and_SE ) {
      chr_template <- '[[C1]] (SE = [[C2]])'
    }

    if ( chr_common %in% lst_common_templates$frequency_and_percent ) {
      chr_template <- '[[C1]] ([[C2]]%)'
    }

    if ( chr_common %in% lst_common_templates$p_value ) {
      chr_template <- 'p = [[C1]]'
      int_digits <- 3
    }

  }

  if ( is.null( int_digits ) ) {
    int_digits <- 2
  }

  if ( length( int_digits ) < N_columns ) {
    int_digits <- rep( int_digits, N_columns )
  }

  int_rows <- 1:nrow( dtf_results )

  mat_values <- matrix( NA, max( int_rows ), N_columns )

  for ( r in int_rows ) {

    mat_values[r, ] <- unlist( dtf_results[r, vec_columns] )

  }
  mat_chr_values <- matrix( '', nrow( mat_values ), ncol( mat_values ) )

  for ( k in 1:ncol( mat_values ) ) {
    mat_values[, k] <- fun_transform( mat_values[,k] )
    if ( lgc_pad ) {
      mat_chr_values[, k] <- ffcar::ffcar_padded_round(
        mat_values[, k], int_digits[k]
      )
    } else {
      mat_chr_values[, k] <- as.character( round(
        mat_values[, k], int_digits[k]
      ) )
    }
  }
  colnames( mat_chr_values ) <- paste0( 'C', 1:ncol( mat_chr_values ) )

  chr_out <- rep( '', max( int_rows ) )

  for ( r in int_rows ) {

    chr_out[r] <- ffcar_sub_in_content(
      chr_template,
      chr_values = mat_chr_values[r, ]
    )

    lgc_p_at_threshold <- chr_out[r] %in% c( 'p = 0.001',
                                             'p = 0',
                                             'p = 0.0',
                                             'p = 0.00',
                                             'p = 0.000' )
    if ( any( lgc_p_at_threshold ) ) {
      chr_out[r] <- gsub(
        chr_out[r],
        'p ' %paste%
          ffcar_unicode_lookup()['MSlte'] %paste%
          ' 0.001',
        chr_out[r], fixed = TRUE
      )
    }

  }

  return( chr_out )
}

#### 3) ffcar_design_table ####
#' Design a Table with Summmary Statistics
#'
#' Function to provide a flexible means of creating a table
#' of summary statistics for a data set (e.g., useful for
#' creating sample characteristics tables).
#'
#' @param dtf_data A data frame with values to summarize over.
#' @param lst_subsets A list of logical vectors (each matching
#'   in length to the number of rows of \code{dtf_data})
#'   specifying the subsets of data to summarize over. Multiple
#'   logical vectors will result in the creation of separate
#'   columns with summary statistics (one for each logical vector).
#'   The names for the columns of summary statistics will be
#'   set to the list names.
#' @param ... Additional lists specifying the rows to include
#'   in the table. Each list should be in the format of:
#'   \code{list( 'Label', 'Column name', 'Statistics', Digits,
#'   'Category' ).} The character string for the row label will
#'   be formatted using the function [ffcar::ffcar_sub_in_content].
#'   The character string for the column name specifies which column
#'   in \code{dtf_data} summary statistics should be computed
#'   over. The character string for the type of statistics to compute
#'   is passed to the syntax argument of the function
#'   [ffcar::ffcar_summa]. The integer for digits indicates the
#'   number of decimal places summary statistics should be round to.
#'   The final element of the list allows users to specify categories
#'   to match over to create a binary variable for computing proportions,
#'   percentages, and/or frequencies (otherwise the column must already
#'   consist of \code{TRUE} or \code{FALSE} values). Elements of the
#'   list must be specified in order; however, fewer than 5 elements
#'   can be specified. For example, to create a row that consists only
#'   of a label, users can simply provide a list with a single character
#'   string.
#'
#' @return A data frame.
#'
#' @examples
#' # Example data set
#' data(mtcars)
#'
#' # Create summary table
#' dtf_table <- ffcar_design_table(
#'   # Data set
#'   mtcars,
#'   # Subsets of data (determines # of columns)
#'   list(
#'     Overall = rep( TRUE, nrow(mtcars) ),
#'     Cylinders_4 = mtcars$cyl %in% 4,
#'     Cylinders_6 = mtcars$cyl %in% 6,
#'     Cylinders_8 = mtcars$cyl %in% 8
#'   ),
#'
#'  # Rows
#'   # list( Label, Column, Summary, Digits, Category )
#'   list( 'Sample size',
#'         'mpg', '[[N]]' ),
#'   list( 'Miles per gallon; M (SD)',
#'         'mpg', '[[M]] ([[SD]])', 1 ),
#'   list( 'Weight (10[[SP3]] lbs); M (SD)',
#'         'wt', '[[M]] ([[SD]])', 1 ),
#'   list( 'Transmission; % automatic (N)',
#'         'am', '[[P]]% ([[C]])', 1, 0 )
#' )
#' dtf_table[,-(2:5)]
#'
#' @export

ffcar_design_table <- function(
    dtf_data,
    lst_subsets,
    ... ) {

  # If no subsets are specified
  if ( is.null( lst_subsets ) ) {

    lst_subsets <- list(
      rep( TRUE, nrow(dtf_data) )
    )

    # Close 'If no subsets are specified'
  }

  # Extract specification of rows
  lst_arg <- list(...)
  int_rows <- seq_along( lst_arg )

  # Initialize table
  dtf_table <- data.frame(
    Label = rep( '', max(int_rows) ),
    Variable = '',
    Statistic = '',
    Digits = 2,
    Category = ''
  )

  # Function to switch between continuous or categorical variable
  fun_category <- function(
    x,
    category ) {

    if ( all(category %in% '') ) {
      return(x)
    } else {
      return(x %in% category)
    }

  }

  # Loop over rows
  for ( r in int_rows ) {

    # Add label
    if ( length( lst_arg[[r]] ) >= 1 ) {

      dtf_table$Label[r] <- ffcar::ffcar_sub_in_content(
        lst_arg[[r]][[1]]
      )

      # Close 'Add label'
    }

    # Add variable and statistics to compute
    if ( length( lst_arg[[r]] ) > 2 ) {

      dtf_table$Variable[r] <- lst_arg[[r]][[2]]
      dtf_table$Statistic[r] <- lst_arg[[r]][[3]]

      # Close 'Add variable and statistics to compute'
    }

    # Add digits to round to
    if ( length( lst_arg[[r]] ) > 3 ) {

      dtf_table$Digits[r] <- lst_arg[[r]][[4]]

      # Close 'Add digits to round to'
    }

    # Add categories to match against
    if ( length( lst_arg[[r]] ) > 4 ) {

      dtf_table$Category[r] <- lst_arg[[r]][[5]]

      # Close 'Add categories to match against'
    }

    # Close 'Loop over rows'
  }


  # Extract columns to
  int_col <- length( lst_subsets )

  # Default column names
  if ( is.null( names( lst_subsets ) ) ) {

    names( lst_subsets ) <- paste0(
      'X', seq_along( lst_subsets )
    )

    # Close 'Default column names'
  }

  # Initialize matrix for summaries
  mat_summary <- matrix(
    '', nrow( dtf_table ), int_col
  )

  # Loop over columns
  for ( k in 1:int_col ) {

    # Loop over rows
    for ( r in int_rows ) {

      lgc_subset <- lst_subsets[[k]]

      # If summary should be computed
      if ( dtf_table$Variable[r] != '' ) {

        mat_summary[r, k] <- ffcar::ffcar_summa(
          fun_category( dtf_data[[ dtf_table$Variable[r] ]][lgc_subset],
                        dtf_table$Category[r] ),
          chr_syntax = dtf_table$Statistic[r],
          int_digits = dtf_table$Digits[r]
        )

        # Close 'If summary should be computed'
      }

      # Close 'Loop over rows'
    }

    # Close 'Loop over columns'
  }

  colnames( mat_summary ) <- names( lst_subsets )

  dtf_table <- cbind( dtf_table, mat_summary )

  return( dtf_table )
}
