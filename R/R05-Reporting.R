# Functions for general-purpose reporting
# Written by Kevin Potter
# email: kevin.w.potter@gmail.com
# Please email me directly if you
# have any questions or comments
# Last updated 2023-06-02

# Table of contents
# 1) Functions for text content
#   1.1) ffcar_unicode_lookup
#   1.2) ffcar_sub_in_content
#   1.3) ffcar_conditional_statement
# 2) Functions for adding content
#   2.1) ffcar_add_header
#   2.2) ffcar_add_paragraph
#   2.3) ffcar_add_table
#   2.4) ffcar_add_figure
#   2.5) ffcar_add_code
#   2.6) ffcar_add_link
#   2.7) ffcar_add_table_of_contents
# 3) Functions to create/update reports
#   3.1) ffcar_create_report
#   3.2) ffcar_update_report_from_list

#### 1) Functions for text content ####

#### 1.1) ffcar_unicode_lookup ####
#' Unicode Lookup Table
#'
#' Function that returns useful unicode with
#' shorthand labels for easy lookup. Note that
#' for certain systems, the code points
#' used here may not render properly.
#'
#' @return A named character vector with unicode
#'   and associated shorthand labels.
#'
#' @examples
#' # Greek letters
#' ffcar_unicode_lookup() %with_name% 'GL'
#' # Superscripts
#' ffcar_unicode_lookup() %with_name% 'SP'
#' # Subscripts
#' ffcar_unicode_lookup() %with_name% 'SB'
#' # Math symbols
#' ffcar_unicode_lookup() %with_name% 'MS'
#' # Error bar symbols
#' ffcar_unicode_lookup() %with_name% 'EB'
#'
#' @export

ffcar_unicode_lookup <- function() {

  chr_unicode <- c(
    # Greek letters
    GLa   = '\U03B1', # alpha
    GLA   = '\U0391',
    GLb   = '\U03B2', # beta
    GLB   = '\U0392',
    GLg   = '\U03B3', # gamma
    GLG   = '\U0393',
    GLd   = '\U03B4', # delta
    GLD   = '\U0394',
    GLep  = '\U03B5', # epsilon
    GLEP  = '\U0395',
    GLz   = '\U03B6', # zeta
    GLZ   = '\U0396',
    GLet  = '\U03B7', # eta
    GLET  = '\U0397',
    GLth  = '\U03B8', # theta
    GLTH  = '\U0398',
    GLi   = '\U03B9', # iota
    GLI   = '\U0399',
    GLkp  = '\U03BA', # kappa
    GLKP  = '\U039A',
    GLl   = '\U03BB', # lambda
    GLL   = '\U039B',
    GLm   = '\U03BC', # mu
    GLM   = '\U039C',
    GLn   = '\U03BD', # nu
    GLN   = '\U039D',
    GLx   = '\U03BE', # xi
    GLX   = '\U039E',
    GLo   = '\U03BF', # omicron
    GlO   = '\U039F',
    GLp   = '\U03C0', # pi
    GLP   = '\U03A0',
    GLr   = '\U03C1', # rho
    GLR   = '\U03A1',
    GLs   = '\U03C3', # sigma
    GLS   = '\U03A3',
    GLt   = '\U03C4', # tau
    GLT   = '\U03A4',
    GLu   = '\U03C5', # upsilon
    GLU   = '\U03A5',
    GLph  = '\U03C6', # phi
    GLPH  = '\U03A6',
    GLc   = '\U03C7', # chi
    GLC   = '\U03A7',
    GLps  = '\U03C8', # psi
    GLPS  = '\U03A8',
    GLom  = '\U03C9', # omega
    GLOM  = '\U03A9',
    # Superscripts
    # - Numbers
    SP0   = '\U2070', # 0
    SP1   = '\U00B9', # 1
    SP2   = '\U00B2', # 2
    SP3   = '\U00B3', # 3
    SP4   = '\U2074', # 4
    SP5   = '\U2075', # 5
    SP6   = '\U2076', # 6
    SP7   = '\U2077', # 7
    SP8   = '\U2078', # 8
    SP9   = '\U2079', # 9
    # - Math symbols
    SPop  = '\U207D', # (
    SPcp  = '\U207E', # )
    SPm   = '\U207B', # -
    SPp   = '\U207A', # +
    SPe   = '\U207C', # =
    # - Letters
    SPla  = '\U1D43',  # a
    SPlA  = '\UA7F9',
    SPlb  = '\U1D47',  # b
    SPlB  = '\U1D2D',
    SPlc  = '\U1D9C',  # c
    SPlC  = '\UA7FD',
    SPld  = '\U1D48',  # d
    SPlD  = '\U1D30',
    SPle  = '\U1D49',  # e
    SPlE  = '\U1D31',
    SPlf  = '\U1DA0',  # f

    SPlg  = '\U1D4D',  # g
    SPlG  = '\U1D33',
    SPlh  = '\U02B0',  # h
    SPlH  = '\U1D34',
    SPli  = '\U2071',  # i
    SPlI  = '\U1D35',
    SPlj  = '\U02B2',  # j
    SPlJ  = '\U1D36',
    SPlk  = '\U1D4F',  # k
    SPlK  = '\U1D37',
    SPll  = '\U02E1',  # l
    SPlL  = '\U1D38',
    SPlm  = '\U1D50',  # m
    SPlM  = '\U1D39',
    SPln  = '\U207F',  # n
    SPlN  = '\U1D3A',
    SPlo  = '\U1D52',  # o
    SPlO  = '\U1D3C',
    SPlp  = '\U1D56',  # p
    SPlP  = '\U1D3E',
    SPlq  = '\U107A5', # q

    SPlr  = '\U02B3',  # r
    SPlR  = '\U1D3F',
    SPls  = '\U02E2',  # s

    SPlt  = '\U1D57',  # t
    SPlT  = '\U1D40',
    SPlu  = '\U1D58',  # u
    SPlU  = '\U1D41',
    SPlv  = '\U1D5B',  # v
    SPlV  = '\U2C7D',
    SPlw  = '\U02B7',  # w
    SPlW  = '\U1D42',
    SPlx  = '\U02E3',  # x

    SPly  = '\U02B8',  # y

    SPlz  = '\U1DBB',  # z

    # Subscripts
    SB0   = '\U2080', # 0
    SB1   = '\U2081', # 1
    SB2   = '\U2082', # 2
    SB3   = '\U2083', # 3
    SB4   = '\U2084', # 4
    SB5   = '\U2085', # 5
    SB6   = '\U2086', # 6
    SB7   = '\U2087', # 7
    SB8   = '\U2088', # 8
    SB9   = '\U2089', # 9
    SBop  = '\U208D', # (
    SBcp  = '\U208E', # )
    # Math symbols
    MStld = '\U007E', # tilde
    MSpm  = '\U00B1', # plus-minus
    MSmlt = '\U00D7', # multiplication
    MSdvs = '\U00F7', # division,
    MSprt = '\U2202', # partial derivative
    MSemp = '\U2205', # empty set
    MSelm = '\U2208', # element of
    MSnte = '\U2209', # not an element of
    MSpdr = '\U220F', # N-ary product
    MSsmm = '\U2211', # N-ary summation
    MSsqr = '\U221A', # square root
    MSprp = '\U221D', # proportional to
    MSinf = '\U221E', # infinity,
    MSdt  = '\U2981', # dot
    MSint = '\U222B', # integral
    MSlte = '\U2264', # less than or equal
    MSgte = '\U2265', # greater than or equal
    MSsbs = '\U2282', # subset
    MSnsb = '\U2285', # not a subset
    # Error bars
    EBes  = '\U29EE', # empty square
    EBfs  = '\U29EF', # filled square
    EBec  = '\U29F2', # empty circle
    EBec  = '\U29F3'  # filled circle
  )

  return( chr_unicode )
}

#### 1.2) ffcar_sub_in_content ####
#' Substitute Content for Placeholder Text
#'
#' Function to substitute in content based on
#' placeholder text in a character string.
#'
#' @param chr_string A character string with placeholder
#'   text to replace.
#' @param chr_values A named character vector (or vector
#'   that can be coerced to a character vector) with
#'   the elements to substitute in based on similarly
#'   labeled placeholder text.
#' @param chr_brackets A character vector with the left
#'   and right bracketing symbols used to denote text to
#'   replace.
#'
#' @return A character string where the placeholder text
#' has been replaced with the relevant content.
#'
#' @examples
#' string <-
#'   'Results were significant, [[GLb]] = [[VCE]]' %paste%
#'   ' (SE = [[VSE]]), p = [[VPV]].'
#' # Named vector with names matching placeholder
#' # text to replace
#' values <- c( VCE = '1.0', VSE = '0.2', VPV = '0.002' )
#' ffcar_sub_in_content( string, values )
#'
#' @export

ffcar_sub_in_content <- function(
    chr_string,
    chr_values = NULL,
    chr_brackets = c( '[[', ']]' ) ) {

  chr_unicode <- ffcar_unicode_lookup()

  if ( is.null( chr_values ) ) {
    chr_values <- 'None'
  }

  chr_parts <- strsplit(
    chr_string, split = chr_brackets[1], fixed = TRUE
  )[[1]]

  chr_parts <- chr_parts[ grepl( chr_brackets[2], chr_parts, fixed = TRUE ) ]
  chr_symbols <- sapply(
    chr_parts, function(s) {
      strsplit( s, split = chr_brackets[2], fixed = TRUE )[[1]][1]
    }
  )

  chr_placeholders <- paste0(
    chr_brackets[1],
    chr_symbols,
    chr_brackets[2]
  )

  chr_to_sub_in <- rep( '?', length( chr_symbols ) )
  for ( s in seq_along( chr_to_sub_in ) ) {

    if ( any( names( chr_values ) %in% chr_symbols[s] ) ) {

      chr_to_sub_in[s] <- chr_values[ chr_symbols[s] ]

    }

    if ( any( names( chr_unicode ) %in% chr_symbols[s] ) ) {

      chr_to_sub_in[s] <- chr_unicode[ chr_symbols[s] ]

    }

  }

  chr_output <- chr_string

  for ( s in seq_along( chr_to_sub_in ) ) {

    chr_output <- gsub(
      chr_placeholders[s],
      chr_to_sub_in[s],
      chr_output,
      fixed = TRUE
    )

  }

  return( chr_output )
}

#### 1.3) ffcar_conditional_statement ####
#' Return Statement Based on Logical Value
#'
#' Function to return one of two possible statements
#' based on whether a logical value is \code{TRUE} or
#' \code{FALSE}. Useful, for example, to automatically
#' generate statements regarding statistical significance.
#'
#' @param lgc_condition A logical value.
#' @param chr_statement A character vector with two strings,
#'   [1] the statement to return when \code{FALSE} and [2]
#'   the statement to return when \code{TRUE}. Defaults to
#'   a statement on statistical significance.
#'
#' @return A character string.
#'
#' @examples
#' ffcar_conditional_statement( FALSE )
#' ffcar_conditional_statement( TRUE )
#'
#' # Custom statements
#' ffcar_conditional_statement(
#'   FALSE, c( 'non-significant', 'significant' )
#' )
#' ffcar_conditional_statement(
#'   TRUE, c( 'non-significant', 'significant' )
#' )
#'
#' @export

ffcar_conditional_statement <- function(
    lgc_condition,
    chr_statement = NULL ) {

  if ( is.null( chr_statement ) ) {

    chr_statement <- c(
      'not statistically significant',
      'statistically significant'
    )

  }

  return( chr_statement[ lgc_condition + 1] )
}

#### 1.4) ffcar_padded_round ####
#' Round a Value and Pad Number of Trailing Zeros
#'
#' Function that rounds a value to a desired number
#' of digits and
#'
#' @return A character string with the rounded values.
#'
#' @examples
#' ffcar_padded_round( c( 1, 1.1, 1.116, 1.1116 ), 2 )
#'
#' @export

ffcar_padded_round <- function( num_value, int_digits ) {

  N_values <- length( num_value )
  chr_values <- rep( '', N_values )

  # Loop over values
  for ( v in 1:N_values ) {

    # Round value and convert to character string
    chr_value <- as.character( round( num_value[v], int_digits ) )

    # If rounding to specified number of decimal places
    if ( int_digits > 0 ) {

      # Check if any decimals
      if ( grepl( '.', chr_value, fixed = TRUE ) ) {

        chr_parts <- strsplit(
          chr_value, split = '.', fixed = TRUE
        )[[1]]

        # If padding is needed
        if ( nchar( chr_parts[2] ) < int_digits ) {

          chr_parts[2] <- paste0(
            chr_parts[2],
            paste(
              rep( 0, int_digits - nchar( chr_parts[2]) ), collapse = ''
            )
          )

        }

        chr_values[v] <- paste0(
          chr_parts[1], '.', chr_parts[2]
        )

        # Close 'Check if any decimals'
      } else {

        chr_values[v] <- paste0(
          chr_value,
          '.',
          paste( rep( 0, int_digits ), collapse = '' )
        )

        # Close else for 'Check if any decimals'
      }

      # Close 'If rounding to specified number of decimal places'
    }

    # Close 'Loop over values'
  }

  return( chr_values )
}

#### 1.5) ffcar_variables_to_labels ####
#' Convert Variable Names to Labels
#'
#' Function to convert variable names (e.g.,
#' columns in a data frame, predictors used
#' in a regression) to nice labels.
#'
#' @param chr_variables A character vector, the
#'   variable names to convert to nice labels.
#' @param chr_remove An optional character vector,
#'   string patterns to remove. Defaults to
#'   \code{'PRD'}, \code{'OUT'}, and \code{'COL'}.
#' @param chr_space A character string, the
#'   pattern to replace with an empty space.
#' @param int_skip An optional integer vector
#'   with values from 1 to 12 indicating
#'   specifc patterns to skip.
#'
#' @return A character vector of nicely formatted labels.
#'
#' @examples
#' # Parentheses
#' ffcar_variables_to_labels( 'PRDGroupZZOPPTreatmentCLP' )
#' # Square brackets
#' ffcar_variables_to_labels( 'PRDGroupZZOPBTreatmentCLB' )
#' # Hyphen
#' ffcar_variables_to_labels( 'PRDShortHPHterm' )
#' # Colon
#' ffcar_variables_to_labels( 'PRDGroupCLNZZTreatment' )
#' # Semicolon
#' ffcar_variables_to_labels( 'PRDGroupSMCZZTreatment' )
#' # Pipe symbol
#' ffcar_variables_to_labels( 'PRDPOPPAPPEBCLP' )
#' # Plus, minus, and equal signs
#' ffcar_variables_to_labels( 'PRDXZZEQLZZYZZPLSZZZ' )
#' # Hash sign
#' ffcar_variables_to_labels( 'PRDHSHZZofZZcounts' )
#'
#' @export

ffcar_variables_to_labels <- function(
  chr_variables,
  chr_remove = NULL,
  chr_space = 'ZZ',
  int_skip = -1 ) {

  chr_labels <- chr_variables

  # Default patterns to remove
  if ( is.null( chr_remove ) ) {

    chr_remove <- c(
      'PRD',
      'OUT',
      'COL'
    )

    # Close 'Default patterns to remove'
  }

  # Loop over elements to remove
  for ( i in seq_along( chr_remove ) ) {

    chr_labels <- gsub(
      chr_remove[i], '', chr_labels
    )

    # Close 'Loop over elements to remove'
  }

  mat_find_replace <- rbind(
    c( 'OPP', '(' ), # 1
    c( 'CLP', ')' ), # 2
    c( 'OPB', '[' ), # 3
    c( 'CLB', ']' ), # 4
    c( 'HPH', '-' ), # 5
    c( 'CLN', ':' ), # 6
    c( 'SMC', ';' ), # 7
    c( 'PPE', '|' ), # 8
    c( 'PLS', '+' ), # 9
    c( 'EQL', '=' ), # 10
    c( 'SLS', '/' ), # 11
    c( 'HSH', '#' ), # 12
    c( 'PRC', '%' ), # 13
    c( 'AMP', '&' ), # 14
    c( chr_space, ' ' )
  )

  mat_find_replace <- mat_find_replace[
    !( 1:nrow( mat_find_replace ) %in% int_skip ),
  ]

  for ( i in 1:nrow( mat_find_replace ) ) {
    chr_labels <- gsub(
      mat_find_replace[i, 1], mat_find_replace[i, 2],
      chr_labels, fixed = TRUE
    )
  }

  return( chr_labels )
}

#### 1.6) ffcar_summa ####
#' Flexible Formatted Summary Statistics
#'
#' Function that creates nicely formatted character strings
#' with summary statistics based on user-supplied identifiers
#' via a simple, intuitive syntax.
#'
#' @param vec_values A vector of values.
#' @param chr_syntax A character string with identifiers in
#'   the form \code{[[.]]} where \code{.} can be a variety
#'   of letter sets for different summary statistics.
#'   The function will substitute the appropriate
#'   summary statistic computed over \code{Vec_values}
#'   in place of the identifier.
#' @param lgc_remove_NA Logical; if \code{TRUE} removes
#'   \code{NA} values before computing statistics.
#' @param int_digits An integer value - the number of
#'   digits to round to.
#' @param lgc_pad Logical; if \code{TRUE} pads decimal
#'   places with zeros if needed.
#' @param vec_categories A vector of values to
#'   match \code{vec_values} for certain statistics
#'   (e.g., percentages, counts).
#' @param fun_custom An optional user-defined function
#'   that takes \code{vec_values} as a first argument
#'   and returns a vector of values. The i-th
#'   outputted value will then be substituted for the
#'   corresponding identifier \code{[[i]]}.
#' @param ... Additional arguments for the user-defined
#'   function.
#'
#' @details
#' This function provides some simple syntax to allow users
#' to write out a custom phrase for reporting summary statistics.
#' The function then searches the input for identifiers -
#' once found, the function the computes the appropriate
#' summary statistic and substitues the numeric result
#' in place of the given identifier.
#'
#' For example, a user can provide the phrase:
#'
#' \code{'Mean = [[M]]'},
#'
#' and the function will then substitute the sample mean
#' for \code{vec_values} for the identifier \code{[[M]]}.
#'
#' Pre-defined identifiers are:
#' \itemize{
#'   \item \code{[[N]]} = Sample size;
#'   \item \code{[[M]]} = Mean;
#'   \item \code{[[SD]]} = Standard deviation;
#'   \item \code{[[SE]]} = Standard error of the mean;
#'   \item \code{[[Mn]]} = Minimum;
#'   \item \code{[[Q1]]} = 1st quartile;
#'   \item \code{[[Md]]} = Median;
#'   \item \code{[[Q3]]} = 2nd quartile;
#'   \item \code{[[Mx]]} = Maximum;
#'   \item \code{[[IQR]]} = Inter-quartile range;
#'   \item \code{[[C]]} = Counts/frequencies;
#'   \item \code{[[P]]} = Percent;
#'   \item \code{[[Pr]]} = Proportion.
#' }
#'
#' Users can also pass in a custom function \code{f}
#' that takes \code{x} as a first argument and
#' returns a vector of values. Then element \code{i}
#' from the outputted vector is substituted for
#' the identifier \code{[[i]]}.
#'
#' @return A character string.
#'
#' @examples
#' # Example using 'iris' data set
#' data("iris")
#' # Continuous variable - sepal length
#' x <- iris$Sepal.Length
#'
#' # Mean and standard deviation
#' ffcar_summa(x)
#' # Median and IQR
#' ffcar_summa(x, "[[Md]] ([[IQR]])")
#' # Pad to 2 decimal places
#' ffcar_summa(x, "[[Md]] ([[IQR]])", lgc_pad = TRUE)
#' # Mean (SD); N [min, 1st, 2nd, and 3rd quartile, max]
#' ffcar_summa(x, paste0(
#'     "[[N]]; [[M]] ([[SD]]); ",
#'     "[[[Mn]], [[Q1]], [[Md]], [[Q3]], [[Mx]]]"
#'   ), int_digits = 1)
#'
#' # Custom measures via user-defined function
#' # Geometric mean
#' fun_geometric <- function(x) {
#'   exp( mean( log(x) ) )
#' }
#' ffcar_summa(x, "[[M]] vs. [[1]]",
#'   fun_custom = fun_geometric
#' )
#'
#' # Example using 'mtcars' data set
#' # Categorical variable - # of forward gears
#' data("mtcars")
#' x <- mtcars$gear
#'
#' # Percent and counts for 3 forward gears
#' ffcar_summa(x == 3, "[[P]]% ([[C]] out of [[N]])")
#' # Percent and counts for 4 or 5 forward gears
#' ffcar_summa(x, "[[P]]% ([[C]] out of [[N]])",
#'   vec_categories = c(4, 5)
#' )
#'
#' @export

ffcar_summa <- function(
    vec_values,
    chr_syntax = "[[M]] ([[SD]])",
    lgc_remove_NA = TRUE,
    int_digits = 2,
    lgc_pad = FALSE,
    vec_categories = NULL,
    fun_custom = NULL,
    ... ) {

  # Initialize output
  chr_output <- chr_syntax

  fun_format <- function(
    num_statistic ) {

    num_statistic <- round( num_statistic, int_digits )
    if ( lgc_pad ) {
      num_statistic <-
        format(num_statistic, nsmall = int_digits)
    }

    return( num_statistic )
  }

  lst_replacements <- list(
    # Sample size
    list(
      syntax = "[[N]]",
      fun = function( x ) {

        n <- length(x)
        if (lgc_remove_NA) length( x[! is.na(x) ] )

        return(n)
      }
    ),
    # Mean
    list(
      syntax = "[[M]]",
      fun = function( x ) {

        m <- mean(x, na.rm = lgc_remove_NA )

        m <- fun_format(m)

        return(m)
      }
    ),
    # Standard deviation
    list(
      syntax = "[[SD]]",
      fun = function( x ) {

        s <- sd(x, na.rm = lgc_remove_NA )

        s <- fun_format(s)

        return(s)
      }
    ),
    # Standard error
    list(
      syntax = "[[SE]]",
      fun = function( x ) {

        n <- length(x)
        if (lgc_remove_NA) n <- length( x[ !is.na(x) ] )
        s <- sd(x, na.rm = lgc_remove_NA )
        se <- s / sqrt(n)

        se <- fun_format(se)

        return(se)
      }
    ),
    # Minimum
    list(
      syntax = "[[Mn]]",
      fun = function( x ) {

        m <- min(x, na.rm = lgc_remove_NA)

        m <- fun_format(m)

        return(m)
      }
    ),
    # 1st quartile
    list(
      syntax = "[[Q1]]",
      fun = function( x ) {

        q <- quantile(x, prob = .25, na.rm = lgc_remove_NA)

        q <- fun_format(q)

        return(q)
      }
    ),
    # Median
    list(
      syntax = "[[Md]]",
      fun = function( x ) {

        m <- median(x, na.rm = lgc_remove_NA)

        m <- fun_format(m)

        return(m)
      }
    ),
    # 3rd quartile
    list(
      syntax = "[[Q3]]",
      fun = function( x ) {

        q <- quantile(x, prob = .75, na.rm = lgc_remove_NA)

        q <- fun_format(q)

        return(q)
      }
    ),
    # Maximum
    list(
      syntax = "[[Mx]]",
      fun = function( x ) {

        m <- max(x, na.rm = lgc_remove_NA)

        m <- fun_format(m)

        return(m)
      }
    ),
    # Inter-quartile range
    list(
      syntax = "[[IQR]]",
      fun = function( x ) {

        iqr <-
          quantile(x, prob = .75, na.rm = lgc_remove_NA) -
          quantile(x, prob = .25, na.rm = lgc_remove_NA)

        iqr <- fun_format(iqr)

        return(iqr)
      }
    ),
    # Counts/frequencies
    list(
      syntax = "[[C]]",
      fun = function( x ) {

        if ( is.null(vec_categories) ) {
          vec_categories <- TRUE
        }
        cnt <- sum(x %in% vec_categories, na.rm = lgc_remove_NA)

        return(cnt)
      }
    ),
    # Percent
    list(
      syntax = "[[P]]",
      fun = function( x ) {

        if ( is.null(vec_categories) ) {
          vec_categories <- TRUE
        }
        p <- 100*mean(x %in% vec_categories, na.rm = lgc_remove_NA)

        p <- fun_format(p)

        return(p)
      }
    ),
    # Proportion
    list(
      syntax = "[[Pr]]",
      fun = function( x ) {

        if ( is.null(vec_categories) ) {
          vec_categories <- TRUE
        }
        p <- mean(x %in% vec_categories, na.rm = lgc_remove_NA)

        p <- fun_format(p)

        return(p)
      }
    )
  )

  # Loop over statistics
  for ( i in seq_along( lst_replacements ) ) {

    chr_output <- gsub(
      lst_replacements[[i]]$syntax,
      lst_replacements[[i]]$fun( vec_values ),
      chr_output,
      fixed = TRUE
    )

  }

  # Custom function
  if ( !is.null( fun_custom ) ) {

    num_additonal <- fun_custom(vec_values, ...)

    # Loop over additional values to add
    for ( i in seq_along(num_additonal) ) {

      chr_output <- gsub(
        paste0( "[[", i, "]]" ),
        fun_format(num_additonal[i]),
        chr_output,
        fixed = TRUE
      )

      # Close 'Loop over additional values to add'
    }

    # Close 'Custom function'
  }

  chr_output <- ffcar::ffcar_sub_in_content(
    chr_output
  )

  return(chr_output)
}

#### 2) Functions for adding content ####

#### 2.1) ffcar_add_header ####
#' Create RMarkdown Code for Headers
#'
#' Function that creates a header at a specified
#' level for a RMarkdown file.
#'
#' @param chr_header A character string, the header content.
#' @param int_level An integer value between 1 and 5, the
#'   level for the header.
#'
#' @return A character vector.
#'
#' @examples
#' ffcar_add_header( 'Example' )
#'
#' @export

ffcar_add_header <- function(
    chr_header,
    int_level = 3 ) {

  if ( chr_header != '' ) {

    chr_level <- paste( rep( '#', int_level ), collapse = '' )

    return( chr_level %paste% ' ' %paste% chr_header )

  } else {

    return( c() )

  }

}

#### 2.2) ffcar_add_paragraph ####
#' Create RMarkdown Code for Paragraph
#'
#' Function that creates a paragraph for a RMarkdown file.
#'
#' @param chr_text A character vector.
#'
#' @returns A character vector.
#'
#' @examples
#' ffcar_add_paragraph( 'Hello world' )
#'
#' @export

ffcar_add_paragraph <- function(
    chr_text ) {

  if ( chr_text != '' ) {

    return( chr_text )

  } else {

    return( c() )

  }

}

#### 2.3) ffcar_add_table ####
#' Create RMarkdown Code for Tables
#'
#' Function that creates the code to add a table
#' (assumed to be a flextable object) to a RMarkdown file.
#'
#' @param chr_index A character string, assumed to be
#'   a pre-existing list containing the specified
#'   table (e.g., \code{lst_example$table}).
#' @param chr_title A character string, an optional
#'   title for the table.
#' @param chr_caption A character vector, an optional
#'   table caption.
#' @param chr_file_name An character string, an optional
#'   file name. If provided, a word document containing the
#'   table will be saved to the 'Downloads' folder.
#'
#' @return A character vector.
#'
#' @export

ffcar_add_table <- function(
    chr_index,
    chr_title,
    chr_caption,
    chr_file_name = '' ) {

  chr_save_table <- ''

  if ( chr_file_name != '' ) {

    chr_save_table <- c(
      '',
      'chr_word_document <- ',
      paste0(
        '  "Downloads/',
        chr_file_name,
        '.docx"'
      ),
      '',
      'obj_docx <- officer::read_docx()',
      '',
      'obj_docx <- flextable::body_add_flextable(',
      '  obj_docx, ',
      paste0(
        '  ',
        chr_index, ', '
      ),
      '  align = "left"',
      ')',
      '',
      'print(',
      '  obj_docx, target = chr_word_document',
      ')'
    )

  }

  chr_lines <- c(
    ffcar_add_header( chr_title ),
    '',
    "```{r, echo=FALSE}",
    chr_index,
    chr_save_table,
    "```",
    '',
    '',
    ffcar_add_paragraph(
      chr_caption
    )
  )

  return( chr_lines )
}

#### 2.4) ffcar_add_figure ####
#' Create RMarkdown Code for Figures
#'
#' Function that creates the code to add a figure
#' (assumed to be a PNG image saved to a local folder)
#' to a RMarkdown file.
#'
#' @param chr_index A character string, assumed to be
#'   an indexed character vector containing the relative
#'   path to the figure (e.g., \code{chr_figure_paths[1]}).
#' @param chr_title A character string, an optional
#'   title for the figure.
#' @param chr_caption A character vector, an optional
#'   figure caption.
#'
#' @return A character vector.
#'
#' @export

ffcar_add_figure <- function(
    chr_index,
    chr_title = '',
    chr_caption = '' ) {

  chr_lines <- c(
    ffcar_add_header( chr_title ),
    '',
    "```{r, results='asis', echo=FALSE}",
    'cat( "![](", ' %paste% chr_index %paste% ', ")" )',
    "```",
    '',
    '',
    ffcar_add_paragraph(
      chr_caption
    )
  )

  return( chr_lines )
}

#### 2.5) ffcar_add_code ####
#' Create RMarkdown Code for R Code
#'
#' Function that creates the code to add an R code
#' segment to a RMarkdown file.
#'
#' @param chr_code A character vector with the lines of
#'   R code to add.
#' @param chr_tag A character string, an optional code
#'   segment label.
#' @param chr_options A character string, optional
#'   R code segment options.
#'
#' @return A character vector.
#'
#' @export

ffcar_add_code <- function(
    chr_code,
    chr_tag = "",
    chr_options = ", echo=FALSE" ) {

  chr_lines <- c(
    paste0( '```{r ', chr_tag, chr_options, '}' ),
    chr_code,
    '```'
  )

  return( chr_lines )
}

#### 2.6) ffcar_add_link ####
#' Create RMarkdown Code for a Link
#'
#' Function that creates the code to add a line
#' with a link (e.g., a link to the table of
#' contents) to a RMarkdown file.
#'
#' @param chr_content A character string, the
#'   text content of the hyperlink.
#' @param chr_link The internal link to use.
#'
#' @return A character string.
#'
#' @export

ffcar_add_link <- function(
    chr_content = '',
    chr_link = '' ) {

  if ( chr_content == '' ) {
    chr_content <- 'Table of contents'
  }
  if ( chr_link == '' ) {
    chr_link <- 'TOC'
  }

  chr_lines <- c(
    paste0( '[', chr_content, '](#', chr_link, ')' )
  )

  return( chr_lines )

}

#### 2.7) ffcar_add_table_of_contents ####
#' Create RMarkdown Code for a Table of Contents
#'
#' Function that creates the code to add
#' a table of contents linking to the section
#' headers to a RMarkdown file.
#'
#' @param chr_headers A character string, the
#'   text content (i.e., the section header).
#' @param chr_sections A character string, the
#'   section number (e.g., \code{'11'}, \code{'22'}).
#'
#' @return A character vector.
#'
#' @export

ffcar_add_table_of_contents <- function(
    chr_headers,
    chr_sections = '' ) {

  int_lines <- length( chr_headers )

  if ( all( chr_sections %in% '' ) ) {

    chr_sections <- as.character(
      seq_along( chr_headers )
    )

  }

  int_char <- sapply(
    chr_sections, nchar
  )
  int_min_char <- min( int_char )

  chr_tabs <- rep( '', int_lines )

  if ( any( int_char > int_min_char ) ) {

    for ( i in 1:4 ) {
      chr_tabs[ int_char == (int_min_char + i) ] <-
        paste( rep( '<tab>', 2*i ), collapse = '' )
    }

  }

  chr_type <- rep( 'section', int_lines )
  chr_type[
    grepl( 'Figure', chr_headers )
  ] <- 'fig'
  chr_type[
    grepl( 'Table', chr_headers )
  ] <- 'tab'

  chr_number <- sapply(
    chr_sections, function(s) {
      chr_parts <- strsplit( s, split = '', fixed = TRUE )[[1]]
      return( tail( chr_parts, n = 1 ) )
    }
  )

  chr_lines <- paste0(
    chr_tabs,
    chr_number, '. ',
    '[', chr_headers, '](#',
    chr_type,
    chr_sections, ')<br>'
  )

  return( chr_lines )

}

#### 3) Functions to create/update reports ####

#### 3.1) ffcar_create_report ####
#' Create RMarkdown File for Report
#'
#' Function to initialize a .Rmd file
#' for a \code{ffcar}-compatible report.
#'
#' @param chr_file_name A character string,
#'   the file name for the report.
#' @param chr_date A character vector, the
#'   date for the file name and the date for
#'   the report header, respectively.
#' @param chr_title A character string, the
#'   title of the report.
#' @param chr_author A character string, the
#'   author(s) of the report.
#'
#' @returns An .Rmd file.
#'
#' @export

ffcar_create_report <- function(
    chr_file_name,
    chr_date = NULL,
    chr_title = 'Analysis Report',
    chr_author = 'Kevin Potter' ) {

  if ( is.null( chr_date ) ) {
    chr_date <- c(
      ffcar_ymd_hm(),
      ffcar_ymd( chr_sep = '-' )
    )
  }

  if ( length( chr_date ) == 1 ) {
    chr_date <- rep( chr_date, 2 )
  }

  chr_full_file_name <-
    chr_file_name %paste% '-' %paste%
    chr_date[1] %paste%
    '.Rmd'

  chr_lines <- c(
    '---',
    'title: "' %paste% chr_title %paste% '"',
    'author: "' %paste% chr_author %paste% '"',
    'date: "' %paste% chr_date[2] %paste% '"',
    'output: html_document',
    '---',
    '',
    '```{r setup, include=FALSE}',
    'knitr::opts_chunk$set(echo = TRUE)',
    '```',
    '',
    '```{r packages, echo=FALSE, message=FALSE}',
    'library(arfpam)',
    'library(dplyr)',
    'library(ffcar)',
    '```',
    '',
    '',
    '```{r files, echo=FALSE}',
    'lst_source <- ffcar_list_file_paths(',
    "  'Source'",
    ')',
    'lst_figures <- ffcar_list_file_paths(',
    "  'Figures'",
    ')',
    '```',
    '',
    '',
    '#### Table of contents {#TOC}',
    '',
    ''
  )

  write(
    chr_lines,
    file = chr_full_file_name,
    sep = '\n'
  )

}

#### 3.2) ffcar_update_report_from_list ####
#' Update Report From Content in List
#'
#' Given a list of lists specifying inputs
#' for the 'add' functions from \code{Ffcar},
#' append new content to a \code{.Rmd} file.
#'
#' @param lst_report A list of lists. Each sublist
#'   contains first, a character string specifying
#'   the 'add' function to call, either \code{'header'},
#'   \code{'paragraph'}, \code{'code'}, \code{'figure'},
#'   \code{'table'}, \code{'link'}, or \code{'table_of_contents'}.
#'   Second, the sublist contains a character string
#'   of the form \code{'X_Y'}, where \code{X} is a
#'   label denoting a section and \code{'Y'} is a
#'   number from 1 to 5 denoting the section level.
#'   Finally, the remainder of the list consists
#'   of the inputs to pass to the specified 'add'
#'   function.
#' @param chr_rmd_file A character string, the
#'   name for the .Rmd file to update.
#' @param int_max_TOC_level A integer value, the maximum
#'   number of tabs to delineate subsections (value between 1 to 6).
#'
#' @returns Updated content for the file specified
#' by \code{chr_rm_file}.
#'
#' @export

ffcar_update_report_from_list <- function(
    lst_report,
    chr_rmd_file,
    int_max_TOC_level = 2 ) {

  L <- length( lst_report )

  chr_sections <- unlist( lst_report %pull_sublist% c( 2 ) )
  S <- length( chr_sections )

  mat_levels <- matrix( NA, S, 5 )

  chr_levels <- rep( '', 5 )
  num_levels <- rep( 0, 5 )

  # Loop over sections labels
  for ( s in seq_along( chr_sections ) ) {

    # Loop over section levels
    for ( l in 1:5 ) {

      if ( grepl( '_' %paste% l, chr_sections[s], fixed = TRUE ) ) {

        if ( chr_sections[s] != chr_levels[l] ) {
          chr_levels[l] <- chr_sections[s]
          num_levels[l] <- num_levels[l] + 1

          if ( l < 5 ) {
            num_levels[l+1] <- 0
          }

        }

      }

      if ( l == 1 ) {
        mat_levels[s,l] <- num_levels[l]
      } else {
        mat_levels[s,l] <- paste0( '.', num_levels[l] )
      }

      # Close 'Loop over section levels'
    }

    # Close 'Loop over sections labels'
  }

  mat_levels[ mat_levels == '.0' ] <- ''

  chr_section_numbers <- apply( mat_levels, 1, paste, collapse = "" )

  # Initialize variables for loop
  chr_lines <- c()
  chr_current_section <- ''
  inc_figure <- 0
  inc_table <- 0
  chr_back_to_toc <- c( '[Table of contents](#TOC)', '', '' )

  chr_toc <- rep( '', L )
  int_toc_levels <- rep( 0, L )

  # Loop over list contents
  for ( l in 1:L ) {

    lst_current <- lst_report[[l]]

    if ( chr_current_section != chr_section_numbers[l] ) {
      chr_current_section <- chr_section_numbers[l]
      inc_figure <- 0
      inc_table <- 0
    }

    # Add section headers
    if ( lst_current[[1]] == 'header' ) {

      chr_tag <- gsub( '.', '', chr_section_numbers[l], fixed = TRUE )
      chr_parts <- strsplit(
        chr_section_numbers[l],
        split = '',
        fixed = TRUE
      )[[1]]

      chr_toc[l] <- paste0(
        chr_parts[ length( chr_parts ) ],
        '. [',
        lst_current[[3]],
        '](#section',
        chr_tag, ')<br>'
      )
      int_toc_levels[l] <- sum(  chr_parts == '.' )*2

      lst_current[[3]] <- paste0(
        chr_section_numbers[l], ') ', lst_current[[3]],
        ' {#section',
        chr_tag,
        '}'
      )

      chr_lines_to_add <-
        do.call( ffcar_add_header, lst_current[-(1:2)] )

      if (l > 1) {
        chr_lines_to_add <- c(
          chr_back_to_toc,
          chr_lines_to_add
        )
      }

      chr_lines <- c(
        chr_lines,
        chr_lines_to_add,
        '',
        ''
      )

      # Close 'Add section headers'
    }

    # Add text paragraph
    if ( lst_current[[1]] == 'paragraph' ) {

      chr_lines_to_add <-
        do.call( ffcar_add_paragraph, lst_current[-(1:2)] )

      chr_lines <- c(
        chr_lines,
        chr_lines_to_add,
        '',
        ''
      )

      # Close 'Add text paragraph'
    }

    # Add figure
    if ( lst_current[[1]] == 'figure' ) {

      inc_figure <- inc_figure + 1

      if ( lst_current[[4]] != '' ) {
        lst_current[[4]] <-
          'Figure ' %paste%
          chr_section_numbers[l] %paste%
          '.' %paste% inc_figure %paste%
          ': ' %paste%
          lst_current[[4]]
      }

      chr_lines_to_add <-
        do.call( ffcar_add_figure, lst_current[-(1:2)] )

      chr_lines <- c(
        chr_lines,
        chr_lines_to_add,
        '',
        ''
      )

      # Close 'Add figure'
    }

    # Add table
    if ( lst_current[[1]] == 'table' ) {

      inc_table <- inc_table + 1

      if ( lst_current[[4]] != '' ) {
        lst_current[[4]] <-
          'Table ' %paste%
          chr_section_numbers[l] %paste%
          '.' %paste% inc_table %paste%
          ': ' %paste%
          lst_current[[4]]
      }

      chr_lines_to_add <-
        do.call( ffcar_add_table, lst_current[-(1:2)] )

      chr_lines <- c(
        chr_lines,
        chr_lines_to_add,
        '',
        ''
      )

      # Close 'Add table'
    }

    # Add code
    if ( lst_current[[1]] == 'code' ) {

      chr_lines_to_add <-
        do.call( ffcar_add_code, lst_current[-(1:2)] )

      chr_lines <- c(
        chr_lines,
        chr_lines_to_add,
        '',
        ''
      )

      # Close 'Add code'
    }

    # Add link
    if ( lst_current[[1]] == 'link' ) {

      chr_lines_to_add <-
        do.call( ffcar_add_link, lst_current[-(1:2)] )

      chr_lines <- c(
        chr_lines,
        chr_lines_to_add,
        '',
        ''
      )

      # Close 'Add link'
    }

    # Add table of contents
    if ( lst_current[[1]] == 'table_of_contents' ) {

      chr_lines_to_add <-
        do.call( ffcar_add_table_of_contents, lst_current[-(1:2)] )

      chr_lines <- c(
        chr_lines,
        chr_lines_to_add,
        '',
        ''
      )

      # Close 'Add table of contents'
    }

    # Close 'Loop over list contents'
  }

  int_toc_levels <- int_toc_levels[ chr_toc != '' ]
  chr_toc_lines <- chr_toc[
    chr_toc != ''
  ]

  for ( i in seq_along( chr_toc_lines ) ) {
    if ( int_toc_levels[i] > 0 & int_toc_levels[i] <= int_max_TOC_level ) {
      chr_toc_lines[i] <- paste0(
        paste(
          rep( '&nbsp;', int_toc_levels[i]*2 ),
          collapse = ""
        ),
        chr_toc_lines[i]
      )
    }

    if ( int_toc_levels[i] > int_max_TOC_level ) {
      chr_toc_lines[i] <- ""
    }

  }

  chr_toc_lines <- chr_toc_lines[ chr_toc_lines != "" ]

  write(
    c(
      chr_toc_lines,
      '',
      '',
      chr_lines,
      chr_back_to_toc
    ),
    file = chr_rmd_file,
    append = TRUE,
    sep = "\n"
  )

}
