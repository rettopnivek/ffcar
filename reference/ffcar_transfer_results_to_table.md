# Transfer Results From List to Table

Given a list with multiple summaries of model estimates, transfers
specified rows and columns to a new data frame. Useful, for example, if
multiple summaries for `lm` or `glm` objects exist and a user wants the
summaries in a single table.

## Usage

``` r
ffcar_transfer_results_to_table(
  lst_results,
  dtf_table,
  obj_rows,
  obj_columns,
  obj_labels
)
```

## Arguments

- lst_results:

  A list with matrices containing the result summaries.

- dtf_table:

  A data frame, the table to add the results to.

- obj_rows:

  Either (a) an integer vector specifying the rows to index from each
  matrix in `lst_results`, or (b) if different rows should be extracted
  for each matrix, a list matching in length to `lst_results` with the
  specific rows to index per each matrix.

- obj_columns:

  Either (a) an integer vector specifying the columns to index from each
  matrix in `lst_results`, or (b) if different columns should be
  extracted for each matrix, a list matching in length to `lst_results`
  with the specific columns to index per each matrix.

- obj_labels:

  Either (a) a character vector specifying the columns to update in
  `dtf_table`, or (b) if different columns in `dtf_table` should be
  updated for each matrix in `lst_results`, a list matching in length to
  `lst_results` with the specific columns to update.

## Value

A data frame.

## Examples

``` r
# List of matrices with regression results
lst_results <- list(
  wt = summary( lm( mpg ~ wt, data = mtcars ) )$coefficients,
  cyl = summary( lm( mpg ~ cyl, data = mtcars ) )$coefficients,
  hp = summary( lm( mpg ~ hp, data = mtcars ) )$coefficients
)

dtf_table <- data.frame(
  Estimate = NA,
  SE = NA,
  P_value = NA
)

dtf_table <- ffcar_transfer_results_to_table(
  lst_results, dtf_table,
  obj_rows = 2, obj_columns = c(1:2, 4),
  obj_labels = c( 'Estimate', 'SE', 'P_value' )
)
#> Error in loadNamespace(x): there is no package called ‘checkmate’
```
