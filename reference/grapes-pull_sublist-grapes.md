# Operator to Index Sublists in a List

Operator that returns the content of a sublist at a specified level for
a list of lists.

## Usage

``` r
lst_lists %pull_sublist% obj_levels
```

## Arguments

- obj_levels:

  Either a character vector with the names for the sublists or an
  integer vector with the indices.

- lst_content:

  A list of lists.

## Value

The element(s) in `lhs` whose names contain the `rhs` character string.

## Examples

``` r
# List with 1 nested level
lst <- list(
  SL1 = list( A = 1, B = 2, C = 3 ),
  SL2 = list( A = 4, B = 5, C = 6 )
)
# Extract element 'A' from each sublist
lst %pull_sublist% 'A'
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> [1] 4
#> 
# Extract 1st element from each sublist
lst %pull_sublist% 1
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> [1] 4
#> 
# Extract element 'B' from each sublist
lst %pull_sublist% 'B'
#> [[1]]
#> [1] 2
#> 
#> [[2]]
#> [1] 5
#> 
# Extract 2nd element from each sublist
lst %pull_sublist% 2
#> [[1]]
#> [1] 2
#> 
#> [[2]]
#> [1] 5
#> 

# List with two nested levels
lst <- list(
  SL1 = list(
    SL11 = list( A = 1, B = 2 ),
    SL12 = list( A = 3, B = 4 )
  ),
  SL2 = list(
    SL21 = list( A = 5, B = 6 ),
    SL22 = list( A = 7, B = 8 )
  )
)
# Extract 1st element from level 1 (SLX1)
lst %pull_sublist% 1
#> [[1]]
#> [[1]]$A
#> [1] 1
#> 
#> [[1]]$B
#> [1] 2
#> 
#> 
#> [[2]]
#> [[2]]$A
#> [1] 5
#> 
#> [[2]]$B
#> [1] 6
#> 
#> 
# Extract 1st element from level 2 (SLX2[[1]])
lst %pull_sublist% c( 2, 1 )
#> [[1]]
#> [1] 3
#> 
#> [[2]]
#> [1] 7
#> 
```
