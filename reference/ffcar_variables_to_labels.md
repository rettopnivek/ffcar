# Convert Variable Names to Labels

Function to convert variable names (e.g., columns in a data frame,
predictors used in a regression) to nice labels.

## Usage

``` r
ffcar_variables_to_labels(
  chr_variables,
  chr_remove = NULL,
  chr_space = "ZZ",
  int_skip = -1
)
```

## Arguments

- chr_variables:

  A character vector, the variable names to convert to nice labels.

- chr_remove:

  An optional character vector, string patterns to remove. Defaults to
  `'PRD'`, `'OUT'`, and `'COL'`.

- chr_space:

  A character string, the pattern to replace with an empty space.

- int_skip:

  An optional integer vector with values from 1 to 18 indicating
  specific patterns to skip.

## Value

A character vector of nicely formatted labels.

## Examples

``` r
# Parentheses
ffcar_variables_to_labels( 'PRDGroupZZOPPTreatmentCLP' )
#> [1] "Group (Treatment)"
# Square brackets
ffcar_variables_to_labels( 'PRDGroupZZOPBTreatmentCLB' )
#> [1] "Group [Treatment]"
# Hyphen
ffcar_variables_to_labels( 'PRDShortHPHterm' )
#> [1] "Short-term"
# Colon
ffcar_variables_to_labels( 'PRDGroupCLNZZTreatment' )
#> [1] "Group: Treatment"
# Semicolon
ffcar_variables_to_labels( 'PRDGroupSMCZZTreatment' )
#> [1] "Group; Treatment"
# Pipe symbol
ffcar_variables_to_labels( 'PRDPOPPAPPEBCLP' )
#> [1] "P(A|B)"
# Plus, minus, and equal signs
ffcar_variables_to_labels( 'PRDXZZEQLZZYZZPLSZZZ' )
#> [1] "X = Y + Z"
# Hash sign
ffcar_variables_to_labels( 'PRDHSHZZofZZcounts' )
#> [1] "# of counts"
# Less than and greater than signs
ffcar_variables_to_labels( 'COLXZZLSSZZYZZAMPZZYZZGRTZZZ' )
#> [1] "X < Y & Y > Z"
# Less than/Greater than or equal to signs
ffcar_variables_to_labels( 'COLXZZLTEZZYZZAMPZZYZZGTEZZZ' )
#> [1] "X ≤ Y & Y ≥ Z"
```
