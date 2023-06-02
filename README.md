## ffcar: Functions for Creating Analysis Reports

The R package **ffcar** provides a collection of functions to help create a basic analysis report using Rmarkdown and the R package [flextable](https://cran.r-project.org/web/packages/flextable/index.html).

### Table of contents

1. [Prerequisites](#section01)
2. [Installation](#section02)
3. [Usage](#section03)

### Prerequisites {#section01}

- [R (version >= 4.2.1)](https://cran.r-project.org/)
- [flextable (version >= 0.9.0)](https://cran.r-project.org/web/packages/flextable/index.html)
- For easy installation it is recommended to have [devtools (version >= 2.4.3)](https://devtools.r-lib.org/)

### Installation {#section02}

```r
# Install development version from Github
devtools::install_github("rettopnivek/ffcar")
```

### Usage {#section03}

We can load in the package to easily access its functions:
```r
library(ffcar)
```

The **ffcar_flextable** function allows one to quickly create a basic APA-style table using **flextable** with minimal formatting effort:
```r
# Example data set
data(mtcars)
dtf_example <- mtcars[1:4, 1:3]

# Create APA-style table
obj_flex <- ffcar_flextable(
  dtf_example,
  c( 'Miles per gallon', 'Cylinders', 'Displacement' )
)
obj_flex
```

The **ffcar_reformat_columns** function allows one to combine and format summary results into a new column:
```r
# Example data set
data(anscombe) # Anscombe's quartet
# Fit linear model
obj_lm_fit <- lm(
  y1 ~ x1, data = anscombe
)
# Summary of results
mat_summary <- summary( obj_lm_fit )$coefficients
dtf_summary <- data.frame( Term = c( 'Intercept', 'Slope' ) )
# Create nicely formatted column
dtf_summary$Test <- ffcar_reformat_columns(
  mat_summary, c(1:2, 4),
 '[[GLb]] = [[C1]] (SE = [[C2]]), p = [[C3]]',
 int_digits = c( 2, 2, 3 )
)
dtf_summary
```

The **ffcar_transfer_results_to_table** function makes it easier to combine results across different analyses into a single table:
```r
# List of matrices with regression results
lst_results <- list(
  A1 = summary( lm( y1 ~ x1, data = anscombe ) )$coefficients,
  A2 = summary( lm( y2 ~ x2, data = anscombe ) )$coefficients,
  A3 = summary( lm( y3 ~ x3, data = anscombe ) )$coefficients,
  A4 = summary( lm( y4 ~ x4, data = anscombe ) )$coefficients
)

# Initialize table
dtf_table <- data.frame(
  Estimate = NA,
  SE = NA,
  P_value = NA
)

# Extract the 2nd row and columsn 1, 2, and 4 from each analysis summary
dtf_table <- ffcar_transfer_results_to_table(
  lst_results, dtf_table,
  obj_rows = 2, obj_columns = c(1:2, 4),
  obj_labels = c( 'Estimate', 'SE', 'P_value' )
)

# Anscombe's classic result showing how 
# very different data can produce very 
# similar regression results
dtf_table
```

The **ffcar_design_table** function provides a flexible way to create a table summarizing different subsets of a data set, which is especially useful for creating sample characteristics tables:
```r
# Create summary table
dtf_table <- ffcar_design_table(
  # Data set
  mtcars,
  # Subsets of data (determines # of columns)
  list(
    Overall = rep( TRUE, nrow(mtcars) ),
    Cylinders_4 = mtcars$cyl %in% 4,
    Cylinders_6 = mtcars$cyl %in% 6,
    Cylinders_8 = mtcars$cyl %in% 8
  ),
  # Rows
  # list( Label, Column, Summary, Digits, Category )
  list( 'Sample size',
        'mpg', '[[N]]' ),
  list( 'Miles per gallon; M (SD)',
        'mpg', '[[M]] ([[SD]])', 1 ),
  list( 'Weight (10[[SP3]] lbs); M (SD)',
        'wt', '[[M]] ([[SD]])', 1 ),
  list( 'Transmission; % automatic (N)',
        'am', '[[P]]% ([[C]])', 1, 0 )
)
dtf_table[,-(2:5)]
```

The functions **ffcar_conditional_statement** and **ffcar_sub_in_content** can be used to create text content that subs in the results of tests and conditionally adapts the content based on the results (useful for constructing reports that can easily be updated if the data changes):
```r
# Test for different in MPG by car cylinder
lst_ttest <- t.test(
  mtcars$mpg[mtcars$cyl %in% 4], 
  mtcars$mpg[mtcars$cyl %in% 6]
)

chr_results <- paste0(
  '4-cylinder cars [[V1]] significantly more miles to the gallon ',
  'compared to 6-cylinder cars, t([[V2]]) = [[V3]], p = [[V4]].'
)
chr_values <- c(
  V1 = ffcar_conditional_statement(
    # Check for significant p-values and 
    # lower MPG for 4-cylinder cars
    lst_ttest$p.value < .05 & diff(lst_ttest$estimate) < 0, 
    c( 
    'did not get', # If not significant or wrong direction
    'got' # If significant and right direction
    )
  ),
  V2 = round( as.numeric( lst_ttest$parameter ) ), # Degrees of freedom
  V3 = round( as.numeric( lst_ttest$statistic ), 2 ), # Test statistic
  V4 = format( round( lst_ttest$p.value, 3 ), nsmall = 3 )
)
chr_results <- ffcar_sub_in_content( chr_results, chr_values )
chr_results
```

