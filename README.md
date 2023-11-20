
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ALSPAC.helpR

<!-- badges: start -->
<!-- badges: end -->

The goal of this little package is to help with loading, cleaning and
exploring datasets from the ALSPAC cohort.

## Installation

You can install the development version of ALSPAC.helpR from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("SereDef/ALSPAC.helpR")
```

## Load dataset

``` r
library(ALSPAC.helpR)

## basic example code
data <- load_alspac() # Opens interactive window to choose file
# Note: by default, the function transforms all variable names to lowercase (set lower.case=FALSE if this is not desired)
```

## Explore dataset

``` r
# Find all variables in "data" whose names start with "f01"
find_var('f01',  method='starts')
# OR, to save some typing 
fv('f01',  method='starts')

# Find all the variable labels that mention "income"
find_lab('income')
# OR, to save some typing 
fl('income')

# Seelect some variables
select_var('depre', times=c(1,3)) # returns, for example c('depre_1','depre_3')
# OR, to save some typing 
sel('depre')
```

## Clean dataset

``` r
# Create unique identifier
data$IDC <- make_idc(mom.id='id123') 
# Create sex factor
data$sex <- make_sex_factor()

# Remove one of the sibling couple randomly
data_nosibl <- rm_siblings(method='random')

# Remove outliers 
data$horses <- rm_outliers('unicorns', cutoff=3.5)
```
