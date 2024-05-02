
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ALSPAC.helpR

<!-- badges: start -->
<!-- badges: end -->

The goal of the
[**`ALSPAC.helpR`**](https://github.com/SereDef/ALSPAC.helpR) package is
to collect a few functions that help you load, clean, explore and
manipulate datasets. These were developed and may come in handy when
working with ALSPAC data.

This is a quick tutorial to help you get started with
**`ALSPAC.helpR`**.

## Installation

You can install the development version of **`ALSPAC.helpR`** from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("SereDef/ALSPAC.helpR")
```

    ## Downloading GitHub repo SereDef/ALSPAC.helpR@HEAD

    ## ── R CMD build ─────────────────────────────────────────────────────────────────
    ## * checking for file ‘/private/var/folders/f9/fknlq25n23l74zmhg8zbckx80000gn/T/Rtmpep069H/remotes44dd11fe80d/SereDef-ALSPAC.helpR-ffd5d78/DESCRIPTION’ ... OK
    ## * preparing ‘ALSPAC.helpR’:
    ## * checking DESCRIPTION meta-information ... OK
    ## * checking for LF line-endings in source and make files and shell scripts
    ## * checking for empty or unneeded directories
    ## * building ‘ALSPAC.helpR_0.0.0.9000.tar.gz’

    ## Installing package into '/private/var/folders/f9/fknlq25n23l74zmhg8zbckx80000gn/T/Rtmp0wP3wT/temp_libpath4253325f5ed'
    ## (as 'lib' is unspecified)

``` r
# Lets load it up 
library(ALSPAC.helpR)
```

## 1. Load the data

First thing you usually need to do is *read* the dataset in memory. So
meet our first function: **`load_alspac()`** that can do a few useful
things:

- reads SPSS (`.sav`) format
- typically, almost 16.000 rows and maybe also loads of columns
  (~10.000), so give it a minute, it will let you know you when it is
  done.

<u>Defaults</u>:

- opens interactive window unless `filepath` is specified
- `lower.case = TRUE` all column names are lower cased (also the
  metadata)
- `keep.value.labels = TRUE` let’s talk about the format of ALSPAC data
  (and SPSS)
- `load.metadata = FALSE` metadata is lazy by default

<u>Tip</u>: call the dataset “`data`”, you’ll see why in a minute…

``` r
# ?load_alspac

data <- load_alspac( # filepath = NULL, 
  lower.case = TRUE,
  keep.value.labels = TRUE,
  load.metadata = TRUE
)
```

#### The metadata file

This is just a dataframe taken from the ALSPAC website (aka what you see
on the [variable search
tool](https://variables.alspac.bris.ac.uk/?_gl=1*1il3dnn*_ga*NzkyNjg3OTQ3LjE3MDcyMTY2ODA.*_ga_6R8SPL3HLT*MTcwNzIxNjY4MC4xLjAuMTcwNzIxNjY4MC42MC4wLjA.&_ga=2.28553148.1407887277.1707216680-792687947.1707216680))
whith some additional information included in it (e.g. age at
measurement). The package uses this file in the background, but you can
also load it in memory if you wish.

Why having this loaded in your R session?

- it is filtered for the variables that you have in the dataset (only
  look through what you have access to)
- you want to inspect the (entire) metadata available in the Viewer
- you want to use metadata information (e.g., variable labels or
  measurement ages) in your script

``` r
?alspac_metadata

# No data file available, just want to inspect _all_ data?
# No problem 
all_metadata <- ALSPAC.helpR::alspac_metadata
```

## 2. Explore dataset

OK, we’ve got the data, let’s do something fun with it now.

Whether you load metadata explicitly or not, you can use it to explore
the dataset through two functions: **`find_var()`** and **`find_lab()`**

- They each have a **shortcut**, because I fully endorse lazy typing:
  `fv()` and `fl()`
- Also if you named your dataset “data” (like I told you to 👀) you win
  extra non-typing time (or you can use the `data` argument otherwise)
- They take a string, and understand *regex* (but can also use the
  `method` argument if you don’t remember regex syntax, possible values:
  “starts”, “ends”, “contains”)
- `print.labels = FALSE` will return a vector of variable names (that
  you can use, for example, for transforming) rather than the metadata
  dataframe

``` r
# ?find_var

# Find all variables names in "data" whose names start with "c2"
find_var('c2', 
         # data = data,
         method='starts', 
         print.labels = TRUE,
         to.data.frame = FALSE)
```

``` r
# OR, to save some typing 
fv('^b')
```

Searching for concepts (aka the labels)

``` r
# ?find_lab
# Find all the variable labels that mention "income"
find_lab('job', 
         case.sensitive = FALSE)
```

``` r
# OR, to save some typing 
fl('depression$')
```

You can also select variables you computed or renamed (more in the docs)

``` r
# Select some variables with common root
select_var('depre', times=c(1,3)) # returns, for example c('depre_1','depre_3')

# OR, to save some typing 
sel('depre')
```

## 3. Clean dataset

#### Create a unique identifier

> <u>Attention</u> what looks like the id variable is **not** a unique
> identifier! You need to make this by combining the `cidb` variable
> with `qlet`. Can do just that with **`make_idc()`**

- same as before, if your data is called “data” that saves some typing

``` r
?make_idc
# Create unique identifier
cbind(idc = make_idc(mom.id='cidb1234'), data)

# If you don't care about the position
# data$IDC <- make_idc(mom.id='cidb1234') 
```

#### Select siblings

Often you want to exclude related individuals (i.e. one of the two) from
the analyses **`rm_siblings()`** helps with that

- it looks for a unique identifier (called `"idc"` by default) of the
  type `make_idc()` outputs. If he doesn’t find it, calls `make_idc()`
  internally (using the `mom.id` and `parity` optional variable names)
- `method` is the most useful argument I think, it allows to control who
  to exclude. Default is **“random”** = one sibling is randomly selected
  for each pair. **“parity”** = the oldest sibling is retained in the
  dataset. **“missing”** = the sibling with the lowest number of missing
  values is retained. Missing rates are computed over the entire data
  dataset or only over the columns specified in `column_selection`.
- as usual, “data” is the default name of the dataframe

``` r
# ?rm_siblings
# Remove one of the sibling couple randomly
data_nosibl <- rm_siblings(
  # idc = "idc",
  method = "missing",
  column_selection = "all",
  # seed = 310896,
  # mom.id = "cidb1234",
  # parity = "qlet",
  # data = NULL
)
# establish rank of importance
```

#### Remove outliers

``` r
# ?rm_outliers

# Remove outliers 
data$horses <- rm_outliers('unicorns', cutoff=3.5)

# Winsorise 
# COMING UP 
```

## 4. Scoring, recoding and other fun stuff

- `dichotomize()`

- `dichotomize_cont()`

- `ccei_score()`

- `epds_score()`

## Future development plans

- include winsorize function
- include SMFQ score computation
- include sleep data handling function
