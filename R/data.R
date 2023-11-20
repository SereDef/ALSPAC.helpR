#' Little toy dataset for testing explore and clean functions
#'
#'
#' @format ## `toy_data`
#' A data frame with 5 rows and 3 columns:
#' \describe{
#'   \item{id}{Fake unique}
#'   \item{f0101, f0102, yikes123, kz021...}{Fake random variables}
#'   ...
#' }
#' @source '{created inhouse to serve as example}'
"toy_data"

#' Metadata file containing variable labels
#'
#'This was downloaded from https://variables.alspac.bris.ac.uk/ and adapted to
#'remove redundant information and include info about time points of measurement
#'when known.
#'
#' @format ## `alspac_metadata`
#' A data frame with 90.443 rows and 5 columns:
#' \describe{
#'   \item{name}{Varaible names}
#'   \item{lab}{Variable labels}
#'   \item{cat1}{Clinic, Quest or Other}
#'   \item{cat2}{Adult, Child, Child Based, Child Completed, Cohort Profile, COVID, Father,
#'   Geodata, Longitudinal, Mother, Obstetric, Partner, Puberty, Samples, Schools, Social_Class}
#' }
#' @source 'https://variables.alspac.bris.ac.uk/'
"alspac_metadata"
