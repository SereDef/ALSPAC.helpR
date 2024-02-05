#' Little toy dataset for testing explore and clean functions
#'
#'
#' @format ## `alspac_toy_data`
#' A data frame with 500 rows and 500 columns:
#' \describe{
#'   \item{cidb1234}{Fake mother id}
#'   \item{qlet}{Fake sibling id}
#'   \item{a237, a257, a258, a271, a274...}{Fake random variables}
#'   ...
#' }
#' @source '{created inhouse to serve as example, see data-raw/alspac_toy_data.R}'
"alspac_toy_data"

#' Metadata file containing variable labels
#'
#'This was downloaded from https://variables.alspac.bris.ac.uk/ and adapted to
#'remove redundant information and include info about time points of measurement
#'when known. See data-raw/alspac_metadata_ages.R
#'
#' @format ## `alspac_metadata`
#' A data frame with 90.443 rows and 7 columns:
#' \describe{
#'   \item{name}{Varaible names}
#'   \item{lab}{Variable labels}
#'   \item{cat1}{Clinic, Quest or Other}
#'   \item{cat2}{Adult, Child, Child Based, Child Completed, Cohort Profile, COVID, Father,
#'   Geodata, Longitudinal, Mother, Obstetric, Partner, Puberty, Samples, Schools, Social_Class}
#'   \item{median_age}{Median age at variable measurement}
#'   \item{age_range}{Age range at variable measurement}
#'   \item{age_variable}{Variable name of the age variable used to extract median and range age}
#' }
#' @source 'https://variables.alspac.bris.ac.uk/'
"alspac_metadata"
