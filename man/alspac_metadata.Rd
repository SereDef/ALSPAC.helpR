% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{alspac_metadata}
\alias{alspac_metadata}
\title{Metadata file containing variable labels}
\format{
\subsection{\code{alspac_metadata}}{

A data frame with 90.443 rows and 7 columns:
\describe{
\item{name}{Varaible names}
\item{lab}{Variable labels}
\item{cat1}{Clinic, Quest or Other}
\item{cat2}{Adult, Child, Child Based, Child Completed, Cohort Profile, COVID, Father,
Geodata, Longitudinal, Mother, Obstetric, Partner, Puberty, Samples, Schools, Social_Class}
\item{median_age}{Median age at variable measurement}
\item{age_range}{Age range at variable measurement}
\item{age_variable}{Variable name of the age variable used to extract median and range age}
}
}
}
\source{
'https://variables.alspac.bris.ac.uk/'
}
\usage{
alspac_metadata
}
\description{
This was downloaded from https://variables.alspac.bris.ac.uk/ and adapted to
remove redundant information and include info about time points of measurement
when known. See data-raw/alspac_metadata_ages.R
}
\keyword{datasets}
