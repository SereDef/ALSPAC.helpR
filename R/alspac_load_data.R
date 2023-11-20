
#' Load the .sav file into R data frame.
#'
#' By default, the function will open a window that you can use to navigate to
#' the file's location. Select the correct file and hit `Select`. Alternatively,
#' you can specify the file full path as a string.
#' By default, the column names of the output data.frame are lower-cased to
#' avoid confusion.
#'
#' @param filepath : (NULL) the full path of the `.sav` file containing the data.
#' @param lower.case : (TRUE) whether column names should be lower-cased.
#' @param keep.value.labels : (TRUE) mantain the string levels of factors. This is
#' internally calls `foreign::read.spss(..., use.value.labels=TRUE)`.
#' @param load.metadata : (FALSE) whether to also load the metadata file (e.g. variable labels)
#' explicitly. If TRUE a dataframe called "metadata" is created in your global environment.
#' By default, metadata is only used in the background for the exploring functions.
#'
#' @return An R data.frame with variables as columns and observations as rows.
#' @export
#'
#' @examples
#'\dontrun{
#' load_alspac()
#' load_alspac(lower.case=FALSE) # Keep original variable names
#' load_alspac('~/ALSPAC_data_folder/ProjectName_DateRelease.sav')
#'}
load_alspac <- function(filepath=NULL, lower.case=TRUE, keep.value.labels=TRUE,
                        load.metadata=FALSE) {
  # File location
  if (is.null(filepath)) { filepath <- base::file.choose() }
  # datapath <- base::dirname(filepath)

  # Load the file
  message('Loading: ',filepath,' ...')
  full <- foreign::read.spss(filepath,
                             use.value.labels=keep.value.labels,
                             to.data.frame=TRUE)
  cat('Done!', base::nrow(full),' observations of ', base::ncol(full), ' variables.\n')

  # All column names to lower case
  if (lower.case) {
    names(full) <- base::tolower(names(full))
    # Lowercase also the names in the metadata file
    alspac_metadata$name <- base::tolower(alspac_metadata$name)
  }

  # Remove metadata for variables not in the set
  alspac_metadata <- alspac_metadata[alspac_metadata$name %in% names(full), ]
  # Identify variables in the set that do not have metadata and add them as empty rows.
  no_label <- setdiff(names(full), alspac_metadata$name)
  if (length(no_label) > 0) {
    no_label_rows <- data.frame('name'=no_label,
                                'lab'=rep('',length(no_label)),
                                'cat1'=rep(NA,length(no_label)),
                                'cat2'=rep(NA,length(no_label)))
    alspac_metadata <- rbind(alspac_metadata, no_label_rows)
  }

  # Add metadata
  if (load.metadata) {
    message('Loading metadata...')
    base::assign("metadata", alspac_metadata,  envir = .GlobalEnv)
    cat('Done! Note: I found ', length(no_label), ' varariables that do not have metadata information.\nYou can maybe find more information about their definition in the Data Dictionary PDFs downloadable from the ALSPAC website.')
  }

  return(full)
}
