
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
#'
#' @return An R data.frame with variables as columns and observations as rows.
#' @export
#'
#' @examples load_alspac()
#' @examples load_alspac('~/ALSPAC_data_folder/ProjectName_DateRelease.sav')
#'
load_alspac <- function(filepath=NULL, lower.case=TRUE) {
  # File location
  if (is.null(filepath)) { filepath <- base::file.choose() }
  # datapath <- base::dirname(filepath)

  # Load the file
  message('Loading...')
  full <- foreign::read.spss(filepath, use.value.labels=FALSE,
                             to.data.frame=TRUE)
  message('Done!')
  cat('\n', base::nrow(full),' observations of ', base::ncol(full), ' variables.\n')

  # All column names to lower case
  if (lower.case) { names(full) <- base::tolower(names(full)) }

  return(full)
}
