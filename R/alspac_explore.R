#' Find out which variables are in the dataset.
#'
#' ALSPAC data likely comes in large dataframes containing many columns.
#' This function helps with quickly finding out which variables are present.
#'
#' @param pref : partial string to search for (understands regular expressions).
#' @param data : (NULL) allows to specify the name of the dataset if this is
#' different than "data".
#'
#' @return Column names in `data` that contain `pref`.
#' @export
#'
#' @examples f('f01') # Assumes data is stored in a data.frame called "data"
#' @examples f('f01', alspac.data) # Specify the data.frame name yourself

f <- function(pref, data=NULL) {
  # Check input
  if (base::is.null(data) &
      !base::exists('data', where=base::globalenv(), mode='list', inherits=FALSE)) {

    base::stop('Specify the dataset name, if this is different from "data"')

  } else if (base::is.null(data)) {

    data <- get("data", envir = .GlobalEnv) # get `data` from global enviroment
  }
  # Output: names in alphabetical order
  var.names <- sort(names(data)[grep(pref, names(data))])

  print(var.names)
  return(var.names)
}
