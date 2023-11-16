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
#' @examples find_alspac('f01', toy_data) # Specify the data.frame name yourself

find_alspac <- function(pref, data=NULL) {
  # Check input
  if (base::is.null(data) &
      !base::exists('data', where=base::globalenv(), mode='list', inherits=FALSE)) {

    base::stop('Specify the dataset name, if this is different from "data"')

  } else if (base::is.null(data)) {

    data <- get("data", envir = .GlobalEnv) # get `data` from global enviroment
  }
  # Output: names in alphabetical order
  var.names <- sort(names(data)[grep(pref, names(data))])

  return(var.names)
}

#' @rdname find_alspac
#' @examples f('f01', toy_data)
#' @examples data <- toy_data
#' f('f01') # Assumes data is stored in a data.frame called "data"
#' @export
f <- find_alspac

# ------------------------------------------------------------------------------
#' Quickly select groups of variables
#'
#' @param var.names : string or vector of (partial) variable names to match.
#' @param times : (NULL) if variables have repeated measures, this argument is
#' used to select only specific times. This can be a string or a vector. It
#' assumes the variable names contain time indication.
#' @param sep : ('_') the character separating the time indication in the
#' variable name (only used if `times` is specified)
#' @param data : (NULL) allows to specify the name of the dataset if this is
#' different than "data".
#'
#' @return Vector of variable names that match the search.
#' @export
#'
#' @examples
#'\dontrun{
#' select_alspac('depre') # returns, for example c('depre_1','depre_2','depre_3','depresion_total')
#' select_alspac('depre', times=c(1,3)) # returns, for example c('depre_1','depre_3')
#'}
select_alspac <- function(var.names, times=NULL, sep='_', data=NULL){
  # Check input
  if (base::is.null(data) &
      !base::exists('data', where=base::globalenv(), mode='list', inherits=FALSE)) {

    base::stop('Specify the dataset name, if this is different from "data"')

  } else if (base::is.null(data)) {

    data <- get("data", envir = .GlobalEnv) # get `data` from global enviroment
  }

  subs = names(data)[grep(paste(var.names, collapse='|'), names(data))]

  if (!is.null(times)) { subs = subs[grep(paste(paste0(sep,times), collapse='|'), subs)] }

  return(subs)
}

#' @rdname select_alspac
#' @examples
#'\dontrun{
#'sel('alcohol', times=c(18, 24.5)) # returns c('alcohol_18years','alcohol_24.5years')
#'}
#' @export
sel <- select_alspac
