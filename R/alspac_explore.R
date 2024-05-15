#' Find out which variables are in the dataset.
#'
#' ALSPAC data likely comes in large dataframes containing many columns.
#' This function helps with quickly finding out which variables are present.
#'
#' @param s : partial string to search for (understands regular expressions).
#' @param data : (NULL) allows to specify the name of the dataset if this is
#' different than "data".
#' @param method : ("contains") how to search: possible values: "starts", "ends","contains" (default).
#' Alternative way to control search with out using regular expressions.
#' @param print.labels : (TRUE) whether to print the metadata (i.e. variable names, labels and categories)
#' of selected variables in the console.
#' @param to.data.frame : (FALSE; only used when `print.labels=TRUE`) whether to return a
#' dataframe containing the metadata of selected variables.
#'
#' @return Prints column names in `data` that contain `pref` (and their labels and categories if print.labels==TRUE).
#' @export
#'
#' @examples find_var('c2', ALSPAC.helpR::alspac_toy_data) # Specify the data.frame name yourself

find_var <- function(s, data=NULL, method='contains', print.labels=TRUE, to.data.frame=FALSE) {

  # Adjust regex based on method input
  if (method=='starts') { s <- paste0('^',s)
  } else if (method=='ends') { s <- paste0(s,'$') }

  # Check input
  if (base::is.null(data) &
      !base::exists('data', where=base::globalenv(), mode='list', inherits=FALSE)) {

    base::message('ATTENTION: You did not specify a dataset!
                  Searching among all ALSPAC variables (but keep in mind that some
                  may not be available to you)')

    meta <- alspac_metadata[grepl(s, alspac_metadata$name, ignore.case = TRUE), ]
    row.names(meta) <- NULL
    # Return dataframe or print to console
    return(meta)

  } else if (base::is.null(data)) {

    data <- get("data", envir = .GlobalEnv) # get `data` from global enviroment
  }

  # Output: names in alphabetical order
  var.names <- sort(names(data)[grep(s, names(data), ignore.case = TRUE)])

  # Print labels
  if (print.labels) {
    # Lowercase also the names in the metadata file
    # Identify variables in the set that do not have metadata and add them as empty rows.
    no_label <- setdiff(base::tolower(var.names), base::tolower(alspac_metadata$name))
    if (length(no_label) > 0) {
      filler <- rep(NA,length(no_label))
      no_label_rows <- data.frame('name'=no_label,
                                  'lab'=rep('',length(no_label)),
                                  'cat1'=filler,
                                  'cat2'=filler,
                                  'median_age'=filler,
                                  'age_range'=filler,
                                  'age_variable'=filler)
      alspac_metadata <- rbind(alspac_metadata, no_label_rows)
    }

    # Subset metadata
    meta <- alspac_metadata[grepl(paste0('^', paste(var.names, collapse='$|^'),'$'),
                                  alspac_metadata$name, ignore.case = TRUE), ]
    row.names(meta) <- NULL
    # Return dataframe or print to console
    if (to.data.frame) { return(meta) } else { print(meta) }

  } else { return(var.names) }

}

#' @rdname find_var
#' @examples fv('^c2', ALSPAC.helpR::alspac_toy_data)
#' @examples data <- ALSPAC.helpR::alspac_toy_data
#' fv('^c2') # Assumes data is stored in a data.frame called "data"
#' @export
fv <- find_var

# ------------------------------------------------------------------------------
#' Search the variable labels are in the dataset.
#'
#' This function helps searching through the sea of concepts and measurements available
#' (based on variable labels).
#'
#' @param s : partial string to search for (understands regular expressions).
#' @param data : (NULL) allows to specify the name of the dataset if this is
#' different than "data".
#' @param case.sensitive : (FALSE) whether search should be case sensitive.
#' @param to.data.frame : (FALSE) whether to return a
#' dataframe containing the metadata of selected variables.
#'
#' @return Prints variable names and labels that match the search.
#' @export
#'
#' @examples find_lab('income')
#' @examples find_lab('PREG', case.sensitive=TRUE)

find_lab <- function(s, data=NULL, case.sensitive=FALSE, to.data.frame=FALSE) {
  # Check input
  if (base::is.null(data) &
      !base::exists('data', where=base::globalenv(), mode='list', inherits=FALSE)) {

    message('No dataset specified. Searching among all ALSPAC variables (but keep in mind that some may not be available to you)')

    # Output
    meta <- alspac_metadata[grep(s, alspac_metadata$lab, ignore.case = !case.sensitive), ]
    row.names(meta) <- NULL

    return(meta)

  } else if (base::is.null(data)) {

    data <- get("data", envir = .GlobalEnv) # get `data` from global enviroment
  }

  # Remove metadata for variables not in the set
  alspac_metadata <- alspac_metadata[alspac_metadata$name %in% names(data), ]

  meta <- alspac_metadata[grep(s, alspac_metadata$lab, ignore.case = !case.sensitive), ]
  row.names(meta) <- NULL

  # Return dataframe or print to console
  if (to.data.frame) {
    return(meta)

  } else {
    print(meta)
    return(meta$name)
  }

}

#' @rdname find_lab
#' @examples fl('insulin')
#' @export
fl <- find_lab

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
#' select_var('depre') # returns, for example c('depre_1','depre_2','depre_3','depresion_total')
#' select_var('depre', times=c(1,3)) # returns, for example c('depre_1','depre_3')
#'}
select_var <- function(var.names, times=NULL, sep='_', data=NULL){
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

#' @rdname select_var
#' @examples
#'\dontrun{
#'sel('alcohol', times=c(18, 24.5)) # returns c('alcohol_18years','alcohol_24.5years')
#'}
#' @export
sel <- select_var
