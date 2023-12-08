#' Dichotomize (and rename) groups of variables
#'
#' @param vars : A data.frame with the original variable names as comun names and
#' the name you want to assign to the new variables as frst row (see example)
#' @param yes : A vector of labels or values to be recoded as 1
#' @param no : A vector of labels or values to be recoded as 0
#' @param verbose : (TRUE) print table detailing the transformation.
#' @param data : (NULL) allows to specify the name of the dataset if this is
#' different than "data".
#'
#' @return A data.frame with dichotomized variables
#' @export
#'
#' @examples
#' \dontrun{
#' LE <- dichotomize(
#'  vars = data.frame('b570'='partner_died', b571'='child_died'),
#'  yes = c('affected a lot','fairly affected','mildly affected','N effect at all'),
#'  no = c('No')
#'  )
#' }
#'
dichotomize <- function(vars, yes = c('1'), no = c('0'), verbose=TRUE, data=NULL) {

  # Check input
  if (base::is.null(data) &
      !base::exists('data', where=base::globalenv(), mode='list', inherits=FALSE)) {

    base::stop('Specify the dataset name, if this is different from "data"')

  } else if (base::is.null(data)) {

    data <- base::get("data", envir = .GlobalEnv) # get `data` from global enviroment
  }

  if (!base::exists('metadata', where=base::globalenv(), inherits=FALSE)) {
    metadata <- alspac_metadata[alspac_metadata$name %in% names(data), ]
  }


  dset <- data.frame(data[, names(vars)])  # subset columns from data.frame
  names(dset) <- names(vars) # make sure this works for single variable inputs too
  answ = c(yes, no)

  # Check if required levels are present / unexpected levels are present
  for (i in names(vars)) {
    dset[,i] <- droplevels(as.factor(dset[,i]))
    lvls = levels(dset[,i])

    # if (!verbose) {
    #   if (length(setdiff(lvls, answ)) > 0) {   # unexpected levels are present
    #     message('Column ', i, ' has these unexpected values: ', setdiff(lvls, answ), '. These will be recoded as NA.')
    #   } else if (length(setdiff(answ, lvls)) > 0) {  # expected levels are not present
    #     message('Column ', i, ' has no registered "', setdiff(answ, lvls), '" answers.')
    #   }
    # }
    # Recoding
    var.out = vars[,i]

    # perform the recoding
    dset[,var.out] <- ifelse(dset[,i] %in% no, 0, ifelse(dset[,i] %in% yes, 1, NA))

    # Check continuous vs. binary columns
    if (verbose) {
      cat('\n', i, ' --> ',var.out, '  # ', metadata[metadata$name==i, 'lab'])
      summ <- stats::addmargins(base::table(dset[,i], dset[,var.out], useNA = 'ifany'),
                                margin=1,
                                FUN=list(list(Total = sum,
                                              Percent = function(x){ paste0(round((sum(x)/nrow(dset))*100),'%') } )))
      print(summ)
    }
  }

  # Return dataframe with dichotomized values
  out <- as.data.frame(dset[, which(names(dset) %in% vars)])
  names(out) <- vars # note this makes sure it works for single variable inputs
  return(out)
}

# ------------------------------------------------------------------------------
#' Dichotomize (and rename) groups of continuous variables
#'
#' @param vars : A data.frame with the original variable names as comun names and
#' the name you want to assign to the new variables as frst row (see example)
#' @param rule : A string specifying the rule / cutoff for assigning a value of 1.
#' @param quantile : (FALSE) whether the cutoff should be specified in quantiles or
#' original scale (default uses the original variable scale)
#' @param verbose : (TRUE) print table detailing the transformation.
#' @param data : (NULL) allows to specify the name of the dataset if this is
#' different than "data".
#'
#' @return A data.frame with dichotomized variables
#' @export
#'
#' @examples
#' \dontrun{
#' np <- dichotomize_cont(
#'   vars=data.frame('g496'='neighbourhood_problems_21m', h366'='neighbourhood_problems_3y'),
#    rule='>= .8',
#'   quantile=TRUE)
#'}
dichotomize_cont <- function(vars, rule = '', quantile=FALSE, verbose=TRUE, data=NULL) {

  # Check input
  if (base::is.null(data) &
      !base::exists('data', where=base::globalenv(), mode='list', inherits=FALSE)) {

    base::stop('Specify the dataset name, if this is different from "data"')

  } else if (base::is.null(data)) {

    data <- base::get("data", envir = .GlobalEnv) # get `data` from global enviroment
  }

  if (!base::exists('metadata', where=base::globalenv(), inherits=FALSE)) {
    metadata <- alspac_metadata[alspac_metadata$name %in% names(data), ]
  }

  dset <- data.frame(data[, names(vars)])  # subset columns from data.frame
  names(dset) <- names(vars) # make sure this works for single variable inputs too

  # Check if variable is a factor
  for (i in names(vars)) {
    if (is.factor(dset[,i])){

      dset[,i] <- droplevels(dset[,i])
      dset[,i] <- suppressWarnings(as.numeric(levels(dset[,i]))[dset[,i]]) # same as as.numeric(as.charachter(f)) but more efficient
    }

    if (quantile) {
      rule_vector <- unlist(strsplit(rule, ' ')) # Assuming eg '< .8'
      q = as.numeric(rule_vector[length(rule_vector)]) # last element
      # Compute qth percentile value
      cutoff <- stats::quantile(dset[,i], q, na.rm = T)
      # update rule with real cutoff
      nrule <- paste(rule_vector[1], cutoff)
    } else { nrule <- rule }

    # Recoding
    var.out <- vars[,i]

    # perform the recoding
    dset[,var.out] <- ifelse(eval(parse(text=paste('dset[,i]', nrule))), 1, 0)

    # Check continuous vs. binary columns
    if (verbose) {
      cat('\n', i, ' --> ',var.out, '  # ', metadata[metadata$name==i, 'lab'])
      if (quantile) { cat('Note: cutoff = ',cutoff, ' (i.e. quantile = ',q,')', sep='') }
      summ <- stats::addmargins(base::table(dset[,i], dset[,var.out], useNA = 'ifany'),
                                margin=1,
                                FUN=list(list(Total = sum,
                                              Percent = function(x){ paste0(round((sum(x)/nrow(dset))*100),'%') } )))
      print(summ)
    }
  }

  # Return dataframe with dichotomized values
  out <- as.data.frame(dset[, which(names(dset) %in% vars)])
  names(out) <- vars # note this makes sure it works for single variable inputs
  return(out)
}
