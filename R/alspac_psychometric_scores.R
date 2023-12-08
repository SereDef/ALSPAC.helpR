#' Compute Crown - Crisp scale anxiety score (CCEI)
#'
#' @param sev1 : A vector of variable names with severity 1 (see CCEI manual)
#' @param sev2 : A vector of variable names with severity 2 (see CCEI manual)
#' @param sev3 : A vector of variable names with severity 3 (see CCEI manual)
#' @param asnumeric : (FALSE) return the score as.numeric. Default behavior returns
#' a factor because that is the typical structure in ALSPAC data.
#' @param verbose : (TRUE) print table detailing the transformation.
#' @param data : (NULL) allows to specify the name of the dataset if this is
#' different than "data".
#'
#' @return The scale score.
#' @export
#'
#' @examples
#' \dontrun{
#' data$m_anx_5y <- ccei_score(
#'   sev1 = c('k3000', 'k3014', 'k3016'),
#'   sev2 = c('k3002', 'k3005', 'k3011', 'k3019'),
#'   sev3 = c('k3008'))
#' }
#'
ccei_score <- function(sev1, sev2, sev3, asnumeric=FALSE, verbose = TRUE, data = NULL) {

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


  vars <- c(sev1, sev2, sev3)
  dset <- data[, vars] # select relevant columns from alspac.table

  for (v in vars) {
    var.out = paste0(v, '_recoded') # create new (recoded) variable name

    # perform the recoding
    if (v %in% sev1) { not_very_often_value <- 0
    } else if (v %in% sev2) { not_very_often_value <- 1
    } else if (v %in% sev3) { not_very_often_value <- 2 }

    dset[,var.out] <- ifelse(dset[,v] %in% c('Very often','Often'), 2,
                             ifelse(dset[,v]=='Not very often', not_very_often_value,
                                    ifelse(dset[,v]=='Never',0, NA)))

    if (verbose) {
      cat('\n', metadata[metadata$name==v, 'lab'], '\n', sep='')
      print(table(dset[,c(v,var.out)], useNA = 'ifany')) }

  }

  sumscore <- rowSums(dset[,grep('_recoded',names(dset))], na.rm = FALSE)

  cat('\nTotal score:\n')
  print(summary(sumscore))

  if (asnumeric) { return(sumscore) } else { return(as.factor(sumscore)) }
}

# ------------------------------------------------------------------------------

#' Compute EPDS total depression score
#'
#' @param set : A vector of variable names coded as (1 = 0) (2 = 1) (3 = 2) (4 = 3)
#' (see EPDS manual)
#' @param revset : A vector of variable names coded as (1 = 3) (2 = 2) (3 = 1) (4 = 0)
#' (see EPDS manual)
#' @param asnumeric : (FALSE) return the score as.numeric. Default behavior returns
#' a factor because that is the typical structure in ALSPAC data.
#' @param verbose : (TRUE) print table detailing the transformation.
#' @param data : (NULL) allows to specify the name of the dataset if this is
#' different than "data".
#'
#' @return The scale score.
#' @export
#'
#' @examples
#' \dontrun{
#' data$p_dep_5y <- epds_score(
#'   set = c('ph3030','ph3031','ph3033'),
#'   revset = c('ph3032','ph3034','ph3035','ph3036','ph3037','ph3038','ph3039'))
#' }
#'
epds_score <- function(set, revset, asnumeric=FALSE, verbose = TRUE, data = NULL) {

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


  vars <- c(set, revset)
  # recode variables as integer, so that labels would become numerical values
  dset <- data.frame(sapply(data[, vars], as.integer))

  for (v in vars) {
    var.out = paste0(v, '_recoded') # create new (recoded) variable name
    # perform the recoding
    if (v %in% set) { tr = min(dset[, v], na.rm = T)
    } else if (v %in% revset) { tr = max(dset[, v], na.rm = T) }

    dset[,var.out] <- abs(dset[, v] - tr)

    if (verbose) {
      cat('\n', metadata[metadata$name==v, 'lab'], '\n', sep='')
      print(table(dset[,c(v,var.out)], useNA = 'ifany')) }
  }

  sumscore <- rowSums(dset[,grep('_recoded',names(dset))], na.rm = FALSE)

  cat('\nTotal score:\n')
  print(summary(sumscore))

  if (asnumeric) { return(sumscore) } else { return(as.factor(sumscore)) }
}
