#' Make a unique child identifier
#'
#' The ALSPAC datasets often lack a unique participant identifier. So let's make
#' one using information about the mother and sibling status.
#'
#' @param mom.id : The variable name of the (project specific) mother ID.
#' @param parity : (qlet) a two-level variable indicating parity (A or B).
#' @param data : (NULL) allows to specify the name of the dataset if this is
#' different than "data".
#'
#' @return The unique child ID in the numeric form. The last digit is a either a
#' 0 (for first born) or 1 (for second born)
#' @export
#'
#' @examples data$IDC <- make_idc(mom.id='yikes123') # Assumes data is stored in a data.frame called "data"
#' @examples data$IDC <- make_idc(mom.id='yikes123', data=alspac.data) # Specify the data.frame name yourself
#'
make_idc <- function(mom.id='cidb2957', parity='qlet', data=NULL) {
  # Check input
  if (base::is.null(data) &
      !base::exists('data', where=base::globalenv(), mode='list', inherits=FALSE)) {

    base::stop('Specify the dataset name, if this is different from "data"')

  } else if (base::is.null(data)) {

    data <- get("data", envir = .GlobalEnv) # get `data` from global enviroment
  }
  # Output: names in alphabetical order
  idc <- as.numeric(paste0(data[,mom.id], # mother ID
                           ifelse(gsub('\\s+', '', data[,sibling.id])=='A', 0,1)))

  return(idc)
}

# ------------------------------------------------------------------------------
#' Make a factor with levels 'male' and 'female' from character vector.
#'
#' @param sex.var : ('kz021') the name of the sex variable.
#' @param data : (NULL) allows to specify the name of the dataset if this is
#' different than "data".
#'
#' @return A factor with levels: 'male' and 'female'.
#' @export
#'
#' @examples data$sex <- make_sex_factor() # Assumes data is stored in a data.frame called "data"
#' @examples data$sex <- make_sex_factor(data=alspac.data) # Specify the data.frame name yourself
#'
make_sex_factor <- function(sex.var='kz021', data=NULL) {
  # Check input
  if (base::is.null(data) &
      !base::exists('data', where=base::globalenv(), mode='list', inherits=FALSE)) {

    base::stop('Specify the dataset name, if this is different from "data"')

  } else if (base::is.null(data)) {

    data <- base::get("data", envir = .GlobalEnv) # get `data` from global enviroment
  }
  # Output: names in alphabetical order
  sex <- base::as.factor(data[,sex.var])
  base::levels(sex) <- c('male','female')

  base::print(base::summary(sex))

  return(sex)
}

# ------------------------------------------------------------------------------
#' Remove siblings from the sample.
#'
#' The function requires either a unique child identifier constructed using
#' `make_idc()` or the mother ID and parity variable (used to call `make_idc()`
#' internally). The `method` parameter allows to control how the exclusion is done
#' and takes values: "random", "parity" and "missing" (see parameter description
#' for more details).
#'
#' @param idc : ('idc') the name of the IDC variable constructed using `make_idc()`.
#' @param method : ('random') controls how to perform the exclusion. If this is
#' set to `"random"` (default option) one sibling is randomly selected for each pair.
#' If `"parity"` is chosen, the oldest sibling is retained in the dataset. When
#' method is set to `"missing"`, the sibling with the lowest number of missing
#' values is retained. Missing rates are computed over the entire `data` dataset
#' or only over the columns specified in `column_selection`.
#' @param column_selection : ('all') specify which columns to use to compute missing
#' data rates (only used when method = "missing").
#' @param seed : specity a seed for random selection (only used when method = "random")
#' @param mom.id : The variable name of the (project specific) mother ID.
#' @param parity : (qlet) a two-level variable indicating parity (A or B).
#' @param data : (NULL) allows to specify the name of the dataset if this is
#' different than "data".
#'
#' @return A dataframe where only one sibling is left for each pair.
#' @export
#'
#' @examples data_nosibl <- rm_siblings(idc='IDC')
#' @examples data_nosibl <- rm_siblings(idc='myID', data=myData)
#' @examples data_nosibl <- rm_siblings(method='missing', column_selection=c('poteto','potato'))
#'
rm_siblings <- function(idc='idc', method = 'random', column_selection = 'all',
                        seed = 310896, mom.id='cidb2957', parity='qlet',
                        data=NULL) {
  # Check input
  if (base::is.null(data) &
      !base::exists('data', where=base::globalenv(), mode='list', inherits=FALSE)) {

    base::stop('Specify the dataset name, if this is different from "data"')

  } else if (base::is.null(data)) {

    data <- base::get("data", envir = .GlobalEnv) # get `data` from global enviroment
  }

  if (!idc %in% names(data) & !mom.id %in% names(data)) {

    base::stop(paste('Please provide at the correct child ID or mother ID.
    There are no variables called',idc,'or',mom.id,'in the dataset you provided.'))

  } else if (!idc %in% names(data)) {

    message('"',idc,'" not found in data, creating child ID variable: "',idc,'".')
    data[,idc] <- make_idc(mom.id=mom.id, sibling.id=sibling.id, data=data)
  }

  if (method=='parity') {

    # IDCs ending in 1 correspond to qlet=='B'
    bad_sib <- data[,idc][ which(data[,idc] %% 10 == 1) ]
    # remove only those that do have the older sibling in the sample
    bad_sib <- bad_sib[ !! bad_sib-1 %in% data[,idc] ]

  } else if (method=='random') {

    set.seed(seed)

    find_pair <- function(i) {
      if ((i %% 10 == 1) & (i-1 %in% data[,idc])) {
        return( sample(c(i-1, i),1) ) # randomly select one of the pair
      }
    }

    bad_sib <- do.call(c, lapply(data[,idc], find_pair))


  } else if (method=='missing') {

    # if no selection is specified, missingness in the entire dataframe is used
    if (column_selection=='all') { column_selection <- names(data)
    } else { column_selection <- c(idc, column_selection) }

    find_pair <- function(i) {
      if ((i %% 10 == 1) & (i-1 %in% data[,idc])) {

        sib_pair = data[ data[,idc] %in% c(i-1, i), column_selection ]
        # compute how many missing values in the columns of interest
        sib_pair$missing <- rowSums(is.na(sib_pair),na.rm = TRUE)

        if (nrow(sib_pair) > 1) {

          if (sib_pair$missing[1]==sib_pair$missing[2]) {
            return(sib_pair[ sample(c(1,2),1), idc])
          } else {
            return(sib_pair[ sib_pair$missing==min(sib_pair$missing), idc])

          }
        }
      }
    }

    bad_sib <- do.call(c, lapply(data[,idc], find_pair))

  }

  message('Removing ', length(bad_sib), ' siblings.')

  return(data[ ! data[,idc] %in% bad_sib, ])
}

# ------------------------------------------------------------------------------
#' Remove outliers from variable
#'
#' This function sets all values that are outside the range specified to NA.
#'
#' @param var : the variable name to be cleaned.
#' @param cutoff : (1.5) how many times the IQR should the upper and lower cutoff
#' be set (the larger the number, the more lenient the outlier removal)
#' @param verbose : (TRUE) print out the number of values below and above the cutoff.
#' @param data : (NULL) allows to specify the name of the dataset if this is
#' different than "data".
#'
#' @return The cleaned variable (with outlier values set to NA)
#' @export
#'
#' @examples data$cheese_clean <- rm_outliers('cheese')
#' @examples better_data$crackers_clean <- rm_outliers('crackers', data=better_data)
#' @examples data$unicorn <- rm_outliers('horses', cutoff=5, verbose=FALSE)
#'
rm_outliers <- function(var, cutoff = 1.5, verbose=TRUE, data=NULL) {

    # Check input
    if (base::is.null(data) &
        !base::exists('data', where=base::globalenv(), mode='list', inherits=FALSE)) {

      base::stop('Specify the dataset name, if this is different from "data"')

    } else if (base::is.null(data)) {

      data <- base::get("data", envir = .GlobalEnv) # get `data` from global enviroment
    }

    if (!var %in% names(data)) {

      base::stop('Wait, I cannot find variable "',var,'" in this dataset.')
    }

  # Compute IQR
  quants <- quantile(data[,var], probs=c(.25, .75), na.rm=TRUE)
  iqr <- quants[2] - quants[1]

  too_high <- which(data[,var] > quants[2] + cutoff*iqr)
  too_low  <- which(data[,var] < quants[1] - cutoff*iqr)

  data[,var][ c(too_high, too_low) ] <- NA

  if (verbose) {
    cat(length(too_low)+length(too_high), ' outliers removed from "',var,'" (',
        length(too_low), ' below and ', length(too_high), ' above the cutoff).\n', sep='')
  }

  return(data[,var])
}

