#' Make a unique child identifier
#'
#' The ALSPAC datasets often lack a unique participant identifier. So let's make
#' one using information about the mother and sibling status.
#'
#' @param mom.id : The variable name of the (project specific) mother ID.
#' @param sibling.id : (qlet) a two-level variable indicating parity (A or B).
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
make_idc <- function(mom.id='cidb2957', sibling.id='qlet', data=NULL) {
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

  message('Note: ', unname(summary(as.factor(data[,sibling.id]))[2]),
          ' siblings in the sample.')

  return(idc)
}

# make_sex
# rm_siblings

# Remove outliers
# rm_outliers <- function(var, cutoff = 5, verbose=TRUE) {
#   quants <- quantile(var, probs=c(.25, .75), na.rm=TRUE);
#   iqr <- quants[2]-quants[1]
#   too_high <- which(var > quants[2]+cutoff*iqr)
#   too_low  <- which(var < quants[1]-cutoff*iqr)
#   var[ c(too_high, too_low) ] <- NA
#   if (verbose){
#     # message(names(var))
#     cat(length(too_low)+length(too_high), ' outliers removed (',
#         length(too_low), ' below and ', length(too_high), ' above the cutoff).\n', sep='')
#   }
#   return(var)
# }

