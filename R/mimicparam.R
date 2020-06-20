#' A function that extracts list of MIMIC parameters to estimate
#'
#' This function takes the outputted lavaan MIMIC model and generates a reproducible table of output
#' @param fit.mimic fitted MIMIC model lavaan object
#' @return a list of mimic parameters
#' @export
#' @examples
#' mimic.param <- mimicparam(fit.mimic)

mimicparam <- function(fit.mimic){
  #Extract parameter estimates and their names
  ests <- as.data.frame(lavaan::parameterestimates(fit.mimic))
  uniqnames <- unique(ests$lhs)

  #Extract name of lv, covlv, and int
  lvname <- uniqnames[1]
  covlvname <- uniqnames[2]
  intname <- uniqnames[3]

  #Extract indicator names
  namenum <- length(uniqnames)
  indnum <- (namenum-4)/2
  indnamemax <- 3 + indnum
  indname <- uniqnames[4:indnamemax]

  #Specify parameters to estimate for DIF testing
  param <- list()
  for(i in 1:indnum){
    param[[i]] <- sprintf("%s ~ %s + %s", indname[[i]], covlvname, intname)
  }

  return(param)
}
